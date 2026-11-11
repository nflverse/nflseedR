#include "engine/status.hpp"

#include <algorithm>
#include <bit>
#include <functional>
#include <optional>

#include "engine/flow.hpp"
#include "engine/season.hpp"

namespace nflseedr {

namespace {

// Does a check need competitors strictly behind (or ahead of) X, or is a tie
// in win percentage enough?
enum class Tie : int8_t { kStrict, kWeak };

// Answer to "can X reach the goal?" or "can X miss it?"
enum class Answer : int8_t { kUnknown, kYes, kNo };

// Who wins a tie in win percentage with X, by the sweep rule
enum class TieWinner : int8_t { kUnknown, kX, kOthers };

// Set of team ids as bits
using TeamMask = uint32_t;
static_assert(kTeams <= 32);

[[nodiscard]] constexpr TeamMask team_bit(TeamId t) noexcept {
  return TeamMask{1} << static_cast<uint32_t>(t);
}

// Calls f(t) for every team in `mask`
template <typename F>
void for_each_team(TeamMask mask, F f) {
  while (mask != 0) {
    f(static_cast<TeamId>(std::countr_zero(mask)));
    mask &= mask - 1;
  }
}

// Calls f(subset) for every subset of `mask` with `size` teams until f
// returns true. True if it did.
template <typename F>
[[nodiscard]] bool any_subset(TeamMask mask, int32_t size, F f) {
  for (TeamMask s = mask;; s = (s - 1) & mask) {
    if (std::popcount(s) == size && f(s)) return true;
    if (s == 0) return false;
  }
}

// Played games and remaining games of one season
struct Season {
  SeasonData played;
  const League* league = nullptr;
  std::span<const Game> remaining;
  TeamArray<int32_t> left{};
  TeamMatrix<int32_t> left_vs{};
  // Games at the end of the season
  TeamArray<int32_t> final_games{};
  int32_t wild_cards = 0;

  void build(std::span<const Game> played_games,
             std::span<const Game> remaining_games, const League& lg,
             int32_t seeds) {
    played.build(played_games, lg, false, true);
    league = &lg;
    remaining = remaining_games;
    left = {};
    left_vs = {};
    for (const Game& g : remaining) {
      ++left[g.away];
      ++left[g.home];
      ++left_vs[g.away][g.home];
      ++left_vs[g.home][g.away];
    }
    for (TeamId t = 0; t < kTeams; ++t) {
      final_games[t] = played.games[t] + left[t];
    }
    wild_cards = seeds - kDivisions / kConferences;
  }

  [[nodiscard]] bool present(TeamId t) const noexcept {
    return final_games[t] > 0;
  }

  // All games between a and b are played and a won every one
  [[nodiscard]] bool swept(TeamId a, TeamId b) const noexcept {
    return played.h2h_games[a][b] > 0 && left_vs[a][b] == 0 &&
           played.h2h_half_wins[a][b] == 2 * played.h2h_games[a][b];
  }
};

// Win totals (half-wins) when X wins out ("can reach" checks) or loses out
// ("can miss" checks), and the flows on top of them.
class Check {
 public:
  Check(const Season& s, TeamId x, bool x_wins_out) : s_(&s), x_(x) {
    for (TeamId y = 0; y < kTeams; ++y) {
      const int32_t vs_x = s.left_vs[x][y];
      current_[y] = s.played.half_wins[y] + (x_wins_out ? 0 : 2 * vs_x);
      maximum_[y] = current_[y] + 2 * (s.left[y] - vs_x);
    }
    x_half_wins_ = s.played.half_wins[x] + (x_wins_out ? 2 * s.left[x] : 0);
  }

  [[nodiscard]] int32_t current(TeamId y) const { return current_[y]; }
  [[nodiscard]] int32_t maximum(TeamId y) const { return maximum_[y]; }

  // Most half-wins y can end with and stay behind X (strict) or not get
  // ahead (weak). Compares final win percentages, so game counts can differ.
  [[nodiscard]] int32_t limit(TeamId y, Tie tie) const {
    const int64_t num = scaled(y);
    const int64_t den = s_->final_games[x_];
    const int64_t below = num / den;
    const bool exact = num % den == 0;
    return static_cast<int32_t>(tie == Tie::kStrict && exact ? below - 1
                                                             : below);
  }

  // Fewest half-wins y needs to get ahead of X (strict) or not stay behind
  // (weak)
  [[nodiscard]] int32_t target(TeamId y, Tie tie) const {
    const int64_t num = scaled(y);
    const int64_t den = s_->final_games[x_];
    const int64_t below = num / den;
    const bool exact = num % den == 0;
    return static_cast<int32_t>(tie == Tie::kWeak && exact ? below : below + 1);
  }

  // y can end with exactly X's win percentage
  [[nodiscard]] bool can_tie(TeamId y) const {
    const int64_t num = scaled(y);
    const int64_t den = s_->final_games[x_];
    if (num % den != 0) return false;
    const int64_t h = num / den;
    return h >= current_[y] && h <= maximum_[y];
  }

  // y can get ahead of X (strict) or tie X (weak) on its own
  [[nodiscard]] bool can_pass(TeamId y, Tie tie) const {
    return target(y, tie) <= maximum_[y];
  }

  // Can every team in `capped` stay behind X (strict) or not get ahead
  // (weak) at once? Teams outside `capped` take any wins.
  [[nodiscard]] bool cap_feasible(TeamMask capped, Tie tie,
                                  MaxFlow& flow) const {
    TeamArray<int32_t> room{};
    bool ok = true;
    for_each_team(capped, [&](TeamId y) {
      room[y] = limit(y, tie) - current_[y];
      if (room[y] < 0) ok = false;
    });
    if (!ok) return false;
    // Only games between two capped teams constrain anything: a game against
    // any other team can go to that team.
    int32_t n_games = 0;
    for (const Game& g : s_->remaining) {
      if (g.away != x_ && g.home != x_ && (capped & team_bit(g.away)) != 0 &&
          (capped & team_bit(g.home)) != 0) {
        ++n_games;
      }
    }
    if (n_games == 0) return true;
    flow.reset(kFirstGameNode + n_games);
    for_each_team(
        capped, [&](TeamId y) { flow.add_edge(team_node(y), kSink, room[y]); });
    int32_t node = kFirstGameNode;
    for (const Game& g : s_->remaining) {
      if (g.away == x_ || g.home == x_ || (capped & team_bit(g.away)) == 0 ||
          (capped & team_bit(g.home)) == 0) {
        continue;
      }
      add_game(flow, node++, g, 2, 2);
    }
    return flow.run(kSource, kSink) == 2 * n_games;
  }

  // Can every team in `pushed` get ahead of X (strict) or tie X (weak) at
  // once? Wins nobody needs can go anywhere.
  [[nodiscard]] bool push_feasible(TeamMask pushed, Tie tie,
                                   MaxFlow& flow) const {
    TeamArray<int32_t> need{};
    TeamMask needy = 0;
    int32_t total = 0;
    bool ok = true;
    for_each_team(pushed, [&](TeamId y) {
      need[y] = target(y, tie) - current_[y];
      if (need[y] > maximum_[y] - current_[y]) ok = false;
      if (need[y] > 0) {
        needy |= team_bit(y);
        total += need[y];
      }
    });
    if (!ok) return false;
    if (total == 0) return true;
    auto counts = [&](const Game& g) {
      return g.away != x_ && g.home != x_ &&
             ((needy & team_bit(g.away)) != 0 ||
              (needy & team_bit(g.home)) != 0);
    };
    const auto n_games =
        static_cast<int32_t>(std::ranges::count_if(s_->remaining, counts));
    flow.reset(kFirstGameNode + n_games);
    for_each_team(
        needy, [&](TeamId y) { flow.add_edge(team_node(y), kSink, need[y]); });
    int32_t node = kFirstGameNode;
    for (const Game& g : s_->remaining) {
      if (!counts(g)) continue;
      add_game(flow, node++, g, (needy & team_bit(g.away)) != 0 ? 2 : 0,
               (needy & team_bit(g.home)) != 0 ? 2 : 0);
    }
    return flow.run(kSource, kSink) == total;
  }

 private:
  // Flow nodes: source, sink, one per team, then one per game
  static constexpr int32_t kSource = 0;
  static constexpr int32_t kSink = 1;
  static constexpr int32_t kFirstGameNode = 2 + kTeams;
  [[nodiscard]] static constexpr int32_t team_node(TeamId t) noexcept {
    return 2 + t;
  }

  // A game hands out 2 half-wins (win 2/0, tie 1/1)
  static void add_game(MaxFlow& flow, int32_t node, const Game& g,
                       int32_t to_away, int32_t to_home) {
    flow.add_edge(kSource, node, 2);
    if (to_away > 0) flow.add_edge(node, team_node(g.away), to_away);
    if (to_home > 0) flow.add_edge(node, team_node(g.home), to_home);
  }

  // X's final half-wins scaled to y's final games: y is level with X at
  // exactly scaled(y) / final_games[x] half-wins
  [[nodiscard]] int64_t scaled(TeamId y) const {
    return static_cast<int64_t>(x_half_wins_) * s_->final_games[y];
  }

  const Season* s_;
  TeamId x_;
  TeamArray<int32_t> current_{};
  TeamArray<int32_t> maximum_{};
  int32_t x_half_wins_ = 0;
};

// Answer from the strict and weak version of a check. Strict feasible proves
// yes and weak infeasible proves no, whatever the tiebreakers say. In between
// it depends on who wins ties with X. `ties_help` is the tie winner that
// makes the answer yes.
[[nodiscard]] Answer decide(const std::function<bool(Tie)>& feasible,
                            TieWinner ties, TieWinner ties_help) {
  if (feasible(Tie::kStrict)) return Answer::kYes;
  if (!feasible(Tie::kWeak)) return Answer::kNo;
  if (ties == TieWinner::kUnknown) return Answer::kUnknown;
  return ties == ties_help ? Answer::kYes : Answer::kNo;
}

[[nodiscard]] Status to_status(Answer reach, Answer miss) {
  if (reach == Answer::kNo && miss == Answer::kNo) {
    throw EngineError("A team can neither reach nor miss a goal");
  }
  if (reach == Answer::kNo) return Status::kEliminated;
  if (miss == Answer::kNo) return Status::kClinched;
  if (reach == Answer::kYes && miss == Answer::kYes) return Status::kAlive;
  return Status::kUndecided;
}

// All checks of one team X
class TeamStatus {
 public:
  TeamStatus(const Season& s, TeamId x, MaxFlow& flow)
      : s_(&s), x_(x), flow_(&flow), reach_(s, x, true), miss_(s, x, false) {
    const League& league = *s.league;
    for (TeamId y = 0; y < kTeams; ++y) {
      if (y == x || !s.present(y) ||
          league.conference[y] != league.conference[x]) {
        continue;
      }
      conference_ |= team_bit(y);
      if (league.division[y] == league.division[x]) rivals_ |= team_bit(y);
      const int32_t d = league.division[y];
      const std::span<const int32_t> seen =
          std::span(divisions_).first(n_divisions_);
      if (std::ranges::find(seen, d) == seen.end()) {
        divisions_[n_divisions_++] = d;
      }
    }
    std::ranges::sort(std::span(divisions_).first(n_divisions_));
  }

  // Status for one goal. The goals are nested (No. 1 seed => division title
  // => playoff berth), so an answer the checks of one goal leave open can
  // come from another goal.
  [[nodiscard]] Status status(Goal goal) {
    Answers a = answers(goal);
    for (const Goal other :
         {Goal::kFirstSeed, Goal::kDivision, Goal::kPlayoffs}) {
      if (other == goal) continue;
      const bool wider = nesting(other) > nesting(goal);
      if (a.reach == Answer::kUnknown) {
        const Answer r = answers(other).reach;
        // Can't reach a wider goal => can't reach this one; can reach a
        // narrower goal => can reach this one
        if (wider && r == Answer::kNo) a.reach = Answer::kNo;
        if (!wider && r == Answer::kYes) a.reach = Answer::kYes;
      }
      if (a.miss == Answer::kUnknown) {
        const Answer m = answers(other).miss;
        if (wider && m == Answer::kYes) a.miss = Answer::kYes;
        if (!wider && m == Answer::kNo) a.miss = Answer::kNo;
      }
    }
    return to_status(a.reach, a.miss);
  }

 private:
  struct Answers {
    Answer reach = Answer::kUnknown;
    Answer miss = Answer::kUnknown;
  };

  [[nodiscard]] static constexpr int32_t nesting(Goal goal) noexcept {
    switch (goal) {
      case Goal::kFirstSeed:
        return 0;
      case Goal::kDivision:
        return 1;
      case Goal::kPlayoffs:
        return 2;
    }
    return 0;
  }

  // Answers of the checks of one goal, computed once
  [[nodiscard]] Answers answers(Goal goal) {
    std::optional<Answers>& cached = answers_[static_cast<size_t>(goal)];
    if (!cached) {
      switch (goal) {
        case Goal::kDivision:
          cached = division();
          break;
        case Goal::kPlayoffs:
          cached = playoffs();
          break;
        case Goal::kFirstSeed:
          cached = first_seed();
          break;
      }
    }
    return *cached;
  }

  [[nodiscard]] Answers division() {
    return {
        .reach = decide(
            [&](Tie tie) { return reach_.cap_feasible(rivals_, tie, *flow_); },
            tie_winner(reach_, rivals_), TieWinner::kX),
        .miss = decide([&](Tie tie) { return any_can_pass(rivals_, tie); },
                       tie_winner(miss_, rivals_), TieWinner::kOthers)};
  }

  // X is the No. 1 seed iff X is ahead of every other conference team: a
  // team ahead of X puts its division winner ahead of X too.
  [[nodiscard]] Answers first_seed() {
    return {
        .reach = decide(
            [&](Tie tie) {
              return reach_.cap_feasible(conference_, tie, *flow_);
            },
            tie_winner(reach_, conference_), TieWinner::kX),
        .miss = decide([&](Tie tie) { return any_can_pass(conference_, tie); },
                       tie_winner(miss_, conference_), TieWinner::kOthers)};
  }

  // X makes the playoffs iff no rival is ahead of X (division title), or at
  // most wild_cards - 1 teams that don't win their division are ahead of X.
  [[nodiscard]] Answers playoffs() {
    return {.reach = decide(
                [&](Tie tie) {
                  return reach_.cap_feasible(rivals_, tie, *flow_) ||
                         reach_wild_card(tie);
                },
                tie_winner(reach_, conference_), TieWinner::kX),
            .miss = decide([&](Tie tie) { return miss_playoffs(tie); },
                           tie_winner(miss_, conference_), TieWinner::kOthers)};
  }

  // Sweep rule (dev/playoff-status.md): if X swept every team of `teams` that
  // can tie X, X wins every tie among them. If they all swept X, X loses.
  [[nodiscard]] TieWinner tie_winner(const Check& check, TeamMask teams) const {
    bool x_swept_all = true;
    bool all_swept_x = true;
    for_each_team(teams, [&](TeamId y) {
      if (!check.can_tie(y)) return;
      x_swept_all = x_swept_all && s_->swept(x_, y);
      all_swept_x = all_swept_x && s_->swept(y, x_);
    });
    if (x_swept_all) return TieWinner::kX;
    if (all_swept_x) return TieWinner::kOthers;
    return TieWinner::kUnknown;
  }

  [[nodiscard]] bool any_can_pass(TeamMask teams, Tie tie) const {
    bool any = false;
    for_each_team(teams,
                  [&](TeamId y) { any = any || miss_.can_pass(y, tie); });
    return any;
  }

  [[nodiscard]] TeamMask division_teams(int32_t d) const {
    TeamMask out = 0;
    for_each_team(conference_, [&](TeamId y) {
      if (s_->league->division[y] == d) out |= team_bit(y);
    });
    return out;
  }

  // X wins out and gets a wild card: try every largest allowed set of teams
  // ahead of X (1 + e_d teams per division, sum of e_d = wild_cards - 1) and
  // cap everyone else.
  [[nodiscard]] bool reach_wild_card(Tie tie) {
    // Per division: teams that are always ahead of X (must be allowed) and
    // teams that may or may not be. The others can never get ahead.
    std::array<TeamMask, kDivisions> forced{};
    std::array<TeamMask, kDivisions> open{};
    for (int32_t i = 0; i < n_divisions_; ++i) {
      for_each_team(division_teams(divisions_[i]), [&](TeamId y) {
        const int32_t limit = reach_.limit(y, tie);
        if (reach_.current(y) > limit) {
          forced[i] |= team_bit(y);
        } else if (reach_.maximum(y) > limit) {
          open[i] |= team_bit(y);
        }
      });
    }
    std::function<bool(int32_t, int32_t, TeamMask)> search =
        [&](int32_t i, int32_t budget, TeamMask capped) {
          if (i == n_divisions_) {
            return reach_.cap_feasible(capped, tie, *flow_);
          }
          const int32_t n_forced = std::popcount(forced[i]);
          const int32_t n_open = std::popcount(open[i]);
          const bool last = i == n_divisions_ - 1;
          for (int32_t e = last ? budget : 0; e <= budget; ++e) {
            const int32_t slots = 1 + e;
            if (n_forced > slots) continue;
            const int32_t allowed = std::min(slots - n_forced, n_open);
            if (any_subset(open[i], allowed, [&](TeamMask ahead) {
                  return search(i + 1, budget - e, capped | (open[i] & ~ahead));
                })) {
              return true;
            }
            // More slots than teams: larger e gives the same sets
            if (allowed == n_open) break;
          }
          return false;
        };
    return search(0, s_->wild_cards - 1, 0);
  }

  // X loses out and misses the playoffs: try every smallest set of teams
  // ahead of X (at least one rival, wild_cards teams that don't win their
  // division) and push them all ahead.
  [[nodiscard]] bool miss_playoffs(Tie tie) {
    // Per division: teams already ahead of X and teams that can get there
    std::array<TeamMask, kDivisions> already{};
    std::array<TeamMask, kDivisions> able{};
    std::array<bool, kDivisions> own{};
    for (int32_t i = 0; i < n_divisions_; ++i) {
      own[i] = divisions_[i] == s_->league->division[x_];
      for_each_team(division_teams(divisions_[i]), [&](TeamId y) {
        if (miss_.current(y) >= miss_.target(y, tie)) {
          already[i] |= team_bit(y);
        } else if (miss_.can_pass(y, tie)) {
          able[i] |= team_bit(y);
        }
      });
    }
    std::function<bool(int32_t, int32_t, TeamMask)> search =
        [&](int32_t i, int32_t budget, TeamMask pushed) {
          if (i == n_divisions_) {
            return budget == 0 && miss_.push_feasible(pushed, tie, *flow_);
          }
          const bool last = i == n_divisions_ - 1;
          for (int32_t f = last ? budget : 0; f <= budget; ++f) {
            // f teams ahead of X besides the division winner. X's division
            // always needs its winner ahead of X.
            const int32_t size = own[i] || f > 0 ? 1 + f : 0;
            const int32_t n_already = std::popcount(already[i]);
            if (n_already + std::popcount(able[i]) < size) continue;
            // Teams already ahead cost nothing, so use them first
            const int32_t extra = std::max(size - n_already, 0);
            if (any_subset(able[i], extra, [&](TeamMask more) {
                  return search(i + 1, budget - f, pushed | more);
                })) {
              return true;
            }
          }
          return false;
        };
    return search(0, s_->wild_cards, 0);
  }

  const Season* s_;
  TeamId x_;
  MaxFlow* flow_;
  Check reach_;
  Check miss_;
  TeamMask rivals_ = 0;
  TeamMask conference_ = 0;
  // Divisions of X's conference
  std::array<int32_t, kDivisions> divisions_{};
  int32_t n_divisions_ = 0;
  std::array<std::optional<Answers>, kGoals> answers_{};
};

}  // namespace

StatusRows compute_clinching_status(const GamesInput& played,
                                    const GamesInput& remaining,
                                    const League& league,
                                    const StatusOptions& options,
                                    const InterruptCheck& interrupt) {
  check_league(league);
  if (played.n_sims != remaining.n_sims ||
      options.seeds.size() != static_cast<size_t>(played.n_sims)) {
    throw EngineError("Need the same seasons for all inputs");
  }
  const GamesBySeason played_by_season = group_games(played);
  const GamesBySeason remaining_by_season = group_games(remaining);
  for (const Game& g : remaining_by_season.sorted) {
    if (g.type != GameType::kRegular) {
      throw EngineError("Remaining games must be regular season games");
    }
  }
  constexpr int32_t kDivisionsPerConference = kDivisions / kConferences;
  for (const int32_t seeds : options.seeds) {
    if (seeds <= kDivisionsPerConference || seeds > kTeamsPerConference) {
      throw EngineError("Invalid number of playoff seeds");
    }
  }

  StatusRows out;
  Season season;
  MaxFlow flow;
  for (int32_t s = 1; s <= played.n_sims; ++s) {
    interrupt();
    season.build(played_by_season.season(s), remaining_by_season.season(s),
                 league, options.seeds[s - 1]);
    for (TeamId x = 0; x < kTeams; ++x) {
      if (!season.present(x)) continue;
      out.sim.push_back(s);
      out.team.push_back(x);
      out.wins.push_back(season.played.true_wins[x]);
      out.losses.push_back(season.played.losses[x]);
      out.ties.push_back(season.played.ties[x]);
      out.games_left.push_back(season.left[x]);
      TeamStatus team(season, x, flow);
      for (int32_t goal = 0; goal < kGoals; ++goal) {
        out.status[goal].push_back(
            options.goals[goal]
                ? static_cast<int32_t>(team.status(static_cast<Goal>(goal)))
                : kMissing);
      }
    }
  }
  return out;
}

}  // namespace nflseedr
