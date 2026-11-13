#include "engine/status.hpp"

#include <algorithm>
#include <bit>
#include <functional>
#include <limits>
#include <optional>

#include "engine/flow.hpp"
#include "engine/scenario.hpp"
#include "engine/season.hpp"
#include "engine/tie_proof.hpp"

namespace nflseedr {

namespace {

// Runs of the procedures per completion (one per coin toss sequence)
constexpr int32_t kTossBudget = 64;
// Runs for the final standings once no games are left. Enough to try every
// coin toss sequence in practice, which makes the answers exact.
constexpr int32_t kSeasonOverBudget = 4096;
// Orders of the greedy tightening in the gray zone
constexpr int32_t kTighteningOrders = 4;

// Does a check need competitors strictly behind (or ahead of) X, or is a tie
// in win percentage enough?
enum class Tie : int8_t { kStrict, kWeak };

// Answer to "can X reach the goal?" or "can X miss it?"
enum class Answer : int8_t { kUnknown, kYes, kNo };

// Ranking that breaks ties with X: the division (division title), the
// conference (No. 1 seed) or the wild cards (playoffs)
enum class Ranking : int8_t { kDivision, kConference, kWildCard };

[[nodiscard]] constexpr TeamMask team_bit(TeamId t) noexcept {
  return TeamMask{1} << static_cast<uint32_t>(t);
}

[[nodiscard]] constexpr bool has(TeamMask mask, TeamId t) noexcept {
  return (mask & team_bit(t)) != 0;
}

// Strictly behind (ahead of) X for teams in `strict`, possibly tied else
[[nodiscard]] Tie tie_of(TeamMask strict, TeamId y) noexcept {
  return has(strict, y) ? Tie::kStrict : Tie::kWeak;
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

// Results of a completion that no check constrains
constexpr int8_t kFree = -1;
constexpr int8_t kHomeWin = 2;
constexpr int8_t kTie = 1;
constexpr int8_t kAwayWin = 0;

// Parts of the season the search for an open answer may check (P12)
constexpr int32_t kSearchBudget = 200;
// Budgets of the checks of one team per question. Parts of the search get
// small ones: the search itself splits the season further.
struct Budgets {
  // Completions tried in the gray zone
  int32_t scenarios = 32;
  // Flows for the tie groups at X's level (P11)
  int32_t group_flows = 200;
  // Tie groups that stay open before giving up
  int32_t open_groups = 16;
};
constexpr Budgets kSearchBudgets{
    .scenarios = 2, .group_flows = 30, .open_groups = 2};
// Late in the season the tie groups get more flows: few teams are still
// open, and that's when teams clinch
constexpr int32_t kLateGames = 4;
constexpr int32_t kLateGroupFlows = 2000;
// A single season in the input gets enough for the tie groups of early No. 1
// seed questions (up to 15 open teams). More flows settle nothing more; with
// many seasons they would take too long (P14).
constexpr Budgets kSingleSeasonBudgets{
    .group_flows = 20000, .open_groups = std::numeric_limits<int32_t>::max()};
// Interrupt checks during the search, every this many parts
constexpr int32_t kInterruptEvery = 64;
// Splitting on X's own games comes last: the checks already fix them
constexpr int32_t kOwnGamePenalty = 10;

[[nodiscard]] Completion filled(Completion c, int8_t free_result) {
  std::ranges::replace(c, kFree, free_result);
  return c;
}

// An answer and, for yes, the completion that proves it. `coin_toss` is
// true if a coin toss was needed in that completion.
struct Proof {
  Answer answer = Answer::kUnknown;
  std::optional<Completion> scenario;
  bool coin_toss = false;
};

struct Answers {
  Proof reach;
  Proof miss;
};

// Played games and remaining games of one season
struct Season {
  SeasonData played;
  std::span<const Game> played_games;
  const League* league = nullptr;
  std::span<const Game> remaining;
  TeamArray<int32_t> left{};
  TeamMatrix<int32_t> left_vs{};
  // Games at the end of the season
  TeamArray<int32_t> final_games{};
  int32_t seeds = 0;
  int32_t wild_cards = 0;

  void build(std::span<const Game> played_games_in,
             std::span<const Game> remaining_games, const League& lg,
             int32_t n_seeds) {
    played.build(played_games_in, lg, false, true);
    played_games = played_games_in;
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
    seeds = n_seeds;
    wild_cards = seeds - kDivisions / kConferences;
  }

  [[nodiscard]] bool present(TeamId t) const noexcept {
    return final_games[t] > 0;
  }

  // Did X reach the goal in these final ranks?
  [[nodiscard]] bool reached(Goal goal, TeamId x, const RankResult& div,
                             const RankResult& conf) const {
    switch (goal) {
      case Goal::kDivision:
        return div.rank[x] == 1;
      case Goal::kPlayoffs:
        return conf.rank[x] != kMissing && conf.rank[x] <= seeds;
      case Goal::kFirstSeed:
        return conf.rank[x] == 1;
    }
    return false;
  }
};

// Win totals (half-wins) when X wins out ("can reach" checks) or loses out
// ("can miss" checks), and the flows on top of them.
class Check {
 public:
  Check(const Season& s, TeamId x, bool x_wins_out)
      : s_(&s), x_(x), x_wins_out_(x_wins_out) {
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

  // Can every team in `capped` stay behind X at once? Teams in `strict`
  // must be strictly behind, the others may tie X. Teams outside `capped`
  // take any wins. If feasible and `out` is given, writes a completion
  // (games that don't matter are kFree).
  [[nodiscard]] bool cap(TeamMask capped, TeamMask strict, MaxFlow& flow,
                         Completion* out) const {
    TeamArray<int32_t> room{};
    bool ok = true;
    for_each_team(capped, [&](TeamId y) {
      room[y] = limit(y, tie_of(strict, y)) - current_[y];
      if (room[y] < 0) ok = false;
    });
    if (!ok) return false;
    // Only games between two capped teams constrain anything: a game against
    // any other team can go to that team.
    auto counts = [&](const Game& g) {
      return !involves_x(g) && has(capped, g.away) && has(capped, g.home);
    };
    const auto n_games =
        static_cast<int32_t>(std::ranges::count_if(s_->remaining, counts));
    std::vector<int32_t> home_edge;
    if (n_games > 0) {
      flow.reset(kFirstGameNode + n_games);
      for_each_team(capped, [&](TeamId y) {
        flow.add_edge(team_node(y), kSink, room[y]);
      });
      int32_t node = kFirstGameNode;
      for (const Game& g : s_->remaining) {
        home_edge.push_back(counts(g) ? add_game(flow, node++, g, 2, 2).home
                                      : -1);
      }
      if (flow.run(kSource, kSink) != 2 * n_games) return false;
    }
    if (out != nullptr) {
      Completion& c = *out;
      c.assign(s_->remaining.size(), kFree);
      for (size_t i = 0; i < c.size(); ++i) {
        const Game& g = s_->remaining[i];
        if (involves_x(g)) {
          c[i] = x_result(g);
        } else if (counts(g)) {
          c[i] = static_cast<int8_t>(flow.flow(home_edge[i]));
        } else if (has(capped, g.away)) {
          c[i] = kHomeWin;
        } else if (has(capped, g.home)) {
          c[i] = kAwayWin;
        }
      }
    }
    return true;
  }

  // Can every team in `pushed` get ahead of X at once? Teams in `strict`
  // must get strictly ahead, the others only need to tie X. Wins nobody needs
  // can go anywhere. Writes a completion like cap().
  [[nodiscard]] bool push(TeamMask pushed, TeamMask strict, MaxFlow& flow,
                          Completion* out) const {
    TeamArray<int32_t> need{};
    TeamMask needy = 0;
    int32_t total = 0;
    bool ok = true;
    for_each_team(pushed, [&](TeamId y) {
      need[y] = target(y, tie_of(strict, y)) - current_[y];
      if (need[y] > maximum_[y] - current_[y]) ok = false;
      if (need[y] > 0) {
        needy |= team_bit(y);
        total += need[y];
      }
    });
    if (!ok) return false;
    auto counts = [&](const Game& g) {
      return !involves_x(g) && (has(needy, g.away) || has(needy, g.home));
    };
    const auto n_games =
        static_cast<int32_t>(std::ranges::count_if(s_->remaining, counts));
    std::vector<GameEdges> edges;
    if (total > 0) {
      flow.reset(kFirstGameNode + n_games);
      for_each_team(needy, [&](TeamId y) {
        flow.add_edge(team_node(y), kSink, need[y]);
      });
      int32_t node = kFirstGameNode;
      for (const Game& g : s_->remaining) {
        edges.push_back(counts(g) ? add_game(flow, node++, g,
                                             has(needy, g.away) ? 2 : 0,
                                             has(needy, g.home) ? 2 : 0)
                                  : GameEdges{});
      }
      if (flow.run(kSource, kSink) != total) return false;
    }
    if (out != nullptr) {
      Completion& c = *out;
      c.assign(s_->remaining.size(), kFree);
      for (size_t i = 0; i < c.size(); ++i) {
        const Game& g = s_->remaining[i];
        if (involves_x(g)) {
          c[i] = x_result(g);
        } else if (total > 0 && counts(g)) {
          // Half-wins nobody needs go to the home team. More wins never
          // hurt a pushed team.
          c[i] = static_cast<int8_t>(2 - flow_or_zero(flow, edges[i].away));
        }
      }
    }
    return true;
  }

  // y can end strictly above (below) X
  [[nodiscard]] bool can_end_above(TeamId y) const {
    return can_pass(y, Tie::kStrict);
  }
  [[nodiscard]] bool can_end_below(TeamId y) const {
    return limit(y, Tie::kStrict) >= current_[y];
  }

  // Can the teams of `above` end strictly above X, the teams of `level`
  // exactly level with X and the teams of `below` strictly below, all at
  // once? The other teams take anything. Writes a completion like cap().
  //
  // A flow with lower and upper bounds on the half-wins of every team: a
  // team sends its lower bound to the sink and the rest through a middle
  // node, which also gets the lower bounds from the source. Feasible iff
  // every edge of the source is used up.
  [[nodiscard]] bool exact(TeamMask above, TeamMask level, TeamMask below,
                           MaxFlow& flow, Completion* out) const {
    TeamArray<int32_t> low{};
    TeamArray<int32_t> high{};
    for (TeamId y = 0; y < kTeams; ++y) high[y] = maximum_[y] - current_[y];
    for_each_team(above, [&](TeamId y) {
      low[y] = std::max(target(y, Tie::kStrict) - current_[y], 0);
    });
    for_each_team(level, [&](TeamId y) {
      low[y] = limit(y, Tie::kWeak) - current_[y];
      high[y] = low[y];
    });
    for_each_team(below, [&](TeamId y) {
      high[y] = std::min(high[y], limit(y, Tie::kStrict) - current_[y]);
    });
    const TeamMask bounded = above | level | below;
    int32_t total_low = 0;
    bool ok = true;
    for_each_team(bounded, [&](TeamId y) {
      if (low[y] < 0 || low[y] > high[y]) ok = false;
      total_low += low[y];
    });
    if (!ok) return false;
    auto counts = [&](const Game& g) {
      return !involves_x(g) && (has(bounded, g.away) || has(bounded, g.home));
    };
    const auto n_games =
        static_cast<int32_t>(std::ranges::count_if(s_->remaining, counts));
    constexpr int32_t kMiddle = kFirstGameNode;
    flow.reset(kMiddle + 1 + n_games);
    for (TeamId y = 0; y < kTeams; ++y) {
      if (low[y] > 0) flow.add_edge(team_node(y), kSink, low[y]);
      if (high[y] > low[y]) {
        flow.add_edge(team_node(y), kMiddle, high[y] - low[y]);
      }
    }
    if (total_low > 0) flow.add_edge(kSource, kMiddle, total_low);
    if (n_games > 0) flow.add_edge(kMiddle, kSink, 2 * n_games);
    std::vector<int32_t> home_edge;
    int32_t node = kMiddle + 1;
    for (const Game& g : s_->remaining) {
      home_edge.push_back(counts(g) ? add_game(flow, node++, g, 2, 2).home
                                    : -1);
    }
    if (flow.run(kSource, kSink) != 2 * n_games + total_low) return false;
    if (out != nullptr) {
      Completion& c = *out;
      c.assign(s_->remaining.size(), kFree);
      for (size_t i = 0; i < c.size(); ++i) {
        const Game& g = s_->remaining[i];
        if (involves_x(g)) {
          c[i] = x_result(g);
        } else if (home_edge[i] >= 0) {
          c[i] = static_cast<int8_t>(flow.flow(home_edge[i]));
        }
      }
    }
    return true;
  }

 private:
  // Flow nodes: source, sink, one per team, then one per game
  static constexpr int32_t kSource = 0;
  static constexpr int32_t kSink = 1;
  static constexpr int32_t kFirstGameNode = 2 + kTeams;
  [[nodiscard]] static constexpr int32_t team_node(TeamId t) noexcept {
    return 2 + t;
  }

  struct GameEdges {
    int32_t away = -1;
    int32_t home = -1;
  };

  // A game hands out 2 half-wins (win 2/0, tie 1/1)
  static GameEdges add_game(MaxFlow& flow, int32_t node, const Game& g,
                            int32_t to_away, int32_t to_home) {
    flow.add_edge(kSource, node, 2);
    GameEdges e;
    if (to_away > 0) e.away = flow.add_edge(node, team_node(g.away), to_away);
    if (to_home > 0) e.home = flow.add_edge(node, team_node(g.home), to_home);
    return e;
  }

  [[nodiscard]] static int32_t flow_or_zero(const MaxFlow& flow, int32_t edge) {
    return edge < 0 ? 0 : flow.flow(edge);
  }

  [[nodiscard]] bool involves_x(const Game& g) const noexcept {
    return g.away == x_ || g.home == x_;
  }

  // Home half-wins of a game of X: X wins out or loses out
  [[nodiscard]] int8_t x_result(const Game& g) const noexcept {
    return (g.home == x_) == x_wins_out_ ? kHomeWin : kAwayWin;
  }

  // X's final half-wins scaled to y's final games: y is level with X at
  // exactly scaled(y) / final_games[x] half-wins
  [[nodiscard]] int64_t scaled(TeamId y) const {
    return static_cast<int64_t>(x_half_wins_) * s_->final_games[y];
  }

  const Season* s_;
  TeamId x_;
  bool x_wins_out_;
  TeamArray<int32_t> current_{};
  TeamArray<int32_t> maximum_{};
  int32_t x_half_wins_ = 0;
};

[[nodiscard]] Status to_status(Answer reach, Answer miss) {
  if (reach == Answer::kNo && miss == Answer::kNo) {
    throw EngineError("A team can neither reach nor miss a goal");
  }
  if (reach == Answer::kNo) return Status::kEliminated;
  if (miss == Answer::kNo) return Status::kClinched;
  if (reach == Answer::kYes && miss == Answer::kYes) return Status::kAlive;
  return Status::kUndecided;
}

// Final standings when no games are left: every coin toss sequence of the
// only completion, so the answers are exact.
struct SeasonOver {
  bool exact = false;
  std::array<TeamArray<Answers>, kGoals> answers{};

  void build(const Season& s, ScenarioRunner& runner) {
    answers = {};
    std::array<TeamArray<Proof>, kGoals> reach{};
    std::array<TeamArray<Proof>, kGoals> miss{};
    const Completion none;
    const SearchEnd end = runner.run(
        none, kSeasonOverBudget,
        [&](const RankResult& div, const RankResult& conf, bool coin_toss) {
          for (int32_t goal = 0; goal < kGoals; ++goal) {
            for (TeamId x = 0; x < kTeams; ++x) {
              if (!s.present(x)) continue;
              Proof& p = s.reached(static_cast<Goal>(goal), x, div, conf)
                             ? reach[goal][x]
                             : miss[goal][x];
              if (p.answer != Answer::kYes) {
                p = {.answer = Answer::kYes,
                     .scenario = none,
                     .coin_toss = coin_toss};
              }
            }
          }
          return false;
        });
    exact = end == SearchEnd::kExhausted;
    for (int32_t goal = 0; goal < kGoals; ++goal) {
      for (TeamId x = 0; x < kTeams; ++x) {
        Answers& a = answers[goal][x];
        a.reach = reach[goal][x];
        a.miss = miss[goal][x];
        if (a.reach.answer == Answer::kUnknown) a.reach.answer = Answer::kNo;
        if (a.miss.answer == Answer::kUnknown) a.miss.answer = Answer::kNo;
      }
    }
  }
};

// All checks of one team X
class TeamStatus {
 public:
  TeamStatus(const Season& s, TeamId x, MaxFlow& flow, ScenarioRunner& runner,
             const SeasonOver& season_over, bool verify,
             const Budgets& budgets = {})
      : s_(&s),
        x_(x),
        flow_(&flow),
        runner_(&runner),
        season_over_(&season_over),
        reach_(s, x, true),
        miss_(s, x, false),
        verify_(verify),
        budgets_(budgets) {
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
    for (int32_t i = 0; i < n_divisions_; ++i) {
      division_masks_[i] = division_teams(divisions_[i]);
    }
  }

  // Answer to "can X reach the goal?" (`reach`) or "can X miss it?". The
  // goals are nested (No. 1 seed => division title => playoff berth), so an
  // answer the checks of one goal leave open can come from another goal,
  // with its scenario.
  [[nodiscard]] Proof final_proof(Goal goal, bool reach) {
    Proof p = proof(goal, reach);
    for (const Goal other :
         {Goal::kFirstSeed, Goal::kDivision, Goal::kPlayoffs}) {
      if (other == goal || p.answer != Answer::kUnknown) continue;
      const bool wider = nesting(other) > nesting(goal);
      const Proof& o = proof(other, reach);
      // Can't reach a wider goal => can't reach this one; can reach a
      // narrower goal => can reach this one. The reverse for "can miss".
      const Answer follows = wider == reach ? Answer::kNo : Answer::kYes;
      if (o.answer == follows) p = o;
    }
    return p;
  }

  [[nodiscard]] Answers final_answers(Goal goal) {
    return {.reach = final_proof(goal, true), .miss = final_proof(goal, false)};
  }

 private:
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

  // Answer of the checks of one goal to one question, computed once
  [[nodiscard]] const Proof& proof(Goal goal, bool reach) {
    const auto g = static_cast<size_t>(goal);
    std::optional<Proof>& cached = (reach ? reach_proofs_ : miss_proofs_)[g];
    if (!cached) {
      if (season_over_->exact) {
        const Answers& a = season_over_->answers[g][x_];
        cached = reach ? a.reach : a.miss;
      } else {
        cached = reach ? reach_answer(goal) : miss_answer(goal);
      }
    }
    return *cached;
  }

  // Ties at X's level when X wins out (`reach`) or loses out, built when
  // first needed
  [[nodiscard]] TieProof& ties(bool reach) {
    std::optional<TieProof>& t = reach ? reach_ties_ : miss_ties_;
    if (!t) {
      t.emplace(s_->played, s_->played_games, s_->remaining, *s_->league, x_,
                reach);
    }
    return *t;
  }

  // A second set of tie proofs for the tie groups (P11): they know which
  // teams end above, level with and below X (TieProof::set_context()), and
  // lose their cached results with every group
  [[nodiscard]] TieProof& context_ties(bool reach) {
    std::optional<TieProof>& t =
        reach ? reach_context_ties_ : miss_context_ties_;
    if (!t) {
      t.emplace(s_->played, s_->played_games, s_->remaining, *s_->league, x_,
                reach);
    }
    return *t;
  }

  // `feasible(strict, c)`: can the check hold with the teams of `strict`
  // strictly behind (ahead of) X and the others possibly tied?
  //
  // Yes if it holds with only the teams X ranks ahead of when tied allowed
  // to tie (`yes_strict` = the others), whatever the other tiebreakers say.
  // No if it fails even with only the teams X ranks behind when tied held
  // strictly (`no_strict`). Otherwise search for a scenario in the gray zone.
  using Feasible = std::function<bool(TeamMask, Completion*)>;
  [[nodiscard]] Proof decide(Goal goal, bool reach, const Feasible& feasible,
                             TeamMask yes_strict, TeamMask no_strict,
                             const std::function<Proof()>& gray) {
    Completion c;
    if (feasible(yes_strict, &c)) {
      return {.answer = Answer::kYes, .scenario = filled(c, kHomeWin)};
    }
    if (!feasible(no_strict, nullptr)) {
      if (verify_ && feasible(0, &c)) verify_no(goal, reach, c);
      return {.answer = Answer::kNo};
    }
    budget_ = budgets_.scenarios;
    return gray();
  }

  // Self-check for tests: a "no" that needed tie proofs must hold in the
  // weak completion, the one with the most ties at X's level, for every
  // coin toss sequence
  void verify_no(Goal goal, bool reach, const Completion& c) {
    for (const int8_t fill : {kHomeWin, kAwayWin}) {
      (void)runner_->run(
          filled(c, fill), kSeasonOverBudget,
          [&](const RankResult& div, const RankResult& conf, bool) {
            if (s_->reached(goal, x_, div, conf) == reach) {
              throw EngineError("A clinching proof failed its self-check");
            }
            return false;
          });
    }
  }

  // Ties with the teams of `teams` that can end with X's win percentage in
  // this check, in `ranking`
  [[nodiscard]] TieSides tie_sides(const Check& check, TeamMask teams,
                                   bool reach, Ranking ranking) {
    TeamMask tied = 0;
    for_each_team(teams, [&](TeamId y) {
      if (check.can_tie(y)) tied |= team_bit(y);
    });
    if (tied == 0) return {};
    TieProof& proof = ties(reach);
    if (ranking == Ranking::kDivision) return proof.division_sides(tied);
    return proof.conference_sides(tied, ranking == Ranking::kWildCard);
  }

  // "Can reach": teams X beats in ties may tie X, teams that beat X in ties
  // must stay strictly behind
  [[nodiscard]] Proof decide_reach(Goal goal, const Feasible& feasible,
                                   TeamMask teams, Ranking ranking,
                                   const std::function<Proof()>& gray) {
    const TieSides sides = tie_sides(reach_, teams, true, ranking);
    return decide(goal, true, feasible, ~sides.x_ahead, sides.x_behind, gray);
  }

  // "Can miss": teams that beat X in ties only need to tie X, teams X beats
  // in ties must get strictly ahead
  [[nodiscard]] Proof decide_miss(Goal goal, const Feasible& feasible,
                                  TeamMask teams, Ranking ranking,
                                  const std::function<Proof()>& gray) {
    const TieSides sides = tie_sides(miss_, teams, false, ranking);
    return decide(goal, false, feasible, ~sides.x_behind, sides.x_ahead, gray);
  }

  [[nodiscard]] Proof reach_answer(Goal goal) {
    switch (goal) {
      case Goal::kDivision:
        return decide_reach(
            goal,
            [&](TeamMask strict, Completion* c) {
              return reach_.cap(rivals_, strict & rivals_, *flow_, c);
            },
            rivals_, Ranking::kDivision,
            [&] {
              return or_tie_groups(gray_reach(goal, rivals_), goal, true);
            });
      case Goal::kFirstSeed:
        // X is the No. 1 seed iff X is ahead of every other conference team:
        // a team ahead of X puts its division winner ahead of X too.
        return decide_reach(
            goal,
            [&](TeamMask strict, Completion* c) {
              return reach_.cap(conference_, strict & conference_, *flow_, c);
            },
            conference_, Ranking::kConference,
            [&] {
              return or_tie_groups(gray_reach(goal, conference_), goal, true);
            });
      case Goal::kPlayoffs:
        // X makes the playoffs iff no rival is ahead of X (division title),
        // or at most wild_cards - 1 teams that don't win their division are
        // ahead of X.
        return decide_reach(
            goal,
            [&](TeamMask strict, Completion* c) {
              return reach_.cap(rivals_, strict & rivals_, *flow_, c) ||
                     wild_card_sets(strict, [&](TeamMask capped) {
                       return reach_.cap(capped, strict & capped, *flow_, c);
                     });
            },
            conference_, Ranking::kWildCard,
            [&] {
              Proof p = gray_reach(goal, rivals_);
              if (p.answer == Answer::kYes) return p;
              Proof found;
              (void)wild_card_sets(0, [&](TeamMask capped) {
                found = gray_reach(goal, capped);
                return found.answer == Answer::kYes || budget_ <= 0;
              });
              if (found.answer == Answer::kYes) return found;
              return tie_groups(goal, true);
            });
    }
    return {};
  }

  [[nodiscard]] Proof miss_answer(Goal goal) {
    switch (goal) {
      case Goal::kDivision:
        return decide_miss(
            goal,
            [&](TeamMask strict, Completion* c) {
              return any_passes(rivals_, strict, c);
            },
            rivals_, Ranking::kDivision,
            [&] {
              return or_tie_groups(gray_miss_single(goal, rivals_), goal,
                                   false);
            });
      case Goal::kFirstSeed:
        return decide_miss(
            goal,
            [&](TeamMask strict, Completion* c) {
              return any_passes(conference_, strict, c);
            },
            conference_, Ranking::kConference,
            [&] {
              return or_tie_groups(gray_miss_single(goal, conference_), goal,
                                   false);
            });
      case Goal::kPlayoffs:
        return decide_miss(
            goal,
            [&](TeamMask strict, Completion* c) {
              return forced_sets(strict, [&](TeamMask pushed) {
                return miss_.push(pushed, strict & pushed, *flow_, c);
              });
            },
            conference_, Ranking::kWildCard,
            [&] {
              Proof found;
              (void)forced_sets(0, [&](TeamMask pushed) {
                found = gray_miss(goal, pushed);
                return found.answer == Answer::kYes || budget_ <= 0;
              });
              if (found.answer == Answer::kYes) return found;
              return tie_groups(goal, false);
            });
    }
    return {};
  }

  // Some team of `teams` gets ahead of X on its own while X loses out:
  // strictly ahead if it's in `strict`, otherwise at least level
  [[nodiscard]] bool any_passes(TeamMask teams, TeamMask strict,
                                Completion* c) {
    bool any = false;
    for_each_team(teams, [&](TeamId y) {
      any = any || miss_.push(team_bit(y), strict & team_bit(y), *flow_, c);
    });
    return any;
  }

  [[nodiscard]] TeamMask division_teams(int32_t d) const {
    TeamMask out = 0;
    for_each_team(conference_, [&](TeamId y) {
      if (s_->league->division[y] == d) out |= team_bit(y);
    });
    return out;
  }

  // Runs `completion` through the procedures until X reaches the goal
  // (`want_reach`) or misses it, for some coin toss sequence
  [[nodiscard]] Proof try_scenario(Goal goal, const Completion& completion,
                                   bool want_reach) {
    Proof found;
    (void)runner_->run(
        completion, kTossBudget,
        [&](const RankResult& div, const RankResult& conf, bool coin_toss) {
          if (s_->reached(goal, x_, div, conf) != want_reach) return false;
          found = {.answer = Answer::kYes,
                   .scenario = completion,
                   .coin_toss = coin_toss};
          return true;
        });
    return found;
  }

  // Gray zone of "can reach": cap `capped` with as many of the teams that
  // can tie X strictly behind as possible (greedy, in a few orders), then let
  // the procedures break the remaining ties. Games no check needs go to the
  // home team, then to the away team.
  [[nodiscard]] Proof gray_reach(Goal goal, TeamMask capped) {
    if (!reach_.cap(capped, 0, *flow_, nullptr)) return {};
    return gray(
        goal, capped, true,
        [&](TeamMask strict, Completion* c) {
          return reach_.cap(capped, strict, *flow_, c);
        },
        reach_);
  }

  // Gray zone of "can miss": push `pushed` ahead of X with as many teams
  // strictly ahead as possible
  [[nodiscard]] Proof gray_miss(Goal goal, TeamMask pushed) {
    if (!miss_.push(pushed, 0, *flow_, nullptr)) return {};
    return gray(
        goal, pushed, false,
        [&](TeamMask strict, Completion* c) {
          return miss_.push(pushed, strict, *flow_, c);
        },
        miss_);
  }

  [[nodiscard]] Proof gray_miss_single(Goal goal, TeamMask teams) {
    Proof found;
    for_each_team(teams, [&](TeamId y) {
      if (found.answer != Answer::kYes && budget_ > 0) {
        found = gray_miss(goal, team_bit(y));
      }
    });
    return found;
  }

  using Tighten = std::function<bool(TeamMask, Completion*)>;
  [[nodiscard]] Proof gray(Goal goal, TeamMask teams, bool want_reach,
                           const Tighten& feasible, const Check& check) {
    std::vector<TeamId> tied;
    for_each_team(teams, [&](TeamId y) {
      if (check.can_tie(y)) tied.push_back(y);
    });
    const auto n = static_cast<int32_t>(tied.size());
    const int32_t orders = std::clamp(n, 1, kTighteningOrders);
    for (int32_t r = 0; r < orders; ++r) {
      TeamMask strict = 0;
      for (int32_t k = 0; k < n; ++k) {
        const TeamMask more = strict | team_bit(tied[(r + k) % n]);
        if (feasible(more, nullptr)) strict = more;
      }
      Completion c;
      if (!feasible(strict, &c)) {
        throw EngineError("A feasible flow became infeasible");
      }
      for (const int8_t fill : {kHomeWin, kAwayWin}) {
        if (budget_-- <= 0) return {};
        Proof p = try_scenario(goal, filled(c, fill), want_reach);
        if (p.answer == Answer::kYes) return p;
      }
    }
    return {};
  }

  // X misses the goal if all teams of `ahead` rank ahead of X. Division
  // title: a rival. No. 1 seed: any team (its division's winner is ahead of
  // X too). Playoffs: a rival and `wild_cards` teams that don't win their
  // division.
  [[nodiscard]] bool keeps_x_from(Goal goal, TeamMask ahead) const {
    if (goal == Goal::kFirstSeed) return (ahead & conference_) != 0;
    if ((ahead & rivals_) == 0) return false;
    if (goal == Goal::kDivision) return true;
    int32_t others = 0;
    for (int32_t i = 0; i < n_divisions_; ++i) {
      others += std::max(std::popcount(ahead & division_masks_[i]) - 1, 0);
    }
    return others >= s_->wild_cards;
  }

  // The gray zone's scenario, or the tie groups if it found none
  [[nodiscard]] Proof or_tie_groups(Proof found, Goal goal, bool reach) {
    if (found.answer == Answer::kYes) return found;
    return tie_groups(goal, reach);
  }

  [[nodiscard]] TieOutcome tie_outcome(TieProof& proof, Goal goal,
                                       TeamMask above, TeamMask level) const {
    switch (goal) {
      case Goal::kDivision:
        return proof.division_outcome(above, level);
      case Goal::kFirstSeed:
        return proof.first_seed_outcome(above, level);
      case Goal::kPlayoffs:
        return proof.playoff_outcome(above, level, s_->wild_cards);
    }
    return TieOutcome::kEither;
  }

  // A question the flows and the gray zone leave open, by the tie groups at
  // X's level (P11): every way the conference teams can end above, level
  // with or below X (a flow with exact bounds says which ways exist), and
  // what X gets in each (TieProof::playoff_outcome() and the like). No if
  // no way gives the answer; yes if the completion of a way that may give
  // it does so in the procedures.
  [[nodiscard]] Proof tie_groups(Goal goal, bool reach) {
    const Check& check = reach ? reach_ : miss_;
    // Only the rivals matter for the division title
    const TeamMask teams = goal == Goal::kDivision ? rivals_ : conference_;
    // Teams with one possible side are set, the others open
    struct Open {
      TeamId team = 0;
      bool above = false;
      bool level = false;
      bool below = false;
    };
    std::vector<Open> open;
    TeamMask fixed_above = 0;
    TeamMask fixed_level = 0;
    TeamMask fixed_below = 0;
    for_each_team(teams, [&](TeamId y) {
      const Open o{.team = y,
                   .above = check.can_end_above(y),
                   .level = check.can_tie(y),
                   .below = check.can_end_below(y)};
      const int32_t sides = static_cast<int32_t>(o.above) +
                            static_cast<int32_t>(o.level) +
                            static_cast<int32_t>(o.below);
      if (sides > 1) {
        open.push_back(o);
      } else if (o.above) {
        fixed_above |= team_bit(y);
      } else if (o.level) {
        fixed_level |= team_bit(y);
      } else {
        fixed_below |= team_bit(y);
      }
    });
    // Teams that can still end level with or above X, from each open team on
    std::vector<TeamMask> can_be_ahead(open.size() + 1, 0);
    for (size_t i = open.size(); i-- > 0;) {
      can_be_ahead[i] =
          can_be_ahead[i + 1] |
          (open[i].above || open[i].level ? team_bit(open[i].team) : 0);
    }
    TieProof& proof = ties(reach);
    int32_t flows = budgets_.group_flows;
    // Groups that stay open before giving up: the answer is open then,
    // unless a later group says yes
    int32_t open_groups = budgets_.open_groups;
    bool unknown = false;
    Proof found;
    // True to stop
    const auto visit = [&](const auto& self, size_t i, TeamMask above,
                           TeamMask level, TeamMask below) -> bool {
      // Whatever the tiebreakers and the open teams do: X reaches the goal
      // if even all teams that can end level or above don't keep X from it,
      // and misses it if the teams above alone do
      if (reach ? keeps_x_from(goal, above)
                : !keeps_x_from(goal, above | level | can_be_ahead[i])) {
        return false;
      }
      if (flows-- <= 0) {
        unknown = true;
        return true;
      }
      const bool leaf = i == open.size();
      Completion c;
      if (!check.exact(above, level, below, *flow_, leaf ? &c : nullptr)) {
        return false;
      }
      if (!leaf) {
        const Open& o = open[i];
        const TeamMask bit = team_bit(o.team);
        return (o.level && self(self, i + 1, above, level | bit, below)) ||
               (o.above && self(self, i + 1, above | bit, level, below)) ||
               (o.below && self(self, i + 1, above, level, below | bit));
      }
      const TieOutcome wanted =
          reach ? TieOutcome::kReached : TieOutcome::kMissed;
      const auto never = [&](TieOutcome o) {
        return o == TieOutcome::kImpossible ||
               o == (reach ? TieOutcome::kMissed : TieOutcome::kReached);
      };
      TieOutcome outcome = tie_outcome(proof, goal, above, level);
      // Not settled: again with ranges that know every team's side
      if (!never(outcome)) {
        TieProof& context = context_ties(reach);
        context.set_context(above, level, below);
        outcome = tie_outcome(context, goal, above, level);
      }
      if (never(outcome)) {
        if (verify_) verify_no(goal, reach, c);
        return false;
      }
      for (const int8_t fill : {kHomeWin, kAwayWin}) {
        found = try_scenario(goal, filled(c, fill), reach);
        if (found.answer == Answer::kYes) return true;
      }
      if (verify_ && outcome == wanted) {
        throw EngineError("A tie group proof failed its self-check");
      }
      unknown = true;
      return --open_groups <= 0;
    };
    (void)visit(visit, 0, fixed_above, fixed_level, fixed_below);
    if (found.answer == Answer::kYes) return found;
    if (unknown) return {};
    return {.answer = Answer::kNo};
  }

  // Calls visit(capped) for every largest allowed set of teams ahead of X
  // that gives X a wild card (1 + e_d teams per division, sum of e_d =
  // wild_cards - 1) until it returns true. `capped` are the other teams that
  // can get ahead of X (strict) or tie X (weak).
  [[nodiscard]] bool wild_card_sets(
      TeamMask strict, const std::function<bool(TeamMask)>& visit) const {
    // Per division: teams that are always ahead of X (must be allowed) and
    // teams that may or may not be. The others can never get ahead.
    std::array<TeamMask, kDivisions> forced{};
    std::array<TeamMask, kDivisions> open{};
    for (int32_t i = 0; i < n_divisions_; ++i) {
      for_each_team(division_teams(divisions_[i]), [&](TeamId y) {
        const int32_t limit = reach_.limit(y, tie_of(strict, y));
        if (reach_.current(y) > limit) {
          forced[i] |= team_bit(y);
        } else if (reach_.maximum(y) > limit) {
          open[i] |= team_bit(y);
        }
      });
    }
    std::function<bool(int32_t, int32_t, TeamMask)> search =
        [&](int32_t i, int32_t budget, TeamMask capped) {
          if (i == n_divisions_) return visit(capped);
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

  // Calls visit(pushed) for every smallest set of teams ahead of X that
  // keeps X out of the playoffs (at least one rival, wild_cards teams that
  // don't win their division) until it returns true. Teams already ahead
  // of X are in `pushed` too.
  [[nodiscard]] bool forced_sets(
      TeamMask strict, const std::function<bool(TeamMask)>& visit) const {
    // Per division: teams already ahead of X and teams that can get there
    std::array<TeamMask, kDivisions> already{};
    std::array<TeamMask, kDivisions> able{};
    std::array<bool, kDivisions> own{};
    for (int32_t i = 0; i < n_divisions_; ++i) {
      own[i] = divisions_[i] == s_->league->division[x_];
      for_each_team(division_teams(divisions_[i]), [&](TeamId y) {
        const Tie tie = tie_of(strict, y);
        if (miss_.current(y) >= miss_.target(y, tie)) {
          already[i] |= team_bit(y);
        } else if (miss_.can_pass(y, tie)) {
          able[i] |= team_bit(y);
        }
      });
    }
    std::function<bool(int32_t, int32_t, TeamMask)> search =
        [&](int32_t i, int32_t budget, TeamMask pushed) {
          if (i == n_divisions_) return budget == 0 && visit(pushed);
          const bool last = i == n_divisions_ - 1;
          for (int32_t f = last ? budget : 0; f <= budget; ++f) {
            // f teams ahead of X besides the division winner. X's division
            // always needs its winner ahead of X.
            const int32_t size = own[i] || f > 0 ? 1 + f : 0;
            const int32_t n_already = std::popcount(already[i]);
            if (n_already + std::popcount(able[i]) < size) continue;
            // Teams already ahead cost nothing, so use them first
            const int32_t extra = std::max(size - n_already, 0);
            const TeamMask base = size > 0 ? already[i] : 0;
            if (any_subset(able[i], extra, [&](TeamMask more) {
                  return search(i + 1, budget - f, pushed | base | more);
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
  ScenarioRunner* runner_;
  const SeasonOver* season_over_;
  Check reach_;
  Check miss_;
  // Ties at X's level when X wins out and loses out
  std::optional<TieProof> reach_ties_;
  std::optional<TieProof> miss_ties_;
  std::optional<TieProof> reach_context_ties_;
  std::optional<TieProof> miss_context_ties_;
  TeamMask rivals_ = 0;
  TeamMask conference_ = 0;
  // Divisions of X's conference and their teams (without X)
  std::array<int32_t, kDivisions> divisions_{};
  std::array<TeamMask, kDivisions> division_masks_{};
  int32_t n_divisions_ = 0;
  std::array<std::optional<Proof>, kGoals> reach_proofs_{};
  std::array<std::optional<Proof>, kGoals> miss_proofs_{};
  // Completions left for the current gray zone search
  int32_t budget_ = 0;
  bool verify_ = false;
  Budgets budgets_;
};

// Budgeted search for answers the checks leave open (P12): split on one
// remaining game at a time (home win, away win, tie) and run the checks on
// each part. A part where they answer yes gives a completion of the whole
// season; the answer is no if every part answers no. Games of teams that
// can end level with X go first, X's own games last.
class GameSearch {
 public:
  GameSearch(const Season& root, TeamId x, Goal goal, MaxFlow& flow,
             bool verify, const InterruptCheck& interrupt)
      : root_(&root),
        x_(x),
        goal_(goal),
        flow_(&flow),
        verify_(verify),
        interrupt_(&interrupt) {}

  // Answer to "can X reach the goal?" (`want_reach`) or "can X miss it?"
  [[nodiscard]] Proof run(bool want_reach) {
    nodes_ = 0;
    fixed_.assign(root_->remaining.size(), kFree);
    return visit(want_reach);
  }

 private:
  // The checks on one part: their answer and, if it's open, the game to
  // split on next (an index into the root's remaining games)
  struct Node {
    Proof proof;
    std::optional<size_t> split;
  };

  [[nodiscard]] Proof visit(bool want_reach) {
    if (nodes_ >= kSearchBudget) return {};
    if (++nodes_ % kInterruptEvery == 0) (*interrupt_)();
    const Node node = evaluate(want_reach);
    if (node.proof.answer != Answer::kUnknown || !node.split) {
      return node.proof;
    }
    const size_t split = *node.split;
    bool unknown = false;
    for (const int8_t result : {kHomeWin, kAwayWin, kTie}) {
      fixed_[split] = result;
      Proof child = visit(want_reach);
      if (child.answer == Answer::kYes) {
        fixed_[split] = kFree;
        return child;
      }
      unknown = unknown || child.answer == Answer::kUnknown;
    }
    fixed_[split] = kFree;
    if (unknown) return {};
    return {.answer = Answer::kNo};
  }

  // Runs the checks with the fixed games played
  [[nodiscard]] Node evaluate(bool want_reach) {
    std::vector<Game> played(root_->played_games.begin(),
                             root_->played_games.end());
    std::vector<Game> remaining;
    std::vector<size_t> index;  // root index of each remaining game
    for (size_t i = 0; i < fixed_.size(); ++i) {
      if (fixed_[i] == kFree) {
        remaining.push_back(root_->remaining[i]);
        index.push_back(i);
      } else {
        Game g = root_->remaining[i];
        g.result = fixed_[i] - 1;  // home - away: 1, 0 or -1
        played.push_back(g);
      }
    }
    Season part;
    part.build(played, remaining, *root_->league, root_->seeds);
    ScenarioRunner runner(played, remaining, *root_->league, part.seeds);
    SeasonOver over;
    if (remaining.empty()) over.build(part, runner);
    TeamStatus team(part, x_, *flow_, runner, over, verify_, kSearchBudgets);
    Node out{.proof = team.final_proof(goal_, want_reach)};
    if (out.proof.answer == Answer::kYes) {
      // The part's completion fills the games that are still open
      Completion c = fixed_;
      if (out.proof.scenario && !out.proof.scenario->empty()) {
        for (size_t j = 0; j < index.size(); ++j) {
          c[index[j]] = (*out.proof.scenario)[j];
        }
      }
      out.proof.scenario = std::move(c);
      return out;
    }
    if (out.proof.answer == Answer::kUnknown && !remaining.empty()) {
      out.split = index[pick(part, remaining, want_reach)];
    }
    return out;
  }

  // The open game that most likely changes who ends level with X
  [[nodiscard]] size_t pick(const Season& part, std::span<const Game> remaining,
                            bool want_reach) const {
    const Check check(part, x_, want_reach);
    const League& league = *part.league;
    const auto conference = [&](TeamId t) {
      return league.conference[t] == league.conference[x_];
    };
    const auto level = [&](TeamId t) {
      return t != x_ && conference(t) && check.can_tie(t);
    };
    size_t best = 0;
    int32_t best_score = std::numeric_limits<int32_t>::min();
    for (size_t j = 0; j < remaining.size(); ++j) {
      const Game& g = remaining[j];
      const int32_t score =
          2 * (static_cast<int32_t>(level(g.away)) +
               static_cast<int32_t>(level(g.home))) +
          static_cast<int32_t>(conference(g.away)) +
          static_cast<int32_t>(conference(g.home)) -
          (g.away == x_ || g.home == x_ ? kOwnGamePenalty : 0);
      if (score > best_score) {
        best = j;
        best_score = score;
      }
    }
    return best;
  }

  const Season* root_;
  TeamId x_;
  Goal goal_;
  MaxFlow* flow_;
  bool verify_;
  const InterruptCheck* interrupt_;
  // Results fixed so far (home half-wins), kFree for open games
  Completion fixed_;
  int32_t nodes_ = 0;
};

void add_scenario(ScenarioRows& out, int32_t sim, TeamId team, int32_t goal,
                  int32_t proves, const Proof& proof) {
  if (proof.answer != Answer::kYes || !proof.scenario) return;
  out.sim.push_back(sim);
  out.team.push_back(team);
  out.goal.push_back(goal);
  out.proves.push_back(proves);
  out.coin_toss.push_back(proof.coin_toss ? 1 : 0);
  for (const int8_t home_half_wins : *proof.scenario) {
    out.result.push_back(home_half_wins - 1);
  }
}

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
  SeasonOver season_over;
  MaxFlow flow;
  for (int32_t s = 1; s <= played.n_sims; ++s) {
    interrupt();
    const std::span<const Game> season_remaining =
        remaining_by_season.season(s);
    season.build(played_by_season.season(s), season_remaining, league,
                 options.seeds[s - 1]);
    ScenarioRunner runner(played_by_season.season(s), season_remaining, league,
                          season.seeds);
    season_over.exact = false;
    if (season_remaining.empty()) season_over.build(season, runner);
    for (TeamId x = 0; x < kTeams; ++x) {
      if (!season.present(x)) continue;
      out.sim.push_back(s);
      out.team.push_back(x);
      out.wins.push_back(season.played.true_wins[x]);
      out.losses.push_back(season.played.losses[x]);
      out.ties.push_back(season.played.ties[x]);
      out.games_left.push_back(season.left[x]);
      Budgets budgets;
      if (played.n_sims == 1) {
        budgets = kSingleSeasonBudgets;
      } else if (season.left[x] <= kLateGames) {
        budgets.group_flows = kLateGroupFlows;
      }
      TeamStatus team(season, x, flow, runner, season_over, options.verify,
                      budgets);
      for (int32_t goal = 0; goal < kGoals; ++goal) {
        if (!options.goals[goal]) {
          out.status[goal].push_back(kMissing);
          continue;
        }
        Answers a = team.final_answers(static_cast<Goal>(goal));
        if (options.search && !season_remaining.empty() &&
            (a.reach.answer == Answer::kUnknown ||
             a.miss.answer == Answer::kUnknown)) {
          GameSearch search(season, x, static_cast<Goal>(goal), flow,
                            options.verify, interrupt);
          if (a.reach.answer == Answer::kUnknown) a.reach = search.run(true);
          if (a.miss.answer == Answer::kUnknown) a.miss = search.run(false);
        }
        Status status = to_status(a.reach.answer, a.miss.answer);
        // With no games left, only a coin toss can make both possible: the
        // goal is undecided until the NFL tosses the coin
        if (season_remaining.empty() && status == Status::kAlive) {
          status = Status::kUndecided;
        }
        out.status[goal].push_back(static_cast<int32_t>(status));
        if (options.scenarios) {
          add_scenario(out.scenarios, s, x, goal, 1, a.reach);
          add_scenario(out.scenarios, s, x, goal, 2, a.miss);
        }
      }
    }
  }
  return out;
}

}  // namespace nflseedr
