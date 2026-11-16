// Shared by the clinching checks (status.cpp) and the group check
// (group_check.cpp): a season with its remaining games, and the flows on
// the win totals when X wins out or loses out. See dev/playoff-status.md.
#pragma once

#include <algorithm>
#include <cstdint>
#include <span>
#include <vector>

#include "flow.hpp"
#include "procedures.hpp"
#include "scenario.hpp"
#include "season.hpp"
#include "status.hpp"
#include "team_mask.hpp"
#include "types.hpp"

namespace nflseedr::clinching {

// Runs of the procedures per completion (one per coin toss sequence)
inline constexpr int32_t kTossBudget = 64;

// Does a check need competitors strictly behind (or ahead of) X, or is a tie
// in win percentage enough?
enum class Tie : int8_t { kStrict, kWeak };

// Strictly behind (ahead of) X for teams in `strict`, possibly tied else
[[nodiscard]] inline Tie tie_of(TeamMask strict, TeamId y) noexcept {
  return has(strict, y) ? Tie::kStrict : Tie::kWeak;
}

// Results of a completion that no check constrains
inline constexpr int8_t kFree = -1;
inline constexpr int8_t kHomeWin = 2;
inline constexpr int8_t kTie = 1;
inline constexpr int8_t kAwayWin = 0;

[[nodiscard]] inline Completion filled(Completion c, int8_t free_result) {
  std::ranges::replace(c, kFree, free_result);
  return c;
}

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
    const TieLevel l = level(y);
    return tie == Tie::kStrict && l.exact ? l.floor - 1 : l.floor;
  }

  // Fewest half-wins y needs to get ahead of X (strict) or not stay behind
  // (weak)
  [[nodiscard]] int32_t target(TeamId y, Tie tie) const {
    const TieLevel l = level(y);
    return tie == Tie::kWeak && l.exact ? l.floor : l.floor + 1;
  }

  // y can end with exactly X's win percentage
  [[nodiscard]] bool can_tie(TeamId y) const {
    const TieLevel l = level(y);
    return l.exact && l.floor >= current_[y] && l.floor <= maximum_[y];
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
  // Games with a result in `fixed` (not kFree) keep it.
  //
  // A flow with lower and upper bounds on the half-wins of every team: a
  // team sends its lower bound to the sink and the rest through a middle
  // node, which also gets the lower bounds from the source. Feasible iff
  // every edge of the source is used up.
  //
  // Without `out`, a game of a bounded team against a team without bounds
  // isn't in the flow: it can bring the bounded team anything, so it only
  // lowers what the team's other games must bring.
  [[nodiscard]] bool exact(TeamMask above, TeamMask level, TeamMask below,
                           MaxFlow& flow, Completion* out,
                           const Completion* fixed = nullptr) const {
    const TeamMask bounded = above | level | below;
    const size_t n_remaining = s_->remaining.size();
    // Half-wins the fixed games already hand out
    TeamArray<int32_t> got{};
    // Half-wins a bounded team can take from the games left out of the flow
    TeamArray<int32_t> spare{};
    // Games of the flow: open games of a bounded team
    std::vector<bool> counts(n_remaining, false);
    int32_t n_games = 0;
    for (size_t i = 0; i < n_remaining; ++i) {
      const Game& g = s_->remaining[i];
      if (involves_x(g)) continue;
      const bool away = has(bounded, g.away);
      const bool home = has(bounded, g.home);
      if (fixed != nullptr && (*fixed)[i] != kFree) {
        got[g.home] += (*fixed)[i];
        got[g.away] += 2 - (*fixed)[i];
      } else if ((away && home) || (out != nullptr && (away || home))) {
        counts[i] = true;
        ++n_games;
      } else if (away) {
        spare[g.away] += 2;
      } else if (home) {
        spare[g.home] += 2;
      }
    }
    TeamArray<int32_t> low{};
    TeamArray<int32_t> high{};
    for (TeamId y = 0; y < kTeams; ++y) {
      high[y] = maximum_[y] - current_[y] - got[y];
    }
    for_each_team(above, [&](TeamId y) {
      low[y] = std::max(target(y, Tie::kStrict) - current_[y] - got[y], 0);
    });
    for_each_team(level, [&](TeamId y) {
      low[y] = limit(y, Tie::kWeak) - current_[y] - got[y];
      high[y] = low[y];
    });
    for_each_team(below, [&](TeamId y) {
      high[y] =
          std::min(high[y], limit(y, Tie::kStrict) - current_[y] - got[y]);
    });
    int32_t total_low = 0;
    bool ok = true;
    for_each_team(bounded, [&](TeamId y) {
      if (low[y] < 0 || low[y] > high[y]) ok = false;
      low[y] = std::max(low[y] - spare[y], 0);
      total_low += low[y];
    });
    if (!ok) return false;
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
    std::vector<int32_t> home_edge(n_remaining, -1);
    int32_t node = kMiddle + 1;
    for (size_t i = 0; i < n_remaining; ++i) {
      if (counts[i]) {
        home_edge[i] = add_game(flow, node++, s_->remaining[i], 2, 2).home;
      }
    }
    if (flow.run(kSource, kSink) != 2 * n_games + total_low) return false;
    if (out != nullptr) {
      Completion& c = *out;
      c.assign(n_remaining, kFree);
      for (size_t i = 0; i < n_remaining; ++i) {
        const Game& g = s_->remaining[i];
        if (involves_x(g)) {
          c[i] = x_result(g);
        } else if (home_edge[i] >= 0) {
          c[i] = static_cast<int8_t>(flow.flow(home_edge[i]));
        } else if (fixed != nullptr) {
          c[i] = (*fixed)[i];
        }
      }
    }
    return true;
  }

  [[nodiscard]] bool involves_x(const Game& g) const noexcept {
    return g.away == x_ || g.home == x_;
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

  // Home half-wins of a game of X: X wins out or loses out
  [[nodiscard]] int8_t x_result(const Game& g) const noexcept {
    return (g.home == x_) == x_wins_out_ ? kHomeWin : kAwayWin;
  }

  // The half-wins that leave y level with X
  [[nodiscard]] TieLevel level(TeamId y) const {
    return tie_level(x_half_wins_, s_->final_games[x_], s_->final_games[y]);
  }

  const Season* s_;
  TeamId x_;
  bool x_wins_out_;
  TeamArray<int32_t> current_{};
  TeamArray<int32_t> maximum_{};
  int32_t x_half_wins_ = 0;
};

// A team that can end on more than one side of X
struct Open {
  TeamId team = 0;
  bool above = false;
  bool level = false;
  bool below = false;
};
// Where teams can end against X: teams with one possible side are set,
// the others open
struct GroupSides {
  std::vector<Open> open;
  TeamMask above = 0;
  TeamMask level = 0;
  TeamMask below = 0;
  // Teams that can still end level with or above X, from each open team on
  std::vector<TeamMask> can_be_ahead;
};

[[nodiscard]] inline GroupSides group_sides(const Check& check,
                                            TeamMask teams) {
  GroupSides out;
  for_each_team(teams, [&](TeamId y) {
    const Open o{.team = y,
                 .above = check.can_end_above(y),
                 .level = check.can_tie(y),
                 .below = check.can_end_below(y)};
    const int32_t sides = static_cast<int32_t>(o.above) +
                          static_cast<int32_t>(o.level) +
                          static_cast<int32_t>(o.below);
    if (sides > 1) {
      out.open.push_back(o);
    } else if (o.above) {
      out.above |= team_bit(y);
    } else if (o.level) {
      out.level |= team_bit(y);
    } else {
      out.below |= team_bit(y);
    }
  });
  out.can_be_ahead.assign(out.open.size() + 1, 0);
  for (size_t i = out.open.size(); i-- > 0;) {
    const Open& o = out.open[i];
    out.can_be_ahead[i] =
        out.can_be_ahead[i + 1] | (o.above || o.level ? team_bit(o.team) : 0);
  }
  return out;
}

}  // namespace nflseedr::clinching
