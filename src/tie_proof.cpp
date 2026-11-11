#include "engine/tie_proof.hpp"

#include <algorithm>
#include <array>
#include <bit>
#include <functional>

namespace nflseedr {

namespace {

[[nodiscard]] constexpr TeamMask team_bit(TeamId t) noexcept {
  return TeamMask{1} << static_cast<uint32_t>(t);
}

[[nodiscard]] constexpr bool has(TeamMask mask, TeamId t) noexcept {
  return (mask & team_bit(t)) != 0;
}

template <typename F>
void for_each_team(TeamMask mask, F f) {
  while (mask != 0) {
    f(static_cast<TeamId>(std::countr_zero(mask)));
    mask &= mask - 1;
  }
}

// Calls f(s) for every subset s of `mask` (including 0 and `mask`) until f
// returns false. True if it never did.
template <typename F>
[[nodiscard]] bool all_subsets(TeamMask mask, F f) {
  for (TeamMask s = mask;; s = (s - 1) & mask) {
    if (!f(s)) return false;
    if (s == 0) return true;
  }
}

// The steps the engine uses through SOS (depth 2), see procedures.cpp
constexpr std::array kDivisionSteps{Step::kHeadToHead, Step::kDivisionPct,
                                    Step::kCommonPct,  Step::kConferencePct,
                                    Step::kSov,        Step::kSos};
constexpr std::array kConferenceSteps{Step::kHeadToHeadSweep,
                                      Step::kConferencePct, Step::kCommonPct,
                                      Step::kSov, Step::kSos};

// D14: the conference common games step needs four common opponents
constexpr int32_t kMinCommonOpponents = 4;

}  // namespace

TieProof::TieProof(const SeasonData& played, std::span<const Game> played_games,
                   std::span<const Game> remaining, const League& league,
                   TeamId x, bool x_wins_out)
    : league_(&league), x_(x) {
  for (TeamId t = 0; t < kTeams; ++t) {
    if (league.division[t] == league.division[x]) division_ |= team_bit(t);
    if (league.conference[t] == league.conference[x]) {
      conference_ |= team_bit(t);
    }
    for (TeamId u = 0; u < kTeams; ++u) {
      games_[t][u] = played.h2h_games[t][u];
      known_games_[t][u] = played.h2h_games[t][u];
      known_half_wins_[t][u] = played.h2h_half_wins[t][u];
    }
  }
  // SOV counts wins, which half-wins can't tell apart from two ties
  for (const Game& g : played_games) {
    if (g.type != GameType::kRegular) continue;
    if (g.result > 0) ++known_wins_[g.home][g.away];
    if (g.result < 0) ++known_wins_[g.away][g.home];
  }
  for (const Game& g : remaining) {
    ++games_[g.away][g.home];
    ++games_[g.home][g.away];
    if (g.away == x || g.home == x) {
      const TeamId other = g.away == x ? g.home : g.away;
      const TeamId winner = x_wins_out ? x : other;
      const TeamId loser = x_wins_out ? other : x;
      ++known_games_[x][other];
      ++known_games_[other][x];
      known_half_wins_[winner][loser] += 2;
      ++known_wins_[winner][loser];
    } else {
      ++open_[g.away][g.home];
      ++open_[g.home][g.away];
    }
  }
  for (TeamId t = 0; t < kTeams; ++t) {
    int32_t known = 0;
    for (TeamId u = 0; u < kTeams; ++u) {
      final_games_[t] += games_[t][u];
      open_games_[t] += open_[t][u];
      known += known_half_wins_[t][u];
    }
    half_wins_low_[t] = known;
    half_wins_high_[t] = known + 2 * open_games_[t];
  }
  // Every team of a tie group ends with X's win percentage, so with exactly
  // these half-wins (X's own are fixed by the check)
  const int32_t x_half_wins = half_wins_low_[x];
  for (TeamId t = 0; t < kTeams; ++t) {
    const int64_t scaled = static_cast<int64_t>(x_half_wins) * final_games_[t];
    const bool exact = scaled % final_games_[x] == 0;
    const auto level = static_cast<int32_t>(scaled / final_games_[x]);
    tie_half_wins_[t] =
        exact && level >= half_wins_low_[t] && level <= half_wins_high_[t]
            ? level
            : kMissing;
  }
}

// Half-wins of a tied team: exactly its tie level
int32_t TieProof::low_in(TeamId t, TeamMask group) const {
  return has(group, t) ? tie_half_wins_[t] : half_wins_low_[t];
}
int32_t TieProof::high_in(TeamId t, TeamMask group) const {
  return has(group, t) ? tie_half_wins_[t] : half_wins_high_[t];
}

// Win percentage of tied team t against `opponents`. t's open games bring
// exactly the half-wins t needs for the tie level, so the open games against
// `opponents` bring between what the other open games can't and all of it.
TieProof::Range TieProof::pct_range(TeamId t, TeamMask opponents) const {
  int32_t games = 0;
  int32_t known = 0;
  int32_t open = 0;
  for_each_team(opponents, [&](TeamId u) {
    games += games_[t][u];
    known += known_half_wins_[t][u];
    open += open_[t][u];
  });
  const int32_t needed = tie_half_wins_[t] - half_wins_low_[t];
  const int32_t open_elsewhere = open_games_[t] - open;
  const int32_t low = std::max(0, needed - 2 * open_elsewhere);
  const int32_t high = std::min(2 * open, needed);
  return {.low = Ratio(known + low, 2 * games),
          .high = Ratio(known + high, 2 * games)};
}

// Head-to-head sweep: 2 = beat every other team of the group, 0 = lost to
// every one, 1 otherwise (or didn't play all of them)
TieProof::Range TieProof::sweep_range(TeamId t, TeamMask group) const {
  bool played_all = true;
  int32_t known_games = 0;
  int32_t known = 0;
  int32_t open = 0;
  for_each_team(group, [&](TeamId u) {
    if (u == t) return;
    played_all = played_all && games_[t][u] > 0;
    known_games += known_games_[t][u];
    known += known_half_wins_[t][u];
    open += open_[t][u];
  });
  if (!played_all) return {.low = Ratio(1, 1), .high = Ratio(1, 1)};
  const bool can_win_all = known == 2 * known_games;
  const bool can_lose_all = known == 0;
  if (open == 0) {
    int32_t v = 1;
    if (can_win_all) v = 2;
    if (can_lose_all) v = 0;
    return {.low = Ratio(v, 1), .high = Ratio(v, 1)};
  }
  // An open game can always end in a tie, which makes it 1
  return {.low = Ratio(can_lose_all ? 0 : 1, 1),
          .high = Ratio(can_win_all ? 2 : 1, 1)};
}

// Opponents' half-wins over their games, once per game against t
TieProof::Range TieProof::sos_range(TeamId t, TeamMask group) const {
  int32_t low = 0;
  int32_t high = 0;
  int32_t games = 0;
  for (TeamId o = 0; o < kTeams; ++o) {
    low += games_[t][o] * low_in(o, group);
    high += games_[t][o] * high_in(o, group);
    games += games_[t][o] * final_games_[o];
  }
  return {.low = Ratio(low, 2 * games), .high = Ratio(high, 2 * games)};
}

// The same for beaten opponents, once per win. Only known if all of t's
// games are; otherwise anything between 0 and 1.
TieProof::Range TieProof::sov_range(TeamId t, TeamMask group) const {
  if (open_games_[t] > 0) return {.low = Ratio(0, 1), .high = Ratio(1, 1)};
  int32_t low = 0;
  int32_t high = 0;
  int32_t games = 0;
  for (TeamId o = 0; o < kTeams; ++o) {
    low += known_wins_[t][o] * low_in(o, group);
    high += known_wins_[t][o] * high_in(o, group);
    games += known_wins_[t][o] * final_games_[o];
  }
  return {.low = Ratio(low, 2 * games), .high = Ratio(high, 2 * games)};
}

TieProof::StepRanges TieProof::ranges(Step step, TeamMask group,
                                      Procedure procedure) const {
  StepRanges out;
  for_each_team(group, [&](TeamId t) {
    if (tie_half_wins_[t] == kMissing) {
      throw EngineError("A team in a tie group can't reach the tie level");
    }
  });
  TeamMask common = 0;
  if (step == Step::kCommonPct) {
    // Opponents every team of the group plays. Tied teams never are.
    for (TeamId o = 0; o < kTeams; ++o) {
      if (has(group, o)) continue;
      bool all = true;
      for_each_team(group, [&](TeamId t) { all = all && games_[t][o] > 0; });
      if (all) common |= team_bit(o);
    }
    if (procedure == Procedure::kConference &&
        std::popcount(common) < kMinCommonOpponents) {
      out.applies = false;
      return out;
    }
  }
  for_each_team(group, [&](TeamId t) {
    const TeamMask others = group & ~team_bit(t);
    const TeamMask own_division = division_of(t) & ~team_bit(t);
    const TeamMask own_conference = conference_of(t) & ~team_bit(t);
    switch (step) {
      case Step::kHeadToHead:
        out.range[t] = pct_range(t, others);
        break;
      case Step::kHeadToHeadSweep:
        out.range[t] = sweep_range(t, group);
        break;
      case Step::kDivisionPct:
        out.range[t] = pct_range(t, own_division);
        break;
      case Step::kCommonPct:
        out.range[t] = pct_range(t, common);
        break;
      case Step::kConferencePct:
        out.range[t] = pct_range(t, own_conference);
        break;
      case Step::kSov:
        out.range[t] = sov_range(t, group);
        break;
      case Step::kSos:
        out.range[t] = sos_range(t, group);
        break;
      default:
        throw EngineError("Step not used in clinching proofs");
    }
  });
  return out;
}

TeamMask TieProof::division_of(TeamId t) const {
  TeamMask out = 0;
  for (TeamId u = 0; u < kTeams; ++u) {
    if (league_->division[u] == league_->division[t]) out |= team_bit(u);
  }
  return out;
}

TeamMask TieProof::conference_of(TeamId t) const {
  TeamMask out = 0;
  for (TeamId u = 0; u < kTeams; ++u) {
    if (league_->conference[u] == league_->conference[t]) out |= team_bit(u);
  }
  return out;
}

std::span<const Step> TieProof::steps(Procedure procedure) {
  if (procedure == Procedure::kDivision) return kDivisionSteps;
  return kConferenceSteps;
}

// X is picked first if a step makes X best for sure. A step where X is at
// least as good as everyone for sure can also leave X with the teams that
// can be level with X; then X must win from every such smaller group (the
// procedure restarts with it). If no step settles it, a coin toss could.
bool TieProof::wins(TeamMask group, Procedure procedure) {
  const TeamMask others = group & ~team_bit(x_);
  if (others == 0) return true;
  auto& cache = cache_[static_cast<size_t>(procedure)][0];
  if (const auto it = cache.find(group); it != cache.end()) return it->second;
  bool result = false;
  for (const Step step : steps(procedure)) {
    const StepRanges r = ranges(step, group, procedure);
    if (!r.applies) continue;
    const Ratio x_low = r.range[x_].low;
    bool best = true;
    bool never_behind = true;
    TeamMask level = 0;  // teams that can end level with X
    for_each_team(others, [&](TeamId y) {
      best = best && x_low > r.range[y].high;
      never_behind = never_behind && x_low >= r.range[y].high;
      if (r.range[y].high == x_low) level |= team_bit(y);
    });
    if (best) {
      result = true;
      break;
    }
    if (!never_behind) break;
    const bool smaller_groups_won = all_subsets(level, [&](TeamMask s) {
      return s == others || wins(team_bit(x_) | s, procedure);
    });
    if (!smaller_groups_won) break;
    // The step always eliminates someone, or nobody and the next step decides
    if (level != others) {
      result = true;
      break;
    }
  }
  cache[group] = result;
  return result;
}

// The mirror of wins(): X isn't picked if some team is better than X for
// sure, or if teams that are at least as good as X for sure stay with X in
// every smaller group.
bool TieProof::loses(TeamMask group, Procedure procedure) {
  const TeamMask others = group & ~team_bit(x_);
  if (others == 0) return false;
  auto& cache = cache_[static_cast<size_t>(procedure)][1];
  if (const auto it = cache.find(group); it != cache.end()) return it->second;
  bool result = false;
  for (const Step step : steps(procedure)) {
    const StepRanges r = ranges(step, group, procedure);
    if (!r.applies) continue;
    const Ratio x_high = r.range[x_].high;
    bool beaten = false;
    TeamMask at_least_x = 0;  // never worse than X
    for_each_team(others, [&](TeamId y) {
      beaten = beaten || r.range[y].low > x_high;
      if (r.range[y].low >= x_high) at_least_x |= team_bit(y);
    });
    if (beaten) {
      result = true;
      break;
    }
    if (at_least_x == 0) break;
    // If X survives the step, so do these teams
    const TeamMask rest = others & ~at_least_x;
    const TeamMask keep = team_bit(x_) | at_least_x;
    const bool smaller_groups_lost = all_subsets(rest, [&](TeamMask s) {
      return (keep | s) == group || loses(keep | s, procedure);
    });
    if (!smaller_groups_lost) break;
    // A team that can't be level with X leaves whenever X stays
    bool shrinks = false;
    for_each_team(rest, [&](TeamId y) {
      shrinks = shrinks || r.range[y].high < r.range[x_].low;
    });
    if (shrinks) {
      result = true;
      break;
    }
  }
  cache[group] = result;
  return result;
}

template <typename F>
bool TieProof::all_representatives(TeamMask teams, F f) const {
  // Teams per division, without X's division
  std::array<TeamMask, kDivisions> by_division{};
  for_each_team(teams & ~division_, [&](TeamId t) {
    by_division[league_->division[t]] |= team_bit(t);
  });
  // Choose one team or none per division
  std::function<bool(int32_t, TeamMask)> choose = [&](int32_t d,
                                                      TeamMask chosen) {
    if (d == kDivisions) return chosen == 0 || f(chosen);
    if (!choose(d + 1, chosen)) return false;
    bool ok = true;
    for_each_team(by_division[d], [&](TeamId t) {
      ok = ok && choose(d + 1, chosen | team_bit(t));
    });
    return ok;
  };
  return choose(0, 0);
}

bool TieProof::wins_division_ties(TeamMask teams) {
  return all_subsets(teams & division_ & ~team_bit(x_), [&](TeamMask s) {
    return s == 0 || wins(team_bit(x_) | s, Procedure::kDivision);
  });
}

// A rival is behind X for sure if X is picked first from every group with
// it (X ranks ahead in the division), and ahead of X for sure if X is never
// picked from such a group.
TieSides TieProof::division_sides(TeamMask tied) {
  TieSides out;
  const TeamMask rivals = tied & division_ & ~team_bit(x_);
  for_each_team(rivals, [&](TeamId y) {
    const TeamMask with_y = team_bit(x_) | team_bit(y);
    const TeamMask rest = rivals & ~team_bit(y);
    if (all_subsets(rest, [&](TeamMask s) {
          return wins(with_y | s, Procedure::kDivision);
        })) {
      out.x_ahead |= team_bit(y);
    }
    if (all_subsets(rest, [&](TeamMask s) {
          return loses(with_y | s, Procedure::kDivision);
        })) {
      out.x_behind |= team_bit(y);
    }
  });
  return out;
}

// Rivals as in division_sides(): their division ranks decide the reduction
// and ties of one division. Teams of another division d: X is never picked
// while one of them is in the tied group if X loses the conference
// procedure against every set of division representatives that includes
// one of d. X is always picked before them if X also survives every
// reduction, i.e. wins every division tie.
TieSides TieProof::conference_sides(TeamMask tied) {
  TieSides out = division_sides(tied);
  const bool never_reduced = wins_division_ties(tied);
  const TeamMask others = tied & conference_ & ~division_;
  for (int32_t d = 0; d < kDivisions; ++d) {
    TeamMask in_d = 0;
    for_each_team(others, [&](TeamId t) {
      if (league_->division[t] == d) in_d |= team_bit(t);
    });
    if (in_d == 0) continue;
    if (never_reduced && all_representatives(others, [&](TeamMask r) {
          return (r & in_d) == 0 ||
                 wins(team_bit(x_) | r, Procedure::kConference);
        })) {
      out.x_ahead |= in_d;
    }
    if (all_representatives(others, [&](TeamMask r) {
          return (r & in_d) == 0 ||
                 loses(team_bit(x_) | r, Procedure::kConference);
        })) {
      out.x_behind |= in_d;
    }
  }
  return out;
}

}  // namespace nflseedr
