#include "engine/tie_proof.hpp"

#include <algorithm>
#include <array>
#include <bit>
#include <limits>
#include <optional>
#include <vector>

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

// Open games of two teams whose results SOV comparisons enumerate (at most
// 3^n results)
constexpr size_t kMaxEnumeratedGames = 4;

}  // namespace

TieProof::TieProof(const SeasonData& played, std::span<const Game> played_games,
                   std::span<const Game> remaining, const League& league,
                   TeamId x, bool x_wins_out)
    : league_(&league), x_(x) {
  for (TeamId t = 0; t < kTeams; ++t) {
    for (TeamId u = 0; u < kTeams; ++u) {
      if (league.division[u] == league.division[t]) {
        division_of_[t] |= team_bit(u);
      }
      if (league.conference[u] == league.conference[t]) {
        conference_of_[t] |= team_bit(u);
      }
    }
  }
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
  for (TeamId u = 0; u < kTeams; ++u) {
    for (TeamId v = u + 1; v < kTeams; ++v) {
      if (open_[u][v] > 0)
        open_pairs_.push_back({.first = u, .second = v, .count = open_[u][v]});
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
// An opponent in the tie `group` also ends at its tie level, which bounds
// what it can lose (or win) against t in the same way.
TieProof::Range TieProof::pct_range(TeamId t, TeamMask opponents,
                                    TeamMask group) const {
  int32_t games = 0;
  int32_t known = 0;
  int32_t open = 0;
  int32_t open_low = 0;   // half-wins t gets at least from these open games
  int32_t open_high = 0;  // and at most
  for_each_team(opponents, [&](TeamId u) {
    games += games_[t][u];
    known += known_half_wins_[t][u];
    const int32_t vs_u = 2 * open_[t][u];
    open += open_[t][u];
    if (vs_u == 0) return;
    if (!has(group, u)) {
      open_high += vs_u;
      return;
    }
    const int32_t needed_u = tie_half_wins_[u] - half_wins_low_[u];
    const int32_t elsewhere_u = 2 * (open_games_[u] - open_[t][u]);
    open_low += vs_u - std::min(vs_u, needed_u);
    open_high += vs_u - std::max(0, needed_u - elsewhere_u);
  });
  const int32_t needed = tie_half_wins_[t] - half_wins_low_[t];
  const int32_t open_elsewhere = open_games_[t] - open;
  const int32_t low = std::max(open_low, needed - 2 * open_elsewhere);
  const int32_t high = std::min({2 * open, needed, open_high});
  // Empty: t and its opponents can't all end at their tie levels
  if (low > high) {
    return {.low = Ratio(1, 1), .high = Ratio(0, 1)};
  }
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
  StepRanges out{.step = step, .group = group, .procedure = procedure};
  for_each_team(group, [&](TeamId t) {
    if (tie_half_wins_[t] == kMissing) {
      throw EngineError("A team in a tie group can't reach the tie level");
    }
  });
  TeamMask common = 0;
  if (step == Step::kCommonPct) {
    common = common_opponents(group);
    out.common = common;
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
        out.range[t] = pct_range(t, others, group);
        break;
      case Step::kHeadToHeadSweep:
        out.range[t] = sweep_range(t, group);
        break;
      case Step::kDivisionPct:
        out.range[t] = pct_range(t, own_division, group);
        break;
      case Step::kCommonPct:
        out.range[t] = pct_range(t, common, group);
        break;
      case Step::kConferencePct:
        out.range[t] = pct_range(t, own_conference, group);
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

// a's SOV (SOS) times b's denominator minus b's times a's, lowest over the
// open games not between two teams of `settled`, so only its sign means
// something. The weights (wins or games against each opponent) are fixed,
// so the difference is linear in the opponents' half-wins: tied teams end
// with their tie level, the others with `half_wins` plus their shares of
// the open games, and each open game goes the way that lowers the
// difference most. Unlike low(a) - high(b), a beaten opponent of both
// counts the same for both.
int64_t TieProof::linear_strength_difference(
    const TeamArray<int32_t>& weight_a, const TeamArray<int32_t>& weight_b,
    const TeamArray<int32_t>& half_wins, TeamMask group,
    TeamMask settled) const {
  const auto denominator = [&](const TeamArray<int32_t>& weight) {
    int64_t out = 0;
    for (TeamId o = 0; o < kTeams; ++o) {
      out += 2 * static_cast<int64_t>(weight[o]) * final_games_[o];
    }
    // Without wins, SOV is 0 (Ratio), and so are all weights
    return out == 0 ? int64_t{1} : out;
  };
  const int64_t den_a = denominator(weight_a);
  const int64_t den_b = denominator(weight_b);
  TeamArray<int64_t> coef{};
  int64_t out = 0;
  for (TeamId o = 0; o < kTeams; ++o) {
    coef[o] = weight_a[o] * den_b - weight_b[o] * den_a;
    out += coef[o] * (has(group, o) ? tie_half_wins_[o] : half_wins[o]);
    // A tied team's open games bring exactly its tie level
    if (has(group, o)) coef[o] = 0;
  }
  for (const OpenGames& g : open_pairs_) {
    if (has(settled, g.first) || has(settled, g.second)) continue;
    out += int64_t{2} * g.count * std::min(coef[g.first], coef[g.second]);
  }
  return out;
}

// Opponents every team of the group plays. Tied teams never are.
TeamMask TieProof::common_opponents(TeamMask group) const {
  TeamMask out = 0;
  for (TeamId o = 0; o < kTeams; ++o) {
    if (has(group, o)) continue;
    bool all = true;
    for_each_team(group, [&](TeamId t) { all = all && games_[t][o] > 0; });
    if (all) out |= team_bit(o);
  }
  return out;
}

// Value of team t at a record step (or the head-to-head sweep, see
// sweep_range()) with all of its games known: `half_wins` against each
// opponent
Ratio TieProof::value(Step step, TeamId t, const TeamArray<int32_t>& half_wins,
                      TeamMask group, TeamMask common) const {
  const auto pct = [&](TeamMask opponents) {
    int32_t games = 0;
    int32_t won = 0;
    for_each_team(opponents, [&](TeamId u) {
      games += games_[t][u];
      won += half_wins[u];
    });
    return Ratio(won, 2 * games);
  };
  const TeamMask others = group & ~team_bit(t);
  switch (step) {
    case Step::kHeadToHead:
      return pct(others);
    case Step::kHeadToHeadSweep: {
      bool played_all = true;
      int32_t games = 0;
      int32_t won = 0;
      for_each_team(others, [&](TeamId u) {
        played_all = played_all && games_[t][u] > 0;
        games += games_[t][u];
        won += half_wins[u];
      });
      if (!played_all) return {1, 1};
      if (won == 2 * games) return {2, 1};
      return won == 0 ? Ratio(0, 1) : Ratio(1, 1);
    }
    case Step::kDivisionPct:
      return pct(division_of(t) & ~team_bit(t));
    case Step::kCommonPct:
      return pct(common);
    case Step::kConferencePct:
      return pct(conference_of(t) & ~team_bit(t));
    default:
      throw EngineError("Step has no value from a team's own games");
  }
}

// The procedure only reaches step r.step with all of r.group if no earlier
// step separated them, so a and b are equal at every earlier step. The
// record steps only depend on a's and b's own games, which `own` fixes; SOV
// doesn't, so it isn't checked.
TieProof::EarlierSteps TieProof::earlier_steps(const StepRanges& r) const {
  static_assert(kDivisionSteps.size() <= kMaxSteps &&
                kConferenceSteps.size() <= kMaxSteps);
  EarlierSteps out;
  for (const Step step : steps(r.procedure)) {
    if (step == r.step) break;
    if (step == Step::kSov || step == Step::kSos) continue;
    TeamMask common = 0;
    if (step == Step::kCommonPct) {
      common = common_opponents(r.group);
      if (r.procedure == Procedure::kConference &&
          std::popcount(common) < kMinCommonOpponents) {
        continue;
      }
    }
    out.steps[out.size++] = {.step = step, .common = common};
  }
  return out;
}

bool TieProof::equal_before(const EarlierSteps& earlier, TeamMask group,
                            TeamId a, TeamId b, const OwnResults& own) const {
  return std::ranges::all_of(
      std::span(earlier.steps).first(earlier.size), [&](const EarlierStep& e) {
        return value(e.step, a, own.half_wins_a, group, e.common) ==
               value(e.step, b, own.half_wins_b, group, e.common);
      });
}

// Lowest value of a's step value minus b's (for SOV and SOS a bound, scaled
// so that only its sign means something) over the results of a's and b's
// open games (if there are few) that leave both at their tie levels and
// equal at every earlier step. No such result: the step is never reached
// with both, and the result is the largest value.
std::optional<int64_t> TieProof::lowest_difference(const StepRanges& r,
                                                   TeamId a, TeamId b) const {
  const TeamMask pair = team_bit(a) | team_bit(b);
  if (open_games_[a] + open_games_[b] >
      static_cast<int32_t>(kMaxEnumeratedGames)) {
    // SOS doesn't depend on who wins a's and b's games
    if (r.step != Step::kSos) return std::nullopt;
    return linear_strength_difference(games_[a], games_[b], half_wins_low_,
                                      r.group, 0);
  }
  const EarlierSteps earlier = earlier_steps(r);
  const bool strength = r.step == Step::kSov || r.step == Step::kSos;
  // A record step with nothing to condition on: the ranges say it all
  if (!strength && earlier.size == 0) return std::nullopt;
  std::array<OpenGames, kMaxEnumeratedGames> games{};
  size_t n_games = 0;
  for (const OpenGames& g : open_pairs_) {
    if (has(pair, g.first) || has(pair, g.second)) {
      for (int32_t i = 0; i < g.count; ++i) {
        games[n_games++] = {.first = g.first, .second = g.second, .count = 1};
      }
    }
  }
  OwnResults own{.half_wins = half_wins_low_,
                 .half_wins_a = known_half_wins_[a],
                 .half_wins_b = known_half_wins_[b],
                 .wins_a = known_wins_[a],
                 .wins_b = known_wins_[b]};
  // Every team of the group can still end at its tie level: games not
  // enumerated yet (or at all) can bring between nothing and two half-wins
  TeamArray<int32_t> undecided = open_games_;
  const auto reachable = [&](TeamId t) {
    return own.half_wins[t] <= tie_half_wins_[t] &&
           own.half_wins[t] + 2 * undecided[t] >= tie_half_wins_[t];
  };
  const auto all_reachable = [&] {
    bool ok = true;
    for_each_team(r.group | pair, [&](TeamId t) { ok = ok && reachable(t); });
    return ok;
  };
  const auto difference = [&]() -> int64_t {
    switch (r.step) {
      case Step::kSov:
        return linear_strength_difference(own.wins_a, own.wins_b, own.half_wins,
                                          r.group, pair);
      case Step::kSos:
        return linear_strength_difference(games_[a], games_[b], own.half_wins,
                                          r.group, pair);
      default: {
        const auto ordering =
            value(r.step, a, own.half_wins_a, r.group, r.common) <=>
            value(r.step, b, own.half_wins_b, r.group, r.common);
        if (ordering > 0) return 1;
        return ordering < 0 ? -1 : 0;
      }
    }
  };
  // Depth first over the results (half-wins of the first team: 0, 1 or 2),
  // dropping results that can't leave the group at its tie levels
  std::optional<int64_t> out;
  bool reached = false;
  const auto visit = [&](const auto& self, size_t i) -> void {
    if (!all_reachable()) return;
    if (i == n_games) {
      if (!equal_before(earlier, r.group, a, b, own)) return;
      reached = true;
      const int64_t d = difference();
      out = out ? std::min(*out, d) : d;
      return;
    }
    const auto [first, second, count] = games[i];
    for (const int32_t result : {0, 1, 2}) {
      const auto record = [&](int32_t sign) {
        undecided[first] -= sign;
        undecided[second] -= sign;
        own.half_wins[first] += sign * result;
        own.half_wins[second] += sign * (2 - result);
        if (first == a) own.half_wins_a[second] += sign * result;
        if (second == a) own.half_wins_a[first] += sign * (2 - result);
        if (first == b) own.half_wins_b[second] += sign * result;
        if (second == b) own.half_wins_b[first] += sign * (2 - result);
        if (result == 1) return;
        const TeamId winner = result == 2 ? first : second;
        const TeamId loser = result == 2 ? second : first;
        if (winner == a) own.wins_a[loser] += sign;
        if (winner == b) own.wins_b[loser] += sign;
      };
      record(1);
      self(self, i + 1);
      record(-1);
    }
  };
  visit(visit, 0);
  if (!reached) return std::numeric_limits<int64_t>::max();
  return out;
}

TieProof::Lead TieProof::lead(const StepRanges& r, TeamId a, TeamId b) const {
  if (r.range[a].low > r.range[b].high) return Lead::kAhead;
  // Behind for sure: a bound of the difference could only make it ahead
  // where the step is never reached with both
  if (r.range[a].high < r.range[b].low) return Lead::kNone;
  Lead out =
      r.range[a].low >= r.range[b].high ? Lead::kNeverBehind : Lead::kNone;
  const std::optional<int64_t> d = lowest_difference(r, a, b);
  if (d && *d > 0) return Lead::kAhead;
  if (d && *d == 0) out = Lead::kNeverBehind;
  return out;
}

TeamMask TieProof::division_of(TeamId t) const { return division_of_[t]; }

TeamMask TieProof::conference_of(TeamId t) const { return conference_of_[t]; }

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
    bool best = true;
    bool never_behind = true;
    TeamMask level = 0;  // teams that can end level with X
    for_each_team(others, [&](TeamId y) {
      const Lead l = lead(r, x_, y);
      best = best && l == Lead::kAhead;
      never_behind = never_behind && l != Lead::kNone;
      if (l == Lead::kNeverBehind) level |= team_bit(y);
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
    bool beaten = false;
    TeamMask at_least_x = 0;  // never worse than X
    for_each_team(others, [&](TeamId y) {
      const Lead l = lead(r, y, x_);
      beaten = beaten || l == Lead::kAhead;
      if (l != Lead::kNone) at_least_x |= team_bit(y);
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
      shrinks = shrinks || lead(r, x_, y) == Lead::kAhead;
    });
    if (shrinks) {
      result = true;
      break;
    }
  }
  cache[group] = result;
  return result;
}

// One pick drops the teams that lose a step and restarts with the others
// until one is left (procedures.cpp). A step drops every team some other team
// beats for sure and keeps every team no team can beat, so the survivors are
// between these two sets. If every step can leave all teams level, a coin
// toss can pick any of them.
TeamMask TieProof::possible_picks(TeamMask group, Procedure procedure) {
  if (std::popcount(group) == 1) return group;
  auto& cache = picks_cache_[static_cast<size_t>(procedure)];
  if (const auto it = cache.find(group); it != cache.end()) return it->second;
  TeamMask out = 0;
  bool all_level = true;
  for (const Step step : steps(procedure)) {
    const StepRanges r = ranges(step, group, procedure);
    if (!r.applies) continue;
    TeamMask can_survive = 0;
    TeamMask must_survive = 0;
    for_each_team(group, [&](TeamId t) {
      bool beaten = false;
      bool unbeatable = true;
      for_each_team(group & ~team_bit(t), [&](TeamId u) {
        beaten = beaten || lead(r, u, t) == Lead::kAhead;
        unbeatable = unbeatable && lead(r, t, u) != Lead::kNone;
      });
      if (!beaten) can_survive |= team_bit(t);
      if (unbeatable) must_survive |= team_bit(t);
    });
    const TeamMask optional = can_survive & ~must_survive;
    (void)all_subsets(optional, [&](TeamMask s) {
      const TeamMask survivors = must_survive | s;
      if (survivors != 0 && survivors != group) {
        out |= possible_picks(survivors, procedure);
      }
      return true;
    });
    if (can_survive != group) {
      all_level = false;
      break;
    }
  }
  if (all_level) out |= group;
  cache[group] = out;
  return out;
}

bool TieProof::ranks_ahead(TeamMask group, TeamId y) {
  const uint64_t key = (static_cast<uint64_t>(y) << 32U) | group;
  auto& cache = rank_cache_[0];
  if (const auto it = cache.find(key); it != cache.end()) return it->second;
  bool result = wins(group, Procedure::kDivision);
  if (!result) {
    const TeamMask picks = possible_picks(group, Procedure::kDivision);
    result = !has(picks, y);
    for_each_team(picks & ~team_bit(x_), [&](TeamId p) {
      result = result && ranks_ahead(group & ~team_bit(p), y);
    });
  }
  cache[key] = result;
  return result;
}

bool TieProof::ranks_behind(TeamMask group, TeamId y) {
  const uint64_t key = (static_cast<uint64_t>(y) << 32U) | group;
  auto& cache = rank_cache_[1];
  if (const auto it = cache.find(key); it != cache.end()) return it->second;
  const TeamMask picks = possible_picks(group, Procedure::kDivision);
  bool result = !has(picks, x_) || loses(group, Procedure::kDivision);
  for_each_team(picks & ~team_bit(x_) & ~team_bit(y), [&](TeamId p) {
    result = result && ranks_behind(group & ~team_bit(p), y);
  });
  cache[key] = result;
  return result;
}

bool TieProof::can_end_above_x(TeamId z) const {
  if (final_games_[z] == 0) return false;
  return static_cast<int64_t>(half_wins_high_[z]) * final_games_[x_] >
         static_cast<int64_t>(half_wins_low_[x_]) * final_games_[z];
}

std::vector<TeamMask> TieProof::representative_sets(TeamMask teams) const {
  // Teams per division, without X's division
  std::array<TeamMask, kDivisions> by_division{};
  for_each_team(teams & ~division_, [&](TeamId t) {
    by_division[league_->division[t]] |= team_bit(t);
  });
  // One team or none per division
  std::vector<TeamMask> out{0};
  for (const TeamMask in_d : by_division) {
    const size_t n = out.size();
    for_each_team(in_d, [&](TeamId t) {
      for (size_t i = 0; i < n; ++i) out.push_back(out[i] | team_bit(t));
    });
  }
  out.erase(out.begin());
  return out;
}

bool TieProof::wins_division_ties(TeamMask teams) {
  return all_subsets(teams & division_ & ~team_bit(x_), [&](TeamMask s) {
    return s == 0 || wins(team_bit(x_) | s, Procedure::kDivision);
  });
}

// For a wild card, X only has to be the best team of its division in the
// wild card pool: the division winner isn't in it. With rivals S tied with
// X, that holds if X wins the division tie (X is the division winner or
// second behind a rival above X's level), or, if no rival can end above X,
// X wins the tie of the others after any first pick.
bool TieProof::survives_wild_card_reduction(TeamMask tied) {
  return all_subsets(tied & division_ & ~team_bit(x_), [&](TeamMask s) {
    if (s == 0) return true;
    const TeamMask group = team_bit(x_) | s;
    if (wins(group, Procedure::kDivision)) return true;
    bool ok = true;
    for_each_team(division_ & ~group,
                  [&](TeamId z) { ok = ok && !can_end_above_x(z); });
    for_each_team(possible_picks(group, Procedure::kDivision) & ~team_bit(x_),
                  [&](TeamId p) {
                    ok = ok && wins(group & ~team_bit(p), Procedure::kDivision);
                  });
    return ok;
  });
}

// For a wild card: the conference procedure reduces the pool again after
// every pick, and a rival that ranks ahead of X in the division is ahead of
// X anyway (the flows count it). So y of division d (`in_d`) only ranks
// ahead of X if it is picked while it represents d and X's division is
// represented by X or by a tied rival X doesn't always rank ahead of. If y
// can't be picked from any such set, X is picked before y.
bool TieProof::never_picked_before_x(TeamId y, TeamMask in_d,
                                     std::span<const TeamMask> reps,
                                     TeamMask x_reps) {
  return std::ranges::all_of(reps, [&](TeamMask r) {
    if ((r & in_d) != team_bit(y)) return true;
    bool ok = true;
    for_each_team(x_reps, [&](TeamId e) {
      ok = ok &&
           !has(possible_picks(team_bit(e) | r, Procedure::kConference), y);
    });
    return ok;
  });
}

// y wins its division whenever it ends with X's win percentage: no other
// team of its division can end above X, and y is the only possible first
// pick of every division tie of y with teams of `tied` (and X, which is
// always level, if y is a rival). Then y is never in the wild card pool.
bool TieProof::wins_division_when_level(TeamId y, TeamMask tied) {
  const TeamMask division = division_of(y);
  const TeamMask with_y = team_bit(y) | (division & team_bit(x_));
  bool above = false;
  for_each_team(division & ~with_y,
                [&](TeamId t) { above = above || can_end_above_x(t); });
  if (above) return false;
  return all_subsets(tied & division & ~with_y, [&](TeamMask s) {
    // No pick at all: the teams can't all end level
    return (possible_picks(with_y | s, Procedure::kDivision) & ~team_bit(y)) ==
           0;
  });
}

// The mirror of never_picked_before_x(): X is only picked while it
// represents its division, and y (of division d, `in_d`) stays in the pool
// until it is picked, represented by itself or a tied team of d that may
// rank ahead of it. If X can't be picked from any such set, y is picked
// before X.
bool TieProof::x_never_picked_before(TeamId y, TeamMask in_d,
                                     std::span<const TeamMask> reps,
                                     TeamMask tied) {
  TeamMask y_reps = team_bit(y);
  for_each_team(tied & division_of(y) & ~team_bit(y), [&](TeamId w) {
    // y is picked before w in every division tie with both
    const TeamMask pair = team_bit(y) | team_bit(w);
    const bool behind_y =
        all_subsets(tied & division_of(y) & ~pair, [&](TeamMask s) {
          return !has(possible_picks(pair | s, Procedure::kDivision), w);
        });
    if (!behind_y) y_reps |= team_bit(w);
  });
  return std::ranges::all_of(reps, [&](TeamMask r) {
    if ((r & in_d & y_reps) == 0) return true;
    const TeamMask group = team_bit(x_) | r;
    return loses(group, Procedure::kConference) ||
           !has(possible_picks(group, Procedure::kConference), x_);
  });
}

// A rival is behind X for sure if X ranks ahead of it in the division
// ranking of every group with it, and ahead of X for sure if X always ranks
// behind it. Other teams of the group may be picked before both.
TieSides TieProof::division_sides(TeamMask tied) {
  TieSides out;
  const TeamMask rivals = tied & division_ & ~team_bit(x_);
  for_each_team(rivals, [&](TeamId y) {
    const TeamMask with_y = team_bit(x_) | team_bit(y);
    const TeamMask rest = rivals & ~team_bit(y);
    if (all_subsets(rest,
                    [&](TeamMask s) { return ranks_ahead(with_y | s, y); })) {
      out.x_ahead |= team_bit(y);
    }
    if (all_subsets(rest,
                    [&](TeamMask s) { return ranks_behind(with_y | s, y); })) {
      out.x_behind |= team_bit(y);
    }
  });
  return out;
}

// Rivals as in division_sides(): their division ranks decide the reduction
// and ties of one division. Teams of another division d: X is never picked
// while one of them is in the tied group if X loses the conference
// procedure against every set of division representatives that includes
// one of d. X is always picked before team y of d if X also survives every
// reduction, i.e. wins every division tie, and is picked from every set of
// representatives in which y represents d. For a wild card, teams that win
// their division whenever they are level with X are never in the tie, and
// their side doesn't matter (one team of a division ahead of X is free); X
// only has to survive the reduction of the wild card pool.
TieSides TieProof::conference_sides(TeamMask tied, bool wild_card) {
  TieSides out = division_sides(tied);
  const bool never_reduced =
      wild_card ? survives_wild_card_reduction(tied) : wins_division_ties(tied);
  TeamMask others = tied & conference_ & ~division_;
  if (wild_card) {
    for_each_team(others, [&](TeamId y) {
      if (wins_division_when_level(y, tied)) others &= ~team_bit(y);
    });
  }
  if (others == 0) return out;
  const std::vector<TeamMask> reps = representative_sets(others);
  // Wild card: the teams that can represent X's division before X is picked.
  // A rival z in the wild card pool isn't the division winner, and neither
  // is X (the division path covers that), so another rival must end at or
  // above X's level.
  TeamMask x_reps = team_bit(x_);
  if (wild_card) {
    for_each_team(
        tied & division_ & ~team_bit(x_) & ~out.x_ahead, [&](TeamId z) {
          bool other_winner = false;
          for_each_team(
              division_ & ~team_bit(x_) & ~team_bit(z), [&](TeamId w) {
                other_winner =
                    other_winner || has(tied, w) || can_end_above_x(w);
              });
          if (other_winner && !wins_division_when_level(z, tied)) {
            x_reps |= team_bit(z);
          }
        });
  }
  for (int32_t d = 0; d < kDivisions; ++d) {
    TeamMask in_d = 0;
    for_each_team(others, [&](TeamId t) {
      if (league_->division[t] == d) in_d |= team_bit(t);
    });
    if (in_d == 0) continue;
    for_each_team(in_d, [&](TeamId y) {
      // y is only picked while it represents d, and then X is picked first
      const bool x_first =
          never_reduced && std::ranges::all_of(reps, [&](TeamMask r) {
            return (r & in_d) != team_bit(y) ||
                   wins(team_bit(x_) | r, Procedure::kConference);
          });
      if (x_first ||
          (wild_card && never_picked_before_x(y, in_d, reps, x_reps))) {
        out.x_ahead |= team_bit(y);
      }
    });
    if (std::ranges::all_of(reps, [&](TeamMask r) {
          return (r & in_d) == 0 ||
                 loses(team_bit(x_) | r, Procedure::kConference);
        })) {
      out.x_behind |= in_d;
    } else if (wild_card) {
      for_each_team(in_d, [&](TeamId y) {
        if (x_never_picked_before(y, in_d, reps, tied)) {
          out.x_behind |= team_bit(y);
        }
      });
    }
  }
  if (wild_card) {
    out.x_ahead &=
        ~level_division_winners(tied, tied & conference_ & ~division_ & ~others,
                                out.x_ahead & others, others);
  }
  return out;
}

// A team y of another division that wins its division while level with X
// ranks ahead of X, whatever the tiebreakers between them say. The playoff
// count lets one team per division ahead of X go free, so y's side only
// matters if another team z of its division is ahead of X at the same time:
// z level with X in the wild card pool and not behind X for sure there. z
// doesn't count if it wins its division whenever level (`winners`) or always
// ranks ahead of y in their division: then y isn't the winner while z is
// level. No y wins its division if a team of it always ends above X.
// Returns the teams of `x_ahead` (behind X in the pool, of `others`) that
// may break this.
TeamMask TieProof::level_division_winners(TeamMask tied, TeamMask winners,
                                          TeamMask x_ahead, TeamMask others) {
  TeamMask out = 0;
  for_each_team(x_ahead, [&](TeamId y) {
    const TeamMask rest = division_of(y) & ~team_bit(y);
    bool above = false;
    for_each_team(rest, [&](TeamId z) { above = above || always_above_x(z); });
    if (above) return;
    bool threat = false;
    for_each_team(rest & others & ~x_ahead & ~winners, [&](TeamId z) {
      // y is never picked in a division tie with z
      const TeamMask pair = team_bit(y) | team_bit(z);
      threat =
          threat || !all_subsets(tied & rest & ~team_bit(z), [&](TeamMask s) {
            return !has(possible_picks(pair | s, Procedure::kDivision), y);
          });
    });
    if (threat) out |= team_bit(y);
  });
  return out;
}

TieOutcome TieProof::playoff_outcome(TeamMask above, TeamMask level,
                                     int32_t wild_cards) {
  // Every team above X but one per division takes a wild card
  int32_t slots = wild_cards;
  for (TeamMask rest = above; rest != 0;) {
    const TeamMask in_d = rest & division_of(std::countr_zero(rest));
    slots -= std::popcount(in_d) - 1;
    rest &= ~in_d;
  }
  const TeamMask pool = level | team_bit(x_);
  // Divisions whose title goes to a level team
  TeamMask divisions = 0;
  for (TeamMask rest = pool; rest != 0;) {
    const TeamId t = std::countr_zero(rest);
    if ((above & division_of(t)) == 0) divisions |= team_bit(t);
    rest &= ~division_of(t);
  }
  Berth out;
  division_titles(divisions, pool, slots, out);
  return outcome_of(out);
}

TieOutcome TieProof::outcome_of(const Berth& berth) {
  if (berth.reached && berth.missed) return TieOutcome::kEither;
  if (berth.reached) return TieOutcome::kReached;
  if (berth.missed) return TieOutcome::kMissed;
  return TieOutcome::kImpossible;
}

TieOutcome TieProof::division_outcome(TeamMask above, TeamMask level) {
  if ((above & division_) != 0) return TieOutcome::kMissed;
  const TeamMask picks =
      possible_picks((level & division_) | team_bit(x_), Procedure::kDivision);
  return outcome_of(
      {.reached = has(picks, x_), .missed = (picks & ~team_bit(x_)) != 0});
}

TieOutcome TieProof::first_seed_outcome(TeamMask above, TeamMask level) {
  // A team above X puts its division's winner above X (or is a rival)
  if (above != 0) return TieOutcome::kMissed;
  const TeamMask picks =
      possible_picks((level & division_) | team_bit(x_), Procedure::kDivision);
  Berth out{.missed = (picks & ~team_bit(x_)) != 0};
  if (has(picks, x_)) {
    // The other divisions with level teams
    TeamMask divisions = 0;
    for (TeamMask rest = level & ~division_; rest != 0;) {
      const TeamId t = std::countr_zero(rest);
      divisions |= team_bit(t);
      rest &= ~division_of(t);
    }
    first_seed_picks(divisions, level, team_bit(x_), out);
  }
  return outcome_of(out);
}

void TieProof::first_seed_picks(TeamMask divisions, TeamMask level,
                                TeamMask winners, Berth& out) {
  if (out.reached && out.missed) return;
  if (divisions == 0) {
    const TeamMask picks = possible_picks(winners, Procedure::kConference);
    out.reached = out.reached || has(picks, x_);
    out.missed = out.missed || (picks & ~team_bit(x_)) != 0;
    return;
  }
  const TeamId t = std::countr_zero(divisions);
  for_each_team(possible_picks(level & division_of(t), Procedure::kDivision),
                [&](TeamId winner) {
                  first_seed_picks(divisions & (divisions - 1), level,
                                   winners | team_bit(winner), out);
                });
}

void TieProof::division_titles(TeamMask divisions, TeamMask pool, int32_t slots,
                               Berth& out) {
  if (divisions == 0) {
    const Berth picks = wild_card_picks(pool, slots);
    out.reached = out.reached || picks.reached;
    out.missed = out.missed || picks.missed;
    return;
  }
  const TeamId t = std::countr_zero(divisions);
  const TeamMask rest = divisions & (divisions - 1);
  const TeamMask in_d = pool & division_of(t);
  for_each_team(possible_picks(in_d, Procedure::kDivision), [&](TeamId winner) {
    if (winner == x_) {
      out.reached = true;
    } else {
      division_titles(rest, pool & ~team_bit(winner), slots, out);
    }
  });
}

TieProof::Berth TieProof::wild_card_picks(TeamMask pool, int32_t slots) {
  if (slots <= 0) return {.missed = true};
  const uint64_t key = (static_cast<uint64_t>(slots) << 32U) | pool;
  if (const auto it = berth_cache_.find(key); it != berth_cache_.end()) {
    return it->second;
  }
  // The teams that can be the best of their division in the pool
  std::array<TeamMask, kDivisions> candidates{};
  size_t n = 0;
  for (TeamMask rest = pool; rest != 0;) {
    const TeamMask in_d = rest & division_of(std::countr_zero(rest));
    candidates[n++] = possible_picks(in_d, Procedure::kDivision);
    rest &= ~in_d;
  }
  Berth out;
  // One team per division, then one pick of the conference procedure
  const auto visit = [&](const auto& self, size_t i, TeamMask reps) -> void {
    if (out.reached && out.missed) return;
    if (i < n) {
      for_each_team(candidates[i],
                    [&](TeamId t) { self(self, i + 1, reps | team_bit(t)); });
      return;
    }
    for_each_team(possible_picks(reps, Procedure::kConference), [&](TeamId p) {
      if (p == x_) {
        out.reached = true;
        return;
      }
      const Berth next = wild_card_picks(pool & ~team_bit(p), slots - 1);
      out.reached = out.reached || next.reached;
      out.missed = out.missed || next.missed;
    });
  };
  visit(visit, 0, 0);
  berth_cache_[key] = out;
  return out;
}

bool TieProof::always_above_x(TeamId z) const {
  if (final_games_[z] == 0) return false;
  return static_cast<int64_t>(half_wins_low_[z]) * final_games_[x_] >
         static_cast<int64_t>(half_wins_low_[x_]) * final_games_[z];
}

}  // namespace nflseedr
