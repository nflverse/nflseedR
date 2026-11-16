#include "engine/tie_proof.hpp"

#include <algorithm>
#include <array>
#include <bit>
#include <limits>
#include <optional>
#include <utility>
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

// Ways to the division winners that the No. 1 seed follows one by one (P15)
constexpr size_t kMaxWinnerPaths = 256;
// The same for the proof that checks the facts jointly (P17): it only runs
// for the last open groups, where the paths decide. It also splits a way by
// the records of the survivors (P18), which makes more of them.
constexpr size_t kMaxJointWinnerPaths = 65536;

// Half-wins per game, 0 without games (like Ratio)
struct Share {
  int64_t half_wins = 0;
  int64_t games = 1;
};

[[nodiscard]] constexpr Share share(int64_t half_wins, int64_t games) noexcept {
  return games == 0 ? Share{} : Share{.half_wins = half_wins, .games = games};
}

[[nodiscard]] constexpr bool operator<(const Share& a,
                                       const Share& b) noexcept {
  return a.half_wins * b.games < b.half_wins * a.games;
}

}  // namespace

// The pair bounds of a group while one pick takes it through the steps: the
// procedure only reaches a step with the whole group if no earlier step
// separated its teams, so the bounds of a step start from those of the step
// before (P15)
struct TieProof::Passage {
  PairBounds bounds;
  EarlierSteps earlier;
  // The step ranges() saw last, if it is a record step: an earlier step of
  // the next one
  std::optional<EarlierStep> last;
  // The group's records at the earlier steps, and the opponents they count
  std::array<TeamArray<HalfWins>, kMaxSteps> records{};
  std::array<TeamArray<TeamMask>, kMaxSteps> opponents{};
  bool started = false;
};

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
      if (open_[u][v] > 0) {
        open_pairs_.push_back({.first = u, .second = v, .count = open_[u][v]});
        open_opponents_[u] |= team_bit(v);
        open_opponents_[v] |= team_bit(u);
      }
    }
  }
  for (TeamId t = 0; t < kTeams; ++t) {
    int32_t known = 0;
    for (TeamId u = 0; u < kTeams; ++u) {
      if (games_[t][u] > 0) opponents_[t] |= team_bit(u);
      final_games_[t] += games_[t][u];
      open_games_[t] += open_[t][u];
      known += known_half_wins_[t][u];
    }
    half_wins_low_[t] = known;
    half_wins_high_[t] = known + 2 * open_games_[t];
  }
  context_low_ = half_wins_low_;
  context_high_ = half_wins_high_;
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
  return has(group, t) ? tie_half_wins_[t] : context_low_[t];
}
int32_t TieProof::high_in(TeamId t, TeamMask group) const {
  return has(group, t) ? tie_half_wins_[t] : context_high_[t];
}

// Win percentage of tied team t against `opponents`. t's open games bring
// exactly the half-wins t needs for the tie level, so the open games against
// `opponents` bring between what the other open games can't and all of it.
// An opponent in the tie `group` also ends at its tie level, which bounds
// what it can lose (or win) against t in the same way.
TieProof::HalfWins TieProof::half_wins_range(TeamId t, TeamMask opponents,
                                             TeamMask group,
                                             const PairBounds* bounds) const {
  if (bounds != nullptr) return bounded_half_wins(*bounds, t, opponents);
  int32_t games = 0;
  int32_t known = 0;
  int32_t open = 0;
  int32_t open_low = 0;   // half-wins t gets at least from these open games
  int32_t open_high = 0;  // and at most
  int32_t other_low = 0;  // the same for t's other open games
  int32_t other_high = 0;
  for (TeamId u = 0; u < kTeams; ++u) {
    const bool counts = has(opponents, u);
    if (counts) {
      games += games_[t][u];
      known += known_half_wins_[t][u];
      open += open_[t][u];
    }
    const int32_t vs_u = 2 * open_[t][u];
    if (vs_u == 0) continue;
    // Half-wins u takes from its open games, at least and at most
    int32_t needed_low = context_low_[u] - half_wins_low_[u];
    int32_t needed_high = context_high_[u] - half_wins_low_[u];
    if (has(group, u)) {
      needed_low = tie_half_wins_[u] - half_wins_low_[u];
      needed_high = needed_low;
    }
    const int32_t elsewhere_u = 2 * (open_games_[u] - open_[t][u]);
    const int32_t low = vs_u - std::clamp(needed_high, 0, vs_u);
    const int32_t high = vs_u - std::clamp(needed_low - elsewhere_u, 0, vs_u);
    (counts ? open_low : other_low) += low;
    (counts ? open_high : other_high) += high;
  }
  const int32_t needed = tie_half_wins_[t] - half_wins_low_[t];
  const int32_t low = std::max(open_low, needed - other_high);
  const int32_t high = std::min({2 * open, needed - other_low, open_high});
  return {
      .games = games, .known = known, .low = known + low, .high = known + high};
}

// The same from the pair bounds: what t's open games against `opponents`
// bring at least and at most, and what t's other open games leave of the
// half-wins t takes from all of them
TieProof::HalfWins TieProof::bounded_half_wins(const PairBounds& bounds,
                                               TeamId t,
                                               TeamMask opponents) const {
  HalfWins out;
  for_each_team(opponents, [&](TeamId u) {
    out.games += games_[t][u];
    out.known += known_half_wins_[t][u];
  });
  if (!bounds.feasible) {
    out.low = 1;
    return out;
  }
  int32_t in_low = 0;
  int32_t in_high = 0;
  int32_t other_low = 0;
  int32_t other_high = 0;
  for_each_team(open_opponents_[t], [&](TeamId u) {
    const bool counts = has(opponents, u);
    (counts ? in_low : other_low) += bounds.low[t][u];
    (counts ? in_high : other_high) += bounds.high[t][u];
  });
  out.low = out.known + std::max(in_low, needed_low(t) - other_high);
  out.high = out.known + std::min(in_high, needed_high(t) - other_low);
  return out;
}

// Propagation needs every team of the group at its tie level in the pair
// bounds, which the context only says of the level teams
bool TieProof::propagates(TeamMask group) const {
  return has_context_ && (group & ~team_bit(x_) & ~context_level_) == 0;
}

int32_t TieProof::needed_low(TeamId t) const {
  return context_low_[t] - half_wins_low_[t];
}

int32_t TieProof::needed_high(TeamId t) const {
  return context_high_[t] - half_wins_low_[t];
}

// One pair brings at least what the others can't, and at most what the
// others leave. The opponent gets the rest of the pair's games.
TeamMask TieProof::tighten(PairBounds& bounds, TeamId t, TeamMask opponents,
                           int32_t low, int32_t high) const {
  if (!bounds.feasible) return 0;
  opponents &= open_opponents_[t];
  int32_t sum_low = 0;
  int32_t sum_high = 0;
  for_each_team(opponents, [&](TeamId u) {
    sum_low += bounds.low[t][u];
    sum_high += bounds.high[t][u];
  });
  if (low > high || low > sum_high || high < sum_low) {
    bounds.feasible = false;
    return 0;
  }
  TeamMask changed = 0;
  for_each_team(opponents, [&](TeamId u) {
    const int32_t vs_u = 2 * open_[t][u];
    const int32_t at_least = low - (sum_high - bounds.high[t][u]);
    const int32_t at_most = high - (sum_low - bounds.low[t][u]);
    if (at_least > bounds.low[t][u]) {
      bounds.low[t][u] = static_cast<int8_t>(at_least);
      bounds.high[u][t] = static_cast<int8_t>(vs_u - at_least);
      changed |= team_bit(u) | team_bit(t);
    }
    if (at_most < bounds.high[t][u]) {
      bounds.high[t][u] = static_cast<int8_t>(at_most);
      bounds.low[u][t] = static_cast<int8_t>(vs_u - at_most);
      changed |= team_bit(u) | team_bit(t);
    }
  });
  return changed;
}

// A changed bound of a pair can tighten the other pairs of both teams
TeamMask TieProof::propagate(PairBounds& bounds, TeamMask dirty) const {
  TeamMask changed = 0;
  while (dirty != 0 && bounds.feasible) {
    const TeamId t = std::countr_zero(dirty);
    dirty &= dirty - 1;
    if (open_opponents_[t] == 0) continue;
    const TeamMask tightened =
        tighten(bounds, t, open_opponents_[t], needed_low(t), needed_high(t));
    dirty |= tightened;
    changed |= tightened;
  }
  return changed;
}

TeamMask TieProof::step_opponents(Step step, TeamId t, TeamMask group,
                                  TeamMask common) const {
  switch (step) {
    case Step::kHeadToHead:
    case Step::kHeadToHeadSweep:
      return group & ~team_bit(t);
    case Step::kDivisionPct:
      return division_of(t) & ~team_bit(t);
    case Step::kCommonPct:
      return common;
    case Step::kConferencePct:
      return conference_of(t) & ~team_bit(t);
    default:
      throw EngineError("Step has no record");
  }
}

// The teams' percentages are equal: every team is within the percentages
// all of them can reach
bool TieProof::equal_records(TeamMask teams, TeamArray<HalfWins>& records) {
  Share low;
  Share high;
  bool first = true;
  bool feasible = true;
  for_each_team(teams, [&](TeamId t) {
    HalfWins& r = records[t];
    r.low = std::max(r.low, 0);
    r.high = std::min(r.high, 2 * r.games);
    feasible = feasible && r.low <= r.high;
    const Share t_low = share(r.low, r.games);
    const Share t_high = share(r.high, r.games);
    if (first || low < t_low) low = t_low;
    if (first || t_high < high) high = t_high;
    first = false;
  });
  if (!feasible || high < low) return false;
  for_each_team(teams, [&](TeamId t) {
    HalfWins& r = records[t];
    if (r.games == 0) {
      feasible = feasible && low.half_wins == 0;
      return;
    }
    const int64_t games = r.games;
    r.low = std::max(
        r.low, static_cast<int32_t>((low.half_wins * games + low.games - 1) /
                                    low.games));
    r.high = std::min(
        r.high, static_cast<int32_t>(high.half_wins * games / high.games));
    feasible = feasible && r.low <= r.high;
  });
  return feasible;
}

// A step the whole group passes leaves all of its teams level there.
// Head-to-head: every game among the group brings it two half-wins, so equal
// percentages are all .500. Head-to-head sweep: nobody won or lost all of
// its games against the others. Other records: equal percentages.
bool TieProof::level_records(Step step, TeamMask group,
                             TeamArray<HalfWins>& records) const {
  if (step == Step::kHeadToHead) {
    return equal_records(group, records) && at_half(group, records);
  }
  if (step != Step::kHeadToHeadSweep) return equal_records(group, records);
  bool feasible = true;
  for_each_team(group, [&](TeamId t) {
    const bool played_all = plays_all(t, group & ~team_bit(t));
    HalfWins& r = records[t];
    r.low = std::max(r.low, played_all ? 1 : 0);
    r.high = std::min(r.high, 2 * r.games - (played_all ? 1 : 0));
    feasible = feasible && r.low <= r.high;
  });
  return feasible;
}

// Every team has as many half-wins as games. False if one can't.
bool TieProof::at_half(TeamMask teams, TeamArray<HalfWins>& records) {
  bool feasible = true;
  for_each_team(teams, [&](TeamId t) {
    HalfWins& r = records[t];
    r.low = std::max(r.low, r.games);
    r.high = std::min(r.high, r.games);
    feasible = feasible && r.low <= r.high;
  });
  return feasible;
}

// The whole group was level at every earlier step. That fixes or bounds
// every team's record there, which bounds its games against single
// opponents, which bounds the opponents' other games, and so on until
// nothing changes (P15). Not feasible: the step is never reached with the
// whole group.
void TieProof::settle(Passage& passage, TeamMask group,
                      std::span<const Fact> facts, TeamMask touched) const {
  PairBounds& bounds = passage.bounds;
  // t takes `low` to `high` half-wins from its open games against `in`,
  // and the rest of its half-wins from its other open games
  const auto apply = [&](TeamId t, TeamMask in, int32_t low, int32_t high) {
    const TeamMask changed = tighten(bounds, t, in, low, high);
    return tighten(bounds, t, open_opponents_[t] & ~in, needed_low(t) - high,
                   needed_high(t) - low) |
           changed;
  };
  // `touched`: teams with a bound that changed since the facts and steps
  // were applied. Only their facts, and the steps of groups with one of
  // them, can say more.
  while (touched != 0 && bounds.feasible) {
    TeamMask changed = 0;
    for (const Fact& f : facts) {
      if (has(touched, f.team)) {
        changed |= apply(f.team, f.opponents, f.low, f.high);
      }
    }
    for (size_t i = 0; i < passage.earlier.size && bounds.feasible; ++i) {
      if ((touched & group) == 0) continue;
      TeamArray<HalfWins>& record = passage.records.at(i);
      const TeamArray<TeamMask>& opponents = passage.opponents.at(i);
      for_each_team(group, [&](TeamId t) {
        const HalfWins h = bounded_half_wins(bounds, t, opponents[t]);
        record[t] = {.games = h.games,
                     .known = h.known,
                     .low = std::max(record[t].low, h.low),
                     .high = std::min(record[t].high, h.high)};
      });
      if (!level_records(passage.earlier.steps.at(i).step, group, record)) {
        bounds.feasible = false;
        break;
      }
      for_each_team(group, [&](TeamId t) {
        changed |= apply(t, opponents[t], record[t].low - record[t].known,
                         record[t].high - record[t].known);
      });
    }
    touched = changed | propagate(bounds, changed);
  }
}

// A team's half-wins from its open games against `opponents`, at least and
// at most: a team's total (`root`) or a record of a fact
struct TieProof::RecordBound {
  TeamId team = 0;
  TeamMask opponents = 0;
  int32_t low = 0;
  int32_t high = 0;
  bool root = false;
  // Overlaps a larger record of its team without being part of it
  bool crossed = false;
};

// The pair bounds are tightened one team at a time, so they can leave
// records that no single way to play the open games gives (P17). Example
// (2026 after week 10, LAC winning out): CIN needs four half-wins from nine
// open games, and each of its six conference games can be lost on its own,
// but not all of them at once: the winners would end above the win
// percentage the tie group gives them.
//
// A flow settles it: every pair of teams hands out the half-wins of its
// open games to both teams, within the pair bounds. A team's half-wins
// against a set of opponents run through a node with the record's bounds
// into the next larger set and at last into the team's total. That needs
// the sets of a team to be nested or disjoint. Records that overlap in
// another way (common games) are left out, which only relaxes, and get a
// second flow where they go first.
bool TieProof::jointly_feasible(const PairBounds& bounds,
                                std::span<const Fact> facts,
                                std::span<const Fact> passed) const {
  std::vector<RecordBound> records;
  for (TeamId t = 0; t < kTeams; ++t) {
    if (open_opponents_[t] == 0) continue;
    records.push_back({.team = t,
                       .opponents = open_opponents_[t],
                       .low = needed_low(t),
                       .high = needed_high(t),
                       .root = true});
  }
  for (const std::span<const Fact> list : {facts, passed}) {
    for (const Fact& f : list) {
      const TeamMask opponents = f.opponents & open_opponents_[f.team];
      if (opponents == 0) {
        if (f.low > 0 || f.high < 0) return false;
        continue;
      }
      const auto same =
          std::ranges::find_if(records, [&](const RecordBound& r) {
            return r.team == f.team && r.opponents == opponents;
          });
      if (same == records.end()) {
        records.push_back({.team = f.team,
                           .opponents = opponents,
                           .low = f.low,
                           .high = f.high});
      } else {
        same->low = std::max(same->low, f.low);
        same->high = std::min(same->high, f.high);
      }
    }
  }
  // Only the totals: the tie group's flow already found a completion
  if (std::ranges::all_of(records, &RecordBound::root)) return true;
  // Per team from the largest set to the smallest, the total first
  std::ranges::stable_sort(
      records, [](const RecordBound& a, const RecordBound& b) {
        if (a.team != b.team) return a.team < b.team;
        return std::popcount(a.opponents) > std::popcount(b.opponents);
      });
  if (!records_feasible(bounds, records, false)) return false;
  return std::ranges::none_of(records, &RecordBound::crossed) ||
         records_feasible(bounds, records, true);
}

bool TieProof::records_feasible(const PairBounds& bounds,
                                std::span<RecordBound> records,
                                bool crossed_first) const {
  // The records of the flow: nested or disjoint within a team
  std::vector<size_t> chosen;
  const auto fits = [&](size_t i) {
    return std::ranges::all_of(chosen, [&](size_t j) {
      if (records[j].team != records[i].team) return true;
      const TeamMask both = records[i].opponents & records[j].opponents;
      return both == 0 || both == records[i].opponents ||
             both == records[j].opponents;
    });
  };
  for (size_t i = 0; i < records.size(); ++i) {
    if (records[i].root) chosen.push_back(i);
  }
  if (crossed_first) {
    for (const bool crossed : {true, false}) {
      for (size_t i = 0; i < records.size(); ++i) {
        if (!records[i].root && records[i].crossed == crossed && fits(i)) {
          chosen.push_back(i);
        }
      }
    }
  } else {
    for (size_t i = 0; i < records.size(); ++i) {
      if (records[i].root) continue;
      records[i].crossed = !fits(i);
      if (!records[i].crossed) chosen.push_back(i);
    }
  }
  // Back to teams in turn, larger sets first
  std::ranges::sort(chosen);

  // Nodes: the two ends of the flow that checks the lower bounds, source
  // and sink of the half-wins, one per pair with open games, one per record
  constexpr int32_t kFrom = 0;
  constexpr int32_t kTo = 1;
  constexpr int32_t kSource = 2;
  constexpr int32_t kSink = 3;
  constexpr int32_t kFirstPair = 4;
  const auto first_record =
      kFirstPair + static_cast<int32_t>(open_pairs_.size());
  const auto nodes = first_record + static_cast<int32_t>(chosen.size());
  const auto node_of = [&](size_t k) {
    return first_record + static_cast<int32_t>(k);
  };
  // The smallest chosen record before `end` of team t that holds `opponents`
  const auto smallest = [&](size_t end, TeamId t, TeamMask opponents) {
    for (size_t k = end; k-- > 0;) {
      const RecordBound& r = records[chosen[k]];
      if (r.team == t && (r.opponents & opponents) == opponents) return k;
    }
    throw EngineError("A record has no total");
  };

  flow_.reset(nodes);
  // An edge with a lower bound carries the rest above it; the bound itself
  // must reach its end and leave its start (`excess`)
  std::vector<int32_t> excess(static_cast<size_t>(nodes), 0);
  bool feasible = true;
  const auto add_edge = [&](int32_t from, int32_t to, int32_t low,
                            int32_t high) {
    if (low > high) {
      feasible = false;
      return;
    }
    flow_.add_edge(from, to, high - low);
    excess[static_cast<size_t>(to)] += low;
    excess[static_cast<size_t>(from)] -= low;
  };
  int32_t all = 0;
  for (size_t p = 0; p < open_pairs_.size(); ++p) {
    const OpenGames& g = open_pairs_[p];
    const int32_t pair = kFirstPair + static_cast<int32_t>(p);
    add_edge(kSource, pair, 2 * g.count, 2 * g.count);
    all += 2 * g.count;
    for (const auto& [t, u] :
         {std::pair{g.first, g.second}, std::pair{g.second, g.first}}) {
      add_edge(pair, node_of(smallest(chosen.size(), t, team_bit(u))),
               bounds.low[t][u], bounds.high[t][u]);
    }
  }
  for (size_t k = 0; k < chosen.size(); ++k) {
    const RecordBound& r = records[chosen[k]];
    int32_t most = 0;
    for_each_team(r.opponents, [&](TeamId u) { most += 2 * open_[r.team][u]; });
    add_edge(node_of(k),
             r.root ? kSink : node_of(smallest(k, r.team, r.opponents)),
             std::max(r.low, 0), std::min(r.high, most));
  }
  if (!feasible) return false;
  flow_.add_edge(kSink, kSource, all);
  int32_t demand = 0;
  for (int32_t node = kSource; node < nodes; ++node) {
    const int32_t e = excess[static_cast<size_t>(node)];
    if (e > 0) {
      flow_.add_edge(kFrom, node, e);
      demand += e;
    } else if (e < 0) {
      flow_.add_edge(node, kTo, -e);
    }
  }
  return flow_.run(kFrom, kTo) == demand;
}

// A step keeps the teams with the best value and drops the others, and the
// procedure restarts with the survivors. They take along what the step and
// the earlier ones said: the whole group was level at the earlier steps,
// the survivors are level at this one and the dropped teams below them.
bool TieProof::reduction_facts(const StepRanges& r, TeamMask survivors,
                               Facts& facts) const {
  facts.insert(facts.end(), r.passed.begin(), r.passed.end());
  if (r.step == Step::kSov || r.step == Step::kSos) return true;
  const TeamMask dropped = r.group & ~survivors;
  TeamArray<HalfWins> records{};
  TeamArray<TeamMask> opponents{};
  for_each_team(r.group, [&](TeamId t) {
    opponents[t] = step_opponents(r.step, t, r.group, r.common);
    records[t] = bounded_half_wins(*r.bounds, t, opponents[t]);
  });
  bool feasible = true;
  const auto narrow = [&](TeamId t, int64_t low, int64_t high) {
    HalfWins& h = records[t];
    h.low = static_cast<int32_t>(std::max<int64_t>(h.low, low));
    h.high = static_cast<int32_t>(std::min<int64_t>(h.high, high));
    feasible = feasible && h.low <= h.high;
  };
  if (r.step == Step::kHeadToHeadSweep) {
    // One survivor swept the others or is the only one not swept: nothing
    // simple to say. More survivors: none of them swept or was swept, and
    // every dropped team lost all of its games against the others.
    if (std::popcount(survivors) == 1) return true;
    for_each_team(r.group, [&](TeamId t) {
      const bool played_all = plays_all(t, r.group & ~team_bit(t));
      if (has(dropped, t)) {
        feasible = feasible && played_all;
        narrow(t, 0, 0);
      } else if (played_all) {
        narrow(t, 1, 2 * records[t].games - 1);
      }
    });
  } else {
    if (!equal_records(survivors, records)) return false;
    // The dropped teams are below the survivors' percentage
    Share top;
    Share bottom;
    bool first = true;
    for_each_team(survivors, [&](TeamId t) {
      const Share s = share(records[t].high, records[t].games);
      if (first || s < top) top = s;
      first = false;
    });
    for_each_team(dropped, [&](TeamId t) {
      const Share s = share(records[t].low, records[t].games);
      if (bottom < s) bottom = s;
    });
    for_each_team(dropped, [&](TeamId t) {
      const int64_t games = records[t].games;
      if (games == 0) {
        feasible = feasible && top.half_wins > 0;
      } else {
        narrow(t, 0, (top.half_wins * games + top.games - 1) / top.games - 1);
      }
    });
    for_each_team(survivors, [&](TeamId t) {
      const int64_t games = records[t].games;
      // Without games the percentage is 0, which is never above another
      narrow(t, games == 0 ? 1 : bottom.half_wins * games / bottom.games + 1,
             2 * games);
    });
  }
  if (!feasible) return false;
  for_each_team(r.group, [&](TeamId t) {
    facts.push_back({.team = t,
                     .opponents = opponents[t],
                     .low = records[t].low - records[t].known,
                     .high = records[t].high - records[t].known});
  });
  return true;
}

// reduction_facts() gives every team a range of its own, so a survivor at
// the bottom of its range and a dropped team at the top of its range can
// meet (P18). Example (2026 after week 10): MIA wins the AFC East over NE
// with the better division record, 8 to 10 half-wins against 7 to 9, and
// every completion that fits the other records has both at 8. So one case
// per record of the survivors: all of them exactly there, the dropped teams
// below it.
std::vector<TieProof::Facts> TieProof::reduction_cases(
    const StepRanges& r, TeamMask survivors, const Facts& facts) const {
  std::vector<Facts> out;
  const bool record_step = r.step != Step::kSov && r.step != Step::kSos &&
                           r.step != Step::kHeadToHeadSweep;
  TeamArray<HalfWins> records{};
  TeamArray<TeamMask> opponents{};
  // A survivor with games: its half-wins say where the survivors are level
  std::optional<TeamId> first;
  if (joint_facts_ && record_step) {
    for_each_team(r.group, [&](TeamId t) {
      opponents[t] = step_opponents(r.step, t, r.group, r.common);
      HalfWins& h = records[t];
      h = bounded_half_wins(*r.bounds, t, opponents[t]);
      h.low = std::max(h.low, 0);
      h.high = std::min(h.high, 2 * h.games);
      if (!first && has(survivors, t) && h.games > 0) first = t;
    });
  }
  if (!first) {
    Facts next = facts;
    if (reduction_facts(r, survivors, next)) out.push_back(std::move(next));
    return out;
  }
  const HalfWins& level = records[*first];
  for (int32_t half_wins = level.low; half_wins <= level.high; ++half_wins) {
    TeamArray<HalfWins> exact = records;
    bool feasible = true;
    for_each_team(r.group, [&](TeamId t) {
      HalfWins& h = exact[t];
      // t's half-wins at the survivors' percentage, times level.games
      const int64_t scaled = static_cast<int64_t>(half_wins) * h.games;
      if (has(survivors, t)) {
        const bool whole = scaled % level.games == 0;
        const auto at = static_cast<int32_t>(scaled / level.games);
        // Without games the percentage is 0
        feasible = feasible && whole && at >= h.low && at <= h.high &&
                   (h.games > 0 || half_wins == 0);
        h.low = at;
        h.high = at;
      } else {
        // Strictly below the survivors
        const auto below =
            static_cast<int32_t>(scaled > 0 ? (scaled - 1) / level.games : -1);
        h.high = h.games > 0 ? std::min(h.high, below) : h.high;
        feasible = feasible && half_wins > 0 && h.low <= h.high;
      }
    });
    if (!feasible) continue;
    Facts& next = out.emplace_back(facts);
    next.insert(next.end(), r.passed.begin(), r.passed.end());
    for_each_team(r.group, [&](TeamId t) {
      next.push_back({.team = t,
                      .opponents = opponents[t],
                      .low = exact[t].low - exact[t].known,
                      .high = exact[t].high - exact[t].known});
    });
  }
  return out;
}

// The teams of `above` end above X's win percentage, the teams of `level`
// level with it and the teams of `below` below it. Nothing is known about
// the other teams.
void TieProof::set_context(TeamMask above, TeamMask level, TeamMask below) {
  if (has_context_ && above == context_above_ && level == context_level_ &&
      below == context_below_) {
    return;
  }
  has_context_ = true;
  context_above_ = above;
  context_level_ = level;
  context_below_ = below;
  context_low_ = half_wins_low_;
  context_high_ = half_wins_high_;
  for_each_team((above | level | below) & ~team_bit(x_), [&](TeamId t) {
    // Most half-wins of t at or below X's win percentage
    const int64_t scaled =
        static_cast<int64_t>(half_wins_low_[x_]) * final_games_[t];
    const auto floor_level = static_cast<int32_t>(scaled / final_games_[x_]);
    const bool exact = scaled % final_games_[x_] == 0;
    if (has(level, t)) {
      if (tie_half_wins_[t] == kMissing) return;
      context_low_[t] = tie_half_wins_[t];
      context_high_[t] = tie_half_wins_[t];
    } else if (has(above, t)) {
      context_low_[t] = std::max(context_low_[t], floor_level + 1);
    } else {
      context_high_[t] =
          std::min(context_high_[t], exact ? floor_level - 1 : floor_level);
    }
  });
  context_bounds_ = {};
  for (const OpenGames& g : open_pairs_) {
    const auto all = static_cast<int8_t>(2 * g.count);
    context_bounds_.high[g.first][g.second] = all;
    context_bounds_.high[g.second][g.first] = all;
  }
  (void)propagate(context_bounds_, ~TeamMask{0});
  clear_caches();
}

void TieProof::carry_facts(bool on, bool jointly) {
  jointly = on && jointly;
  if (on == carry_facts_ && jointly == joint_facts_) return;
  carry_facts_ = on;
  joint_facts_ = jointly;
  clear_caches();
}

void TieProof::clear_caches() {
  for (auto& cache : picks_cache_) cache.clear();
  for (auto& cache : rank_cache_) cache.clear();
  berth_cache_.clear();
}

TieProof::Range TieProof::pct_range(TeamId t, TeamMask opponents,
                                    TeamMask group,
                                    const PairBounds* bounds) const {
  const HalfWins h = half_wins_range(t, opponents, group, bounds);
  // Empty: t and its opponents can't all end at their tie levels
  if (h.low > h.high) {
    return {.low = Ratio(1, 1), .high = Ratio(0, 1)};
  }
  return {.low = Ratio(h.low, 2 * h.games), .high = Ratio(h.high, 2 * h.games)};
}

// Head-to-head among the teams of a group: every game among them brings two
// half-wins to the group. Team t is beaten for sure if the other teams can't
// hold what t leaves without one of them ending above t's best percentage.
bool TieProof::beaten_head_to_head(TeamId t, TeamMask group,
                                   const PairBounds* bounds) const {
  const HalfWins own = half_wins_range(t, group & ~team_bit(t), group, bounds);
  if (own.games == 0 || own.low > own.high) return false;
  int64_t total = own.games;
  int64_t held = own.high;
  bool empty = false;
  for_each_team(group & ~team_bit(t), [&](TeamId u) {
    const HalfWins h = half_wins_range(u, group & ~team_bit(u), group, bounds);
    empty = empty || h.low > h.high;
    total += h.games;
    // u's percentage at most t's best
    const int64_t cap = static_cast<int64_t>(own.high) * h.games / own.games;
    held += std::min<int64_t>(h.high, cap);
  });
  return !empty && held < total;
}

// Head-to-head sweep: 2 = beat every other team of the group, 0 = lost to
// every one, 1 otherwise (or didn't play all of them)
TieProof::Range TieProof::sweep_range(TeamId t, TeamMask group,
                                      const PairBounds* bounds) const {
  int32_t known_games = 0;
  int32_t known = 0;
  int32_t open = 0;
  if (!plays_all(t, group & ~team_bit(t))) {
    return {.low = Ratio(1, 1), .high = Ratio(1, 1)};
  }
  for_each_team(group & ~team_bit(t), [&](TeamId u) {
    known_games += known_games_[t][u];
    known += known_half_wins_[t][u];
    open += open_[t][u];
  });
  if (bounds != nullptr) {
    const HalfWins h = bounded_half_wins(*bounds, t, group & ~team_bit(t));
    if (h.low > h.high) return {.low = Ratio(1, 1), .high = Ratio(0, 1)};
    const auto sweep = [&](int32_t half_wins) {
      if (half_wins == 2 * h.games) return 2;
      return half_wins == 0 ? 0 : 1;
    };
    return {.low = Ratio(sweep(h.low), 1), .high = Ratio(sweep(h.high), 1)};
  }
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
                                      Procedure procedure, Passage& passage,
                                      std::span<const Fact> facts) const {
  StepRanges out;
  out.step = step;
  out.group = group;
  out.procedure = procedure;
  for_each_team(group, [&](TeamId t) {
    if (tie_half_wins_[t] == kMissing) {
      throw EngineError("A team in a tie group can't reach the tie level");
    }
  });
  const bool propagating = propagates(group);
  if (!passage.started) {
    passage.started = true;
    if (propagating) {
      passage.bounds = context_bounds_;
      settle(passage, group, facts, facts.empty() ? 0 : ~TeamMask{0});
    }
  }
  // The pick went on from the last step, so the whole group was level there
  if (passage.last) {
    const size_t i = passage.earlier.size++;
    passage.earlier.steps.at(i) = *passage.last;
    passage.last.reset();
    if (propagating) {
      for_each_team(group, [&](TeamId t) {
        passage.opponents.at(i)[t] =
            step_opponents(passage.earlier.steps.at(i).step, t, group,
                           passage.earlier.steps.at(i).common);
        passage.records.at(i)[t].high = std::numeric_limits<int32_t>::max();
      });
      settle(passage, group, facts, group);
    }
  }
  out.earlier = passage.earlier;
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
  if (step != Step::kSov && step != Step::kSos) {
    passage.last = {.step = step, .common = common};
  }
  if (propagating) {
    out.bounds = passage.bounds;
    // Never reached with the whole group: no team survives the step
    if (!out.bounds->feasible) {
      for_each_team(group, [&](TeamId t) {
        out.range[t] = {.low = Ratio(1, 1), .high = Ratio(0, 1)};
      });
      return out;
    }
    // The records of the group's teams at the earlier steps, all level
    for (size_t i = 0; i < passage.earlier.size; ++i) {
      for_each_team(group, [&](TeamId t) {
        const HalfWins& record = passage.records.at(i)[t];
        out.passed.push_back({.team = t,
                              .opponents = passage.opponents.at(i)[t],
                              .low = record.low - record.known,
                              .high = record.high - record.known});
      });
    }
  }
  const PairBounds* bounds = out.pair_bounds();
  for_each_team(group, [&](TeamId t) {
    const TeamMask others = group & ~team_bit(t);
    const TeamMask own_division = division_of(t) & ~team_bit(t);
    const TeamMask own_conference = conference_of(t) & ~team_bit(t);
    switch (step) {
      case Step::kHeadToHead:
        out.range[t] = pct_range(t, others, group, bounds);
        break;
      case Step::kHeadToHeadSweep:
        out.range[t] = sweep_range(t, group, bounds);
        break;
      case Step::kDivisionPct:
        out.range[t] = pct_range(t, own_division, group, bounds);
        break;
      case Step::kCommonPct:
        out.range[t] = pct_range(t, common, group, bounds);
        break;
      case Step::kConferencePct:
        out.range[t] = pct_range(t, own_conference, group, bounds);
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
// counts the same for both. With `bounds`, an open game only goes the ways
// they allow.
int64_t TieProof::linear_strength_difference(
    const TeamArray<int32_t>& weight_a, const TeamArray<int32_t>& weight_b,
    const TeamArray<int32_t>& half_wins, TeamMask group, TeamMask settled,
    const PairBounds* bounds) const {
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
    const int32_t all = 2 * g.count;
    const int32_t low = bounds != nullptr ? bounds->low[g.first][g.second] : 0;
    const int32_t high =
        bounds != nullptr ? bounds->high[g.first][g.second] : all;
    // The first team's half-wins that lower the difference most
    const int32_t share = coef[g.first] > coef[g.second] ? low : high;
    out += coef[g.first] * share + coef[g.second] * (all - share);
  }
  return out;
}

// Opponents every team of the group plays. Tied teams never are.
TeamMask TieProof::common_opponents(TeamMask group) const {
  TeamMask out = ~group;
  for_each_team(group, [&](TeamId t) { out &= opponents_[t]; });
  return out;
}

bool TieProof::plays_all(TeamId t, TeamMask teams) const noexcept {
  return (teams & ~opponents_[t]) == 0;
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
      if (!plays_all(t, others)) return {1, 1};
      int32_t games = 0;
      int32_t won = 0;
      for_each_team(others, [&](TeamId u) {
        games += games_[t][u];
        won += half_wins[u];
      });
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

// The procedure only reaches a step with all of a group if no earlier step
// separated them, so a and b are equal at every earlier step. The record
// steps only depend on a's and b's own games, which `own` fixes; SOV
// doesn't, so it isn't checked.
bool TieProof::equal_before(const EarlierSteps& earlier, TeamMask group,
                            TeamId a, TeamId b, const OwnResults& own) const {
  return std::ranges::all_of(
      std::span(earlier.steps).first(earlier.size), [&](const EarlierStep& e) {
        return value(e.step, a, own.half_wins_a, group, e.common) ==
               value(e.step, b, own.half_wins_b, group, e.common);
      });
}

// Lowest value of a's step value minus b's, and of b's minus a's (for SOV
// and SOS bounds, scaled so that only their signs mean something) over the
// results of a's and b's open games (if there are few) that leave both at
// their tie levels and equal at every earlier step. No such result: the
// step is never reached with both, and the results are the largest value.
TieProof::Differences TieProof::lowest_differences(const StepRanges& r,
                                                   TeamId a, TeamId b) const {
  const TeamMask pair = team_bit(a) | team_bit(b);
  if (open_games_[a] + open_games_[b] >
      static_cast<int32_t>(kMaxEnumeratedGames)) {
    // SOS doesn't depend on who wins a's and b's games
    if (r.step != Step::kSos) return {};
    return {
        .a_minus_b = linear_strength_difference(
            games_[a], games_[b], half_wins_low_, r.group, 0, r.pair_bounds()),
        .b_minus_a = linear_strength_difference(
            games_[b], games_[a], half_wins_low_, r.group, 0, r.pair_bounds())};
  }
  const EarlierSteps& earlier = r.earlier;
  const bool strength = r.step == Step::kSov || r.step == Step::kSos;
  // A record step with nothing to condition on: the ranges say it all
  if (!strength && earlier.size == 0) return {};
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
  const auto strength_difference = [&](const TeamArray<int32_t>& weight,
                                       const TeamArray<int32_t>& other) {
    return linear_strength_difference(weight, other, own.half_wins, r.group,
                                      pair, r.pair_bounds());
  };
  // a's value minus b's, and b's minus a's
  const auto differences = [&]() -> std::pair<int64_t, int64_t> {
    switch (r.step) {
      case Step::kSov:
        return {strength_difference(own.wins_a, own.wins_b),
                strength_difference(own.wins_b, own.wins_a)};
      case Step::kSos:
        return {strength_difference(games_[a], games_[b]),
                strength_difference(games_[b], games_[a])};
      default: {
        const auto ordering =
            value(r.step, a, own.half_wins_a, r.group, r.common) <=>
            value(r.step, b, own.half_wins_b, r.group, r.common);
        if (ordering > 0) return {1, -1};
        if (ordering < 0) return {-1, 1};
        return {0, 0};
      }
    }
  };
  // The results of t's open games are among those the pair bounds allow
  const auto within_bounds = [&](TeamId t, const TeamArray<int32_t>& won) {
    const PairBounds* bounds = r.pair_bounds();
    bool ok = true;
    if (bounds == nullptr) return ok;
    for_each_team(open_opponents_[t], [&](TeamId u) {
      const int32_t open = won[u] - known_half_wins_[t][u];
      ok = ok && open >= bounds->low[t][u] && open <= bounds->high[t][u];
    });
    return ok;
  };
  // Depth first over the results (half-wins of the first team: 0, 1 or 2),
  // dropping results that can't leave the group at its tie levels
  Differences out;
  bool reached = false;
  const auto visit = [&](const auto& self, size_t i) -> void {
    if (!all_reachable()) return;
    if (i == n_games) {
      if (!equal_before(earlier, r.group, a, b, own)) return;
      if (!within_bounds(a, own.half_wins_a) ||
          !within_bounds(b, own.half_wins_b)) {
        return;
      }
      const auto [a_minus_b, b_minus_a] = differences();
      out.a_minus_b = reached ? std::min(*out.a_minus_b, a_minus_b) : a_minus_b;
      out.b_minus_a = reached ? std::min(*out.b_minus_a, b_minus_a) : b_minus_a;
      reached = true;
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
  if (!reached) {
    return {.a_minus_b = std::numeric_limits<int64_t>::max(),
            .b_minus_a = std::numeric_limits<int64_t>::max()};
  }
  return out;
}

TieProof::Leads TieProof::leads(const StepRanges& r, TeamId a, TeamId b) const {
  const Range& range_a = r.range[a];
  const Range& range_b = r.range[b];
  // Ahead or behind for sure: a bound of the difference could only say
  // something else where the step is never reached with both
  // (an empty range is ahead and behind)
  const bool a_ahead = range_a.low > range_b.high;
  const bool b_ahead = range_b.low > range_a.high;
  if (a_ahead || b_ahead) {
    return {.a = a_ahead ? Lead::kAhead : Lead::kNone,
            .b = b_ahead ? Lead::kAhead : Lead::kNone};
  }
  const Differences d = lowest_differences(r, a, b);
  const auto lead = [](const Range& own, const Range& other,
                       const std::optional<int64_t>& lowest) {
    if (lowest && *lowest > 0) return Lead::kAhead;
    if ((lowest && *lowest == 0) || own.low >= other.high) {
      return Lead::kNeverBehind;
    }
    return Lead::kNone;
  };
  return {.a = lead(range_a, range_b, d.a_minus_b),
          .b = lead(range_b, range_a, d.b_minus_a)};
}

TeamMask TieProof::division_of(TeamId t) const { return division_of_[t]; }

TeamMask TieProof::conference_of(TeamId t) const { return conference_of_[t]; }

std::span<const Step> TieProof::steps(Procedure procedure) {
  if (procedure == Procedure::kDivision) return kDivisionSteps;
  return kConferenceSteps;
}

// X is picked first (wins) if no other team can be picked, and never (loses)
// if X can't be.
bool TieProof::wins(TeamMask group, Procedure procedure) {
  return possible_picks(group, procedure) == team_bit(x_);
}

bool TieProof::loses(TeamMask group, Procedure procedure) {
  return !has(possible_picks(group, procedure), x_);
}

// One pick drops the teams that lose a step and restarts with the others
// until one is left (procedures.cpp). A step drops every team some other team
// beats for sure and keeps every team no team can beat, so the survivors are
// between these two sets. A team that is never behind a survivor has the best
// value too, so it survives with it (P19). If every step can leave all teams
// level, a coin toss can pick any of them.
TeamMask TieProof::possible_picks(TeamMask group, Procedure procedure) {
  if (std::popcount(group) == 1) return group;
  auto& cache = picks_cache_[static_cast<size_t>(procedure)];
  if (const auto it = cache.find(group); it != cache.end()) return it->second;
  const TeamMask out = possible_picks(group, procedure, {});
  cache[group] = out;
  return out;
}

TeamMask TieProof::possible_picks(TeamMask group, Procedure procedure,
                                  const Facts& facts,
                                  std::vector<PickPath>* paths) {
  if (std::popcount(group) == 1) {
    if (!facts.empty()) {
      // The steps of the larger groups can leave this team
      Passage passage;
      passage.bounds = context_bounds_;
      settle(passage, group, facts, ~TeamMask{0});
      if (!passage.bounds.feasible ||
          (joint_facts_ && !jointly_feasible(passage.bounds, facts, {}))) {
        return 0;
      }
    }
    if (paths != nullptr) {
      paths->push_back({.pick = std::countr_zero(group), .facts = facts});
    }
    return group;
  }
  TeamMask out = 0;
  bool all_level = true;
  // The group's records if it is level at every step
  Facts level_facts;
  Passage passage;
  for (const Step step : steps(procedure)) {
    const StepRanges r = ranges(step, group, procedure, passage, facts);
    if (!r.applies) continue;
    if (paths != nullptr || joint_facts_) level_facts = r.passed;
    // Every pair of teams once: the teams that are never behind a team, and
    // the teams some team is ahead of for sure
    TeamArray<TeamMask> never_behind{};
    TeamMask beaten = 0;
    const auto note = [&](Lead lead, TeamId ahead, TeamId behind) {
      if (lead == Lead::kAhead) beaten |= team_bit(behind);
      if (lead != Lead::kNone) never_behind[behind] |= team_bit(ahead);
    };
    for (TeamMask rest = group; rest != 0; rest &= rest - 1) {
      const TeamId t = std::countr_zero(rest);
      for_each_team(rest & (rest - 1), [&](TeamId u) {
        const Leads l = leads(r, t, u);
        note(l.a, t, u);
        note(l.b, u, t);
      });
    }
    TeamMask can_survive = 0;
    TeamMask must_survive = 0;
    for_each_team(group, [&](TeamId t) {
      if (has(beaten, t)) return;
      if (step == Step::kHeadToHead &&
          beaten_head_to_head(t, group, r.pair_bounds())) {
        return;
      }
      can_survive |= team_bit(t);
    });
    // A team no team can beat is never behind any other
    for_each_team(group, [&](TeamId t) {
      const TeamMask others = group & ~team_bit(t);
      bool unbeatable = true;
      for_each_team(others, [&](TeamId u) {
        unbeatable = unbeatable && has(never_behind[u], t);
      });
      if (unbeatable) must_survive |= team_bit(t);
    });
    const TeamMask optional = can_survive & ~must_survive;
    (void)all_subsets(optional, [&](TeamMask s) {
      const TeamMask survivors = must_survive | s;
      if (survivors == 0 || survivors == group) return true;
      bool closed = true;
      for_each_team(survivors, [&](TeamId t) {
        closed = closed && (never_behind[t] & ~survivors) == 0;
      });
      if (!closed) return true;
      if ((!r.bounds || !carry_facts_) && paths == nullptr) {
        out |= possible_picks(survivors, procedure);
        return true;
      }
      if (!r.bounds) {
        out |= possible_picks(survivors, procedure, facts, paths);
        return true;
      }
      for (const Facts& next : reduction_cases(r, survivors, facts)) {
        out |= possible_picks(survivors, procedure, next, paths);
      }
      return true;
    });
    if (can_survive != group) {
      all_level = false;
      break;
    }
  }
  // Level at every step: only if the records that takes can hold at once
  if (all_level && joint_facts_ && !facts.empty() &&
      !jointly_feasible(context_bounds_, facts, level_facts)) {
    all_level = false;
  }
  if (all_level) {
    out |= group;
    if (paths != nullptr) {
      Facts next = facts;
      next.insert(next.end(), level_facts.begin(), level_facts.end());
      for_each_team(group, [&](TeamId t) {
        paths->push_back({.pick = t, .facts = next});
      });
    }
  }
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
  bool result = !has(picks, x_);
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
    return loses(group, Procedure::kConference);
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
    if (!has_context_ || !carry_facts_ ||
        !first_seed_paths(divisions, level, out)) {
      first_seed_picks(divisions, level, team_bit(x_), out);
    }
  }
  return outcome_of(out);
}

// How a division's winner is picked says something about its records: the
// steps it passed level with others, and the step that put it ahead. The
// pick among the division winners takes that along, for every way to every
// winner.
bool TieProof::first_seed_paths(TeamMask divisions, TeamMask level,
                                Berth& out) {
  std::vector<std::vector<PickPath>> paths;
  std::vector<PickPath>& own = paths.emplace_back();
  (void)possible_picks((level & division_) | team_bit(x_), Procedure::kDivision,
                       {}, &own);
  std::erase_if(own, [&](const PickPath& p) { return p.pick != x_; });
  size_t combinations = own.size();
  for (TeamMask rest = divisions; rest != 0; rest &= rest - 1) {
    std::vector<PickPath>& winners = paths.emplace_back();
    (void)possible_picks(level & division_of(std::countr_zero(rest)),
                         Procedure::kDivision, {}, &winners);
    combinations *= winners.size();
    if (combinations >
        (joint_facts_ ? kMaxJointWinnerPaths : kMaxWinnerPaths)) {
      return false;
    }
  }
  Facts facts;
  const auto visit = [&](const auto& self, size_t i, TeamMask winners) -> void {
    if (out.reached && out.missed) return;
    if (i == paths.size()) {
      const TeamMask picks =
          possible_picks(winners, Procedure::kConference, facts);
      out.reached = out.reached || has(picks, x_);
      out.missed = out.missed || (picks & ~team_bit(x_)) != 0;
      return;
    }
    for (const PickPath& p : paths[i]) {
      const size_t size = facts.size();
      facts.insert(facts.end(), p.facts.begin(), p.facts.end());
      self(self, i + 1, winners | team_bit(p.pick));
      facts.resize(size);
    }
  };
  visit(visit, 0, 0);
  return true;
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
