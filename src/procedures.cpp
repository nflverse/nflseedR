#include "engine/procedures.hpp"

#include <algorithm>
#include <cmath>
#include <span>
#include <tuple>
#include <utility>

namespace nflseedr {

void TeamSet::erase(TeamId t) noexcept {
  auto* it = std::find(begin(), end(), t);
  if (it == end()) return;
  std::copy(it + 1, end(), it);
  --size_;
}

namespace {

enum class Procedure { kDivision, kConference, kDraft };

struct Pick {
  TeamId team = 0;
  int32_t label = kMissing;
};

struct StepSpec {
  Step step;
  int32_t min_depth;
};

constexpr std::array kDivisionSteps{
    StepSpec{Step::kHeadToHead, 1},
    StepSpec{Step::kDivisionPct, 1},
    StepSpec{Step::kCommonPct, 1},
    StepSpec{Step::kConferencePct, 1},
    StepSpec{Step::kSov, 2},
    StepSpec{Step::kSos, 2},
    StepSpec{Step::kConferencePoints, 3},
    StepSpec{Step::kLeaguePoints, 3},
    StepSpec{Step::kCommonPd, 3},
    StepSpec{Step::kPd, 3},
};

constexpr std::array kConferenceSteps{
    StepSpec{Step::kHeadToHeadSweep, 1},
    StepSpec{Step::kConferencePct, 1},
    StepSpec{Step::kCommonPct, 1},
    StepSpec{Step::kSov, 2},
    StepSpec{Step::kSos, 2},
    StepSpec{Step::kConferencePoints, 3},
    StepSpec{Step::kLeaguePoints, 3},
    StepSpec{Step::kConferencePd, 3},
    StepSpec{Step::kPd, 3},
};

// D3: the article's steps with the usual depth cut-offs
constexpr std::array kDraftSteps{
    StepSpec{Step::kHeadToHeadSweep, 1},
    StepSpec{Step::kCommonPct, 1},
    StepSpec{Step::kSov, 2},
    StepSpec{Step::kLeaguePoints, 3},
    StepSpec{Step::kPd, 3},
};

std::span<const StepSpec> steps_of(Procedure procedure) {
  switch (procedure) {
    case Procedure::kDivision:
      return kDivisionSteps;
    case Procedure::kConference:
      return kConferenceSteps;
    case Procedure::kDraft:
      return kDraftSteps;
  }
  return {};
}

// Worse than any net points value
constexpr int32_t kNoPoints = -1000000000;

Ratio whole(double x) { return {static_cast<int32_t>(std::lround(x)), 1}; }

// Values of one step for the candidates (higher is better), or nothing if the
// step doesn't apply.
struct StepValues {
  std::array<Value, kTeams> value{};
  bool applies = true;
};

StepValues evaluate(const SeasonData& s, const TeamSet& c, Step step,
                    Procedure procedure) {
  StepValues out;
  const int32_t n = c.size();

  if (step == Step::kCommonPct || step == Step::kCommonPd) {
    // Opponents that every candidate played. Candidates are never common
    // opponents.
    std::array<bool, kTeams> is_candidate{};
    for (TeamId t : c) is_candidate[t] = true;
    std::array<bool, kTeams> common{};
    for (TeamId o = 0; o < kTeams; ++o) {
      common[o] = !is_candidate[o] &&
                  std::all_of(c.begin(), c.end(),
                              [&](TeamId t) { return s.h2h_games[t][o] > 0; });
    }
    // D14: conference and draft need at least four common opponents
    if (step == Step::kCommonPct && procedure != Procedure::kDivision &&
        std::count(common.begin(), common.end(), true) < 4) {
      out.applies = false;
    }
    for (int32_t i = 0; i < n; ++i) {
      const TeamId t = c[i];
      int32_t games = 0;
      int32_t half = 0;
      double pd = 0;
      for (TeamId o = 0; o < kTeams; ++o) {
        if (!common[o]) continue;
        games += s.h2h_games[t][o];
        half += s.h2h_half_wins[t][o];
        pd += s.h2h_pd[t][o];
      }
      out.value[i] =
          step == Step::kCommonPct ? Ratio(half, 2 * games) : whole(pd);
    }
  } else {
    for (int32_t i = 0; i < n; ++i) {
      const TeamId t = c[i];
      Value v;
      switch (step) {
        case Step::kHeadToHead: {
          int32_t games = 0;
          int32_t half = 0;
          for (TeamId u : c) {
            games += s.h2h_games[t][u];
            half += s.h2h_half_wins[t][u];
          }
          v = Ratio(half, 2 * games);
          break;
        }
        case Step::kHeadToHeadSweep: {
          // 2 = beat every other candidate, 0 = lost to every other
          // candidate, 1 = anything else (or didn't play all of them)
          bool played_all = true;
          int32_t games = 0;
          int32_t half = 0;
          for (TeamId u : c) {
            if (u == t) continue;
            played_all = played_all && s.h2h_games[t][u] > 0;
            games += s.h2h_games[t][u];
            half += s.h2h_half_wins[t][u];
          }
          int32_t sweep = 1;
          if (played_all && half == 2 * games) sweep = 2;
          if (played_all && half == 0) sweep = 0;
          v = Ratio(sweep, 1);
          break;
        }
        case Step::kDivisionPct:
          v = s.div_pct(t);
          break;
        case Step::kConferencePct:
          v = s.conf_pct(t);
          break;
        case Step::kSov:
          v = s.sov(t);
          break;
        case Step::kSos:
          v = s.sos(t);
          break;
        case Step::kConferencePoints:
          v = Ratio(-s.conf_points_rank[t], 1);
          break;
        case Step::kLeaguePoints:
          v = Ratio(-s.league_points_rank[t], 1);
          break;
        case Step::kConferencePd:
          v = s.conf_games[t] == 0 ? Ratio(kNoPoints, 1) : whole(s.conf_pd[t]);
          break;
        case Step::kPd:
          v = whole(s.pf[t] - s.pa[t]);
          break;
        default:
          throw EngineError("Unknown tiebreaking step");
      }
      out.value[i] = v;
    }
  }

  if (procedure == Procedure::kDraft) {
    for (int32_t i = 0; i < n; ++i) out.value[i] = out.value[i].negated();
  }
  return out;
}

void shuffle(TeamSet& set, RandomSource& rng) {
  for (int32_t i = set.size() - 1; i > 0; --i) {
    const int32_t j = rng.index(i + 1);
    std::swap(*(set.begin() + i), *(set.begin() + j));
  }
}

// Assign ranks first, first + 1, ... in the order of `set`
void assign_in_order(const TeamSet& set, int32_t first, int32_t label,
                     RankResult& out) {
  int32_t r = first;
  for (TeamId t : set) {
    out.rank[t] = r++;
    out.label[t] = label;
  }
}

template <typename Less>
void stable_sort(TeamSet& set, Less less) {
  std::stable_sort(set.begin(), set.end(), less);
}

// Blocks of consecutive teams in `sorted` with equal `key`. Calls
// fn(block, position of the first team).
template <typename Key, typename Fn>
void for_each_block(const TeamSet& sorted, Key key, Fn fn) {
  int32_t i = 0;
  while (i < sorted.size()) {
    TeamSet block;
    int32_t j = i;
    while (j < sorted.size() && key(sorted[j]) == key(sorted[i])) {
      block.push(sorted[j]);
      ++j;
    }
    fn(block, i);
    i = j;
  }
}

// Run the steps of a procedure on the candidates until one team is best.
// Candidates that lose a step sit out the rest of this pick, the others
// restart at the first step. Draft steps are reversed (worse team wins).
Pick pick_one(const SeasonData& s, TeamSet candidates, Procedure procedure,
              int32_t depth, RandomSource& rng) {
  bool restart = true;
  while (restart) {
    restart = false;
    for (const StepSpec& spec : steps_of(procedure)) {
      if (spec.min_depth > depth) continue;
      const int32_t n = candidates.size();
      const StepValues v = evaluate(s, candidates, spec.step, procedure);
      if (!v.applies) continue;
      const Value best =
          *std::max_element(v.value.begin(), v.value.begin() + n);
      TeamSet remaining;
      for (int32_t i = 0; i < n; ++i) {
        if (v.value[i] == best) remaining.push(candidates[i]);
      }
      if (remaining.size() == 1) {
        return {remaining[0], label_code(spec.step, n)};
      }
      if (remaining.size() < n) {
        candidates = remaining;
        restart = true;
        break;
      }
    }
  }
  return {candidates[rng.index(candidates.size())],
          label_code(Step::kCoinToss, 0)};
}

}  // namespace

RankResult rank_divisions(const SeasonData& s, const League& league,
                          int32_t depth, RandomSource& rng) {
  RankResult out;
  for (int32_t d = 0; d < kDivisions; ++d) {
    TeamSet teams;
    for (TeamId t = 0; t < kTeams; ++t) {
      if (league.division[t] == d && s.played(t)) teams.push(t);
    }
    stable_sort(
        teams, [&](TeamId a, TeamId b) { return s.win_pct(a) > s.win_pct(b); });
    for_each_block(
        teams, [&](TeamId t) { return s.win_pct(t); },
        [&](TeamSet block, int32_t pos) {
          int32_t r = pos + 1;
          if (block.size() > 1 && depth == 0) {
            shuffle(block, rng);
            assign_in_order(block, r, label_code(Step::kCoinToss, 0), out);
            return;
          }
          // D1: one team per step, the others restart at step 1. The step
          // that decides the last duel also fixes the last team's rank.
          int32_t label = kMissing;
          while (block.size() > 1) {
            const Pick p = pick_one(s, block, Procedure::kDivision, depth, rng);
            out.rank[p.team] = r++;
            out.label[p.team] = p.label;
            label = p.label;
            block.erase(p.team);
          }
          out.rank[block[0]] = r;
          out.label[block[0]] = label;
        });
  }
  return out;
}

RankResult rank_conferences(const SeasonData& s, const League& league,
                            const TeamArray<int32_t>& div_rank,
                            int32_t playoff_seeds, int32_t depth,
                            RandomSource& rng) {
  RankResult out;
  auto rank_group = [&](TeamSet teams, int32_t offset) {
    stable_sort(
        teams, [&](TeamId a, TeamId b) { return s.win_pct(a) > s.win_pct(b); });
    for_each_block(
        teams, [&](TeamId t) { return s.win_pct(t); },
        [&](TeamSet block, int32_t pos) {
          int32_t r = offset + pos + 1;
          if (r > playoff_seeds) return;
          if (block.size() > 1 && depth == 0) {
            shuffle(block, rng);
            assign_in_order(block, r, label_code(Step::kCoinToss, 0), out);
            return;
          }
          while (block.size() > 1) {
            const int32_t first_div = league.division[block[0]];
            if (std::all_of(block.begin(), block.end(), [&](TeamId t) {
                  return league.division[t] == first_div;
                })) {
              stable_sort(block, [&](TeamId a, TeamId b) {
                return div_rank[a] < div_rank[b];
              });
              assign_in_order(block, r,
                              label_code(Step::kDivisionTiebreaker, 0), out);
              return;
            }
            // Division reduction: only the best club of each division
            TeamSet candidates;
            for (TeamId t : block) {
              const bool best_of_division =
                  std::none_of(block.begin(), block.end(), [&](TeamId u) {
                    return league.division[u] == league.division[t] &&
                           div_rank[u] < div_rank[t];
                  });
              if (best_of_division) candidates.push(t);
            }
            const Pick p =
                pick_one(s, candidates, Procedure::kConference, depth, rng);
            out.rank[p.team] = r++;
            out.label[p.team] = p.label;
            block.erase(p.team);
          }
          // D4: the last team of a block gets no label
          out.rank[block[0]] = r;
        });
  };
  for (int32_t c = 0; c < kConferences; ++c) {
    TeamSet winners;
    TeamSet others;
    for (TeamId t = 0; t < kTeams; ++t) {
      if (league.conference[t] != c || !s.played(t)) continue;
      if (div_rank[t] == 1) {
        winners.push(t);
      } else {
        others.push(t);
      }
    }
    rank_group(winners, 0);
    rank_group(others, 4);
  }
  return out;
}

RankResult rank_draft(const SeasonData& s, const League& league,
                      const TeamArray<int32_t>& div_rank,
                      const TeamArray<int32_t>& conf_rank,
                      const TeamArray<int32_t>& exit, int32_t depth,
                      RandomSource& rng) {
  RankResult out;
  TeamSet teams;
  for (TeamId t = 0; t < kTeams; ++t) {
    if (!s.played(t)) continue;
    if (div_rank[t] == kMissing || conf_rank[t] == kMissing) {
      throw EngineError("Draft ranks need all division and conference ranks");
    }
    teams.push(t);
  }
  auto key = [&](TeamId t) {
    return std::tuple(exit[t], s.win_pct(t), s.sos(t));
  };
  stable_sort(teams, [&](TeamId a, TeamId b) { return key(a) < key(b); });
  for_each_block(teams, key, [&](TeamSet block, int32_t pos) {
    int32_t r = pos + 1;
    if (block.size() > 1 && depth == 0) {
      shuffle(block, rng);
      assign_in_order(block, r, label_code(Step::kCoinToss, 0), out);
      return;
    }
    auto all_same = [&](const TeamArray<int32_t>& group) {
      return std::all_of(block.begin(), block.end(),
                         [&](TeamId t) { return group[t] == group[block[0]]; });
    };
    while (block.size() > 1) {
      // Worse division or conference rank picks first
      if (all_same(league.division)) {
        stable_sort(block, [&](TeamId a, TeamId b) {
          return div_rank[a] > div_rank[b];
        });
        assign_in_order(block, r, label_code(Step::kDivisionTiebreaker, 0),
                        out);
        return;
      }
      if (all_same(league.conference)) {
        stable_sort(block, [&](TeamId a, TeamId b) {
          return conf_rank[a] > conf_rank[b];
        });
        assign_in_order(block, r, label_code(Step::kConferenceTiebreaker, 0),
                        out);
        return;
      }
      // Reduction: the worst conference rank of each conference. That is also
      // the worst division rank of its division.
      TeamSet candidates;
      for (int32_t c = 0; c < kConferences; ++c) {
        TeamId worst = -1;
        for (TeamId t : block) {
          if (league.conference[t] == c &&
              (worst < 0 || conf_rank[t] > conf_rank[worst])) {
            worst = t;
          }
        }
        candidates.push(worst);
      }
      const Pick p = pick_one(s, candidates, Procedure::kDraft, depth, rng);
      out.rank[p.team] = r++;
      out.label[p.team] = p.label;
      block.erase(p.team);
    }
    // D4: the last team of a block gets no label
    out.rank[block[0]] = r;
  });
  return out;
}

}  // namespace nflseedr
