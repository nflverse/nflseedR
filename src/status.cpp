#include "engine/status.hpp"

#include <algorithm>
#include <array>
#include <bit>
#include <functional>
#include <limits>
#include <optional>
#include <utility>

#include "engine/checks.hpp"
#include "engine/flow.hpp"
#include "engine/group_check.hpp"
#include "engine/scenario.hpp"
#include "engine/season.hpp"
#include "engine/tie_proof.hpp"

namespace nflseedr {

namespace {

using namespace clinching;

// Runs for the final standings once no games are left. Enough to try every
// coin toss sequence in practice, which makes the answers exact.
constexpr int32_t kSeasonOverBudget = 4096;
// Orders of the greedy tightening in the gray zone
constexpr int32_t kTighteningOrders = 4;

// Answer to "can X reach the goal?" or "can X miss it?"
enum class Answer : int8_t { kUnknown, kYes, kNo };

// Ranking that breaks ties with X: the division (division title), the
// conference (No. 1 seed) or the wild cards (playoffs)
enum class Ranking : int8_t { kDivision, kConference, kWildCard };

// Calls f(subset) for every subset of `mask` with `size` teams until f
// returns true. True if it did.
template <typename F>
[[nodiscard]] bool any_subset(TeamMask mask, int32_t size, F f) {
  for (TeamMask s = mask;; s = (s - 1) & mask) {
    if (std::popcount(s) == size && f(s)) return true;
    if (s == 0) return false;
  }
}

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
  // Tie groups that get the proof that carries facts from step to step
  // (P15), which is costly
  int32_t fact_groups = 4;
  // Tie groups still open after that (or left without it) which get the
  // proof that checks the facts of a pick jointly (P17), which is costlier
  // still. It only runs if no more groups than this are open: one open
  // group leaves the question open anyway.
  int32_t joint_groups = 0;
};
constexpr Budgets kSearchBudgets{
    .scenarios = 2, .group_flows = 30, .open_groups = 2, .fact_groups = 2};
// Late in the season the tie groups get more flows: few teams are still
// open, and that's when teams clinch
constexpr int32_t kLateGames = 4;
constexpr int32_t kLateGroupFlows = 2000;
// A single season in the input gets enough for the tie groups of early No. 1
// seed questions (up to 15 open teams). More flows settle nothing more; with
// many seasons they would take too long (P14).
constexpr Budgets kSingleSeasonBudgets{
    .group_flows = 20000,
    .open_groups = std::numeric_limits<int32_t>::max(),
    .fact_groups = 128,
    .joint_groups = 512};
// Interrupt checks during the search, every this many parts
constexpr int32_t kInterruptEvery = 64;
// Splitting on X's own games comes last: the checks already fix them
constexpr int32_t kOwnGamePenalty = 10;

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

  // The group check of the answer "no" to "can X reach the goal?"
  // (`reach`) or "can X miss it?" (tests and diagnostics, see
  // engine/group_check.hpp)
  [[nodiscard]] bool check_no(Goal goal, bool reach, int32_t samples,
                              RandomSource& rng,
                              const InterruptCheck& interrupt) {
    const GroupCheck group{
        .season = s_,
        .check = reach ? &reach_ : &miss_,
        .x = x_,
        .goal = goal,
        .reach = reach,
        .teams = group_teams(goal),
        .settled =
            [&](TeamMask above, TeamMask level, TeamMask may_follow) {
              return settled_without_ties(goal, reach, above, level,
                                          may_follow);
            },
        .flow = flow_,
        .runner = runner_};
    return clinching::check_no(group, samples, rng, interrupt);
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
    return built(reach ? reach_ties_ : miss_ties_, reach);
  }

  // A second set of tie proofs for the tie groups (P11): they know which
  // teams end above, level with and below X (TieProof::set_context()), and
  // lose their cached results with every group
  [[nodiscard]] TieProof& context_ties(bool reach) {
    return built(reach ? reach_context_ties_ : miss_context_ties_, reach);
  }

  [[nodiscard]] TieProof& built(std::optional<TieProof>& proof, bool reach) {
    if (!proof) {
      proof.emplace(s_->played, s_->played_games, s_->remaining, *s_->league,
                    x_, reach);
    }
    return *proof;
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

  // The teams whose final position against X decides the goal: only the
  // rivals matter for the division title
  [[nodiscard]] TeamMask group_teams(Goal goal) const noexcept {
    return goal == Goal::kDivision ? rivals_ : conference_;
  }

  // Whatever the tiebreakers and the teams still open do, the answer to
  // "can X reach the goal?" (`reach`) or "can X miss it?" is no: X misses
  // the goal if the teams above alone keep X from it, and reaches it if
  // even all teams that can end level or above (`may_follow`) don't
  [[nodiscard]] bool settled_without_ties(Goal goal, bool reach, TeamMask above,
                                          TeamMask level,
                                          TeamMask may_follow) const {
    return reach ? keeps_x_from(goal, above)
                 : !keeps_x_from(goal, above | level | may_follow);
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

  [[nodiscard]] Proof gray(Goal goal, TeamMask teams, bool want_reach,
                           const Feasible& feasible, const Check& check) {
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
    const GroupSides sides = group_sides(check, group_teams(goal));
    const std::vector<Open>& open = sides.open;
    TieProof& proof = ties(reach);
    int32_t flows = budgets_.group_flows;
    // Groups that stay open before giving up: the answer is open then,
    // unless a later group says yes
    int32_t open_groups = budgets_.open_groups;
    // Groups that get the proof that carries facts (P15)
    int32_t fact_groups = budgets_.fact_groups;
    bool unknown = false;
    Proof found;
    // Groups still open, for the joint proof
    struct OpenGroup {
      TeamMask above = 0;
      TeamMask level = 0;
      TeamMask below = 0;
      Completion completion;
    };
    std::vector<OpenGroup> open_groups_left;
    const TieOutcome wanted =
        reach ? TieOutcome::kReached : TieOutcome::kMissed;
    const auto never = [&](TieOutcome o) {
      return o == TieOutcome::kImpossible ||
             o == (reach ? TieOutcome::kMissed : TieOutcome::kReached);
    };
    // True to stop
    const auto visit = [&](const auto& self, size_t i, TeamMask above,
                           TeamMask level, TeamMask below) -> bool {
      if (settled_without_ties(goal, reach, above, level,
                               sides.can_be_ahead[i])) {
        return false;
      }
      if (flows-- <= 0) {
        unknown = true;
        return true;
      }
      const bool leaf = i == open.size();
      if (!check.exact(above, level, below, *flow_, nullptr)) return false;
      // The flow that only answers is smaller, and few groups need more
      Completion c;
      const auto complete = [&] {
        if (c.empty() && !check.exact(above, level, below, *flow_, &c)) {
          throw EngineError("A feasible flow became infeasible");
        }
      };
      if (!leaf) {
        const Open& o = open[i];
        const TeamMask bit = team_bit(o.team);
        return (o.level && self(self, i + 1, above, level | bit, below)) ||
               (o.above && self(self, i + 1, above | bit, level, below)) ||
               (o.below && self(self, i + 1, above, level, below | bit));
      }
      TieOutcome outcome = tie_outcome(proof, goal, above, level);
      // Not settled: again with ranges that know every team's side
      TieProof& context = context_ties(reach);
      if (!never(outcome)) {
        context.set_context(above, level, below);
        context.carry_facts(false);
        outcome = tie_outcome(context, goal, above, level);
      }
      if (never(outcome)) {
        if (verify_) {
          complete();
          verify_no(goal, reach, c);
        }
        return false;
      }
      complete();
      for (const int8_t fill : {kHomeWin, kAwayWin}) {
        found = try_scenario(goal, filled(c, fill), reach);
        if (found.answer == Answer::kYes) return true;
      }
      // Still open: the costly proof that carries facts from step to step
      if (fact_groups-- > 0) {
        context.carry_facts(true);
        outcome = tie_outcome(context, goal, above, level);
        if (never(outcome)) {
          if (verify_) verify_no(goal, reach, c);
          return false;
        }
      }
      if (verify_ && outcome == wanted) {
        throw EngineError("A tie group proof failed its self-check");
      }
      if (!unknown && std::ssize(open_groups_left) < budgets_.joint_groups) {
        open_groups_left.push_back({.above = above,
                                    .level = level,
                                    .below = below,
                                    .completion = std::move(c)});
      } else {
        unknown = true;
      }
      return --open_groups <= 0;
    };
    (void)visit(visit, 0, sides.above, sides.level, sides.below);
    if (found.answer == Answer::kYes) return found;
    if (unknown) return {};
    // Only a few groups are open: the proof that checks the facts of every
    // pick jointly (P17), until one of them stays open
    TieProof& context = context_ties(reach);
    for (const OpenGroup& g : open_groups_left) {
      context.set_context(g.above, g.level, g.below);
      context.carry_facts(true, true);
      if (!never(tie_outcome(context, goal, g.above, g.level))) return {};
      if (verify_) verify_no(goal, reach, g.completion);
    }
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
      for_each_team(division_masks_[i], [&](TeamId y) {
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
      for_each_team(division_masks_[i], [&](TeamId y) {
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
  if (options.group_samples > 0 && options.rng == nullptr) {
    throw EngineError("The group check needs random numbers");
  }
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
        if (options.group_samples > 0 && !season_remaining.empty()) {
          for (const bool reach : {true, false}) {
            if ((reach ? a.reach : a.miss).answer != Answer::kNo) continue;
            const bool complete =
                team.check_no(static_cast<Goal>(goal), reach,
                              options.group_samples, *options.rng, interrupt);
            ++(complete ? out.group_checks : out.group_checks_incomplete);
          }
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
