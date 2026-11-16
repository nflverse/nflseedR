#include "engine/group_check.hpp"

#include <algorithm>
#include <array>
#include <bit>
#include <cmath>
#include <span>
#include <string>
#include <utility>
#include <vector>

namespace nflseedr::clinching {

namespace {

// The group check (StatusOptions::group_samples): flows to find the tie
// groups of one answer, and the share of tie games in its random completions
constexpr int32_t kCheckFlows = 20000;
constexpr int32_t kSampleTiePercent = 6;
// Standard deviations of the team strengths (log odds of a win) in the
// random completions, one after the other: 0 gives coin flips
constexpr std::array<double, 3> kSampleSpreads{0.0, 1.5, 3.0};
// Steered completions: log odds per half-win of difference between the
// opponents of two teams, and the standard deviation on top
constexpr double kSampleSteer = 1.5;
constexpr double kSampleSteerSpread = 1.0;
// Steps of the uniform draw that decides a game
constexpr int32_t kSampleResolution = 1'000'000;

// Group check: X must not reach the goal (`reach`) or miss it in
// `completion`, whatever the coin tosses
void expect_no(const GroupCheck& g, const Completion& completion) {
  (void)g.runner->run(
      completion, kTossBudget,
      [&](const RankResult& div, const RankResult& conf, bool) {
        if (g.season->reached(g.goal, g.x, div, conf) != g.reach) return false;
        // Enough to rebuild the case: results are home half-wins of the
        // remaining games in input order
        std::string results;
        for (const int8_t r : completion) results += std::to_string(r);
        throw EngineError("A clinching answer failed its group check: team " +
                          std::to_string(g.x) + ", goal " +
                          std::to_string(static_cast<int32_t>(g.goal)) +
                          (g.reach ? ", can reach" : ", can miss") +
                          ", completion " + results);
      });
}

// How the random completions of a tie group pick results: every team
// gets a strength (log odds of a win), `lean` plus a random part with
// standard deviation `spread`
struct SamplePlan {
  TeamArray<double> lean{};
  double spread = 0.0;
};

// The plan of the k-th completion of a tie group. Even k: random
// strengths only, with the spreads in turn. A large one makes teams win
// out or lose out together. Odd k: steered against the answer "no"
// through the SOV, then the SOS of one level team y after the other: for
// "can miss", the teams y beat (played) are strong and the teams X beat
// weak, the reverse for "can reach". Coin flips almost never line up the
// ten or so games such a step can take.
[[nodiscard]] SamplePlan sample_plan(const GroupCheck& g, int32_t k,
                                     TeamMask level) {
  const bool reach = g.reach;
  const TeamId x = g.x;
  const Season& s = *g.season;
  SamplePlan plan;
  const int32_t n_level = std::popcount(level);
  if (k % 2 == 0 || n_level == 0) {
    plan.spread =
        kSampleSpreads.at(static_cast<size_t>(k / 2) % kSampleSpreads.size());
    return plan;
  }
  const int32_t turn = k / 2;
  TeamMask rest = level;
  for (int32_t skip = (turn / 2) % n_level; skip > 0; --skip) {
    rest &= rest - 1;
  }
  const auto y = static_cast<TeamId>(std::countr_zero(rest));
  const bool sov = turn % 2 == 0;
  const SeasonData& played = s.played;
  // X's remaining games are wins for "can reach" and losses for "can miss"
  const int32_t x_wins_left = reach ? 2 : 0;
  for (TeamId t = 0; t < kTeams; ++t) {
    const int32_t for_y = sov ? played.h2h_half_wins[y][t]
                              : 2 * (played.h2h_games[y][t] + s.left_vs[y][t]);
    const int32_t for_x =
        sov ? played.h2h_half_wins[x][t] + x_wins_left * s.left_vs[x][t]
            : 2 * (played.h2h_games[x][t] + s.left_vs[x][t]);
    plan.lean[t] = (reach ? -kSampleSteer : kSampleSteer) * (for_y - for_x);
  }
  plan.spread = kSampleSteerSpread;
  return plan;
}

// A random completion with the teams of `above`, `level` and `below` on
// their side of X: the games in random order, each with a random result
// by the strengths of `plan` if the flow says the rest can still follow,
// with another one if not
[[nodiscard]] Completion sample(const GroupCheck& g, TeamMask above,
                                TeamMask level, TeamMask below,
                                const SamplePlan& plan, RandomSource& rng) {
  const Check& check = *g.check;
  TeamArray<double> strength = plan.lean;
  for (double& value : strength) value += plan.spread * rng.normal();
  const auto home_wins = [&](const Game& g) {
    const double p =
        1.0 / (1.0 + std::exp(strength[g.away] - strength[g.home]));
    return rng.index(kSampleResolution) < p * kSampleResolution;
  };
  const std::span<const Game> games = g.season->remaining;
  std::vector<size_t> order;
  for (size_t i = 0; i < games.size(); ++i) {
    if (!check.involves_x(games[i])) order.push_back(i);
  }
  for (size_t i = order.size(); i > 1; --i) {
    const auto j = static_cast<size_t>(rng.index(static_cast<int32_t>(i)));
    std::swap(order[i - 1], order[j]);
  }
  const TeamMask bounded = above | level | below;
  Completion fixed(games.size(), kFree);
  const auto feasible = [&](Completion* out) {
    return check.exact(above, level, below, *g.flow, out, &fixed);
  };
  for (const size_t i : order) {
    const bool tie = rng.index(100) < kSampleTiePercent;
    const int8_t win = home_wins(games[i]) ? kHomeWin : kAwayWin;
    const auto other = static_cast<int8_t>(kHomeWin - win);
    const std::array<int8_t, 3> results =
        tie ? std::array{kTie, win, other} : std::array{win, other, kTie};
    // Games between teams without a side never matter to the flow
    const bool is_free =
        !has(bounded, games[i].away) && !has(bounded, games[i].home);
    const bool found = std::ranges::any_of(results, [&](int8_t result) {
      fixed[i] = result;
      return is_free || feasible(nullptr);
    });
    if (!found) throw EngineError("A feasible flow became infeasible");
  }
  Completion c;
  if (!feasible(&c)) throw EngineError("A feasible flow became infeasible");
  return c;
}

}  // namespace

bool check_no(const GroupCheck& g, int32_t samples, RandomSource& rng,
              const InterruptCheck& interrupt) {
  const Check& check = *g.check;
  const GroupSides sides = group_sides(check, g.teams);
  int32_t flows = kCheckFlows;
  bool complete = true;
  const auto visit = [&](const auto& self, size_t i, TeamMask above,
                         TeamMask level, TeamMask below) -> void {
    if (!complete || g.settled(above, level, sides.can_be_ahead[i])) {
      return;
    }
    if (flows-- <= 0) {
      complete = false;
      return;
    }
    const bool leaf = i == sides.open.size();
    Completion c;
    if (!check.exact(above, level, below, *g.flow, leaf ? &c : nullptr)) {
      return;
    }
    if (!leaf) {
      const Open& o = sides.open[i];
      const TeamMask bit = team_bit(o.team);
      if (o.level) self(self, i + 1, above, level | bit, below);
      if (o.above) self(self, i + 1, above | bit, level, below);
      if (o.below) self(self, i + 1, above, level, below | bit);
      return;
    }
    interrupt();
    for (const int8_t fill : {kHomeWin, kAwayWin}) {
      expect_no(g, filled(c, fill));
    }
    for (int32_t k = 0; k < samples; ++k) {
      const SamplePlan plan = sample_plan(g, k, level);
      expect_no(g, sample(g, above, level, below, plan, rng));
    }
  };
  visit(visit, 0, sides.above, sides.level, sides.below);
  return complete;
}

}  // namespace nflseedr::clinching
