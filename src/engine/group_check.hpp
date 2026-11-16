// The group check of the clinching answers (tests and diagnostics only):
// StatusOptions::group_samples. See dev/playoff-status.md.
#pragma once

#include <cstdint>
#include <functional>

#include "checks.hpp"
#include "engine.hpp"
#include "flow.hpp"
#include "scenario.hpp"
#include "status.hpp"
#include "team_mask.hpp"
#include "types.hpp"

namespace nflseedr::clinching {

// The answer "no" of team `x` to "can X reach the goal?" (`reach`) or "can
// X miss it?"
struct GroupCheck {
  const Season* season = nullptr;
  // X wins out (`reach`) or loses out
  const Check* check = nullptr;
  TeamId x = 0;
  Goal goal = Goal::kDivision;
  bool reach = false;
  // The teams whose final position against X decides the goal
  TeamMask teams = 0;
  // Whether the answer is no whatever the tiebreakers do, with the teams
  // `above` and `level` and those that can still end level or above
  std::function<bool(TeamMask above, TeamMask level, TeamMask may_follow)>
      settled;
  MaxFlow* flow = nullptr;
  ScenarioRunner* runner = nullptr;
};

// Checks the answer without the tie proofs: every way the teams can end
// above, level with or below X, as in the tie groups of status.cpp, with
// the completion of its flow and `samples` random completions run through
// the procedures. Random completions of the whole season rarely hit a tie
// group that needs many teams to win or lose out together; these do.
// Throws if X gets the other outcome in one of them. False if the flows
// ran out before every way was visited.
[[nodiscard]] bool check_no(const GroupCheck& g, int32_t samples,
                            RandomSource& rng, const InterruptCheck& interrupt);

}  // namespace nflseedr::clinching
