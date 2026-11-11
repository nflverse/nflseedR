// Clinching and elimination: can a team still reach or miss a division title,
// a playoff berth and the No. 1 seed? See dev/playoff-status.md.
#pragma once

#include <array>
#include <cstdint>
#include <span>
#include <vector>

#include "engine.hpp"
#include "types.hpp"

namespace nflseedr {

enum class Goal : int32_t { kDivision = 0, kPlayoffs = 1, kFirstSeed = 2 };
inline constexpr int32_t kGoals = 3;

// Status of a team for one goal. R/clinching_status.R decodes the codes.
enum class Status : int32_t {
  kClinched = 1,
  kAlive = 2,
  kEliminated = 3,
  kUndecided = 4,
};

struct StatusOptions {
  // Which goals to compute, indexed by Goal
  std::array<bool, kGoals> goals{};
  // Playoff seeds per conference, one value per season
  std::span<const int32_t> seeds;
};

// One row per season and team with a played or remaining game, sorted by
// season and team. Goals that weren't requested are kMissing.
struct StatusRows {
  std::vector<int32_t> sim, team, wins, losses, ties, games_left;
  std::array<std::vector<int32_t>, kGoals> status;
};

// `played` are all games with a result, `remaining` the regular season games
// without one (their results are ignored). Both use the same season ids.
[[nodiscard]] StatusRows compute_clinching_status(
    const GamesInput& played, const GamesInput& remaining, const League& league,
    const StatusOptions& options, const InterruptCheck& interrupt);

}  // namespace nflseedr
