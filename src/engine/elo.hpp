// Elo model of cpp_compute_results(): game results of one week and elo
// updates. A port of nflseedR_compute_results() with identical results.
#pragma once

#include <cstdint>
#include <limits>
#include <span>
#include <vector>

#include "types.hpp"

namespace nflseedr {

// Missing game result (NA_INTEGER in R)
inline constexpr int32_t kNoResult = std::numeric_limits<int32_t>::min();

// Games of one week. Teams are ids 0..n-1 of the team table or kMissing.
struct EloGames {
  std::span<const int32_t> sim;
  std::span<const int32_t> away;
  std::span<const int32_t> home;
  // home_rest - away_rest (NaN if missing)
  std::span<const double> rest_diff;
  // 1 if location is "Home", 1 if postseason game
  std::span<const int32_t> home_field;
  std::span<const int32_t> postseason;
  // kNoResult if missing
  std::span<const int32_t> result;
};

// One row per simulation and team. The first row of a sim and team wins.
struct EloTeams {
  std::span<const int32_t> sim;
  std::span<const int32_t> team;
  std::span<const double> elo;
};

struct EloWeek {
  // Results of all games, missing ones simulated
  std::vector<int32_t> result;
  // New elo of all team rows
  std::vector<double> elo;
};

// Simulates missing results with normal draws from `rng` (in game order, like
// rnorm()) and shifts the elo of the teams that played.
[[nodiscard]] EloWeek simulate_elo_week(const EloGames& games,
                                        const EloTeams& teams,
                                        RandomSource& rng);

}  // namespace nflseedr
