// Sets of teams as bits, for the clinching checks
#pragma once

#include <bit>
#include <cstdint>

#include "types.hpp"

namespace nflseedr {

using TeamMask = uint32_t;
static_assert(kTeams <= 32);

[[nodiscard]] constexpr TeamMask team_bit(TeamId t) noexcept {
  return TeamMask{1} << static_cast<uint32_t>(t);
}

[[nodiscard]] constexpr bool has(TeamMask mask, TeamId t) noexcept {
  return (mask & team_bit(t)) != 0;
}

// Calls f(t) for every team in `mask`
template <typename F>
void for_each_team(TeamMask mask, F f) {
  while (mask != 0) {
    f(static_cast<TeamId>(std::countr_zero(mask)));
    mask &= mask - 1;
  }
}

// The half-wins that give a team with `games` games the win percentage of
// `x_half_wins` half-wins in `x_games` games: the most that aren't above it
// (`floor`), and whether they are exactly there
struct TieLevel {
  int32_t floor = 0;
  bool exact = false;
};

[[nodiscard]] constexpr TieLevel tie_level(int32_t x_half_wins, int32_t x_games,
                                           int32_t games) noexcept {
  const int64_t scaled = static_cast<int64_t>(x_half_wins) * games;
  return {.floor = static_cast<int32_t>(scaled / x_games),
          .exact = scaled % x_games == 0};
}

}  // namespace nflseedr
