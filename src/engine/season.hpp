// Records and head-to-head data of one season.
#pragma once

#include <array>
#include <cstdint>
#include <span>

#include "types.hpp"

namespace nflseedr {

struct Game {
  TeamId away = 0;
  TeamId home = 0;
  int32_t type = kRegular;
  double result = 0;  // home score - away score
  double away_score = 0;
  double home_score = 0;
};

// Win percentage of a record in half-wins. During the regular season, net
// wins break ties of equal percentages (D13).
[[nodiscard]] constexpr Value record(int32_t half_wins, int32_t games,
                                     bool net_wins) noexcept {
  return {Ratio(half_wins, 2 * games), net_wins ? half_wins - games : 0};
}

template <typename T>
using TeamArray = std::array<T, kTeams>;
template <typename T>
using TeamMatrix = std::array<std::array<T, kTeams>, kTeams>;

// All counts of wins are half-wins (win = 2, tie = 1) so that every
// percentage is a ratio of integers.
struct SeasonData {
  TeamArray<int32_t> games{};
  TeamArray<int32_t> half_wins{};
  TeamArray<int32_t> true_wins{};
  TeamArray<int32_t> losses{};
  TeamArray<int32_t> ties{};
  TeamArray<int32_t> div_games{};
  TeamArray<int32_t> div_half_wins{};
  TeamArray<int32_t> conf_games{};
  TeamArray<int32_t> conf_half_wins{};
  TeamArray<double> conf_pd{};
  TeamArray<double> pf{};
  TeamArray<double> pa{};
  // Half-wins and games of beaten opponents (SOV) and all opponents (SOS)
  TeamArray<int32_t> sov_half_wins{};
  TeamArray<int32_t> sov_games{};
  TeamArray<int32_t> sos_half_wins{};
  TeamArray<int32_t> sos_games{};
  // Combined ranking of points scored and allowed. Lower is better.
  TeamArray<int32_t> conf_points_rank{};
  TeamArray<int32_t> league_points_rank{};
  TeamArray<int32_t> exit{};
  TeamMatrix<int32_t> h2h_games{};
  TeamMatrix<int32_t> h2h_half_wins{};
  TeamMatrix<double> h2h_pd{};
  // After the final regular season week, records compare by percentage only
  bool regular_season_complete = true;

  // Compute everything from the games of one season. Without scores, every
  // team scores 3 points per game (the R implementation does the same).
  void build(std::span<const Game> season_games, const League& league,
             bool has_scores, bool complete);

  [[nodiscard]] bool played(TeamId t) const noexcept { return games[t] > 0; }
  [[nodiscard]] bool all_played() const noexcept;

  [[nodiscard]] Value win_pct(TeamId t) const noexcept {
    return record(half_wins[t], games[t], !regular_season_complete);
  }
  [[nodiscard]] Value div_pct(TeamId t) const noexcept {
    return record(div_half_wins[t], div_games[t], !regular_season_complete);
  }
  [[nodiscard]] Value conf_pct(TeamId t) const noexcept {
    return record(conf_half_wins[t], conf_games[t], !regular_season_complete);
  }
  [[nodiscard]] Ratio sov(TeamId t) const noexcept {
    return {sov_half_wins[t], 2 * sov_games[t]};
  }
  [[nodiscard]] Ratio sos(TeamId t) const noexcept {
    return {sos_half_wins[t], 2 * sos_games[t]};
  }
};

}  // namespace nflseedr
