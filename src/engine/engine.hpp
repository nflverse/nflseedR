// Entry points of the standings engine: loop over seasons and write outputs.
#pragma once

#include <cstdint>
#include <functional>
#include <span>
#include <vector>

#include "procedures.hpp"
#include "types.hpp"

namespace nflseedr {

// Games of all seasons. `sim` holds dense season ids 1..n_sims, teams are ids
// 0..31 and `type` a GameType.
struct GamesInput {
  std::span<const int32_t> sim;
  std::span<const int32_t> type;
  std::span<const int32_t> away;
  std::span<const int32_t> home;
  std::span<const double> result;
  std::span<const double> away_score;
  std::span<const double> home_score;
  bool has_scores = false;
  int32_t n_sims = 0;
};

enum class Ranks : int32_t {
  kNone = 0,
  kDivision = 1,
  kConference = 2,
  kDraft = 3
};

struct StandingsOptions {
  Ranks ranks = Ranks::kConference;
  int32_t playoff_seeds = kTeamsPerConference;
  int32_t depth = 2;
  // One value per season: 1 if the regular season is complete (D13)
  std::span<const int32_t> season_complete;
};

// One row per season and team that played, sorted by season, division,
// division rank and team. Missing values are kMissing.
struct StandingsRows {
  std::vector<int32_t> sim, team, games, true_wins, losses, ties;
  std::vector<double> wins, pf, pa, win_pct, div_pct, conf_pct, sov, sos;
  std::vector<int32_t> div_rank, div_label, conf_rank, conf_label;
  std::vector<int32_t> exit, draft_rank, draft_label;
};

// Called every 1,000 seasons. May throw to cancel.
using InterruptCheck = std::function<void()>;

[[nodiscard]] StandingsRows compute_standings(const GamesInput& games,
                                              const League& league,
                                              const StandingsOptions& options,
                                              RandomSource& rng,
                                              const InterruptCheck& interrupt);

// Existing ranks of the teams of all seasons (e.g. from simulations)
struct TeamRanksInput {
  std::span<const int32_t> sim;
  std::span<const int32_t> team;
  std::span<const int32_t> div_rank;
  std::span<const int32_t> conf_rank;
  std::span<const int32_t> exit;
};

struct DraftRanks {
  std::vector<int32_t> rank, label;
};

// Draft ranks for the rows of `teams`, in the same order. Seasons where not
// every team played get kMissing.
[[nodiscard]] DraftRanks compute_draft_ranks(const GamesInput& games,
                                             const League& league,
                                             const TeamRanksInput& teams,
                                             int32_t depth, RandomSource& rng,
                                             const InterruptCheck& interrupt);

}  // namespace nflseedr
