// Proofs that a team wins (or loses) every tie in win percentage it can end
// up in, through the tiebreaking steps up to SOS, whatever the remaining
// games do. Milestone 4 of dev/playoff-status.md.
#pragma once

#include <array>
#include <cstdint>
#include <span>
#include <unordered_map>

#include "season.hpp"
#include "types.hpp"

namespace nflseedr {

// Set of team ids as bits
using TeamMask = uint32_t;
static_assert(kTeams <= 32);

// Teams X always ranks ahead of, or behind, when tied with them
struct TieSides {
  TeamMask x_ahead = 0;
  TeamMask x_behind = 0;
};

class TieProof {
 public:
  // X's remaining games are all wins (`x_wins_out`) or all losses, like the
  // check the proof is used in. `played` was built from `played_games`.
  TieProof(const SeasonData& played, std::span<const Game> played_games,
           std::span<const Game> remaining, const League& league, TeamId x,
           bool x_wins_out);

  // Teams of `tied` (the teams that can end with X's win percentage) that
  // X ranks ahead of whenever they are tied with X, and teams X ranks behind
  // whenever tied. Division sides use the division ranking among rivals,
  // conference sides the conference ranking.
  [[nodiscard]] TieSides division_sides(TeamMask tied);
  [[nodiscard]] TieSides conference_sides(TeamMask tied);

 private:
  enum class Procedure : int8_t { kDivision = 0, kConference = 1 };

  // Range of a step value over all completions with X's games fixed
  struct Range {
    Ratio low;
    Ratio high;
  };

  // A step's ranges for the teams of a group, or not applicable
  struct StepRanges {
    TeamArray<Range> range{};
    bool applies = true;
  };

  [[nodiscard]] static std::span<const Step> steps(Procedure procedure);
  [[nodiscard]] StepRanges ranges(Step step, TeamMask group,
                                  Procedure procedure) const;
  [[nodiscard]] Range pct_range(TeamId t, TeamMask opponents) const;
  [[nodiscard]] Range sweep_range(TeamId t, TeamMask group) const;
  [[nodiscard]] Range sos_range(TeamId t, TeamMask group) const;
  [[nodiscard]] Range sov_range(TeamId t, TeamMask group) const;
  [[nodiscard]] int32_t low_in(TeamId t, TeamMask group) const;
  [[nodiscard]] int32_t high_in(TeamId t, TeamMask group) const;
  [[nodiscard]] TeamMask division_of(TeamId t) const;
  [[nodiscard]] TeamMask conference_of(TeamId t) const;

  // X wins every division tie with teams of `teams`, so X survives every
  // division reduction of the conference ranking
  [[nodiscard]] bool wins_division_ties(TeamMask teams);

  // X is picked first (wins) or not (loses) from `group` by one pick of a
  // procedure. Proven by induction over the smaller groups a step can leave.
  [[nodiscard]] bool wins(TeamMask group, Procedure procedure);
  [[nodiscard]] bool loses(TeamMask group, Procedure procedure);

  // Calls f(r) for every nonempty set r of at most one team per division of
  // `teams` outside X's division, until f returns false. True if it never
  // did.
  template <typename F>
  [[nodiscard]] bool all_representatives(TeamMask teams, F f) const;

  const League* league_;
  TeamId x_;
  TeamMask division_ = 0;  // X's division, with X
  TeamMask conference_ = 0;
  // Games of t against u, and what's known of them: played, or X's games
  // (fixed by the check). Open games are the other remaining ones.
  TeamMatrix<int32_t> games_{};
  TeamMatrix<int32_t> known_games_{};
  TeamMatrix<int32_t> known_half_wins_{};
  TeamMatrix<int32_t> known_wins_{};
  TeamMatrix<int32_t> open_{};
  TeamArray<int32_t> final_games_{};
  TeamArray<int32_t> open_games_{};
  TeamArray<int32_t> half_wins_low_{};
  TeamArray<int32_t> half_wins_high_{};
  // Half-wins at X's win percentage, kMissing if a team can't get there
  TeamArray<int32_t> tie_half_wins_{};
  // Results of wins() and loses() per procedure and group
  std::array<std::array<std::unordered_map<TeamMask, bool>, 2>, 2> cache_{};
};

}  // namespace nflseedr
