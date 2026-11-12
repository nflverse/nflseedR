// Proofs that a team wins (or loses) every tie in win percentage it can end
// up in, through the tiebreaking steps up to SOS, whatever the remaining
// games do. Milestone 4 of dev/playoff-status.md.
#pragma once

#include <array>
#include <cstdint>
#include <optional>
#include <span>
#include <unordered_map>
#include <vector>

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
  // conference sides the conference ranking. With `wild_card`, only ties
  // for a wild card count: teams of other divisions that win their division
  // whenever they are level with X never are in one.
  [[nodiscard]] TieSides division_sides(TeamMask tied);
  [[nodiscard]] TieSides conference_sides(TeamMask tied, bool wild_card);

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
    Step step = Step::kHeadToHead;
    TeamMask group = 0;
    Procedure procedure = Procedure::kDivision;
    // Common opponents of the group (common games step only)
    TeamMask common = 0;
  };

  // One result of the open games of two teams a and b: everyone's half-wins
  // with these results, a's and b's half-wins and wins against each
  // opponent
  struct OwnResults {
    TeamArray<int32_t> half_wins{};
    TeamArray<int32_t> half_wins_a{};
    TeamArray<int32_t> half_wins_b{};
    TeamArray<int32_t> wins_a{};
    TeamArray<int32_t> wins_b{};
  };

  // How a team's step value compares with another's in every completion
  enum class Lead : int8_t { kNone, kNeverBehind, kAhead };

  // Open games between two teams (`count` of them)
  struct OpenGames {
    TeamId first = 0;
    TeamId second = 0;
    int32_t count = 0;
  };

  [[nodiscard]] static std::span<const Step> steps(Procedure procedure);
  [[nodiscard]] StepRanges ranges(Step step, TeamMask group,
                                  Procedure procedure) const;
  [[nodiscard]] Range pct_range(TeamId t, TeamMask opponents,
                                TeamMask group) const;
  [[nodiscard]] Range sweep_range(TeamId t, TeamMask group) const;
  [[nodiscard]] Range sos_range(TeamId t, TeamMask group) const;
  [[nodiscard]] Range sov_range(TeamId t, TeamMask group) const;
  [[nodiscard]] Lead lead(const StepRanges& r, TeamId a, TeamId b) const;
  [[nodiscard]] int64_t linear_strength_difference(
      const TeamArray<int32_t>& weight_a, const TeamArray<int32_t>& weight_b,
      const TeamArray<int32_t>& half_wins, TeamMask group,
      TeamMask settled) const;
  [[nodiscard]] std::optional<int64_t> lowest_difference(const StepRanges& r,
                                                         TeamId a,
                                                         TeamId b) const;
  [[nodiscard]] Ratio value(Step step, TeamId t,
                            const TeamArray<int32_t>& half_wins, TeamMask group,
                            TeamMask common) const;
  // The record steps before a step, with their common opponents. At most
  // the division procedure's six steps through SOS.
  static constexpr size_t kMaxSteps = 6;
  struct EarlierStep {
    Step step = Step::kHeadToHead;
    TeamMask common = 0;
  };
  struct EarlierSteps {
    std::array<EarlierStep, kMaxSteps> steps;
    size_t size = 0;
  };
  [[nodiscard]] EarlierSteps earlier_steps(const StepRanges& r) const;
  [[nodiscard]] bool equal_before(const EarlierSteps& earlier, TeamMask group,
                                  TeamId a, TeamId b,
                                  const OwnResults& own) const;
  [[nodiscard]] TeamMask common_opponents(TeamMask group) const;
  [[nodiscard]] int32_t low_in(TeamId t, TeamMask group) const;
  [[nodiscard]] int32_t high_in(TeamId t, TeamMask group) const;
  [[nodiscard]] TeamMask division_of(TeamId t) const;
  [[nodiscard]] TeamMask conference_of(TeamId t) const;

  // X wins every division tie with teams of `teams`, so X survives every
  // division reduction of the conference ranking
  [[nodiscard]] bool wins_division_ties(TeamMask teams);

  // X survives the division reduction of every wild card pool with teams of
  // `tied` (the division winner isn't in the pool)
  [[nodiscard]] bool survives_wild_card_reduction(TeamMask tied);

  // Wild card: team y of another division (`in_d`, its tied teams) is never
  // picked before X from the sets of representatives `reps`, whichever team
  // of `x_reps` represents X's division
  [[nodiscard]] bool never_picked_before_x(TeamId y, TeamMask in_d,
                                           std::span<const TeamMask> reps,
                                           TeamMask x_reps);

  // Wild card, the mirror: X is never picked before team y of another
  // division, whichever tied team of y's division represents it
  [[nodiscard]] bool x_never_picked_before(TeamId y, TeamMask in_d,
                                           std::span<const TeamMask> reps,
                                           TeamMask tied);

  // Rival z can end with a higher win percentage than X
  [[nodiscard]] bool can_end_above_x(TeamId z) const;
  // Team z ends with a higher win percentage than X in every completion
  [[nodiscard]] bool always_above_x(TeamId z) const;

  // Wild card: teams of `x_ahead` (of `others`, other divisions) that may
  // win their division while level with X, next to a team of `others` from
  // that division that may rank ahead of X. `winners` win their division
  // whenever level with X.
  [[nodiscard]] TeamMask level_division_winners(TeamMask tied, TeamMask winners,
                                                TeamMask x_ahead,
                                                TeamMask others);

  // X is picked first (wins) or not (loses) from `group` by one pick of a
  // procedure. Proven by induction over the smaller groups a step can leave.
  [[nodiscard]] bool wins(TeamMask group, Procedure procedure);
  [[nodiscard]] bool loses(TeamMask group, Procedure procedure);

  // Teams of `group` that one pick of a procedure can pick (a superset)
  [[nodiscard]] TeamMask possible_picks(TeamMask group, Procedure procedure);

  // X ranks ahead of (behind) rival y in the division ranking of `group`,
  // which holds both: the picks go on until one of them is picked
  [[nodiscard]] bool ranks_ahead(TeamMask group, TeamId y);
  [[nodiscard]] bool ranks_behind(TeamMask group, TeamId y);

  // y wins its division whenever it ends with X's win percentage: no other
  // team of its division can end above X, and y wins every division tie
  // with teams of `tied` (and X, if y is a rival)
  [[nodiscard]] bool wins_division_when_level(TeamId y, TeamMask tied);

  // Every nonempty set of at most one team per division of `teams` outside
  // X's division
  [[nodiscard]] std::vector<TeamMask> representative_sets(TeamMask teams) const;

  const League* league_;
  TeamId x_;
  TeamMask division_ = 0;  // X's division, with X
  TeamArray<TeamMask> division_of_{};
  TeamArray<TeamMask> conference_of_{};
  TeamMask conference_ = 0;
  // Games of t against u, and what's known of them: played, or X's games
  // (fixed by the check). Open games are the other remaining ones.
  TeamMatrix<int32_t> games_{};
  TeamMatrix<int32_t> known_games_{};
  TeamMatrix<int32_t> known_half_wins_{};
  TeamMatrix<int32_t> known_wins_{};
  TeamMatrix<int32_t> open_{};
  std::vector<OpenGames> open_pairs_;
  TeamArray<int32_t> final_games_{};
  TeamArray<int32_t> open_games_{};
  TeamArray<int32_t> half_wins_low_{};
  TeamArray<int32_t> half_wins_high_{};
  // Half-wins at X's win percentage, kMissing if a team can't get there
  TeamArray<int32_t> tie_half_wins_{};
  // Results of wins() and loses() per procedure and group
  std::array<std::array<std::unordered_map<TeamMask, bool>, 2>, 2> cache_{};
  // Results of possible_picks() per procedure and group
  std::array<std::unordered_map<TeamMask, TeamMask>, 2> picks_cache_{};
  // Results of ranks_ahead() and ranks_behind() per group and rival
  std::array<std::unordered_map<uint64_t, bool>, 2> rank_cache_{};
};

}  // namespace nflseedr
