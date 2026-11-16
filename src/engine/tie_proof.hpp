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

#include "flow.hpp"
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

// Whether X gets a playoff berth when the teams above X and level with X
// are known (P11)
enum class TieOutcome : int8_t {
  kReached,     // in every completion
  kMissed,      // in every completion
  kEither,      // not proven
  kImpossible,  // the level teams can't all end level with X
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

  // Playoff berth of X in the completions where exactly the teams of `above`
  // (of X's conference) end with a higher win percentage than X and exactly
  // the teams of `level` with the same. Follows the procedures: the
  // divisions rank their level teams, a division without a team above X
  // gives its first one the title, and the others go through the wild card
  // picks (one team per division at a time) until X is picked or the wild
  // cards the teams above X leave are gone. Every pick is a superset
  // (possible_picks()), so kReached and kMissed are proven.
  [[nodiscard]] TieOutcome playoff_outcome(TeamMask above, TeamMask level,
                                           int32_t wild_cards);
  // The same for the division title: no rival above X, and X is the first
  // of the level rivals
  [[nodiscard]] TieOutcome division_outcome(TeamMask above, TeamMask level);
  // And for the No. 1 seed: no team above X, X wins its division and is
  // picked first from the level division winners
  [[nodiscard]] TieOutcome first_seed_outcome(TeamMask above, TeamMask level);

  // What the outcomes above may use about the other teams: the teams of
  // `above` end above X, the teams of `level` level with X and the teams of
  // `below` below X (the tie groups, P11). Without a context, only the teams
  // of a tie group are known to be level.
  void set_context(TeamMask above, TeamMask level, TeamMask below);
  // Whether a group that a step reduced, and the pick among the division
  // winners, take along what the earlier steps and picks said (P15). Only
  // with a context. Stronger, but much slower: nothing of it is cached.
  // With `jointly`, a pick also needs all records on the way to it to hold
  // at once (P17), which takes a flow per pick.
  void carry_facts(bool on, bool jointly = false);

 private:
  enum class Procedure : int8_t { kDivision = 0, kConference = 1 };

  // Range of a step value over all completions with X's games fixed
  struct Range {
    Ratio low;
    Ratio high;
  };

  // Half-wins a team takes from its open games with each opponent, at least
  // and at most (P15). Not feasible: no completion fits what is known.
  struct PairBounds {
    TeamMatrix<int8_t> low{};
    TeamMatrix<int8_t> high{};
    bool feasible = true;
  };

  // Team t takes between `low` and `high` half-wins from its open games
  // against `opponents`: what a step says of a record when it keeps or drops
  // teams of a group (P15)
  struct Fact {
    TeamId team = 0;
    TeamMask opponents = 0;
    int32_t low = 0;
    int32_t high = 0;
  };
  using Facts = std::vector<Fact>;

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

  // A step's ranges for the teams of a group, or not applicable
  struct StepRanges {
    TeamArray<Range> range{};
    bool applies = true;
    Step step = Step::kHeadToHead;
    TeamMask group = 0;
    Procedure procedure = Procedure::kDivision;
    // Common opponents of the group (common games step only)
    TeamMask common = 0;
    // The record steps the group passed level before this one
    EarlierSteps earlier;
    // What the context and the steps the group passed leave of every pair's
    // open games (only with a context, see Passage)
    std::optional<PairBounds> bounds;
    // The records of the group's teams at the earlier steps, all level
    Facts passed;
    [[nodiscard]] const PairBounds* pair_bounds() const noexcept {
      return bounds ? &*bounds : nullptr;
    }
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
  // A group on its way through the steps of one pick, see ranges()
  struct Passage;
  // The steps of a pick in turn, with one `passage` per group and pick.
  // `facts`: what the steps of larger groups said before they left `group`.
  [[nodiscard]] StepRanges ranges(Step step, TeamMask group,
                                  Procedure procedure, Passage& passage,
                                  std::span<const Fact> facts = {}) const;
  // Half-wins of tied team t in its games against `opponents`, lowest and
  // highest. `known` of them come from games that aren't open.
  struct HalfWins {
    int32_t games = 0;
    int32_t known = 0;
    int32_t low = 0;
    int32_t high = 0;
  };
  // With `bounds`, from the pair bounds instead of the group
  [[nodiscard]] HalfWins half_wins_range(TeamId t, TeamMask opponents,
                                         TeamMask group,
                                         const PairBounds* bounds) const;
  [[nodiscard]] HalfWins bounded_half_wins(const PairBounds& bounds, TeamId t,
                                           TeamMask opponents) const;
  [[nodiscard]] Range pct_range(TeamId t, TeamMask opponents, TeamMask group,
                                const PairBounds* bounds) const;
  [[nodiscard]] bool beaten_head_to_head(TeamId t, TeamMask group,
                                         const PairBounds* bounds) const;
  [[nodiscard]] Range sweep_range(TeamId t, TeamMask group,
                                  const PairBounds* bounds) const;

  // Propagation (P15), only with a context and for groups of level teams.
  // Half-wins team t takes from its open games, at least and at most
  [[nodiscard]] bool propagates(TeamMask group) const;
  [[nodiscard]] int32_t needed_low(TeamId t) const;
  [[nodiscard]] int32_t needed_high(TeamId t) const;
  // t takes between `low` and `high` half-wins from its open games against
  // `opponents`: tightens the bounds of these pairs. Returns the teams with
  // a changed bound.
  [[nodiscard]] TeamMask tighten(PairBounds& bounds, TeamId t,
                                 TeamMask opponents, int32_t low,
                                 int32_t high) const;
  // Tightens the bounds with the totals of the teams of `dirty`, and of
  // every team a bound changes for, until nothing changes. Returns the teams
  // with a changed bound.
  [[nodiscard]] TeamMask propagate(PairBounds& bounds, TeamMask dirty) const;
  // Tightens the pair bounds of a passage with the facts of the teams of
  // `touched` and, if one of them is in `group`, with the records of the
  // steps the group passed level, until nothing changes
  void settle(Passage& passage, TeamMask group, std::span<const Fact> facts,
              TeamMask touched) const;
  // Whether the open games can hand out their half-wins so that every team
  // gets a total the context allows, every pair what `bounds` allow and
  // every record of `facts` and `passed` holds, all at once (P17). The pair
  // bounds only look at one team at a time.
  [[nodiscard]] bool jointly_feasible(const PairBounds& bounds,
                                      std::span<const Fact> facts,
                                      std::span<const Fact> passed) const;
  // The same with a given choice among records that can't all be in one flow
  struct RecordBound;
  [[nodiscard]] bool records_feasible(const PairBounds& bounds,
                                      std::span<RecordBound> records,
                                      bool crossed_first) const;
  // Step r.step leaves `survivors` of r.group: adds what that says of the
  // group's records to `facts`. False if it can't happen.
  [[nodiscard]] bool reduction_facts(const StepRanges& r, TeamMask survivors,
                                     Facts& facts) const;
  // The same, one case per record the survivors can share at a record step
  // (with the joint check, P18): ranges per team can't say that the
  // survivors are level with each other and the dropped teams below them.
  // No case: it can't happen.
  [[nodiscard]] std::vector<Facts> reduction_cases(const StepRanges& r,
                                                   TeamMask survivors,
                                                   const Facts& facts) const;
  // Narrows the half-wins of `teams` at a record step to those that give
  // all of them the same percentage. False if there are none.
  [[nodiscard]] static bool equal_records(TeamMask teams,
                                          TeamArray<HalfWins>& records);
  [[nodiscard]] static bool at_half(TeamMask teams,
                                    TeamArray<HalfWins>& records);
  // The opponents a record step counts for team t
  [[nodiscard]] TeamMask step_opponents(Step step, TeamId t, TeamMask group,
                                        TeamMask common) const;
  // Narrows the half-wins of the teams of a group at a record step to those
  // that leave the whole group level there. False if there are none.
  [[nodiscard]] bool level_records(Step step, TeamMask group,
                                   TeamArray<HalfWins>& records) const;
  [[nodiscard]] Range sos_range(TeamId t, TeamMask group) const;
  [[nodiscard]] Range sov_range(TeamId t, TeamMask group) const;
  // How a compares with b, and b with a
  struct Leads {
    Lead a = Lead::kNone;
    Lead b = Lead::kNone;
  };
  [[nodiscard]] Leads leads(const StepRanges& r, TeamId a, TeamId b) const;
  [[nodiscard]] int64_t linear_strength_difference(
      const TeamArray<int32_t>& weight_a, const TeamArray<int32_t>& weight_b,
      const TeamArray<int32_t>& half_wins, TeamMask group, TeamMask settled,
      const PairBounds* bounds) const;
  // The lowest value of a's step value minus b's, and of b's minus a's
  struct Differences {
    std::optional<int64_t> a_minus_b;
    std::optional<int64_t> b_minus_a;
  };
  [[nodiscard]] Differences lowest_differences(const StepRanges& r, TeamId a,
                                               TeamId b) const;
  [[nodiscard]] Ratio value(Step step, TeamId t,
                            const TeamArray<int32_t>& half_wins, TeamMask group,
                            TeamMask common) const;
  [[nodiscard]] bool equal_before(const EarlierSteps& earlier, TeamMask group,
                                  TeamId a, TeamId b,
                                  const OwnResults& own) const;
  [[nodiscard]] TeamMask common_opponents(TeamMask group) const;
  // t has a game (played or not) against every team of `teams`
  [[nodiscard]] bool plays_all(TeamId t, TeamMask teams) const noexcept;
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

  // X is picked first (wins) or never (loses) from `group` by one pick of a
  // procedure: what possible_picks() says about X
  [[nodiscard]] bool wins(TeamMask group, Procedure procedure);
  [[nodiscard]] bool loses(TeamMask group, Procedure procedure);

  // Teams of `group` that one pick of a procedure can pick (a superset)
  [[nodiscard]] TeamMask possible_picks(TeamMask group, Procedure procedure);
  // A team one pick can pick, and what the steps on the way to it said
  struct PickPath {
    TeamId pick = 0;
    Facts facts;
  };
  // The same for a group that the steps of larger groups left, with what
  // they said (`facts`). With `paths`, also every way to a pick.
  [[nodiscard]] TeamMask possible_picks(TeamMask group, Procedure procedure,
                                        const Facts& facts,
                                        std::vector<PickPath>* paths = nullptr);

  // X ranks ahead of (behind) rival y in the division ranking of `group`,
  // which holds both: the picks go on until one of them is picked
  [[nodiscard]] bool ranks_ahead(TeamMask group, TeamId y);
  [[nodiscard]] bool ranks_behind(TeamMask group, TeamId y);

  // y wins its division whenever it ends with X's win percentage: no other
  // team of its division can end above X, and y wins every division tie
  // with teams of `tied` (and X, if y is a rival)
  [[nodiscard]] bool wins_division_when_level(TeamId y, TeamMask tied);

  // What X can get in playoff_outcome()
  struct Berth {
    bool reached = false;
    bool missed = false;
  };
  // The division titles of the level teams, one division of `divisions`
  // (a team of each) at a time, then the wild card picks from `pool`
  void division_titles(TeamMask divisions, TeamMask pool, int32_t slots,
                       Berth& out);
  // Wild card picks from the level teams left (`pool`, with X) while
  // `slots` wild cards are left
  [[nodiscard]] Berth wild_card_picks(TeamMask pool, int32_t slots);
  // The No. 1 seed from the level division winners: `winners` so far, then
  // one of each division of `divisions` (a team of each), `level` its teams
  void first_seed_picks(TeamMask divisions, TeamMask level, TeamMask winners,
                        Berth& out);
  // The same with what every division's pick says about its winner (P15).
  // False if there are too many ways to the division winners.
  [[nodiscard]] bool first_seed_paths(TeamMask divisions, TeamMask level,
                                      Berth& out);
  [[nodiscard]] static TieOutcome outcome_of(const Berth& berth);

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
  // Opponents of every team in all of its games, and in its open games
  TeamArray<TeamMask> opponents_{};
  TeamArray<TeamMask> open_opponents_{};
  std::vector<OpenGames> open_pairs_;
  TeamArray<int32_t> final_games_{};
  TeamArray<int32_t> open_games_{};
  TeamArray<int32_t> half_wins_low_{};
  TeamArray<int32_t> half_wins_high_{};
  // Half-wins at X's win percentage, kMissing if a team can't get there
  TeamArray<int32_t> tie_half_wins_{};
  // Half-wins of every team within the context (set_context()), otherwise
  // half_wins_low_ and half_wins_high_
  TeamArray<int32_t> context_low_{};
  TeamArray<int32_t> context_high_{};
  bool has_context_ = false;
  bool carry_facts_ = false;
  bool joint_facts_ = false;
  TeamMask context_above_ = 0;
  TeamMask context_level_ = 0;
  TeamMask context_below_ = 0;
  // The pair bounds the context leaves, at their fixed point
  PairBounds context_bounds_;
  // The flow of jointly_feasible(), kept for its memory
  mutable MaxFlow flow_;
  void clear_caches();
  // Results of possible_picks() per procedure and group
  std::array<std::unordered_map<TeamMask, TeamMask>, 2> picks_cache_{};
  // Results of ranks_ahead() and ranks_behind() per group and rival
  std::array<std::unordered_map<uint64_t, bool>, 2> rank_cache_{};
  // Results of wild_card_picks() per pool and slots
  std::unordered_map<uint64_t, Berth> berth_cache_;
};

}  // namespace nflseedr
