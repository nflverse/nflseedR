// Basic types and league constants of the standings engine.
// See dev/cpp-standings-engine.md for the rule spec.
#pragma once

#include <array>
#include <compare>
#include <cstdint>
#include <stdexcept>

namespace nflseedr {

// The league always has 32 teams in 2 conferences and 8 divisions of 4 teams
// (decision D9 in dev/cpp-standings-engine.md).
inline constexpr int32_t kTeams = 32;
inline constexpr int32_t kConferences = 2;
inline constexpr int32_t kDivisions = 8;
inline constexpr int32_t kTeamsPerDivision = kTeams / kDivisions;
inline constexpr int32_t kTeamsPerConference = kTeams / kConferences;

// Team ids are 0..31, assigned in alphabetical order of the abbreviations.
using TeamId = int32_t;

// Missing integer output (turned into NA by bindings.cpp).
inline constexpr int32_t kMissing = -1;

// A broken assumption inside the engine. bindings.cpp turns it into an R
// error.
class EngineError : public std::runtime_error {
 public:
  using std::runtime_error::runtime_error;
};

// Exact ratio of two integers with a non-negative denominator, e.g.
// half-wins / (2 * games). Compared by cross-multiplication, so equal
// percentages are always equal. A ratio with denominator 0 counts as 0
// (e.g. win percentage without games).
class Ratio {
 public:
  constexpr Ratio() noexcept = default;
  constexpr Ratio(int32_t num, int32_t den) noexcept
      : num_(den == 0 ? 0 : num), den_(den == 0 ? 1 : den) {}

  [[nodiscard]] constexpr int32_t num() const noexcept { return num_; }
  [[nodiscard]] constexpr int32_t den() const noexcept { return den_; }
  [[nodiscard]] constexpr Ratio negated() const noexcept {
    return {-num_, den_};
  }

  friend constexpr std::strong_ordering operator<=>(Ratio a, Ratio b) noexcept {
    return static_cast<int64_t>(a.num_) * b.den_ <=>
           static_cast<int64_t>(b.num_) * a.den_;
  }
  friend constexpr bool operator==(Ratio a, Ratio b) noexcept {
    return (a <=> b) == 0;
  }

 private:
  int32_t num_ = 0;
  int32_t den_ = 1;
};

// Value of a team in a comparison, higher is better. Records (overall,
// division, conference) compare by percentage and, if equal, by net wins
// (wins - losses), like the NFL's standings while teams have played a different
// number of games (D13). All other values only use `pct`.
struct Value {
  Ratio pct;
  int32_t net = 0;

  constexpr Value() noexcept = default;
  constexpr Value(Ratio pct, int32_t net = 0) noexcept : pct(pct), net(net) {}

  [[nodiscard]] constexpr Value negated() const noexcept {
    return {pct.negated(), -net};
  }
  friend constexpr auto operator<=>(const Value&, const Value&) = default;
};

// Tiebreaking steps. The R side turns them into labels like "SOV (2)", see
// R/standings_cpp.R. Some steps have different names per procedure.
enum class Step : int32_t {
  kHeadToHead = 1,       // division: win percentage among the tied teams
  kHeadToHeadSweep = 2,  // conference and draft
  kDivisionPct = 3,
  kCommonPct = 4,
  kConferencePct = 5,
  kSov = 6,
  kSos = 7,
  kConferencePoints = 8,
  kLeaguePoints = 9,
  kCommonPd = 10,
  kConferencePd = 11,
  kPd = 12,
  kCoinToss = 13,
  kDivisionTiebreaker = 14,
  kConferenceTiebreaker = 15,
};

// Label code: step * 10 + number of tied teams (0 if not part of the label).
// A step never has more than 9 candidates: at most 4 teams of a division, the
// best team of each of the 4 divisions of a conference, or one team per
// conference.
static_assert(kTeamsPerDivision < 10 && kDivisions / kConferences < 10 &&
              kConferences < 10);
[[nodiscard]] constexpr int32_t label_code(Step step, int32_t n) noexcept {
  return static_cast<int32_t>(step) * 10 + n;
}

// Source of random numbers for coin tosses. bindings.cpp implements it with
// R's RNG.
class RandomSource {
 public:
  virtual ~RandomSource() = default;
  // Uniform integer in [0, n)
  virtual int32_t index(int32_t n) = 0;
};

// Division and conference of every team. Division ids 0..7, conference ids
// 0..1, both in alphabetical order.
struct League {
  std::array<int32_t, kTeams> division{};
  std::array<int32_t, kTeams> conference{};
};

// Game types. Exit codes use the same numbers plus kSuperBowlWin.
enum class GameType : int32_t {
  kRegular = 0,
  kWildCard = 1,
  kDivisional = 2,
  kConference = 3,
  kSuperBowl = 4,
  kSuperBowlWin = 5,
};

}  // namespace nflseedr
