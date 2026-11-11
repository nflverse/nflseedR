// Scenarios for the clinching checks: a completion of a season run through
// the division and conference procedures, for every sequence of coin toss
// results. See dev/playoff-status.md.
#pragma once

#include <cstdint>
#include <functional>
#include <span>
#include <vector>

#include "procedures.hpp"
#include "season.hpp"
#include "types.hpp"

namespace nflseedr {

// Results of the remaining games of a season, in their order: half-wins of
// the home team (2 win, 1 tie, 0 loss).
using Completion = std::vector<int8_t>;

// Called with the division and conference ranks of every outcome, and
// whether a coin toss was needed. Returns true to stop.
using OutcomeVisitor = std::function<bool(
    const RankResult& division, const RankResult& conference, bool coin_toss)>;

enum class SearchEnd : int8_t {
  kStopped,    // the visitor returned true
  kExhausted,  // all coin toss outcomes were visited
  kBudget,     // ran out of budget
};

class ScenarioRunner {
 public:
  // Conference ranks are only computed through `seeds` (blocks of tied teams
  // below it are left unranked), so ties outside the playoffs need no
  // coin tosses.
  ScenarioRunner(std::span<const Game> played, std::span<const Game> remaining,
                 const League& league, int32_t seeds);

  // Visits the final ranks of `completion` for every sequence of coin toss
  // results, at most `budget` runs of the procedures. Tiebreakers through
  // SOS, everything after them is a coin toss (P2).
  SearchEnd run(const Completion& completion, int32_t budget,
                const OutcomeVisitor& visit);

 private:
  std::vector<Game> games_;  // played, then remaining
  size_t n_played_ = 0;
  const League* league_;
  int32_t seeds_ = 0;
  SeasonData data_;
};

}  // namespace nflseedr
