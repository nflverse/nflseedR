// Division, conference and draft tiebreaking procedures.
#pragma once

#include <array>
#include <cstdint>

#include "season.hpp"
#include "types.hpp"

namespace nflseedr {

// Small ordered set of teams without allocation.
class TeamSet {
 public:
  void push(TeamId t) noexcept { teams_[size_++] = t; }
  void erase(TeamId t) noexcept;
  void clear() noexcept { size_ = 0; }
  [[nodiscard]] int32_t size() const noexcept { return size_; }
  [[nodiscard]] TeamId operator[](int32_t i) const noexcept {
    return teams_[i];
  }
  [[nodiscard]] TeamId* begin() noexcept { return teams_.data(); }
  [[nodiscard]] TeamId* end() noexcept { return teams_.data() + size_; }
  [[nodiscard]] const TeamId* begin() const noexcept { return teams_.data(); }
  [[nodiscard]] const TeamId* end() const noexcept {
    return teams_.data() + size_;
  }

 private:
  std::array<TeamId, kTeams> teams_{};
  int32_t size_ = 0;
};

// Rank, label code (see label_code()) or kMissing for every team
struct RankResult {
  TeamArray<int32_t> rank;
  TeamArray<int32_t> label;
  RankResult() {
    rank.fill(kMissing);
    label.fill(kMissing);
  }
};

// `depth` is the tiebreaker depth: 0 = coin toss only, 1 = before SOV,
// 2 = through SOS, 3 = through points.

// Division ranks of all teams that played.
[[nodiscard]] RankResult rank_divisions(const SeasonData& s,
                                        const League& league, int32_t depth,
                                        RandomSource& rng);

// Conference ranks: division winners 1-4, wild cards 5-16. Blocks of tied
// teams that start below `playoff_seeds` are left unranked.
[[nodiscard]] RankResult rank_conferences(const SeasonData& s,
                                          const League& league,
                                          const TeamArray<int32_t>& div_rank,
                                          int32_t playoff_seeds, int32_t depth,
                                          RandomSource& rng);

// Draft ranks of all teams that played. Worse teams pick first.
[[nodiscard]] RankResult rank_draft(const SeasonData& s, const League& league,
                                    const TeamArray<int32_t>& div_rank,
                                    const TeamArray<int32_t>& conf_rank,
                                    const TeamArray<int32_t>& exit,
                                    int32_t depth, RandomSource& rng);

}  // namespace nflseedr
