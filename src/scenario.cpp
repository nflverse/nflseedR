#include "engine/scenario.hpp"

namespace nflseedr {

namespace {

// Tiebreakers through SOS (P2)
constexpr int32_t kDepthSos = 2;

// Coin tosses with given results. Calls past the given ones pick the first
// candidate. Records every call, so the caller can try the other results.
class ScriptedTosses final : public RandomSource {
 public:
  explicit ScriptedTosses(std::span<const int32_t> forced) : forced_(forced) {}

  int32_t index(int32_t n) override {
    const size_t i = choices_.size();
    const int32_t choice = i < forced_.size() ? forced_[i] : 0;
    if (choice < 0 || choice >= n) {
      throw EngineError("Coin toss sequence doesn't fit the procedures");
    }
    choices_.push_back(choice);
    candidates_.push_back(n);
    return choice;
  }
  double normal() override {
    throw EngineError("The standings procedures don't draw normal numbers");
  }

  [[nodiscard]] const std::vector<int32_t>& choices() const noexcept {
    return choices_;
  }
  [[nodiscard]] const std::vector<int32_t>& candidates() const noexcept {
    return candidates_;
  }

 private:
  std::span<const int32_t> forced_;
  std::vector<int32_t> choices_;
  std::vector<int32_t> candidates_;
};

}  // namespace

ScenarioRunner::ScenarioRunner(std::span<const Game> played,
                               std::span<const Game> remaining,
                               const League& league, int32_t seeds)
    : n_played_(played.size()), league_(&league), seeds_(seeds) {
  games_.reserve(played.size() + remaining.size());
  games_.insert(games_.end(), played.begin(), played.end());
  games_.insert(games_.end(), remaining.begin(), remaining.end());
}

SearchEnd ScenarioRunner::run(const Completion& completion, int32_t budget,
                              const OutcomeVisitor& visit) {
  if (completion.size() != games_.size() - n_played_) {
    throw EngineError("A completion needs one result per remaining game");
  }
  for (size_t i = 0; i < completion.size(); ++i) {
    games_[n_played_ + i].result = completion[i] - 1;
  }
  data_.build(games_, *league_, false, true);

  // Depth-first search over coin toss sequences. Each entry is a sequence
  // of forced results; later tosses pick the first candidate.
  std::vector<std::vector<int32_t>> todo{{}};
  int32_t runs = 0;
  while (!todo.empty()) {
    if (runs++ == budget) return SearchEnd::kBudget;
    const std::vector<int32_t> forced = std::move(todo.back());
    todo.pop_back();
    ScriptedTosses tosses(forced);
    const RankResult div = rank_divisions(data_, *league_, kDepthSos, tosses);
    const RankResult conf =
        rank_conferences(data_, *league_, div.rank, seeds_, kDepthSos, tosses);
    if (visit(div, conf, !tosses.choices().empty())) {
      return SearchEnd::kStopped;
    }
    // Every toss after the forced ones could have gone another way
    const std::vector<int32_t>& choices = tosses.choices();
    for (size_t i = forced.size(); i < choices.size(); ++i) {
      for (int32_t other = 1; other < tosses.candidates()[i]; ++other) {
        std::vector<int32_t> next(choices.begin(),
                                  choices.begin() + static_cast<int64_t>(i));
        next.push_back(other);
        todo.push_back(std::move(next));
      }
    }
  }
  return SearchEnd::kExhausted;
}

}  // namespace nflseedr
