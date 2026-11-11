#include "engine/flow.hpp"

#include <algorithm>
#include <limits>

namespace nflseedr {

namespace {

// Edges are added in pairs, so the reverse of edge 2k is 2k + 1 and vice versa
[[nodiscard]] constexpr int32_t reverse(int32_t edge) noexcept {
  return edge % 2 == 0 ? edge + 1 : edge - 1;
}

}  // namespace

void MaxFlow::reset(int32_t nodes) {
  edges_.clear();
  first_edge_.assign(nodes, -1);
  level_.assign(nodes, -1);
  next_edge_.assign(nodes, -1);
}

void MaxFlow::add_edge(int32_t from, int32_t to, int32_t capacity) {
  const auto index = static_cast<int32_t>(edges_.size());
  edges_.push_back({.to = to, .capacity = capacity, .next = first_edge_[from]});
  first_edge_[from] = index;
  edges_.push_back({.to = from, .capacity = 0, .next = first_edge_[to]});
  first_edge_[to] = index + 1;
}

// Breadth-first search for the distances from the source in the residual
// graph. False if the sink can't be reached.
bool MaxFlow::build_levels(int32_t source, int32_t sink) {
  std::ranges::fill(level_, -1);
  queue_.clear();
  level_[source] = 0;
  queue_.push_back(source);
  for (size_t i = 0; i < queue_.size(); ++i) {
    const int32_t node = queue_[i];
    for (int32_t e = first_edge_[node]; e != -1; e = edges_[e].next) {
      const Edge& edge = edges_[e];
      if (edge.capacity > 0 && level_[edge.to] < 0) {
        level_[edge.to] = level_[node] + 1;
        queue_.push_back(edge.to);
      }
    }
  }
  return level_[sink] >= 0;
}

// One augmenting path along increasing levels. next_edge_ skips edges that
// are known to be blocked.
int32_t MaxFlow::augment(int32_t node, int32_t sink, int32_t limit) {
  if (node == sink) return limit;
  for (int32_t& e = next_edge_[node]; e != -1; e = edges_[e].next) {
    const int32_t to = edges_[e].to;
    const int32_t capacity = edges_[e].capacity;
    if (capacity <= 0 || level_[to] != level_[node] + 1) continue;
    const int32_t pushed = augment(to, sink, std::min(limit, capacity));
    if (pushed > 0) {
      edges_[e].capacity -= pushed;
      edges_[reverse(e)].capacity += pushed;
      return pushed;
    }
  }
  return 0;
}

int32_t MaxFlow::run(int32_t source, int32_t sink) {
  int32_t total = 0;
  while (build_levels(source, sink)) {
    next_edge_ = first_edge_;
    while (const int32_t pushed =
               augment(source, sink, std::numeric_limits<int32_t>::max())) {
      total += pushed;
    }
  }
  return total;
}

}  // namespace nflseedr
