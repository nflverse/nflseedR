// Small max-flow (Dinic) for the clinching checks: remaining games on one
// side, teams on the other. See dev/playoff-status.md.
#pragma once

#include <cstdint>
#include <vector>

namespace nflseedr {

class MaxFlow {
 public:
  // Remove all edges and use the nodes 0..nodes-1. Keeps the memory.
  void reset(int32_t nodes);
  void add_edge(int32_t from, int32_t to, int32_t capacity);
  // Maximum flow from `source` to `sink`. Uses up the capacities, so call
  // reset() before the next problem.
  [[nodiscard]] int32_t run(int32_t source, int32_t sink);

 private:
  struct Edge {
    int32_t to = 0;
    int32_t capacity = 0;
    int32_t next = -1;  // next edge leaving the same node
  };

  [[nodiscard]] bool build_levels(int32_t source, int32_t sink);
  [[nodiscard]] int32_t augment(int32_t node, int32_t sink, int32_t limit);

  // Edges 2k and 2k + 1 are each other's reverse edge
  std::vector<Edge> edges_;
  std::vector<int32_t> first_edge_;
  std::vector<int32_t> level_;
  std::vector<int32_t> next_edge_;
  std::vector<int32_t> queue_;
};

}  // namespace nflseedr
