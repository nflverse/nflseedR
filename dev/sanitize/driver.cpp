// Runs compute_clinching_status() on states read from a text file, for
// sanitizer builds of the pure C++ engine.
#include <cstdint>
#include <cstdio>
#include <fstream>
#include <vector>

#include "engine/engine.hpp"
#include "engine/status.hpp"

namespace {

// Random numbers for the group check: a small generator is enough here
class DriverRandom final : public nflseedr::RandomSource {
 public:
  int32_t index(int32_t n) override {
    return static_cast<int32_t>(next() % static_cast<uint64_t>(n));
  }
  // Sum of 12 uniform numbers, close enough to a normal draw
  double normal() override {
    double sum = -6.0;
    for (int i = 0; i < 12; ++i)
      sum += static_cast<double>(index(1 << 20)) / (1 << 20);
    return sum;
  }

 private:
  uint64_t next() {
    state_ = state_ * 6364136223846793005ULL + 1442695040888963407ULL;
    return state_ >> 33;
  }
  uint64_t state_ = 20261113;
};

}  // namespace

int main(int argc, char** argv) {
  if (argc < 2) return 2;
  // A third argument turns the self-check and the group check off (for
  // timing)
  const bool verify = argc < 3;
  std::ifstream in(argv[1]);
  int n_states = 0;
  in >> n_states;
  nflseedr::League league;
  for (auto& d : league.division) in >> d;
  for (auto& c : league.conference) in >> c;
  uint64_t checksum = 0;
  DriverRandom rng;
  for (int s = 0; s < n_states; ++s) {
    int seeds = 0, n = 0;
    in >> seeds >> n;
    std::vector<int32_t> psim, ptype, paway, phome, rsim, rtype, raway, rhome;
    std::vector<double> pres, rres;
    for (int i = 0; i < n; ++i) {
      int a = 0, h = 0, r = 0;
      in >> a >> h >> r;
      if (r == 9) {
        rsim.push_back(1);
        rtype.push_back(0);
        raway.push_back(a);
        rhome.push_back(h);
        rres.push_back(0);
      } else {
        psim.push_back(1);
        ptype.push_back(0);
        paway.push_back(a);
        phome.push_back(h);
        pres.push_back(r);
      }
    }
    const nflseedr::GamesInput played{psim, ptype, paway, phome, pres,
                                      {},   {},    false, 1};
    const nflseedr::GamesInput remaining{rsim, rtype, raway, rhome, rres,
                                         {},   {},    false, 1};
    const std::vector<int32_t> seed_vec{seeds};
    for (int mode = 0; mode < 2; ++mode) {
      nflseedr::StatusOptions options;
      options.goals = {true, true, true};
      options.scenarios = true;
      options.search = mode == 1;
      options.verify = verify;
      // The group check too, with few completions per tie group. Only with
      // the search: it takes most of the time.
      options.group_samples = verify && mode == 1 ? 2 : 0;
      options.rng = &rng;
      options.seeds = seed_vec;
      const nflseedr::StatusRows rows = nflseedr::compute_clinching_status(
          played, remaining, league, options, [] {});
      for (const auto& st : rows.status) {
        for (const int32_t v : st)
          checksum = checksum * 31 + static_cast<uint64_t>(v);
      }
      checksum += rows.scenarios.result.size();
    }
  }
  std::printf("states %d checksum %llu\n", n_states,
              static_cast<unsigned long long>(checksum));
  return 0;
}
