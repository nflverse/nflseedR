// Runs compute_clinching_status() on states read from a text file, for
// sanitizer builds of the pure C++ engine.
#include <cstdint>
#include <cstdio>
#include <fstream>
#include <vector>

#include "engine/engine.hpp"
#include "engine/status.hpp"

int main(int argc, char** argv) {
  if (argc < 2) return 2;
  // A third argument turns the self-check off (for timing)
  const bool verify = argc < 3;
  std::ifstream in(argv[1]);
  int n_states = 0;
  in >> n_states;
  nflseedr::League league;
  for (auto& d : league.division) in >> d;
  for (auto& c : league.conference) in >> c;
  uint64_t checksum = 0;
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
