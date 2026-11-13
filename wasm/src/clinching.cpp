// WebAssembly entry point of the clinching engine: a C interface around
// nflseedr::compute_clinching_status() and nflseedr::compute_standings() for
// wasm/js/nflseedr-clinching.js. It plays the part of clinching_status_cpp()
// and standings_cpp() in src/bindings.cpp. Only wasm/build.sh compiles this
// file; R never sees it.
#include <emscripten/emscripten.h>

#include <algorithm>
#include <cstdint>
#include <exception>
#include <memory>
#include <random>
#include <span>
#include <string>
#include <vector>

#include "engine/engine.hpp"
#include "engine/status.hpp"

namespace {

// Coin tosses of the standings. R uses its own RNG (RRandom in
// src/bindings.cpp), so the same seed doesn't give the same coin tosses as R.
class SeededRandom final : public nflseedr::RandomSource {
 public:
  explicit SeededRandom(uint32_t seed) : gen_(seed) {}
  int32_t index(int32_t n) override {
    return std::uniform_int_distribution<int32_t>(0, n - 1)(gen_);
  }
  double normal() override { return std::normal_distribution<>()(gen_); }

 private:
  std::mt19937 gen_;
};

// Output of one call: the status rows and the standings of the played games,
// or the message of the exception that ended it
struct Result {
  nflseedr::StatusRows rows;
  nflseedr::StandingsRows standings;
  std::string error;
};

// Columns of a Result, numbered as in COLUMNS of nflseedr-clinching.js
const std::vector<int32_t>* column(const Result& r, int32_t id) {
  const nflseedr::StatusRows& rows = r.rows;
  const nflseedr::ScenarioRows& sc = rows.scenarios;
  const nflseedr::StandingsRows& st = r.standings;
  switch (id) {
    case 0:
      return &rows.sim;
    case 1:
      return &rows.team;
    case 2:
      return &rows.wins;
    case 3:
      return &rows.losses;
    case 4:
      return &rows.ties;
    case 5:
      return &rows.games_left;
    case 6:
      return &rows.status.at(0);
    case 7:
      return &rows.status.at(1);
    case 8:
      return &rows.status.at(2);
    case 9:
      return &sc.sim;
    case 10:
      return &sc.team;
    case 11:
      return &sc.goal;
    case 12:
      return &sc.proves;
    case 13:
      return &sc.coin_toss;
    case 14:
      return &sc.result;
    case 15:
      return &st.sim;
    case 16:
      return &st.team;
    case 17:
      return &st.div_rank;
    case 18:
      return &st.div_label;
    case 19:
      return &st.conf_rank;
    case 20:
      return &st.conf_label;
    default:
      return nullptr;
  }
}

// `ints` holds four columns of `n` values (season id, game type, away team,
// home team), `result` one. Like make_games() in src/bindings.cpp.
nflseedr::GamesInput make_games(const int32_t* ints, const double* result,
                                int32_t n, int32_t n_sims) {
  const auto size = static_cast<size_t>(n);
  const std::span<const int32_t> all(ints, 4 * size);
  return {all.subspan(0, size),
          all.subspan(size, size),
          all.subspan(2 * size, size),
          all.subspan(3 * size, size),
          std::span(result, size),
          {},
          {},
          false,
          n_sims};
}

// Standings of the played games as nfl_clinching_status() adds them
// (clinching_status_standings() in R/clinching_status.R): conference ranks,
// tiebreakers through SOS, no playoff seed cut-off, and a season is complete
// when it has no games left.
nflseedr::StandingsRows current_standings(const nflseedr::GamesInput& played,
                                          const nflseedr::League& league,
                                          const nflseedr::StatusRows& status,
                                          uint32_t seed) {
  std::vector<int32_t> complete(static_cast<size_t>(played.n_sims), 1);
  for (size_t i = 0; i < status.sim.size(); ++i) {
    if (status.games_left[i] > 0) {
      complete.at(static_cast<size_t>(status.sim[i] - 1)) = 0;
    }
  }
  nflseedr::StandingsOptions options;
  options.ranks = nflseedr::Ranks::kConference;
  options.playoff_seeds = nflseedr::kTeamsPerConference;
  options.depth = 2;  // "SOS"
  options.season_complete = complete;
  SeededRandom rng(seed);
  return nflseedr::compute_standings(played, league, options, rng, [] {});
}

}  // namespace

extern "C" {

// Clinching status of every team in every season (ids 1..n_sims) and the
// standings of the played games. `league` holds the division ids of the 32
// teams followed by their conference ids, `seeds` one value per season, bit
// g of `goals` requests Goal g, and `seed` seeds the coin tosses of the
// standings. Never throws: the returned handle holds the rows or an error
// message. Free it with nflseedr_free().
EMSCRIPTEN_KEEPALIVE Result* nflseedr_clinching_status(
    const int32_t* played_ints, const double* played_result, int32_t n_played,
    const int32_t* remaining_ints, const double* remaining_result,
    int32_t n_remaining, int32_t n_sims, const int32_t* league,
    const int32_t* seeds, uint32_t goals, int32_t scenarios, int32_t search,
    uint32_t seed) {
  auto out = std::make_unique<Result>();
  try {
    nflseedr::League teams;
    const std::span<const int32_t> ids(
        league, 2 * static_cast<size_t>(nflseedr::kTeams));
    std::ranges::copy(ids.first(nflseedr::kTeams), teams.division.begin());
    std::ranges::copy(ids.last(nflseedr::kTeams), teams.conference.begin());

    nflseedr::StatusOptions options;
    for (size_t g = 0; g < options.goals.size(); ++g) {
      options.goals.at(g) = ((goals >> g) & 1U) == 1U;
    }
    options.scenarios = scenarios != 0;
    options.search = search != 0;
    options.seeds = std::span(seeds, static_cast<size_t>(n_sims));

    const nflseedr::GamesInput played =
        make_games(played_ints, played_result, n_played, n_sims);
    out->rows = nflseedr::compute_clinching_status(
        played,
        make_games(remaining_ints, remaining_result, n_remaining, n_sims),
        teams, options, [] {});
    out->standings = current_standings(played, teams, out->rows, seed);
  } catch (const std::exception& e) {
    out->error = e.what();
  } catch (...) {
    out->error = "Unknown error in the clinching engine";
  }
  return out.release();
}

// Error message, or an empty string if the call succeeded
EMSCRIPTEN_KEEPALIVE const char* nflseedr_error(const Result* r) {
  return r->error.c_str();
}

// Number of values of a column and a pointer to them
EMSCRIPTEN_KEEPALIVE int32_t nflseedr_column_size(const Result* r, int32_t id) {
  const std::vector<int32_t>* col = column(*r, id);
  return col == nullptr ? -1 : static_cast<int32_t>(col->size());
}

EMSCRIPTEN_KEEPALIVE const int32_t* nflseedr_column_data(const Result* r,
                                                         int32_t id) {
  const std::vector<int32_t>* col = column(*r, id);
  return col == nullptr ? nullptr : col->data();
}

EMSCRIPTEN_KEEPALIVE void nflseedr_free(Result* r) {
  const std::unique_ptr<Result> owned(r);
}

}  // extern "C"
