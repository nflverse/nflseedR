// R interface of the standings engine. This is the only engine file that
// includes R or cpp11 headers.
#include <R_ext/Random.h>
#include <Rmath.h>

#include <algorithm>
#include <cpp11.hpp>
#include <span>
#include <vector>

#include "engine/elo.hpp"
#include "engine/engine.hpp"

namespace {

using nflseedr::kMissing;
using nflseedr::kTeams;

// Coin tosses with R's RNG, so set.seed() makes results reproducible
class RRandom final : public nflseedr::RandomSource {
 public:
  RRandom() { cpp11::safe[GetRNGstate](); }
  ~RRandom() override { PutRNGstate(); }
  int32_t index(int32_t n) override {
    return static_cast<int32_t>(R_unif_index(static_cast<double>(n)));
  }
  double normal() override { return norm_rand(); }
};

std::span<const int32_t> int_span(SEXP x) {
  if (TYPEOF(x) != INTSXP) {
    throw nflseedr::EngineError("Expected an integer vector");
  }
  return {INTEGER_RO(x), static_cast<size_t>(Rf_xlength(x))};
}

std::span<const double> dbl_span(SEXP x) {
  if (TYPEOF(x) != REALSXP) {
    throw nflseedr::EngineError("Expected a double vector");
  }
  return {REAL_RO(x), static_cast<size_t>(Rf_xlength(x))};
}

// `games` is the list built by standings_encode_games() in R. The spans point
// into its elements, so it must outlive the result.
nflseedr::GamesInput make_games(const cpp11::list& games) {
  return {int_span(games["sim"]),
          int_span(games["type"]),
          int_span(games["away"]),
          int_span(games["home"]),
          dbl_span(games["result"]),
          dbl_span(games["away_score"]),
          dbl_span(games["home_score"]),
          cpp11::as_cpp<bool>(games["has_scores"]),
          cpp11::as_cpp<int>(games["n_sims"])};
}

nflseedr::League make_league(const cpp11::list& games) {
  const std::span<const int32_t> division = int_span(games["team_division"]);
  const std::span<const int32_t> conference =
      int_span(games["team_conference"]);
  if (division.size() != kTeams || conference.size() != kTeams) {
    throw nflseedr::EngineError("The league needs 32 teams");
  }
  nflseedr::League league;
  std::ranges::copy(division, league.division.begin());
  std::ranges::copy(conference, league.conference.begin());
  return league;
}

cpp11::writable::integers to_r(const std::vector<int32_t>& x) {
  cpp11::writable::integers out(static_cast<R_xlen_t>(x.size()));
  std::ranges::transform(x, INTEGER(out), [](int32_t v) {
    return v == kMissing ? NA_INTEGER : v;
  });
  return out;
}

// Game results. A plain copy: to_r() would turn -1 (kMissing), a valid result,
// into NA. kNoResult is INT_MIN, which is R's NA_INTEGER.
cpp11::writable::integers results_to_r(const std::vector<int32_t>& x) {
  cpp11::writable::integers out(static_cast<R_xlen_t>(x.size()));
  std::ranges::copy(x, INTEGER(out));
  return out;
}

cpp11::writable::doubles to_r(const std::vector<double>& x) {
  cpp11::writable::doubles out(static_cast<R_xlen_t>(x.size()));
  std::ranges::copy(x, REAL(out));
  return out;
}

std::span<const int32_t> lgl_span(SEXP x) {
  if (TYPEOF(x) != LGLSXP) {
    throw nflseedr::EngineError("Expected a logical vector");
  }
  return {LOGICAL_RO(x), static_cast<size_t>(Rf_xlength(x))};
}

void check_interrupt() { cpp11::check_user_interrupt(); }

}  // namespace

// Standings of all seasons in `sim` (dense ids 1..n_sims). Team ids are
// 0-based. See R/standings.R.
[[cpp11::register]]
cpp11::writable::list standings_cpp(cpp11::list games_list,
                                    cpp11::integers season_complete, int ranks,
                                    int depth, int playoff_seeds) {
  const nflseedr::GamesInput games = make_games(games_list);
  const nflseedr::League league = make_league(games_list);
  nflseedr::StandingsOptions options;
  options.ranks = static_cast<nflseedr::Ranks>(ranks);
  options.playoff_seeds = playoff_seeds;
  options.depth = depth;
  options.season_complete = int_span(season_complete);

  nflseedr::StandingsRows rows;
  {
    RRandom rng;
    rows = nflseedr::compute_standings(games, league, options, rng,
                                       check_interrupt);
  }

  using namespace cpp11::literals;
  return cpp11::writable::list({
      "sim"_nm = to_r(rows.sim),
      "team"_nm = to_r(rows.team),
      "games"_nm = to_r(rows.games),
      "wins"_nm = to_r(rows.wins),
      "true_wins"_nm = to_r(rows.true_wins),
      "losses"_nm = to_r(rows.losses),
      "ties"_nm = to_r(rows.ties),
      "pf"_nm = to_r(rows.pf),
      "pa"_nm = to_r(rows.pa),
      "win_pct"_nm = to_r(rows.win_pct),
      "div_pct"_nm = to_r(rows.div_pct),
      "conf_pct"_nm = to_r(rows.conf_pct),
      "sov"_nm = to_r(rows.sov),
      "sos"_nm = to_r(rows.sos),
      "div_rank"_nm = to_r(rows.div_rank),
      "div_label"_nm = to_r(rows.div_label),
      "conf_rank"_nm = to_r(rows.conf_rank),
      "conf_label"_nm = to_r(rows.conf_label),
      "exit"_nm = to_r(rows.exit),
      "draft_rank"_nm = to_r(rows.draft_rank),
      "draft_label"_nm = to_r(rows.draft_label),
  });
}

// Draft ranks for existing standings rows (simulations).
[[cpp11::register]]
cpp11::writable::list draft_ranks_cpp(cpp11::list games_list,
                                      cpp11::integers row_sim,
                                      cpp11::integers row_team,
                                      cpp11::integers div_rank,
                                      cpp11::integers conf_rank,
                                      cpp11::integers exit, int depth) {
  const nflseedr::GamesInput games = make_games(games_list);
  const nflseedr::League league = make_league(games_list);
  // NA ranks become kMissing
  auto ranks = [](SEXP x) {
    const std::span<const int32_t> in = int_span(x);
    std::vector<int32_t> out(in.size());
    std::ranges::transform(in, out.begin(), [](int32_t v) {
      return v == NA_INTEGER ? kMissing : v;
    });
    return out;
  };
  const std::vector<int32_t> div = ranks(div_rank);
  const std::vector<int32_t> conf = ranks(conf_rank);
  const nflseedr::TeamRanksInput teams{int_span(row_sim), int_span(row_team),
                                       div, conf, int_span(exit)};
  nflseedr::DraftRanks draft;
  {
    RRandom rng;
    draft = nflseedr::compute_draft_ranks(games, league, teams, depth, rng,
                                          check_interrupt);
  }
  using namespace cpp11::literals;
  return cpp11::writable::list({
      "draft_rank"_nm = to_r(draft.rank),
      "draft_label"_nm = to_r(draft.label),
  });
}

// 1-based positions of `value` in the integer (or factor) vector `x`
[[cpp11::register]]
cpp11::writable::integers positions_of_cpp(SEXP x, int value) {
  const std::span<const int32_t> in = int_span(x);
  cpp11::writable::integers out(
      static_cast<R_xlen_t>(std::ranges::count(in, value)));
  int* pos = INTEGER(out);
  for (size_t i = 0; i < in.size(); ++i) {
    if (in[i] == value) *pos++ = static_cast<int>(i + 1);
  }
  return out;
}

// One week of cpp_compute_results(). Games are the rows of the week, teams
// are ids into the team table (NA if unknown). See R/simulations_utils.R.
[[cpp11::register]]
cpp11::writable::list elo_week_cpp(SEXP game_sim, SEXP away, SEXP home,
                                   SEXP rest_diff, SEXP home_field,
                                   SEXP postseason, SEXP result, SEXP team_sim,
                                   SEXP team, SEXP elo) {
  // NA team ids become kMissing
  auto ids = [](SEXP x) {
    const std::span<const int32_t> in = int_span(x);
    std::vector<int32_t> out(in.size());
    std::ranges::transform(in, out.begin(), [](int32_t v) {
      return v == NA_INTEGER ? kMissing : v - 1;
    });
    return out;
  };
  const std::vector<int32_t> away_id = ids(away);
  const std::vector<int32_t> home_id = ids(home);
  const std::vector<int32_t> team_id = ids(team);
  // R's NA_INTEGER is INT_MIN, which is kNoResult, so results need no
  // conversion in either direction
  const nflseedr::EloGames games{int_span(game_sim),
                                 away_id,
                                 home_id,
                                 dbl_span(rest_diff),
                                 lgl_span(home_field),
                                 lgl_span(postseason),
                                 int_span(result)};
  const nflseedr::EloTeams teams{int_span(team_sim), team_id, dbl_span(elo)};
  const size_t n = games.sim.size();
  if (games.away.size() != n || games.home.size() != n ||
      games.rest_diff.size() != n || games.home_field.size() != n ||
      games.postseason.size() != n || games.result.size() != n ||
      teams.team.size() != teams.sim.size() ||
      teams.elo.size() != teams.sim.size()) {
    throw nflseedr::EngineError("Inputs must have the same length");
  }
  nflseedr::EloWeek week;
  {
    RRandom rng;
    week = nflseedr::simulate_elo_week(games, teams, rng);
  }
  using namespace cpp11::literals;
  return cpp11::writable::list({
      "result"_nm = results_to_r(week.result),
      "elo"_nm = to_r(week.elo),
  });
}
