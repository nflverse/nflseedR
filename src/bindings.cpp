// R interface of the standings engine. This is the only engine file that
// includes R or cpp11 headers.
#include <R_ext/Random.h>

#include <algorithm>
#include <cpp11.hpp>
#include <span>
#include <vector>

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

cpp11::writable::doubles to_r(const std::vector<double>& x) {
  cpp11::writable::doubles out(static_cast<R_xlen_t>(x.size()));
  std::ranges::copy(x, REAL(out));
  return out;
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
