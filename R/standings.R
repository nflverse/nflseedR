#' Compute NFL Standings
#'
#' @param games A data frame containing real or simulated game scores. The
#' following variables are required:
#' \describe{
#'  \item{sim or season}{A simulation ID. Normally 1 - n simulated seasons.}
#'  \item{game_type}{One of 'REG', 'WC', 'DIV', 'CON', 'SB' indicating if a
#'     game was a regular season game or one of the playoff rounds.}
#'  \item{week}{The week of the corresponding NFL season.}
#'  \item{away_team}{Team abbreviation of the away team (please see
#'    \code{\link{divisions}} for valid team abbreviations).}
#'  \item{home_team}{Team abbreviation of the home team (please see
#'    \code{\link{divisions}} for valid team abbreviations).}
#'  \item{result}{Equals home score - away score.}
#' }
#' @param ... currently not used
#' @param ranks One of `"DIV"`, `"CONF"`, `"DRAFT"`, or `"NONE"` to specify
#'   which ranks - and thus the associated tiebreakers - are to be determined.
#'   - `"DIV"`: Adds the division ranking variable `div_rank`
#'   - `"CONF"` (default): `"DIV"` + the conference variable `conf_rank`. For better
#'   performance, it is possible to set `playoff_seeds` to a value < 16 to make
#'   the function skip tiebreakers of irrelevant conference ranks.
#'   - `"DRAFT"`: `"CONF"` + the draft variable `draft_rank`. This is the actual
#'   pick in the draft based off game results. No trades of course.
#' @param tiebreaker_depth One of `"SOS"`, `"PRE-SOV"`, or `"RANDOM"`. Controls
#'   which tiebreakers are to be applied. The implemented tiebreakers are
#'   documented here <https://nflseedr.com/articles/tiebreaker.html>.
#'   The values mean:
#'   - `"SOS"` (default): Apply all tiebreakers through Strength of Schedule. If there are
#'   still remaining ties, break them through coin toss.
#'   - `"PRE-SOV"`: Apply all tiebreakers before Strength of Victory. If there are
#'   still remaining ties, break them through coin toss. Why Pre SOV? It's the
#'   first tiebreaker that requires knowledge of how OTHER teams played.
#'   - `"RANDOM"`: Breaks all tiebreakers with a coin toss. I don't really know,
#'   why I allow this...
#' @param playoff_seeds If `NULL` (the default), will compute all 16 conference
#'   ranks. This means, the function applies conference tiebreakers to all
#'   conference ranks. For better performance, it is possible to set this to a
#'   value < 16 to make the function skip tiebreakers of those conference ranks.
#' @param verbosity One of `"MIN"`, `"MAX"`, or `"NONE"` allowing the user
#'  to set the grade of verbosity of status reports. They mean:
#'  - `"MIN"` (default): Prints main steps of the process.
#'  - `"MAX"`: Prints all steps of the complete tiebreaking process.
#'  - `"NONE"`: No status reports at all. Do this to maximize the performance.
#'
#' @return A data.table of NFL standings including the ranks selected in the
#'   argument `ranks`
#' @export
#'
#' @seealso For more information on the implemented tiebreakers, see
#'   <https://nflseedr.com/articles/tiebreaker.html>
#'
#' @examples
#' \donttest{
#' try({#to avoid CRAN test problems
#'   games <- nflreadr::load_schedules(2021:2022)
#'   standings <- nflseedR::nfl_standings(games)
#'   print(standings, digits = 3)
#' })
#' }
nfl_standings <- function(games,
                          ...,
                          ranks = c("CONF", "DIV", "DRAFT", "NONE"),
                          tiebreaker_depth = c("SOS", "PRE-SOV", "RANDOM"),
                          playoff_seeds = NULL,
                          verbosity = c("MIN", "MAX", "NONE")){

  # VALIDATE INPUT ----------------------------------------------------------
  games <- standings_validate_games(games)
  ranks <- rlang::arg_match(ranks)
  tiebreaker_depth <- rlang::arg_match(tiebreaker_depth)
  verbosity <- rlang::arg_match(verbosity)
  verbosity <- switch (verbosity,
    "MIN" = 1L,
    "MAX" = 2L,
    "NONE" = 0L
  )
  if ( !is.null(playoff_seeds) && !inrange(playoff_seeds, 1L, 16L) ){
    cli::cli_abort("The {.arg playoff_seeds} argument must be in range {.pkg 1} - {.pkg 16}")
  }

  # INITIATE STANDINGS WITHOUT ANY RANKINGS ---------------------------------
  if (verbosity > 0L) report("Initiate Standings & Tiebreaking Data")
  dg <- standings_double_games(games, verbosity = verbosity)
  standings <- standings_init(dg, verbosity = verbosity)
  h2h <- standings_h2h(dg, verbosity = verbosity)

  if (ranks == "NONE") return(finalize_standings(standings, games))

  # DIVISION RANKS ----------------------------------------------------------
  if (verbosity > 0L) report("Compute Division Ranks")
  standings <- add_div_ranks(
    standings = standings,
    h2h = h2h,
    tiebreaker_depth = tiebreaker_depth,
    verbosity = verbosity
  )
  if (ranks == "DIV") return(finalize_standings(standings, games))

  # CONFERENCE RANKS --------------------------------------------------------
  if (verbosity > 0L) report("Compute Conference Ranks")
  standings <- add_conf_ranks(
    standings = standings,
    h2h = h2h,
    tiebreaker_depth = tiebreaker_depth,
    playoff_seeds = playoff_seeds,
    verbosity = verbosity
  )
  if (ranks == "CONF"){
    out <- finalize_standings(standings, games)
    dots <- list(...)
    in_sim <- if ("in_sim" %in% names(dots)) dots$in_sim else FALSE
    if (isTRUE(in_sim)) data.table::setattr(out, "h2h", h2h)
    return(out)
  }

  # DRAFT ORDER -------------------------------------------------------------
  if (verbosity > 0L) report("Compute Draft Order")
  standings <- add_draft_ranks(
    standings = standings,
    h2h = h2h,
    dg = dg,
    tiebreaker_depth = tiebreaker_depth,
    verbosity = verbosity
  )

  return(finalize_standings(standings, games))
}
