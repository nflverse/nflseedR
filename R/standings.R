#' Compute NFL Standings
#'
#' @param games A data frame containing real or simulated game scores. Outside
#' of simulations, this is simply the output of [nflreadr::load_schedules].
#' The following variables are required as a minimum:
#' \describe{
#'  \item{sim or season}{A season or simulation ID. Normally 1 - n simulated seasons.
#'  If both sim and season are included, a warning is triggered and work continues with sim.}
#'  \item{game_type}{One of 'REG', 'WC', 'DIV', 'CON', 'SB' indicating if a
#'     game was a regular season game or one of the playoff rounds.}
#'  \item{week}{The week of the corresponding NFL season.}
#'  \item{away_team}{Team abbreviation of the away team (please see
#'    \code{\link{divisions}} for valid team abbreviations).}
#'  \item{home_team}{Team abbreviation of the home team (please see
#'    \code{\link{divisions}} for valid team abbreviations).}
#'  \item{result}{Equals home score - away score.}
#' }
#' If tiebreakers beyond SOS are to be used, then the actual scores of the
#' home (`home_score`) and away (`away_score`) teams must also be available.
#'
#' @param ... currently not used
#'
#' @param ranks One of `"DIV"`, `"CONF"`, `"DRAFT"`, or `"NONE"` to specify
#'   which ranks - and thus the associated tiebreakers - are to be determined.
#'   - `"DIV"`: Adds the division ranking variable `div_rank`
#'   - `"CONF"` (default): `"DIV"` + the conference variable `conf_rank`. For better
#'   performance, it is possible to set `playoff_seeds` to a value < 16 to make
#'   the function skip tiebreakers of irrelevant conference ranks.
#'   - `"DRAFT"`: `"CONF"` + the draft variable `draft_rank`. This is the actual
#'   pick in the draft based off game results. No trades of course.
#'
#' @param tiebreaker_depth One of `"SOS"`, `"PRE-SOV"`, `"POINTS"` or `"RANDOM"`.
#'   Controls which tiebreakers are to be applied. The implemented tiebreakers
#'   are documented here <https://nflseedr.com/articles/tiebreaker.html>.
#'   The values mean:
#'   - `"SOS"` (default): Apply all tiebreakers through Strength of Schedule. If there are
#'   still remaining ties, break them through coin toss.
#'   - `"PRE-SOV"`: Apply all tiebreakers before Strength of Victory. If there are
#'   still remaining ties, break them through coin toss. Why Pre SOV? It's the
#'   first tiebreaker that requires knowledge of how OTHER teams played.
#'   - `"POINTS"`: Apply all tiebreakers through point differential. If there are
#'   still remaining ties, break them through coin toss. This will go beyond SOS
#'   and requires knowledge of points scored and points allowed. As this is not
#'   usually part of season simulations, caution is advised in this case.
#'   These tiebreakers should only be used if the scores are real or are
#'   deliberately simulated.
#'   - `"RANDOM"`: Breaks all tiebreakers with a coin toss. I don't really know,
#'   why I allow this...
#' @param playoff_seeds If `NULL` (the default), will compute all 16 conference
#'   ranks. This means, the function applies conference tiebreakers to all
#'   conference ranks. For better performance, it is possible to set this to a
#'   value < 16 to make the function skip tiebreakers of those conference ranks.
#'   Ignored if `ranks = "DRAFT"` because draft ranks need all conference
#'   ranks.
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
#' @details
#' Division, conference and draft ranks are only computed once every team has
#' played at least one regular season game. Until then, they are `NA`.
#'
#' Only the 32 current team abbreviations are valid (see [divisions]).
#'
#' nflseedR does not support all levels of tie-breakers at the moment. The
#' deepest tie-breaker currently is "best net points in all games".
#' After that, the decision is made at random. However, the need for
#' the last level ("best net touchdowns in all games") is extremely unlikely
#' in practice. Deeper levels than strength of schedule have never actually been
#' needed to resolve season-end standings since the NFL expanded to 32 teams.
#'
#' @seealso For more information on the implemented tiebreakers, see
#'   <https://nflseedr.com/articles/tiebreaker.html>
#'
#' @examples
#' \donttest{
#' try({#to avoid CRAN test problems
#'   games <- nflreadr::load_schedules(2021:2022)
#' })
#' standings <- nflseedR::nfl_standings(games)
#' print(standings, digits = 3)
#' }
nfl_standings <- function(
  games,
  ...,
  ranks = c("CONF", "DIV", "DRAFT", "NONE"),
  tiebreaker_depth = c("SOS", "PRE-SOV", "POINTS", "RANDOM"),
  playoff_seeds = NULL,
  verbosity = c("MIN", "MAX", "NONE")
) {
  ranks <- rlang::arg_match(ranks)
  tiebreaker_depth <- rlang::arg_match(tiebreaker_depth)
  verbosity <- rlang::arg_match(verbosity)
  standings_compute(
    games,
    ranks = ranks,
    tiebreaker_depth = tiebreaker_depth,
    playoff_seeds = playoff_seeds,
    verbosity = verbosity
  )
}
