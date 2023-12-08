#' Compute NFL Division Rankings using Game Results
#'
#' @param games A data frame containing real or simulated game scores. The
#' following variables are required:
#' \describe{
#'  \item{sim}{A simulation ID. Normally 1 - n simulated seasons.}
#'  \item{game_type}{One of 'REG', 'WC', 'DIV', 'CON', 'SB' indicating if a game was a regular season game or one of the playoff rounds.}
#'  \item{week}{The week of the corresponding NFL season.}
#'  \item{away_team}{Team abbreviation of the away team (please see
#'    \code{\link{divisions}} for valid team abbreviations).}
#'  \item{home_team}{Team abbreviation of the home team (please see
#'    \code{\link{divisions}} for valid team abbreviations).}
#'  \item{result}{Equals home score - away score.}
#' }
#' @param teams `r lifecycle::badge("deprecated")` This argument is no longer
#'   supported. Instead, the function computes it internally.
#' @param tiebreaker_depth One of `"SOS"`, `"PRE-SOV"`, or `"RANDOM"`.
#'  The default `"SOS"`. The value controls the depth of tiebreakers that
#'  shall be applied. The deepest currently implemented tiebreaker is strength
#'  of schedule (SOS). The following values are valid:
#'  \describe{
#'  \item{tiebreaker_depth = `"RANDOM"`}{Break all ties with a coinflip. Fastest variant.}
#'  \item{tiebreaker_depth = `"PRE-SOV"`}{Apply head-to-head, division win percentage, common games win percentage, conference games win percentage tiebreakers. Random if still tied.}
#'  \item{tiebreaker_depth = `"SOS"`}{Apply all tiebreakers through strength of schedule. Random if still tied.}
#'  }
#' @param .debug Either \code{TRUE} or \code{FALSE}. Controls whether additional
#' messages are printed to the console showing what the tie-breaking algorithms
#' are currently performing.
#' @param h2h `r lifecycle::badge("deprecated")` This argument is no longer
#'   supported. Instead, the function computes it internally.
#' @returns A Division standings data table
#' @seealso The examples [on the package website](https://nflseedr.com/articles/articles/nflseedR.html)
#' @export
#' @examples
#' \donttest{
#' # Change some options for better output
#' old <- options(list(digits = 3, tibble.print_min = 64))
#' library(dplyr, warn.conflicts = FALSE)
#'
#' try({#to avoid CRAN test problems
#' games <- nflreadr::load_schedules(2019:2020) |>
#'   dplyr::select(sim = season, game_type, week, away_team, home_team, result)
#'
#' nflseedR::compute_division_ranks(games)
#' })
#'
#' # Restore old options
#' options(old)
#' }
compute_division_ranks <- function(games,
                                   teams = lifecycle::deprecated(),
                                   tiebreaker_depth = c("SOS", "PRE-SOV", "RANDOM"),
                                   .debug = FALSE,
                                   h2h = lifecycle::deprecated()) {

  # Size of teams: 32 * n_sims
  # Size of double_games: nrow(games) * n_sims
  # Size of h2h: 32 teams * 13 opponents/team * n_sims

  # games <- nflreadr::load_schedules(2010:2019) |>
  #   dplyr::select(sim = season, game_type, week, away_team, home_team, result)
  #
  # s <- nflreadr::load_schedules()
  #
  # games <- tidyr::crossing(y = 2002:2019, w = 5:17) |>
  #   purrr::pmap(function(y, w, sched){
  #   sched |>
  #       dplyr::filter(season == y, week <= w, game_type == "REG") |>
  #       dplyr::mutate(
  #         sim = paste(season, week, sep = "_")
  #       )
  # }, sched = s
  # ) |>
  #   purrr::list_rbind() |>
  #   dplyr::select(sim, game_type, week, away_team, home_team, result)

  tiebreaker_depth <- rlang::arg_match(tiebreaker_depth)

  required_vars <- c(
    "sim",
    "game_type",
    "week",
    "away_team",
    "home_team",
    "result"
  )

  if ( !all(required_vars %in% names(games)) ) {
    cli::cli_abort(
      "The argument {.arg games} has to be a data frame including \\
      all of the following variables: {.val {required_vars}}!"
    )
  }

  if (lifecycle::is_present(teams)) {
    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = "compute_division_ranks(teams)",# = 'is computed internally')"
      details = "The function computes the corresponding data internally."
    )
  }

  report("Calculating team data")

  # record of each team
  teams <- init_teams(double_games(games, update = TRUE))

  # Set ranks by win percentage in descending order by sim and division.
  # If ties method is "random", data.table will break all ties randomly
  # and we won't need any further tie-breaking methods
  dt_ties_method <- if (tiebreaker_depth == "RANDOM") "random" else "min"
  teams[
    , div_rank := frankv(-win_pct, ties.method = dt_ties_method),
    by = c("sim", "division")
  ]

  # If tiebreaker_depth == "RANDOM", all ties are broken at this stage. We add
  # tiebreaker information to the tied teams.
  if (tiebreaker_depth == "RANDOM") {
    teams[, div_rank_counter := .N, by = c("sim", "division", "win_pct")]
    teams[
      div_rank_counter > 1,
      div_tie_broken_by := "Coin Toss",
    ]
  }

  # Count division ranks by sim and division. If each rank only exists once,
  # then there are no ties that need to be broken
  teams[, div_rank_counter := .N, by = c("sim", "division", "div_rank")]

  # enter tie breaking procedure only if there are actual ties,
  # i.e. a division rank exists more than once per sim and division
  if ( any(teams$div_rank_counter > 1) ) {
    # Report if we have to break ties
    report("Breaking Ties")

    # If we have to break ties, we need the h2h data
    h2h <- compute_h2h(double_games(games), update = TRUE)

    # 3 or 4-Team ties need to go through all these steps until at least 2 tied
    # teams remain. If that's the case, we have to jump back to the beginning
    # of the process with the 2 remaining teams. That's why we have to loop over
    # this process and check the number of tied teams after each step.
    # A 3 iterations for loop is fine. No need to go crazy about it.
    for (tied_teams in 4:2) {

      if (tie_break_done(teams, tied_teams)) next

      # Head To Head ------------------------------------------------------------
      if (isTRUE(.debug)) report("DIV ({tied_teams}): Head-to-Head Win PCT")
      teams <- break_div_ties_by_h2h(teams = teams, h2h = h2h, n_tied = tied_teams)
      if (tie_break_done(teams, tied_teams)) next

      # Division Record ---------------------------------------------------------
      if (isTRUE(.debug)) report("DIV ({tied_teams}): Division Win PCT")
      teams <- break_div_ties_by_div_win_pct(teams = teams, n_tied = tied_teams)
      if (tie_break_done(teams, tied_teams)) next

      # Common Games Win Pct ----------------------------------------------------
      if (isTRUE(.debug)) report("DIV ({tied_teams}): Common Games Win PCT")
      teams <- break_div_ties_by_common_win_pct(teams = teams, h2h = h2h, n_tied = tied_teams)
      if (tie_break_done(teams, tied_teams)) next

      # Conference Win PCT ------------------------------------------------------
      if (isTRUE(.debug)) report("DIV ({tied_teams}): Conference Win PCT")
      teams <- break_div_ties_by_conf_win_pct(teams = teams, n_tied = tied_teams)
      if (tie_break_done(teams, tied_teams)) next

      # SOV ---------------------------------------------------------------------
      if (isTRUE(.debug)) report("DIV ({tied_teams}): SOV")
      teams <- break_div_ties_by_sov(teams = teams, n_tied = tied_teams)
      if (tie_break_done(teams, tied_teams)) next

      # SOS ---------------------------------------------------------------------
      if (isTRUE(.debug)) report("DIV ({tied_teams}): SOS")
      teams <- break_div_ties_by_sos(teams = teams, n_tied = tied_teams)
      if (tie_break_done(teams, tied_teams)) next
    }

    # We've worked through all implemented tie-breakers.
    # If there are still ties, we break them randomly
    if ( any(teams$div_rank_counter > 1) ) {
      if (isTRUE(.debug)) report("DIV : Coin Toss")
      teams[
        div_rank_counter > 1,
        div_rank := min(div_rank) - 1 + frank(list(div_rank, -win_pct), ties.method = "random"),
        by = c("sim", "division")
      ]
      teams[
        div_rank_counter > 1,
        div_tie_broken_by := "Coin Toss",
      ]
    }
  }

  # In simulations, we need to know the maximum regular season week as this has
  # changed over the time. We compute the max week by sim and join it
  # to the teams data
  max_reg_week <- games[
    game_type == "REG",
    list(max_reg_week = max(week)),
    by = "sim"
  ]
  teams <- merge(teams, max_reg_week, by = "sim")

  # Finally, the div_rank_counter can be removed and we sort data by
  # sim, division, and division rank
  teams <- teams[,!c("div_rank_counter")][order(sim, division, div_rank)]
  report("DONE: Division Standings")
  teams
}
