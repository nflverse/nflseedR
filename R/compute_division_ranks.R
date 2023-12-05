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
#'   supported. Instead, the functions computes it internally.
#' @param tiebreaker_depth A single value equal to 1, 2, or 3. The default is 3. The
#'  value controls the depth of tiebreakers that shall be applied. The deepest
#'  currently implemented tiebreaker is strength of schedule. The following
#'  values are valid:
#'  \describe{
#'  \item{tiebreaker_depth = 1}{Break all ties with a coinflip. Fastest variant.}
#'  \item{tiebreaker_depth = 2}{Apply head-to-head and division win percentage tiebreakers. Random if still tied.}
#'  \item{tiebreaker_depth = 3}{Apply all tiebreakers through strength of schedule. Random if still tied.}
#'  }
#' @param .debug Either \code{TRUE} or \code{FALSE}. Controls whether additional
#' messages are printed to the console showing what the tie-breaking algorithms
#' are currently performing.
#' @param h2h A data frame that is used for head-to-head tiebreakers across the
#' tie-breaking functions. It is computed by the function
#' \code{\link{compute_division_ranks}}.
#' @returns A list of two data frames:
#'  \describe{
#'  \item{standings}{Division standings.}
#'  \item{h2h}{A data frame that is used for head-to-head tiebreakers across the
#'  tie-breaking functions.}
#'  }
#' @seealso The examples [on the package website](https://nflseedr.com/articles/articles/nflseedR.html)
#' @export
#' @examples
#' \donttest{
#' # Change some options for better output
#' old <- options(list(digits = 3, tibble.print_min = 64))
#' library(dplyr, warn.conflicts = FALSE)
#'
#' try({#to avoid CRAN test problems
#' nflseedR::load_sharpe_games() %>%
#'   dplyr::filter(season %in% 2019:2020) %>%
#'   dplyr::select(sim = season, game_type, week, away_team, home_team, result) %>%
#'   nflseedR::compute_division_ranks() %>%
#'   purrr::pluck("standings")
#' })
#'
#' # Restore old options
#' options(old)
#' }
compute_division_ranks <- function(games,
                                   teams = lifecycle::deprecated(),
                                   tiebreaker_depth = 3,
                                   .debug = FALSE,
                                   h2h = NULL) {

  # games <- nflreadr::load_schedules(2021:2022) |>
  #   dplyr::select(sim = season, game_type, week, away_team, home_team, result)

  # catch invalid input
  if (!isTRUE(tiebreaker_depth %in% 1:3)) {
    cli::cli_abort(
      "The argument {.arg tiebreaker_depth} has to be \\
      a single value in the range of 1-3!"
    )
  }

  required_vars <- c(
    "sim",
    "game_type",
    "week",
    "away_team",
    "home_team",
    "result"
  )

  if (!sum(names(games) %in% required_vars, na.rm = TRUE) >= 6 | !is.data.frame(games)) {
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

  # double games
  games_doubled <- double_games(games)

  report("Calculating team data")

  # record of each team
  teams <- init_teams(games_doubled)

  # below only if there are tiebreakers
  if (is.null(h2h) & tiebreaker_depth > TIEBREAKERS_NONE) {
    h2h <- compute_h2h(games_doubled)
  }

  #### FIND DIVISION RANKS ####

  #######################################################
  # Seb's new code #
  #######################################################
  dt_ties_method <- if (tiebreaker_depth == 0) "random" else "min"

  # Set ranks. If tied method is "random" we will break

  teams[, div_rank := frankv(win_pct, ties.method = dt_ties_method), by = c("sim", "division")]
  teams[, div_rank_counter := .N, by = c("sim", "division", "div_rank")]

  # LOOK FOR TIED TEAMS, i.e. div_ranks exist more than one per rank
  ties <- teams[div_rank_counter > 1]

  if(any(teams$div_rank_counter > 1)){

    # larger ties before smaller ties
    #
    for (tied_teams in 3:2) {

      ties <- teams[div_rank_counter == tied_teams]

      if (nrow(ties) == 0) next

      # Head To Head ------------------------------------------------------------
      if (isTRUE(.debug)) report("DIV ({tied_teams}): Head-to-head")

      h2h_games_played <- merge(
        ties[, list(sim, team)],
        ties[, list(sim, opp = team)],
        by = c(sim),
        allow.cartesian = TRUE
      )[team != opp]

      h2h_win_pct <- merge(
        h2h_games_played, h2h, by = c("sim", "team", "opp")
      )[, list(h2h_win_pct = sum(h2h_wins) / sum(h2h_games)), by = c("sim", "team")]

      teams <- merge(teams, h2h_win_pct, by = c("sim", "team"), all.x = TRUE)
      teams <- teams[
        !is.na(h2h_win_pct), div_rank := frank(list(div_rank, -h2h_win_pct), ties.method = "min"),
        by = c("sim", "division")
      ][order(sim, division, div_rank)]
      teams[, div_rank_counter := .N, by = c("sim", "division", "div_rank")]
      teams[!is.na(h2h_win_pct) & div_rank_counter == 1, div_tie_broken_by := "Head-To-Head Win PCT"]
      teams <- teams[,!c("h2h_win_pct")]

      ties <- teams[div_rank_counter == tied_teams]

      if (nrow(ties) == 0) next

      # Division Record ---------------------------------------------------------
      if (isTRUE(.debug)) report("DIV ({tied_teams}): Division Record")

      teams <- teams[
        div_rank_counter == tied_teams,
        `:=`(div_rank = frank(list(div_rank, -div_pct), ties.method = "min"),
             div_tie_broken_by = "Division Win PCT"),
        by = c("sim", "division")
      ][order(sim, division, div_rank)]
      teams[, div_rank_counter := .N, by = c("sim", "division", "div_rank")]
      teams[div_rank_counter > 1, div_tie_broken_by := NA_character_]

      ties <- teams[div_rank_counter == tied_teams]

      if (nrow(ties) == 0) next
    }


  }


  #################################################
  # initialize division rank
  teams$div_rank <- NA_real_
  teams$tie_broken_by <- NA_character_

  # determine division ranks
  dr <- 0
  while (any(is.na(teams$div_rank))) {
    # increment division rank
    dr <- dr + 1
    if(dr > 4){
      cli::cli_abort("Aborting because division rank computation entered infinite loop!")
    }
    report("Calculating division rank #{dr}")

    # update teams with this rank
    update <- teams %>%
      filter(is.na(div_rank)) %>%
      group_by(sim, division) %>%
      filter(win_pct == max(win_pct)) %>%
      mutate(div_rank = ifelse(n() == 1, dr, div_rank)) %>%
      ungroup() %>%
      break_division_ties(dr, h2h = h2h, tb_depth = tiebreaker_depth, .debug = .debug)

    # store updates
    teams <- teams %>%
      left_join(update, by = c("sim", "team")) %>%
      mutate(
        div_rank = ifelse(!is.na(new_rank), new_rank, div_rank),
        tie_broken_by = ifelse(!is.na(new_rank), tb_new, tie_broken_by)
      ) %>%
      select(-new_rank, -tb_new)
  }

  teams <- teams %>%
    rename(div_tie_broken_by = tie_broken_by)

  teams$max_reg_week <- max(games$week[games$game_type == "REG"], na.rm = TRUE)

  list(
    "standings" = tibble::as_tibble(teams),
    "h2h" = tibble::as_tibble(h2h)
  )
}
