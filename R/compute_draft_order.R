#' Compute NFL Draft Order using Game Results and Divisional Rankings
#'
#' @inheritParams compute_division_ranks
#' @param teams The division standings data frame including playoff seeds as
#' computed by \code{\link{compute_conference_seeds}}
#'
#' @returns A data frame of standings including the final draft pick number and
#'  the variable \code{exit} which indicates the week number of the teams final
#'  game (Super Bowl Winner is one week higher).
#' @seealso The examples [on the package website](https://nflseedr.com/articles/articles/nflseedR.html)
#' @export
#' @examples
#' # Change some options for better output
#' old <- options(list(digits = 3, tibble.print_min = 64))
#' library(dplyr, warn.conflicts = FALSE)
#'
#' games <-
#'   nflseedR::load_sharpe_games() %>%
#'   dplyr::filter(season %in% 2018:2019) %>%
#'   dplyr::select(sim = season, game_type, week, away_team, home_team, result)
#'
#' games %>%
#'   nflseedR::compute_division_ranks() %>%
#'   nflseedR::compute_conference_seeds(h2h = .$h2h, playoff_seeds = 6) %>%
#'   nflseedR::compute_draft_order(games = games, h2h = .$h2h)
#'
#' # Restore old options
#' options(old)
compute_draft_order <- function(teams,
                                games,
                                h2h = NULL,
                                tiebreaker_depth = 3,
                                .debug = FALSE) {
  # catch invalid input
  if (!isTRUE(tiebreaker_depth %in% 1:3)) {
    stop(
      "The argument `tiebreaker_depth` has to be",
      "a single value in the range of 1-3!"
    )
  }

  if (!is_tibble(teams)) teams <- teams$standings

  required_vars <- c(
    "sim",
    "game_type",
    "week",
    "away_team",
    "home_team",
    "result"
  )

  if (!sum(names(games) %in% required_vars, na.rm = TRUE) >= 6 | !is.data.frame(games)) {
    stop(
      "The argument `games` has to be a data frame including ",
      "all of the following variables: ",
      glue_collapse(required_vars, sep = ", ", last = " and "),
      "!"
    )
  }

  if (!any(games$game_type %in% "SB")) {
    stop(
      "Can't compute draft order for an incomplete season. It looks like the ",
      "`games` data frame is missing the game_type 'SB'!"
    )
  } else if (any(is.na(games$result[games$game_type == "SB"]))){
    stop(
      "Can't compute draft order for an incomplete season. It looks like the ",
      "`games` data frame is missing the result for game_type 'SB'!"
    )
  }

  if (is.null(h2h) & tiebreaker_depth > TIEBREAKERS_NONE) {
    stop(
      "You asked for tiebreakers but the argument `h2h` is NULL. ",
      "Did you forget to pass the `h2h` data frame? It is computed with the ",
      "function `compute_division_ranks()`."
    )
  }

  if (any(is.na(teams$exit))){
    # week tracker
    week_num <- games %>%
      filter(game_type == "REG") %>%
      pull(week) %>%
      max()

    # identify playoff teams
    playoff_teams <- teams %>%
      filter(!is.na(seed)) %>%
      select(sim, conf, seed, team) %>%
      arrange(sim, conf, seed)

    # num teams tracker
    num_teams <- playoff_teams %>%
      group_by(sim, conf) %>%
      summarize(count = n()) %>%
      pull(count) %>%
      max()

    # bye count (per conference)
    num_byes <- 2^ceiling(log(num_teams, 2)) - num_teams

    # first playoff week
    first_playoff_week <- week_num + 1

    # final week of season (Super Bowl week)
    week_max <- week_num +
      ceiling(log(num_teams * length(unique(playoff_teams$conf)), 2))

    # playoff weeks
    for (week_num in first_playoff_week:week_max) {

      # record losers
      teams <- games %>%
        filter(week == week_num) %>%
        double_games() %>%
        filter(outcome == 0) %>%
        select(sim, team, outcome) %>%
        right_join(teams, by = c("sim", "team")) %>%
        mutate(exit = ifelse(!is.na(outcome), week_num, exit)) %>%
        select(-outcome)

      # if super bowl, record winner
      if (any(playoff_teams$conf == "SB")) {
        # super bowl winner exit is +1 to SB week
        teams <- games %>%
          filter(week == week_num) %>%
          double_games() %>%
          filter(outcome == 1) %>%
          select(sim, team, outcome) %>%
          right_join(teams, by = c("sim", "team")) %>%
          mutate(exit = ifelse(!is.na(outcome), week_num + 1, exit)) %>%
          select(-outcome)
      }

      # filter to winners or byes
      playoff_teams <- games %>%
        filter(week == week_num) %>%
        double_games() %>%
        right_join(playoff_teams, by = c("sim", "team")) %>%
        filter(is.na(result) | result > 0) %>%
        select(sim, conf, seed, team) %>%
        arrange(sim, conf, seed)

      # update number of teams
      num_teams <- playoff_teams %>%
        group_by(sim, conf) %>%
        summarize(count = n()) %>%
        pull(count) %>%
        max()

      # if at one team per conf, loop once more for the super bowl
      if (num_teams == 1 && !any(playoff_teams$conf == "SB")) {
        playoff_teams <- playoff_teams %>%
          mutate(conf = "SB", seed = rep(1:2, n() / 2))
        num_teams <- 2
      }
    } # end playoff loop
  }

  # set draft order variable
  teams <- teams %>%
    mutate(draft_order = NA_real_) %>%
    arrange(sim, division, team)

  max_do_num <- min(length(unique(teams$team)), 32)

  # draft order loop
  for (do_num in rev(seq_len(max_do_num)))
  {
    # progress
    report(paste0("Calculating draft order #", do_num))

    # teams we can update
    update <- teams %>%
      filter(is.na(draft_order)) %>%
      group_by(sim) %>%
      filter(exit == max(exit)) %>%
      filter(win_pct == max(win_pct)) %>%
      filter(sos == max(sos)) %>%
      mutate(draft_order = ifelse(n() == 1, do_num, draft_order)) %>%
      ungroup() %>%
      break_draft_ties(do_num, h2h = h2h, tb_depth = tiebreaker_depth, .debug = .debug)

    # store updates
    teams <- teams %>%
      left_join(update, by = c("sim", "team")) %>%
      mutate(draft_order = ifelse(!is.na(new_do), new_do, draft_order)) %>%
      select(-new_do)
  } # end draft order loop

  # playoff error?
  if (any(is.na(teams$draft_order))) {
    stop(
      "The playoff games did not function normally. Make sure that either `fresh_season` ",
      "or `fresh_playoffs` to `TRUE`, or have playoff_seeds match the correct number of ",
      "seeds for the season being simulated."
    )
  }

  return(teams)
}
