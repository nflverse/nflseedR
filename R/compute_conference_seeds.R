#' Compute NFL Playoff Seedings using Game Results and Divisional Rankings
#'
#' @inheritParams compute_division_ranks
#'
#' @returns A list of two data frames:
#'  \describe{
#'  \item{teams}{The argument \code{teams} including playoff seeds.}
#'  \item{h2h}{A data frame that is used for head-to-head tiebreakers across the
#'  tiebreaking functions.}
#'  }
#'
#' @export
compute_conference_seeds <- function(games,
                                     teams,
                                     tiebreaker_depth = 3,
                                     .debug = FALSE,
                                     h2h) {
  # catch invalid input
  if (!isTRUE(tiebreaker_depth %in% 1:3)) {
    stop(
      "The argument `tiebreaker_depth` has to be",
      "a single value in the range of 1-3!"
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

  if (!all(names(games) %in% required_vars) | !is.data.frame(games)) {
    stop(
      "The argument `games` has to be a data frame including ",
      "all of the following variables: ",
      glue_collapse(required_vars, sep = ", ", last = " and "),
      "!"
    )
  }

  if (!(names(teams) %in% "div_rank") | !is.data.frame(teams)) {
    stop(
      "The argument `teams` has to be a data frame including ",
      "the variable `div_rank` as computed by `compute_division_ranks()`!"
    )
  }

  if(is.null(h2h) & tiebreaker_depth > TIEBREAKERS_NONE){
    stop("You asked for tiebreakers but the argument `h2h` is NULL. ",
         "Did you forget to pass the `h2h` data frame? It is computed with the ",
         "function `compute_division_ranks()`."
    )
  }

  teams <- teams %>%
    mutate(conf_rank = NA_real_)

  # seed loop
  for (seed_num in 1:playoff_seeds)
  {
    report(paste0("Calculating seed #", seed_num))

    # find teams at this seed
    update <- teams %>%
      filter(is.na(conf_rank)) %>%
      mutate(div_winner = (div_rank == 1)) %>%
      group_by(sim, conf) %>%
      filter(div_winner == max(div_winner)) %>%
      filter(win_pct == max(win_pct)) %>%
      mutate(conf_rank = ifelse(n() == 1, as.numeric(seed_num), conf_rank)) %>%
      ungroup() %>%
      group_by(sim, conf, division) %>%
      mutate(div_best_left = (div_rank == min(div_rank))) %>%
      ungroup() %>%
      break_conference_ties(seed_num, h2h = h2h, tb_depth = tiebreaker_depth, .debug = .debug)

    # store updates
    teams <- teams %>%
      left_join(update, by = c("sim", "team")) %>%
      mutate(conf_rank = ifelse(!is.na(new_rank), new_rank, conf_rank)) %>%
      select(-new_rank)
  } # end conference rank loop

  # rename conference rank to seed
  teams <- teams %>%
    rename(seed = conf_rank)

  return(list(teams = teams, h2h = h2h))
}
