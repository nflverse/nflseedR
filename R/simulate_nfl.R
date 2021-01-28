#' Simulate an NFL Season
#'
#' @inheritParams compute_conference_seeds
#' @param nfl_season Season to simulate
#' @param estimate_games A function to estimate the games to simulate. Uses the
#'   schedule as argument.
#' @param simulate_games A function to simulate the results of the games to
#'   simulate. Uses the schedule and the week as argument.
#' @param if_ended_today Either \code{TRUE} or \code{FALSE}. ...
#' @param fresh_season Either \code{TRUE} or \code{FALSE}. ...
#' @param fresh_playoffs Either \code{TRUE} or \code{FALSE}. ...
#' @param simulations Equals the number of times the given NFL season shall be simulated
#' @param sims_per_round The number of \code{simulations} can be split into
#'   multiple rounds and be processed parallel. This parameter controls the number
#'   of simulations per round.
#' @description This function simulates a given NFL season multiple times using custom functions
#'   to estimate and simulate game results and computes the outcome of the given
#'   season including playoffs and draft order.
#'   It is possible to run the function in parallel processes by calling the
#'   appropriate \link[future]{plan}.
#'   Progress updates can be activated by calling \link[progressr]{handlers}
#'   before the start of the simulations.
#' @export
#' @examples
#' \donttest{
#'   library(nflseedR)
#'   library(dplyr)
#'
#'   # round out (away from zero)
#'   round_out <- function(x) {
#'     x[x < 0] <- floor(x[x < 0])
#'     x[x > 0] <- ceiling(x[x > 0])
#'     return(x)
#'   }
#'
#'   # function to estimate games
#'   my_estimate <- function(g) {
#'     # replace with your own function
#'     # g = games data
#'
#'     # define
#'     # estimate = is the median spread expected (positive = home team favored)
#'     # wp = is the probability of the team winning the game
#'
#'     # this example estimates at PK/0 and 50%
#'
#'     g <- g %>%
#'       dplyr::mutate(estimate = 0, wp = 0.5)
#'     return(g)
#'   }
#'
#'   # function to simulate games
#'   my_simulate <- function(g, w) {
#'     # replace with your own function
#'     # only simulate games through week w
#'     # only simulate games with is.na(result)
#'
#'     # define
#'     # result = how many points home team won by
#'     # can add additional columns as well (e.g. Elo)
#'
#'     g <- g %>%
#'       dplyr::mutate(result = ifelse(is.na(result) & week <= w,
#'         round_out(rnorm(n(), estimate, 14)),
#'         result
#'       ))
#'     return(g)
#'   }
#'
#'   # Activate progress updates
#'   progressr::handlers(global = TRUE)
#'
#'   # Parallel processing can be activated via the following line
#'   # future::plan("multisession")
#'
#'   # Simulate season 4 times in 2 rounds
#'   sim <- nflseedR::simulate_nfl(
#'     2020,
#'     estimate_games = my_estimate,
#'     simulate_games = my_simulate,
#'     fresh_season = TRUE,
#'     simulations = 4,
#'     sims_per_round = 2
#'   )
#' }
simulate_nfl <- function(nfl_season,
                         estimate_games,
                         simulate_games,
                         playoff_seeds = ifelse(nfl_season >= 2020, 7, 6),
                         if_ended_today = FALSE,
                         fresh_season = FALSE,
                         fresh_playoffs = FALSE,
                         tiebreaker_depth = 3,
                         simulations = 10,
                         sims_per_round = simulations,
                         .debug = FALSE) {


  #### LOAD DATA ####

  # load games data
  report("Loading games data")
  schedule <- readRDS(url("https://github.com/leesharpe/nfldata/blob/master/data/games.rds?raw=true")) %>%
    filter(season == nfl_season) %>%
    select(game_type, week, away_team, home_team, result)

  #### PREPROCESSING ####

  # if simulating fresh season, clear out all results and playoff games
  if (isTRUE(fresh_season)) {
    schedule <- schedule %>%
      filter(game_type == "REG") %>%
      mutate(result = as.numeric(NA))
  }

  # if simulating fresh playoffs, clear out playoff games
  if (isTRUE(fresh_playoffs)) {
    schedule <- schedule %>%
      filter(game_type == "REG")
  }

  # if ended today just needs one simulation
  if (isTRUE(if_ended_today)) {
    schedule <- schedule %>%
      filter(!is.na(result))
    simulations <- 1
  }

  # weeks to sim
  weeks_to_sim <- schedule %>%
    filter(game_type == "REG") %>%
    filter(is.na(result)) %>%
    pull(week) %>%
    unique() %>%
    sort()

  #### SET UP SIMULATIONS ####
  sim_rounds <- ceiling(simulations / sims_per_round)

  report(glue("Beginning simulation of {simulations} seasons in {sim_rounds} rounds"))

  p <- progressr::progressor(along = seq_len(sim_rounds))

  suppressMessages({
    all <- furrr::future_map(
      .x = seq_len(sim_rounds),
      .f = simulate_round,
      sim_rounds = sim_rounds,
      sims_per_round = sims_per_round,
      schedule = schedule,
      simulations = simulations,
      weeks_to_sim = weeks_to_sim,
      estimate_games = estimate_games,
      simulate_games = simulate_games,
      tiebreaker_depth = tiebreaker_depth,
      .debug = .debug,
      playoff_seeds = playoff_seeds,
      p = p,
      .options = furrr::furrr_options(seed = TRUE)
    )
  })

  report("Combining simulation data")

  all_teams <- furrr::future_map_dfr(all, ~ .x$teams)
  all_games <- furrr::future_map_dfr(all, ~ .x$games)

  # aggregated data
  report("Aggregating across simulations")
  overall <- all_teams %>%
    group_by(conf, division, team) %>%
    summarize(
      wins = mean(wins),
      playoff = mean(!is.na(seed)),
      div1 = mean(div_rank == 1),
      seed1 = mean(!is.na(seed) & seed == 1),
      won_conf = mean(draft_order >= (length(unique(all_teams$team)) - 1)),
      won_sb = mean(draft_order == length(unique(all_teams$team))),
      draft1 = mean(draft_order == 1),
      draft5 = mean(draft_order <= 5)
    ) %>%
    ungroup()

  print(overall)

  list("teams" = all_teams, "games" = all_games, "overall" = overall)
}
