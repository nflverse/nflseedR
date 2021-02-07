#' Simulate an NFL Season
#'
#' @inheritParams compute_conference_seeds
#' @param nfl_season Season to simulate
#' @param process_games A function to estimate and simulate the results of games. Uses team,
#'   schedule, and week number as arguments.
#' @param ... Additional parameters passed on to the functions
#'   \code{estimate_games} and \code{simulate_games}.
#' @param if_ended_today Either \code{TRUE} or \code{FALSE}. ...
#' @param fresh_season Either \code{TRUE} or \code{FALSE}. ...
#' @param fresh_playoffs Either \code{TRUE} or \code{FALSE}. ...
#' @param simulations Equals the number of times the given NFL season shall be simulated
#' @param sims_per_round The number of \code{simulations} can be split into
#'   multiple rounds and be processed parallel. This parameter controls the number
#'   of simulations per round.
#' @param print_summary If \code{TRUE}, prints the summary statistics to the console.
#' @description This function simulates a given NFL season multiple times using custom functions
#'   to estimate and simulate game results and computes the outcome of the given
#'   season including playoffs and draft order.
#'   It is possible to run the function in parallel processes by calling the
#'   appropriate \link[future]{plan}.
#'   Progress updates can be activated by calling \link[progressr]{handlers}
#'   before the start of the simulations.
#' @returns A list of three data frames with the results of all simulated games,
#'   the final standings in each simulated season (incl. playoffs and draft order)
#'   and summary statistics across all simulated seasons.
#' @export
#' @examples
#' \donttest{
#' library(nflseedR)
#'
#' # Activate progress updates
#' progressr::handlers(global = TRUE)
#'
#' # Parallel processing can be activated via the following line
#' # future::plan("multisession")
#'
#' # Simulate the season 4 times in 2 rounds
#' sim <- nflseedR::simulate_nfl(
#'   2020,
#'   fresh_season = TRUE,
#'   simulations = 4,
#'   sims_per_round = 2
#' )
#' }
simulate_nfl <- function(nfl_season,
                         process_games = NULL,
                         ...,
                         playoff_seeds = ifelse(nfl_season >= 2020, 7, 6),
                         if_ended_today = FALSE,
                         fresh_season = FALSE,
                         fresh_playoffs = FALSE,
                         tiebreaker_depth = 3,
                         simulations = 1000,
                         sims_per_round = min(1000,simulations),
                         .debug = FALSE,
                         print_summary = FALSE) {

  # Define simple estimate and simulate functions

  if (is.null(process_games)) {
    process_games <- function(t, g, w, ...) {
      # t = teams data
      # g = games data
      # estimate = is the median spread expected (positive = home team favored)
      # wp = is the probability of the team winning the game
      # this example estimates at PK/0 and 50%
      # only simulate games through week w
      # only simulate games with is.na(result)
      # result = how many points home team won by

      # round out (away from zero)
      round_out <- function(x) {
        x[!is.na(x) & x < 0] <- floor(x[!is.na(x) & x < 0])
        x[!is.na(x) & x > 0] <- ceiling(x[!is.na(x) & x > 0])
        return(x)
      }

      # add estimate if missing
      if (!("estimate" %in% colnames(g)))
      {
        g <- g %>%
          dplyr::mutate(estimate=NA_real_)
      }

      # add wp if missing
      if (!("wp" %in% colnames(g)))
      {
        g <- g %>%
          dplyr::mutate(wp=NA_real_)
      }

      # mark estimate, wp, and result for games
      g <- g %>%
        dplyr::mutate(
          estimate = ifelse(is.na(result) & week <= w, estimate, 0),
          wp = ifelse(is.na(result) & week <= w, wp, 0.5),
          result = ifelse(is.na(result) & week <= w,
            round_out(rnorm(n(), estimate, 14)),
            result
          )
        )
      return(list(teams=t,games=g))

    }
  }

  # Catch invalid input

  if (!all(is_single_digit_numeric(nfl_season),
           is_single_digit_numeric(tiebreaker_depth),
           is_single_digit_numeric(simulations),
           is_single_digit_numeric(sims_per_round))) {
    stop("One or more of the parameters `nfl_season`, `tiebreaker_depth`, ",
         "`simulations` and `sims_per_round` are not single digit numeric values!")
  }

  if (!is.function(process_games)) {
    stop("The parameter `process_games` has to be a function!")
  }

  #### LOAD DATA ####

  # load games data
  report("Loading games data")
  schedule <- load_sharpe_games() %>%
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
      process_games = process_games,
      ...,
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

  if (isTRUE(print_summary)) print(overall)

  list("teams" = all_teams, "games" = all_games, "overall" = overall)
}
