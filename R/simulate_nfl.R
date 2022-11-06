#' Simulate an NFL Season
#'
#' @inheritParams compute_conference_seeds
#' @param nfl_season Season to simulate
#' @param process_games A function to estimate and simulate the results of games. Uses team,
#'   schedule, and week number as arguments.
#' @param ... Additional parameters passed on to the function \code{process_games}.
#' @param if_ended_today Either \code{TRUE} or \code{FALSE}. If TRUE, ignore remaining regular
#'   season games and cut to playoffs based on current regular season data.
#' @param fresh_season Either \code{TRUE} or \code{FALSE}. Whether to blank out all game results
#'   and simulate the the season from scratch (TRUE) or take game results so far as a given
#'   and only simulate the rest (FALSE).
#' @param fresh_playoffs Either \code{TRUE} or \code{FALSE}. Whether to blank out all playoff
#'   game results and simulate the postseason from scratch (TRUE) or take game results so far
#'   as a given and only simulate the rest (FALSE).
#' @param test_week Aborts after the simulator reaches this week and returns the results
#'   from your process games call.
#' @param simulations Equals the number of times the given NFL season shall be simulated
#' @param sims_per_round The number of \code{simulations} can be split into
#'   multiple rounds and be processed parallel. This parameter controls the number
#'   of simulations per round. The default value determines the number of
#'   locally available cores and calculates the number of simulations per round
#'   to be equal to half of the available cores (various benchmarks showed this
#'   results in optimal performance).
#' @param print_summary If \code{TRUE}, prints the summary statistics to the console.
#' @description This function simulates a given NFL season multiple times using custom functions
#'   to estimate and simulate game results and computes the outcome of the given
#'   season including playoffs and draft order.
#'   It is possible to run the function in parallel processes by calling the
#'   appropriate \link[future]{plan}.
#'   Progress updates can be activated by calling \link[progressr]{handlers}
#'   before the start of the simulations.
#'   Please see the below given section "Details" for further information.
#' @details ## More Speed Using Parallel Processing
#' We recommend choosing a default parallel processing method and saving it
#' as an environment variable in the R user profile to make sure all futures
#' will be resolved with the chosen method by default.
#' This can be done by following the below given steps.
#'
#' First, run the following line and the user profile should be opened automatically.
#' If you haven't saved any environment variables yet, this will be an empty file.
#' ```
#' usethis::edit_r_environ()
#'```
#' In the opened file add the next line, then save the file and restart your R session.
#' Please note that this example sets "multisession" as default. For most users
#' this should be the appropriate plan but please make sure it truly is.
#' ```
#' R_FUTURE_PLAN="multisession"
#' ```
#' After the session is freshly restarted please check if the above method worked
#' by running the next line. If the output is `FALSE` you successfully set up a
#' default non-sequential [future::plan()]. If the output is `TRUE` all functions
#' will behave like they were called with [purrr::map()] and NOT in multisession.
#' ```
#' inherits(future::plan(), "sequential")
#' ```
#' For more information on possible plans please see
#' [the future package Readme](https://github.com/HenrikBengtsson/future/blob/develop/README.md).
#'
#' ## Get Progress Updates while Functions are Running
#'
#' Most nflfastR functions are able to show progress updates
#' using [progressr::progressor()] if they are turned on before the function is
#' called. There are at least two basic ways to do this by either activating
#' progress updates globally (for the current session) with
#' ```
#' progressr::handlers(global = TRUE)
#' ```
#' or by piping the function call into [progressr::with_progress()]:
#' ```
#' simulate_nfl(2020, fresh_season = TRUE) %>%
#'   progressr::with_progress()
#' ```
#'
#' For more information how to work with progress handlers please see [progressr::progressr].
#' @returns An `nflseedR_simulation` object containing a list of 6 data frames
#'   data frames with the results of all simulated games,
#'   the final standings in each simulated season (incl. playoffs and draft order),
#'   summary statistics across all simulated seasons, and the siumulation parameters. For a full list,
#'   please see [the package website](https://nflseedr.com/articles/articles/nflsim.html#simulation-output).
#' @seealso The examples [on the package website](https://nflseedr.com/articles/articles/nflsim.html)
#' @seealso The method [nflseedR::summary.nflseedR_simulation()] that creates a pretty html summary table.
#' @export
#' @examples
#' \donttest{
#' library(nflseedR)
#'
#' # Activate progress updates
#' # progressr::handlers(global = TRUE)
#'
#' # Parallel processing can be activated via the following line
#' # future::plan("multisession")
#'
#' # Simulate the season 4 times in 2 rounds
#' sim <- nflseedR::simulate_nfl(
#'   nfl_season = 2020,
#'   fresh_season = TRUE,
#'   simulations = 4,
#'   sims_per_round = 2
#' )
#'
#' # Overview output
#' dplyr::glimpse(sim)
#' }
simulate_nfl <- function(nfl_season = NULL,
                         process_games = NULL,
                         ...,
                         playoff_seeds = ifelse(nfl_season >= 2020, 7, 6),
                         if_ended_today = FALSE,
                         fresh_season = FALSE,
                         fresh_playoffs = FALSE,
                         tiebreaker_depth = 3,
                         test_week = NULL,
                         simulations = 1000,
                         sims_per_round = max(ceiling(simulations / future::availableCores() * 2), 100),
                         .debug = FALSE,
                         print_summary = FALSE) {

  # Define simple estimate and simulate functions

  if (is.null(process_games)) {
    process_games <- function(teams, games, week_num, ...) {
      # teams = teams data
      # games = games data
      #
      # this example estimates at PK/0 and 50%
      # estimate = is the median spread expected (positive = home team favored)
      # wp = is the probability of the team winning the game
      #
      # only simulate games through week week_num
      # only simulate games with is.na(result)
      # result = how many points home team won by

      # round out (away from zero)
      round_out <- function(x) {
        x[!is.na(x) & x < 0] <- floor(x[!is.na(x) & x < 0])
        x[!is.na(x) & x > 0] <- ceiling(x[!is.na(x) & x > 0])
        return(x)
      }

      # get elo if not in teams data already
      if (!("elo" %in% colnames(teams))) {
        args <- list(...)
        if ("elo" %in% names(args)) {
          # pull from custom arguments
          teams <- teams %>%
            dplyr::inner_join(args$elo %>% select(team, elo), by = c("team" = "team"), multiple = "all")
        } else {
          # start everyone at a random default elo
          ratings <- tibble(
            team = unique(teams$team),
            elo = rnorm(length(unique(team)), 1500, 150)
          )
          teams <- teams %>%
            dplyr::inner_join(ratings, by = "team", multiple = "all")
        }
      }

      # pull ratings from teams data
      ratings <- teams %>% select(sim, team, elo)

      # mark estimate, wp, and result for games
      games <- games %>%
        dplyr::inner_join(ratings, by = c("sim" = "sim", "away_team" = "team"), multiple = "all") %>%
        dplyr::rename(away_elo = elo) %>%
        dplyr::inner_join(ratings, by = c("sim" = "sim", "home_team" = "team"), multiple = "all") %>%
        dplyr::rename(home_elo = elo) %>%
        dplyr::mutate(
          elo_diff = home_elo - away_elo,
          elo_diff = elo_diff + ifelse(location == "Home", 20, 0),
          elo_diff = elo_diff + (home_rest - away_rest) / 7 * 25,
          elo_diff = elo_diff * ifelse(game_type == "REG", 1, 1.2),
          wp = 1 / (10^(-elo_diff / 400) + 1),
          estimate = elo_diff / 25,
          result = case_when(
            is.na(result) & week == week_num ~
            as.integer(round_out(rnorm(n(), estimate, 13))),
            TRUE ~ as.integer(result)
          ),
          outcome = case_when(
            is.na(result) ~ NA_real_,
            result > 0 ~ 1,
            result < 0 ~ 0,
            TRUE ~ 0.5
          ),
          elo_input = case_when(
            is.na(result) ~ NA_real_,
            result > 0 ~ elo_diff * 0.001 + 2.2,
            result < 0 ~ -elo_diff * 0.001 + 2.2,
            TRUE ~ 1.0,
          ),
          elo_mult = log(pmax(abs(result), 1) + 1.0) * 2.2 / elo_input,
          elo_shift = 20 * elo_mult * (outcome - wp)
        ) %>%
        dplyr::select(
          -away_elo, -home_elo, -elo_diff, -wp, -estimate,
          -outcome, -elo_input, -elo_mult
        )

      # apply elo shifts
      teams <- teams %>%
        dplyr::left_join(games %>%
          filter(week == week_num) %>%
          select(sim, away_team, elo_shift),
        by = c("sim" = "sim", "team" = "away_team"), multiple = "all"
        ) %>%
        dplyr::mutate(elo = elo - ifelse(!is.na(elo_shift), elo_shift, 0)) %>%
        dplyr::select(-elo_shift) %>%
        dplyr::left_join(games %>%
          filter(week == week_num) %>%
          select(sim, home_team, elo_shift),
        by = c("sim" = "sim", "team" = "home_team"), multiple = "all"
        ) %>%
        dplyr::mutate(elo = elo + ifelse(!is.na(elo_shift), elo_shift, 0)) %>%
        dplyr::select(-elo_shift)

      # remove elo shift
      games <- games %>%
        dplyr::select(-elo_shift)

      return(list(teams = teams, games = games))
    }
  }

  # Catch invalid input

  if (!all(
    is.null(nfl_season) || is_single_digit_numeric(nfl_season),
    is.null(test_week) || is_single_digit_numeric(test_week),
    is_single_digit_numeric(tiebreaker_depth),
    is_single_digit_numeric(simulations),
    is_single_digit_numeric(sims_per_round)
  )) {
    stop(
      "One or more of the parameters `nfl_season`, `tiebreaker_depth`, `test_week`, ",
      "`simulations` and `sims_per_round` are not single digit numeric values!"
    )
  }

  if (!is.function(process_games)) {
    stop("The parameter `process_games` has to be a function!")
  }

  if (nfl_season < 2002) {
    stop("The earliest season that can be simulated is 2002.")
  }

  #### LOAD DATA ####

  # load games data
  report("Loading games data")
  schedule <- load_sharpe_games() %>%
    select(
      season, game_type, week, away_team, home_team,
      away_rest, home_rest, location, result
    )

  if (is.null(nfl_season)) {
    nfl_season <- max(schedule$season)
  }

  schedule <- schedule %>%
    filter(season == nfl_season) %>%
    select(-season)

  if (nrow(schedule) == 0)
  {
    fn <- glue::glue("https://github.com/nflverse/nfldata/blob/master/fake_schedule_{nfl_season}.csv?raw=true")
    tryCatch({
      options(readr.num_columns = 0)
      schedule <- readr::read_csv(fn)
      sim_info(glue::glue("No actual schedule exists for {nfl_season}, using fake schedule with correct opponents"))
    }, error = function(cond) {
      stop("Unable to locate a schedule for ", nfl_season)
    })
  }

  #### PREPROCESSING ####

  # if simulating fresh season, clear out all results and playoff games
  if (isTRUE(fresh_season)) {
    schedule <- schedule %>%
      filter(game_type == "REG") %>%
      mutate(result = NA_real_)
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
  if (!is.null(test_week)) {
    sim_rounds <- 1
  }

  if (sim_rounds > 1 && is_sequential()) {
    sim_info(c(
      "Computation in multiple rounds can be accelerated with parallel processing.",
      "You should consider calling a `future::plan()`. Please see the function documentation for further information.",
      "Will go on sequentially..."
    ))
  }

  report(glue("Beginning simulation of {simulations} seasons in {sim_rounds} {ifelse(sim_rounds == 1, 'round', 'rounds')}"))

  p <- progressr::progressor(along = seq_len(sim_rounds))

  run <- quote({
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
      test_week = test_week,
      .debug = .debug,
      playoff_seeds = playoff_seeds,
      p = p,
      .options = furrr::furrr_options(seed = TRUE)
    )
  })

  if (isTRUE(.debug)) eval(run) else suppressMessages(eval(run))

  if (!is.null(test_week)) {
    report(glue(
      "Aborting and returning your `process_games` function's results from Week {test_week}"
    ))
    return(all[[1]])
  }

  report("Combining simulation data")

  all_teams <- furrr::future_map_dfr(all, ~ .x$teams)
  all_games <- furrr::future_map_dfr(all, ~ .x$games)

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

  team_wins <-
    tibble(
      team = rep(sort(unique(all_teams$team)), each = max(all_teams$games) * 2 + 1),
      wins = rep(seq(0, max(all_teams$games), 0.5), length(unique(all_teams$team)))
    ) %>%
    inner_join(
      all_teams %>% select(team, true_wins),
      by = c("team"), multiple = "all"
    ) %>%
    group_by(team, wins) %>%
    summarize(
      over_prob = mean(true_wins > wins),
      under_prob = mean(true_wins < wins)
    ) %>%
    ungroup()

  game_summary <-
    all_games %>%
    group_by(game_type, week, away_team, home_team) %>%
    summarise(
      away_wins = sum(result < 0),
      home_wins = sum(result > 0),
      ties = sum(result == 0),
      result = mean(result),
      # != number of simulations in the postseason
      games_played = away_wins + home_wins + ties,
      away_percentage = (away_wins + 0.5 * ties) / games_played,
      home_percentage = (home_wins + 0.5 * ties) / games_played
    ) %>%
    ungroup() %>%
    arrange(week)

  if (isTRUE(print_summary)) print(overall)

  out <- structure(
    list(
      "teams" = all_teams,
      "games" = all_games,
      "overall" = overall,
      "team_wins" = team_wins,
      "game_summary" = game_summary,
      "sim_params" = list(
        "nfl_season" = nfl_season,
        "playoff_seeds" = playoff_seeds,
        "if_ended_today" = if_ended_today,
        "fresh_season" = fresh_season,
        "fresh_playoffs" = fresh_playoffs,
        "tiebreaker_depth" = tiebreaker_depth,
        "test_week" = test_week,
        "simulations" = simulations,
        "sims_per_round" = sims_per_round,
        ".debug" = .debug,
        "print_summary" = print_summary
      )
    ),
    class = "nflseedR_simulation"
  )

  out
}
