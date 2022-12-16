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
#' @param sim_include One of `"REG"`, `"POST"`, `"DRAFT"` (the default).
#'   Simulation will behave as follows:
#'   \describe{
#'     \item{REG}{Simulate the regular season and compute standings, division ranks, and playoff seeds}
#'     \item{POST}{Do REG + simulate the postseason}
#'     \item{DRAFT}{Do POST + compute draft order}
#'   }
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
                         print_summary = FALSE,
                         sim_include = c("DRAFT", "REG", "POST")) {

  sim_include <- rlang::arg_match0(sim_include, c("REG", "POST", "DRAFT"))

  # Define simple estimate and simulate functions

  if (is.null(process_games)) process_games <- default_process_games

  # Catch invalid input

  if (!all(
    is.null(nfl_season) || is_single_digit_numeric(nfl_season),
    is.null(test_week) || is_single_digit_numeric(test_week),
    is_single_digit_numeric(tiebreaker_depth),
    is_single_digit_numeric(simulations),
    is_single_digit_numeric(sims_per_round)
  )) {
    cli::cli_abort(
      "One or more of the parameters {.arg nfl_season}, {.arg tiebreaker_depth}, \\
      {.arg test_week}, {.arg simulations} and {.arg sims_per_round} are not \\
      single digit numeric values!"
    )
  }

  if (!is.function(process_games)) {
    cli::cli_abort("The parameter {.arg process_games} has to be a function!")
  }

  if (nfl_season < 2002) {
    cli::cli_abort("The earliest season that can be simulated is 2002.")
  }

  #### LOAD DATA ####

  # load games data
  report("Loading games data")
  schedule <- nflreadr::load_schedules() %>%
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
    fn <- paste0(
      "https://github.com/nflverse/nfldata/blob/master/fake_schedule_",
      nfl_season,
      ".csv?raw=true"
    )
    tryCatch({
      schedule <- data.table::fread(fn)
      cli::cli_alert_info("No actual schedule exists for {.val {nfl_season}}, using fake schedule with correct opponents.")
    }, error = function(cond) {
      cli::cli_abort("Unable to locate a schedule for {.val {nfl_season}}")
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
    cli::cli_inform(c(
      "i" = "Computation in multiple rounds can be accelerated
            with parallel processing.",
      "i" = "You should consider calling a {.code future::plan()}.
            Please see the function documentation for further information.",
      "i" = "Will go on sequentially..."
    ), wrap = TRUE
    )
  }

  report(
    "Beginning simulation of {.val {simulations}} season{?s} \\
    in {.val {sim_rounds}} round{?s}"
  )

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
      sim_include = sim_include,
      .options = furrr::furrr_options(seed = TRUE)
    )
  })

  if (isTRUE(.debug)) eval(run) else suppressMessages(eval(run))

  if (!is.null(test_week)) {
    report(
      "Aborting and returning your {.code process_games} function's \\
      results from Week {test_week}"
      , wrap = TRUE
    )
    return(all[[1]])
  }

  report("Combining simulation data")

  # `all` is a list of rounds where every round is containing the dataframes
  # "teams" and "games". We loop over the list with purrr (that's not really bad
  # because the length of the loop only is the number of rounds) but don't
  # convert to a dataframe/tibble because dplyr::bind_rows() is too slow.
  # Instead, we bind with data.table afterwards, it's a reverse dependency
  # through nflreadr anyways.
  all_teams <- data.table::rbindlist(purrr::map(all, ~ .x$teams))
  all_games <- data.table::rbindlist(purrr::map(all, ~ .x$games))

  report("Aggregating across simulations")

  # we need the exit number of the sb winner to compute sb and conf percentages
  # with "exit" because draft_order might not be available depending on the
  # value of `sim_include`. Need to remove NAs here because Exit will be NA
  # for postseason teams
  sb_exit <- max(all_teams$exit, na.rm = TRUE)
  # If we simulate regular season only this will be < 20. We don't really simulate
  # postseason then and set sb_exit to NA which result in NA percentages of sb
  # and conf columns
  if(sb_exit < 20) sb_exit <- NA_real_

  overall <- all_teams[, list(
    wins = mean(wins),
    playoff = mean(!is.na(seed)),
    div1 = mean(div_rank == 1),
    seed1 = mean(!is.na(seed) & seed == 1),
    won_conf = mean(exit >= sb_exit - 1),
    won_sb = mean(exit == sb_exit),
    draft1 = mean(draft_order == 1),
    draft5 = mean(draft_order <= 5)
  ), keyby = c("conf", "division", "team")]

  # take all teams and repeat them for each half win and repeat this for each
  # simulation. The length of the half win sequence equals 2 * games + 1
  team_vec <- rep(
    sort(unique(all_teams$team)),
    each = (max(all_teams$games) * 2 + 1) * length(unique(all_teams$sim))
  )

  # Create the win sequence vector and repeat every win for every sim
  # Take this and repeat it for every team
  wins_vec <- rep(
    seq(0, max(all_teams$games), 0.5),
    each = length(unique(all_teams$sim))
  ) %>%
    rep(length(unique(all_teams$team)))

  # create sequence of sims and repeat it for every half win and for every team
  sims_vec <- rep(
    sort(unique(all_teams$sim)),
    (max(all_teams$games) * 2 + 1) * length(unique(all_teams$team))
  )

  team_wins <- data.table(
    sim = sims_vec,
    team = team_vec,
    wins = wins_vec,
    key = c("team", "wins")
  ) %>%
    merge(
      all_teams[,list(sim, team, true_wins)],
      by = c("sim", "team"),
      sort = FALSE
    )

  team_wins <- team_wins[,list(
    over_prob = mean(true_wins > wins),
    under_prob = mean(true_wins < wins)
  ), keyby = c("team", "wins")]


  ## Game Summary
  game_summary <- all_games[,list(
    away_wins = sum(result < 0),
    home_wins = sum(result > 0),
    ties = sum(result == 0),
    result = mean(result)
  ), keyby = c("game_type", "week", "away_team", "home_team")]
  game_summary[, games_played := away_wins + home_wins + ties]
  game_summary[,`:=`(
    away_percentage = (away_wins + 0.5 * ties) / games_played,
    home_percentage = (home_wins + 0.5 * ties) / games_played
  )]

  report("DONE!")

  if (isTRUE(print_summary)) print(overall)

  out <- structure(
    list(
      "teams" = tibble::as_tibble(all_teams),
      "games" = tibble::as_tibble(all_games),
      "overall" = tibble::as_tibble(overall),
      "team_wins" = tibble::as_tibble(team_wins),
      "game_summary" = tibble::as_tibble(game_summary),
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
        "print_summary" = print_summary,
        "sim_include" = sim_include,
        "nflseedR_version" = utils::packageVersion("nflseedR"),
        "finished_at" = Sys.time()
      )
    ),
    class = "nflseedR_simulation"
  )

  out
}


default_process_games <- function(teams, games, week_num, ...) {
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
        dplyr::inner_join(args$elo %>% select(team, elo), by = c("team" = "team"))
    } else {
      # start everyone at a random default elo
      ratings <- tibble(
        team = unique(teams$team),
        elo = rnorm(length(unique(team)), 1500, 150)
      )
      teams <- teams %>%
        dplyr::inner_join(ratings, by = "team")
    }
  }

  # pull ratings from teams data
  ratings <- teams %>% select(sim, team, elo)

  # mark estimate, wp, and result for games
  games <- games %>%
    dplyr::inner_join(ratings, by = c("sim" = "sim", "away_team" = "team")) %>%
    dplyr::rename(away_elo = elo) %>%
    dplyr::inner_join(ratings, by = c("sim" = "sim", "home_team" = "team")) %>%
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
                     by = c("sim" = "sim", "team" = "away_team")
    ) %>%
    dplyr::mutate(elo = elo - ifelse(!is.na(elo_shift), elo_shift, 0)) %>%
    dplyr::select(-elo_shift) %>%
    dplyr::left_join(games %>%
                       filter(week == week_num) %>%
                       select(sim, home_team, elo_shift),
                     by = c("sim" = "sim", "team" = "home_team")
    ) %>%
    dplyr::mutate(elo = elo + ifelse(!is.na(elo_shift), elo_shift, 0)) %>%
    dplyr::select(-elo_shift)

  # remove elo shift
  games <- games %>%
    dplyr::select(-elo_shift)

  return(list(teams = teams, games = games))
}

# rewritten in data.table
default_process_games_dt <- function(teams, games, week_num, ...) {
  cli::cli_progress_step(
    "Compute week {.val #{week_num}}"
  )
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

  setDT(games, key = c("sim", "week"))
  setDT(teams, key = c("sim", "team"))

  # get elo if not in teams data already
  if (!("elo" %in% colnames(teams))) {
    args <- list(...)
    if ("elo" %in% names(args)) {
      # pull from custom arguments
      ratings <- setDT(args$elo, key = "team")
      teams <- merge(teams, ratings[,list(team, elo)])
    } else {
      # start everyone at a random default elo
      ratings <- data.table(
        team = unique(teams$team),
        elo = rnorm(length(unique(teams$team)), 1500, 150),
        key = "team"
      )
      teams <- merge(teams, ratings)
    }
  }

  # merge elo values to home and away teams
  games <- merge(x = games, y = teams[,list(sim, team, away_elo = elo)],
                 by.x = c("sim", "away_team"),
                 by.y = c("sim", "team"),
                 sort = FALSE)
  games <- merge(x = games, y = teams[,list(sim, team, home_elo = elo)],
                 by.x = c("sim", "home_team"),
                 by.y = c("sim", "team"),
                 sort = FALSE)

  # create elo diff
  games[, elo_diff := home_elo - away_elo + (home_rest - away_rest) / 7 * 25]
  # adjust elo diff for location = HOME
  games["Home", elo_diff := elo_diff + 20, on = "location"]
  # adjust elo_diff for game type = REG
  games["REG", elo_diff := elo_diff * 1.2, on = "game_type"]
  # create wp and estimate
  games[, `:=`(wp = 1 / (10^(-elo_diff / 400) + 1),
               estimate = elo_diff / 25)]
  # adjust result in current week
  games[week_num == week & is.na(result),
        result := as.integer(round_out(rnorm(.N, estimate, 13)))]
  # compute elo shift
  games[, `:=`(
    outcome = fcase(
      is.na(result), NA_real_,
      result > 0, 1,
      result < 0, 0,
      default = 0.5
    ),
    elo_input = fcase(
      is.na(result), NA_real_,
      result > 0, elo_diff * 0.001 + 2.2,
      result < 0, -elo_diff * 0.001 + 2.2,
      default = 1.0
    )
  )]
  games[, elo_mult := log(pmax(abs(result), 1) + 1.0) * 2.2 / elo_input]
  games[, elo_shift := 20 * elo_mult * (outcome - wp)]

  # drop irrelevant columns
  drop_cols <- c("away_elo", "home_elo", "elo_diff", "wp", "estimate",
                 "outcome", "elo_input", "elo_mult")
  games[, (drop_cols) := NULL]

  # apply away team elo shift
  away_teams <- games[list(week_num),
                      list(sim, team = away_team, elo_shift = -elo_shift),
                      on = "week"]
  teams <- merge(teams, away_teams, by = c("sim", "team"), all = TRUE)
  teams[!is.na(elo_shift), elo := elo + elo_shift]
  teams[, elo_shift := NULL]

  # apply home team elo shift
  home_teams <- games[list(week_num),
                      list(sim, team = home_team, elo_shift),
                      on = "week"]
  teams <- merge(teams, home_teams, by = c("sim", "team"), all = TRUE)
  teams[!is.na(elo_shift), elo := elo + elo_shift]
  teams[, elo_shift := NULL]

  # remove elo shift
  games[, elo_shift := NULL]

  list("teams" = teams, "games" = games)
}
