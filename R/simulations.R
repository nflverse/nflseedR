#' Simulate an NFL Season
#'
#' @description
#' Simulate NFL games based on a user provided games/schedule object that
#' holds matchups with and without results. Missing results are computed using
#' the argument `compute_results` and possible further arguments to `compute_results`
#' in `...`.
#' It is possible to let the function calculate playoff participants
#' and simulate the post-season.
#' The code is also developed for maximum performance and allows parallel
#' computation by splitting the number of simulations into chunks and calling the
#' appropriate \link[future]{plan}.
#' Progress updates can be activated by calling \link[progressr]{handlers}
#' before the start of the simulations.
#' Please see the below given section "Details" for further information.
#'
#' @inheritParams nfl_standings
#' @param compute_results A function to compute results of games. Uses team,
#'   schedule, and week number as arguments.
#' @param ... Additional parameters passed on to the function \code{compute_results}.
#' @param simulations Equals the number of times the given NFL season shall be simulated
#' @param chunks The number of chunks \code{simulations} should be split into
#'   multiple rounds and be processed parallel. This parameter controls the number
#'   of simulations per round. The default value determines the number of
#'   locally available cores and calculates the number of simulations per round
#'   to be equal to half of the available cores (various benchmarks showed this
#'   results in optimal performance).
#' @param byes_per_conf The number of teams with a playoff bye week per conference.
#'   This number influences the number of wildcard games that are simulated.
#' @param sim_include One of `"REG"`, `"POST"`, `"DRAFT"` (the default).
#'   Simulation will behave as follows:
#'   - `"REG"`: Simulate the regular season and compute standings, division ranks, and playoff seeds
#'   - `"POST"`: Do `"REG"` + simulate the postseason
#'   - `"DRAFT"` (default): Do `"POST"` + compute draft order
#'
#' @details ## More Speed Using Parallel Processing
#' We recommend choosing a default parallel processing method and saving it
#' as an environment variable in the R user profile to make sure all futures
#' will be resolved with the chosen method by default.
#' This can be done by following the below given steps.
#'
#' First, run the below line and the user profile should be opened automatically.
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
#' will behave like they were called with [purrr::map()] and **NOT** in multisession.
#' ```
#' inherits(future::plan(), "sequential")
#' ```
#' For more information on possible plans please see
#' [the future package Readme](https://github.com/HenrikBengtsson/future/blob/develop/README.md).
#'
#' ## Get Progress Updates while Functions are Running
#'
#' nflseedR is able to show progress updates
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
#'   summary statistics across all simulated seasons, and the simulation parameters. For a full list,
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
#' try({#to avoid CRAN test problems
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
#' })
#' }
nfl_simulations <- function(games,
                            compute_results = default_compute_results,
                            ...,
                            playoff_seeds = 7L,
                            simulations = 10000L,
                            chunks = 8L,
                            byes_per_conf = 1L,
                            tiebreaker_depth = c("SOS", "PRE-SOV", "RANDOM"),
                            sim_include = c("DRAFT", "REG", "POST"),
                            verbosity = c("MIN", "MAX", "NONE")) {

  # VALIDATE INPUT ----------------------------------------------------------
  games <- sims_validate_games(games)
  tiebreaker_depth <- rlang::arg_match(tiebreaker_depth)
  sim_include <- rlang::arg_match(sim_include)
  sim_include <- switch (sim_include,
    "REG" = 0L,
    "POST" = 1L,
    "DRAFT" = 2L
  )
  verbosity <- rlang::arg_match(verbosity)
  verbosity <- switch (verbosity,
    "MIN" = 1L,
    "MAX" = 2L,
    "NONE" = 0L
  )

  # No NA results means
  if (all(!is.na(games$result))){

  }
  # if (!all(
  #   is.null(test_week) || is_single_digit_numeric(test_week),
  #   is_single_digit_numeric(simulations),
  #   is_single_digit_numeric(sims_per_round)
  # )) {
  #   cli::cli_abort(
  #     "One or more of the parameters \\
  #     {.arg test_week}, {.arg simulations} and {.arg sims_per_round} are not \\
  #     single digit numeric values!"
  #   )
  # }
  if (!is.function(compute_results)) {
    cli::cli_abort("The {.arg compute_results} argument must be a function!")
  }
  if (chunks > 1 && is_sequential()) {
    cli::cli_inform(c(
      "i" = "Computation in multiple chunks can be accelerated
            with parallel processing.",
      "i" = "You should consider calling a {.code future::plan()}.
            Please see the function documentation for further information.",
      "i" = "Will go on sequentially..."
    ), wrap = TRUE
    )
  }

  # PREPARE SIMULATIONS -----------------------------------------------------
  weeks_to_simulate <- games[is.na(result), unique(week)]
  if (sim_include == 0L && any(playoff_weeks() %chin% weeks_to_simulate)){
    cli::cli_abort(
      "Detected post-season games to simulate but you have set \
      {.arg sim_include} to {.val REG}."
    )
  }
  teams <- data.table::as.data.table(nflseedR::divisions)
  teams <- teams[team %chin% games$away_team | team %chin% games$home_team]

  # User asked for playoff simulation. Append missing playoff weeks to games
  if (sim_include > 0L){
    playoff_dummy <- sims_compute_playoff_dummy(num_byes = byes_per_conf)
    # If games already list some or all playoff weeks, we gotta remove them
    # from the dummy
    playoff_dummy <- playoff_dummy[!week %in% games$week]
    # attach playoff games to games. If the above filter resulted in a empty
    # table then nothing happens. We use fill = TRUE because the playoff_dummy
    # doesn't have "old_week"
    games <- rbind(games, playoff_dummy, fill = TRUE)
    # Now add old_week numbers by adding playoff summands to the last
    # reg season week
    max_reg_week <- games[game_type == "REG", max(old_week)]
    games[is.na(old_week), old_week := max_reg_week + playoff_summands()[game_type]]
  }

  # Calculate chunk size from the number of simulations and chunks
  # Check chunk size afterwards to make sure that the requested number of sims
  # can be evenly distributed over the number of requested chunks
  # It's probably not absolutely necessary to error if chunk_size * nchunks != nsims
  # but it's easier to catch this here and force users to provide better inputs.
  chunk_size <- sims_calculate_chunk_size(nsims = simulations, nchunks = chunks)
  sims_check_chunk_size(nsims = simulations, nchunks = chunks, chunk_size = chunk_size)

  # What this should do:
  # create games and teams in the size of one chunk instead of the complete
  # size. This reduces the size of data spread across sessions in multisession
  #
  # pass the vector of sim identifiers to simulate_chunk and extract the
  # correct part of this vector to put it into the games and teams data that are
  # in chunk size.
  #
  # if postseason shall be simulated, calculate an empty postseason table and
  # append it to games BEFORE it is used to create the chunk_games data
  # This means that the complete table of games that are to be simulated
  # is passed to simulate_chunks. Postseason is missing participating teams tho
  #
  # after the remainder of regular season games is simulated, calculate standings,
  # and fill in missing teams to participate in playoffs. This has to be done in
  # a loop because we have to figure out winners.
  #
  # when preparing chunks we also have to check if there are any regular season
  # games remaining. If not, we can calc standings and move on with playoffs.

  # Repeat games and teams to fit the chunk size
  game_number <- nrow(games)
  sim_games <- games[rep(seq_len(game_number), times = chunk_size)]
  team_number <- nrow(teams)
  sim_teams <- teams[rep(seq_len(team_number), times = chunk_size)]

  # Compute vectors of simulation identifiers
  games_sim_vec <- rep(seq_len(simulations), each = game_number)
  teams_sim_vec <- rep(seq_len(simulations), each = team_number)

  # RUN SIMULATIONS ---------------------------------------------------------
  report(
    "Start simulation of {.pkg {prettyNum(simulations, big.mark = ' ')}} \\
    {cli::qty(simulations)}season{?s} in {.val {chunks}} chunk{?s} with a chunk \\
    size of {.pkg {prettyNum(chunk_size, big.mark = ' ')}}."
  )
  p <- progressr::progressor(along = seq_len(chunks))
  all <- furrr::future_map(
    .x = seq_len(chunks),
    .f = simulate_chunk,
    compute_results = default_compute_results,
    ...,
    games_sim_vec = games_sim_vec,
    teams_sim_vec = teams_sim_vec,
    weeks_to_simulate = weeks_to_simulate,
    nsims = simulations,
    nchunks = chunks,
    sim_games = sim_games,
    sim_teams = sim_teams,
    tiebreaker_depth = tiebreaker_depth,
    verbosity = verbosity,
    playoff_seeds = playoff_seeds,
    byes_per_conf = byes_per_conf,
    p = p,
    sim_include = sim_include,
    .options = furrr::furrr_options(seed = TRUE)
  )

  # POSTPROCESS SIMULATIONS -------------------------------------------------
  if (verbosity > 0L) report("Combine simulation data")
  # `all` is a list of chunks where every chunk is containing the tables
  # "teams" and "games". We loop over the list (that's not really bad
  # because the length of the loop only is the number of chunks) and
  # bind with data.table afterwards
  all_teams <- data.table::rbindlist(lapply(all, function(i) i[["teams"]]))
  all_games <- data.table::rbindlist(lapply(all, function(i) i[["games"]]))

  if (verbosity > 0L) report("Aggregate across simulations")
  # we need the exit number of the sb winner to compute sb and conf percentages
  # with "exit" because draft_order might not be available depending on the
  # value of `sim_include`. Need to remove NAs here because Exit will be NA
  # for postseason teams
  # sb_exit <- max(all_teams$exit, na.rm = TRUE)
  # If we simulate regular season only this will be < 20. We don't really simulate
  # postseason then and set sb_exit to NA which result in NA percentages of sb
  # and conf columns
  # if(sb_exit < 20) sb_exit <- NA_real_

  overall <- all_teams[, list(
    wins = mean(wins),
    playoff = mean(conf_rank <= playoff_seeds),
    div1 = mean(div_rank == 1),
    seed1 = mean(!is.na(conf_rank) & conf_rank == 1),
    won_conf = if (sim_include > 0L) mean(grepl("SB", exit)) else NA_real_,
    won_sb = if (sim_include > 0L) mean(exit == "SB_WIN") else NA_real_,
    draft1 = if (sim_include > 1L) mean(draft_order == 1) else NA_real_,
    draft5 = if (sim_include > 1L) mean(draft_order <= 5) else NA_real_
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
  ) |>
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

  if (verbosity > 0L) report("DONE!")

  out <- structure(
    list(
      "teams" = tibble::as_tibble(all_teams),
      "games" = tibble::as_tibble(all_games),
      "overall" = tibble::as_tibble(overall),
      "team_wins" = tibble::as_tibble(team_wins),
      "game_summary" = tibble::as_tibble(game_summary),
      "sim_params" = list(
        "playoff_seeds" = playoff_seeds,
        "tiebreaker_depth" = tiebreaker_depth,
        # "test_week" = test_week,
        "simulations" = simulations,
        "chunks" = chunks,
        "verbosity" = verbosity,
        "sim_include" = sim_include,
        "nflseedR_version" = utils::packageVersion("nflseedR"),
        "finished_at" = Sys.time()
      )
    ),
    class = "nflseedR_simulation"
  )

  out
}
