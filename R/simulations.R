#' Simulate an NFL Season
#'
#' @description
#' Simulate NFL games based on a user provided games/schedule object that
#' holds matchups with and without results. Missing results are computed using
#' the argument `compute_results` and possible further arguments to
#' `compute_results` in `...` (please see [simulations_verify_fct] for
#' further information.).
#'
#' It is possible to let the function calculate playoff participants
#' and simulate the post-season.
#' The code is also developed for maximum performance and allows parallel
#' computation by splitting the number of simulations into chunks and calling the
#' appropriate [future::plan].
#' Progress updates can be activated by calling [progressr::handlers]
#' before the start of the simulations.
#' Please see the below given section "Details" for further information.
#'
#' @inheritParams nfl_standings
#' @param compute_results Defaults to the nflseedR function `nflseedR_compute_results`.
#' A function to compute results of games. Uses team, schedule, and week number
#' as arguments. Please see [simulations_verify_fct] for further information.
#' @param ... Additional parameters passed on to the function `compute_results`.
#' @param simulations Equals the number of times the given NFL season shall be simulated
#' @param chunks The number of chunks `simulations` should be split into
#'   and potentially be processed parallel. This parameter controls the number
#'   of simulations per chunk. There is no obvious way to determine the ideal
#'   number of chunks in advance because there are too many dependencies on the
#'   hardware. Too many chunks can be just as slow as too few. It is therefore
#'   up to the user to determine the optimum number themselves.
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
#' nflseedR::nfl_simulations(
#'   games = nflseedR::sims_games_example,
#'   simulations = 4,
#'   chunks = 2
#' ) |>
#'   progressr::with_progress()
#' ```
#' For more information how to work with progress handlers please see
#' [progressr::progressr].
#'
#' ## Reproducible Random Number Generation (RNG)
#' It is to be expected that some form of random number generation is required
#' in the function in argument `compute_results`.
#' For better performance, nflseedR uses the furrr package to parallelize chunks.
#' furrr functions are guaranteed to generate the exact same sequence of random
#' numbers given the same initial seed if, and only if, the initial seed is of
#' the type "L'Ecuyer-CMRG".
#' So if you want a consistent seed to be used across all chunks, you must ensure
#' that the correct type is specified in `set.seed`, e.g. with the following code
#' ```
#' set.seed(5, "L'Ecuyer-CMRG")
#' ```
#' It is sufficient to set the seed before nfl_simulations is called.
#' To check that the type has been set correctly, you can use the following code.
#'
#' ```
#' RNGkind()
#' "L'Ecuyer-CMRG" "Inversion"     "Rejection"
#'
#' # Should be a integer vector of length 7
#' .Random.seed
#' 10407  1157214768 -1674567567 -1532971138 -1249749529  1302496508  -253670963
#' ```
#' For more information, please see the section "Reproducible random number
#' generation (RNG)" in [furrr::furrr_options].
#'
#' @returns An `nflseedR_simulation` object containing a list of 6
#'   data frames with the results of all simulated games,
#'   the final standings in each simulated season,
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
#' sim <- nflseedR::nfl_simulations(
#'   games = nflseedR::sims_games_example,
#'   simulations = 4,
#'   chunks = 2
#' )
#'
#' # Overview output
#' str(sim, max.level = 3)
#' }
nfl_simulations <- function(games,
                            compute_results = nflseedR_compute_results,
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
  nfl_season <- attr(games, "season")
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

  if (!is.function(compute_results)) {
    cli::cli_abort("The {.arg compute_results} argument must be a function!")
  }

  if (!all(
    is_single_digit_numeric(playoff_seeds),
    is_single_digit_numeric(simulations),
    is_single_digit_numeric(chunks),
    is_single_digit_numeric(byes_per_conf)
  )) {
    args <- c("playoff_seeds", "simulations", "chunks", "byes_per_conf")
    cli::cli_abort(
      "One or more of the arguments {.arg {args}} are not single digit numeric values!"
    )
  }

  # Remind user to validate compute_results
  if (as.character(substitute(compute_results)) != "nflseedR_compute_results"){
    cli::cli_inform(c(
      "i" = "You have provided your own function to {.arg compute_results}, cool!",
      "i" = "To maximize performance, {.fun nfl_simulations} does not control the output
      of your function during the simulation. Please use {.fun simulations_verify_fct}
      in advance to ensure that you do not get any unexpected results or errors."),
      .frequency = "regularly",
      .frequency_id = "user_defined_function"
    )
  }
  if (chunks > 1L && is_sequential()) {
    cli::cli_inform(c(
      "i" = "Computation in multiple chunks can be accelerated
            with parallel processing.",
      "i" = "You should consider calling a {.code future::plan()}.
            Please see the function documentation for further information.",
      "i" = "Will go on sequentially..."
    ),
    wrap = TRUE,
    .frequency = "regularly",
    .frequency_id = "sequential_sim"
    )
  }

  # PREPARE SIMULATIONS -----------------------------------------------------
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

  weeks_to_simulate <- games[is.na(result), unique(week)]
  # If there are no weeks to simulate at this point, then games didn't have
  # any NA results. This only happens if REG season is done but sim_include
  # isn't set to "POST" or if all games are done.
  if (length(weeks_to_simulate) == 0){
    if (sim_include == 0L){
      cli::cli_abort(
        "You have set {.arg sim_include} to {.val REG} but there are no \
        {.val {NA_integer_}} values in the result column of {.arg games}."
      )
    } else {
      cli::cli_abort(
        "It seems like there are no games left to simulate because there are no \
        {.val {NA_integer_}} values in the result column of {.arg games}.\
        Did you pass the wrong season? If you want standings, please see \
        {.fun nfl_standings}."
      )
    }
  }
  if (sim_include == 0L && any(playoff_weeks() %in% weeks_to_simulate)){
    cli::cli_abort(
      "Detected post-season games to simulate but you have set \
      {.arg sim_include} to {.val REG}."
    )
  }
  teams <- data.table::as.data.table(nflseedR::divisions)
  teams <- teams[team %chin% games$away_team | team %chin% games$home_team]

  # Calculate chunk size from the number of simulations and chunks
  # Check chunk size afterwards to make sure that the requested number of sims
  # can be evenly distributed over the number of requested chunks
  # It's probably not absolutely necessary to error if chunk_size * nchunks != nsims
  # but it's easier to catch this here and force users to provide better inputs.
  chunk_size <- sims_calculate_chunk_size(nsims = simulations, nchunks = chunks)
  sims_check_chunk_size(nsims = simulations, nchunks = chunks, chunk_size = chunk_size)

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
    compute_results = nflseedR_compute_results,
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
  all_standings <- data.table::rbindlist(lapply(all, function(i) i[["standings"]]), fill = TRUE)
  all_games <- data.table::rbindlist(lapply(all, function(i) i[["games"]]), fill = TRUE)

  if (verbosity > 0L) report("Aggregate across simulations")
  # we need the exit number of the sb winner to compute sb and conf percentages
  # with "exit" because draft_rank might not be available depending on the
  # value of `sim_include`.
  sb_exit <- max(sims_exit_translate_to("INT"))

  overall <- all_standings[, list(
    wins = mean(wins),
    playoff = mean(!is.na(conf_rank) & conf_rank <= playoff_seeds),
    div1 = mean(div_rank == 1L),
    seed1 = mean(!is.na(conf_rank) & conf_rank == 1L),
    won_conf = if (sim_include > 0L) mean(exit >= (sb_exit - 1L)) else NA_real_,
    won_sb = if (sim_include > 0L) mean(exit == sb_exit) else NA_real_,
    draft1 = if (sim_include > 1L) mean(draft_rank == 1L) else NA_real_,
    draft5 = if (sim_include > 1L) mean(draft_rank <= 5L) else NA_real_
  ), keyby = c("conf", "division", "team")]

  all_standings[, exit := sims_exit_translate_to("CHAR")[as.character(exit)]]

  # take all teams and repeat them for each half win and repeat this for each
  # simulation. The length of the half win sequence equals 2 * games + 1
  team_vec <- rep(
    sort(unique(all_standings$team)),
    each = (max(all_standings$games) * 2L + 1L) * length(unique(all_standings$sim))
  )

  # Create the win sequence vector and repeat every win for every sim
  # Take this and repeat it for every team
  wins_vec <- rep(
    seq(0, max(all_standings$games), 0.5),
    each = length(unique(all_standings$sim))
  ) %>%
    rep(length(unique(all_standings$team)))

  # create sequence of sims and repeat it for every half win and for every team
  sims_vec <- rep(
    sort(unique(all_standings$sim)),
    (max(all_standings$games) * 2L + 1L) * length(unique(all_standings$team))
  )

  team_wins <- data.table(
    sim = sims_vec,
    team = team_vec,
    wins = wins_vec,
    key = c("team", "wins")
  )

  team_wins <- merge(
    team_wins,
    all_standings[,list(sim, team, true_wins)],
    by = c("sim", "team"),
    sort = FALSE
  )

  team_wins <- team_wins[,list(
    over_prob = mean(true_wins > wins),
    under_prob = mean(true_wins < wins)
  ), keyby = c("team", "wins")]


  ## Game Summary
  game_summary <- all_games[,list(
    away_wins = sum(result < 0L),
    home_wins = sum(result > 0L),
    ties = sum(result == 0L),
    result = mean(result)
  ), keyby = c("game_type", "week", "away_team", "home_team")]
  game_summary[, games_played := away_wins + home_wins + ties]
  game_summary[,`:=`(
    away_percentage = (away_wins + 0.5 * ties) / games_played,
    home_percentage = (home_wins + 0.5 * ties) / games_played
  )]

  out <- structure(
    list(
      "standings" = data.table::setDF(all_standings),
      "games" = data.table::setDF(all_games),
      "overall" = data.table::setDF(overall),
      "team_wins" = data.table::setDF(team_wins),
      "game_summary" = data.table::setDF(game_summary),
      "sim_params" = list(
        "nfl_season" = nfl_season,
        "playoff_seeds" = playoff_seeds,
        "simulations" = simulations,
        "chunks" = chunks,
        "byes_per_conf" = byes_per_conf,
        "tiebreaker_depth" = tiebreaker_depth,
        "sim_include" = sim_include,
        "verbosity" = verbosity,
        "nflseedR_version" = utils::packageVersion("nflseedR"),
        "finished_at" = Sys.time()
      )
    ),
    class = "nflseedR_simulation"
  )

  report("DONE!")
  out
}
