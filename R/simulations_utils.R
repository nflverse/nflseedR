playoff_weeks <- function() c("WC", "DIV", "CON", "SB")
playoff_summands <- function(){
  setNames(
    seq_along(playoff_weeks()),
    playoff_weeks()
  )
}

sims_exit_translate_to <- function(to = c("INT", "CHAR")){
  to <- rlang::arg_match(to)
  translation_vec <- c(
    "REG"    = 0L,
    "WC"     = 1L,
    "DIV"    = 2L,
    "CON"    = 3L,
    "SB"     = 4L,
    "SB_WIN" = 5L
  )
  if (to == "INT"){
    translation_vec
  } else {
    setNames(names(translation_vec), translation_vec)
  }
}

sims_calculate_chunk_size <- function(nsims, nchunks) ceiling(nsims / nchunks)

sims_check_chunk_size <- function(nsims, nchunks, chunk_size){
  resulting_sims <- nchunks * chunk_size
  if (resulting_sims != nsims){
    cli::cli_abort(
      "Can't simulate {.pkg {prettyNum(nsims, big.mark = ' ')}} \\
      {cli::qty(nsims)}season{?s} in {.val {nchunks}} equally sized
      chunk{?s} of size {.pkg {prettyNum(chunk_size, big.mark = ' ')}}. \\
      Please make sure that the number of seasons can be evenly distributed \\
      over the number of chunks."
    )
  }
  invisible(NULL)
}

sims_validate_games <- function(games){
  setDT(games)

  # Check required columns --------------------------------------------------
  games_names <- colnames(games)
  required_vars <- c(
    "game_type", "week", "away_team", "home_team",
    "away_rest", "home_rest", "location", "result"
  )
  uses_sim <- all(c("sim", required_vars) %in% games_names)
  uses_season <- all(c("season", required_vars) %in% games_names)
  setattr(games, "uses_season", uses_season)
  if( !any(uses_sim, uses_season) ){
    cli::cli_abort(
      "The {.arg games} argument has to be a table including one of the \\
      identifiers {.val sim} or {.val season} as well as \\
      all of the following variables: {.val {required_vars}}!"
    )
  }
  games <- games[, required_vars, with = FALSE]
  games[, old_week := week]
  # Make week a factor so we can filter postseason weeks correctly
  games[, week := fifelse(game_type == "REG", as.character(week), game_type)]
  games[, week := factor(week, levels = unique(week))]

  # Error on too many seasons -----------------------------------------------
  unique_seasons <- if (uses_sim){
    data.table::uniqueN(games[["sim"]])
  } else if (uses_season){
    data.table::uniqueN(games[["season"]])
  } else {
    1L
  }
  if (length(unique_seasons) > 1){
    cli::cli_abort(
      "The identifiers {.val sim} or {.val season} in your  \\
      {.arg games} argument consist of the following unique values: \\
      {.val {unique_seasons}}. {.code nfl_simulations} can only handle one \\
      unique value."
    )
  }

  games
}

sims_compute_playoff_dummy <- function(num_byes){

  n_playoff_games <- c(
    "WC" = 2^3 - num_byes * 2L,
    "DIV" = 2^2,
    "CON" = 2^1,
    "SB" = 2^0
  )

  game_type <- c(
    rep("WC", n_playoff_games[["WC"]]),
    rep("DIV", n_playoff_games[["DIV"]]),
    rep("CON", n_playoff_games[["CON"]]),
    rep("SB", n_playoff_games[["SB"]])
  )

  conf <- c(
    rep("AFC", n_playoff_games[["WC"]] / 2),
    rep("NFC", n_playoff_games[["WC"]] / 2),
    rep("AFC", n_playoff_games[["DIV"]] / 2),
    rep("NFC", n_playoff_games[["DIV"]] / 2),
    rep("AFC", n_playoff_games[["CON"]] / 2),
    rep("NFC", n_playoff_games[["CON"]] / 2),
    NA_character_
  )

  playoff_games <- data.table(
    "game_type" = game_type,
    "week" = as.factor(game_type),
    "conf" = conf,
    "away_team" = NA_character_,
    "home_team" = NA_character_,
    "away_rest" = 7L, # only bye teams have 14 days rest in div round
    "home_rest" = 7L, # only bye teams have 14 days rest in div round
    "location" = "Home", # we don't expect a neutral site playoff game
    "result" = NA_integer_
  )

  wc_home_seeds <- seq(1 + num_byes, length.out = n_playoff_games[["WC"]] / 2)
  wc_away_seeds <- rev(wc_home_seeds + n_playoff_games[["WC"]] / 2)

  # add ids to fill WC games
  playoff_games["WC", home_round_id := paste(conf, wc_home_seeds, sep = "-"), on = "game_type"]
  playoff_games["WC", away_round_id := paste(conf, wc_away_seeds, sep = "-"), on = "game_type"]

  # adjust location and rest default values for SB
  # this means that SB is always simulated as neutral site game although there
  # is a chance that a team can play a home SB. Doesn't happen too often and the
  # home field advantage shouldn't be overestimated anyways
  playoff_games["SB", location := "Neutral", on = "game_type"]
  playoff_games["SB", away_rest := 14L, on = "game_type"]
  playoff_games["SB", home_rest := 14L, on = "game_type"]
  playoff_games
}

default_compute_results <- function(teams, games, week_num, ...) {
  # cli::cli_progress_step(
  #   "Compute week {.val #{week_num}}"
  # )
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
    as.integer(x)
  }

  setDT(games, key = c("sim", "week"))
  setDT(teams, key = c("sim", "team"))

  # get elo if not in teams data already
  if (!("elo" %chin% colnames(teams))) {
    args <- list(...)
    if ("elo" %chin% names(args)) {
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
                 sort = FALSE,
                 all.x = TRUE)
  games <- merge(x = games, y = teams[,list(sim, team, home_elo = elo)],
                 by.x = c("sim", "home_team"),
                 by.y = c("sim", "team"),
                 sort = FALSE,
                 all.x = TRUE)

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
        result := round_out(rnorm(.N, estimate, 13))]
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

  games[, elo_shift := NULL]
  list("teams" = teams, "games" = games)
}

validate_userfunction <- function(compute_results,
                                  ...,
                                  games = nflseedR::sims_games_example,
                                  teams = nflseedR::sims_teams_example){

  if (!is.function(compute_results)) {
    cli::cli_abort("The {.arg compute_results} argument must be a function!")
  }

  games <- data.table::as.data.table(games)

  weeks_to_simulate <- games[game_type == "REG" & is.na(result), sort(unique(week))]

  for (week_num in weeks_to_simulate) {
    # recall old data for comparison
    old_teams <- data.table::copy(teams)
    old_games <- data.table::copy(games)[,.old_result := result]

    return_value <- compute_results(
      teams = teams,
      games = games,
      week_num = week_num,
      ...
    )

    # did we get the right data back?
    # currently, we will catch a maximum of 9 problems. Allocate the vector
    problems <- vector("character", length = 9L)
    i <- 0
    if (typeof(return_value) != "list") {
      i <- i + 1
      problems[i] <- "the returned value was not a list"
    } else {
      if (!("teams" %in% names(return_value))) {
        i <- i + 1
        problems[i] <- "`teams` was not in the returned list"
      } else {
        teams <- return_value$teams
        if (nrow(teams) != nrow(old_teams)) {
          i <- i + 1
          problems[i] <- paste(
            "`teams` changed from", nrow(old_teams), "to",
            nrow(teams), "rows",
            collapse = " "
          )
        }
        if ( any( !colnames(old_teams) %in% colnames(teams) ) ){
          i <- i + 1
          removed_names <- colnames(old_teams)[!colnames(old_teams) %in% colnames(teams)]
          problems[i] <- paste(
            "`teams` column(s) `", removed_names, "` removed",
            collapse = ", "
          )
        }
      }
      if (!("games" %in% names(return_value))) {
        i <- i + 1
        problems[i] <- "`games` was not in the returned list"
      } else {
        games <- return_value$games
        if (nrow(games) != nrow(old_games)) {
          i <- i + 1
          problems[i] <- paste(
            "`games` changed from", nrow(old_games), "to",
            nrow(games), "rows",
            collapse = " "
          )
        }
        if ( any( !colnames(old_games) %in% colnames(games) ) ){
          i <- i + 1
          removed_names <- colnames(old_games)[!colnames(old_games) %in% colnames(games)]
          problems[i] <- paste(
            "`games` column(s) `", removed_names, "` removed",
            collapse = ", "
          )
        }
      }
    }
  }

  # report data structure problems
  problems <- problems[problems != ""]
  if (length(problems)) {
    cli::cli_abort(c(
      "During Week {.val {week_num}}, the {.code compute_results} function \\
       produced the following issues:",
      sapply(problems, function(p) c("x" = p), USE.NAMES = FALSE)
    ))
  }

  # identify improper results values
  problems <- old_games %>%
    inner_join(games, by = intersect(colnames(old_games), colnames(games))) %>%
    mutate(problem = case_when(
      week == week_num & is.na(result) ~
        "a result from the current week is missing",
      week != week_num & !is.na(.old_result) & is.na(result) ~
        "a known result outside the current week was blanked out",
      week != week_num & is.na(.old_result) & !is.na(result) ~
        "a result outside the current week was entered",
      week != week_num & .old_result != result ~
        "a known result outside the current week was updated",
      !is.na(.old_result) & is.na(result) ~
        "a known result was blanked out",
      !is.na(result) & result == 0 & game_type != "REG" ~
        "a playoff game resulted in a tie (had result == 0)",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(problem)) %>%
    pull(problem) %>%
    unique()

  # report result value problems
  if (problems != "") {
    cli::cli_abort(
      "During Week {week_num}, your {.code process_games} function had the \\
        following issues: {problems}. Make sure you only change results \\
        when {.code week == week_num} & {.code is.na(result)}"
    )
  }
}
