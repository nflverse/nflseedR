simulate_chunk <- function(chunk,
                           nsims,
                           nchunks,
                           sim_games,
                           sim_teams,
                           weeks_to_simulate,
                           tiebreaker_depth,
                           verbosity,
                           playoff_seeds,
                           sim_include,
                           compute_results,
                           p){

  games <- sim_games[
    data.table::inrange(
      sim,
      lower = ceiling(nsims / nchunks) * (chunk - 1) + 1,
      upper = ceiling(nsims / nchunks) * chunk
    )
  ]

  teams <- sim_teams[
    data.table::inrange(
      sim,
      lower = ceiling(nsims / nchunks) * (chunk - 1) + 1,
      upper = ceiling(nsims / nchunks) * chunk
    )
  ]

  # REMAINDER OF REGULAR SEASON ---------------------------------------------
  reg_season_weeks <-  base::setdiff(weeks_to_simulate, playoff_weeks)
  if (length(reg_season_weeks) > 0){
    if (verbosity > 0){
      print_n <- switch (verbosity,
        "1L" = 6L,
        "2L" = 18L
      )
      vec <- cli::cli_vec(reg_season_weeks, list("vec-trunc" = print_n))
      report("CHUNK #{.val {chunk}}: Start simulation of regular season weeks {.val {vec}}", wrap = TRUE)
    }
    for (week_num in reg_season_weeks) {
      if (verbosity > 1L){
        report("Simulate regular season week {.val {week_num}}")
      }
      return_value <- compute_results(
        teams = teams,
        games = games,
        week_num = week_num,
        ...
      )
      teams <- return_value[["teams"]]
      games <- return_value[["games"]]
    }
  }
  user_verbosity <- switch (verbosity,
    "0L" = "NONE",
    "1L" = "MIN",
    "2L" = "MAX",
  )
  # We need conference ranks to identify the playoff teams
  standings <- nfl_standings(
    games = games,
    ranks = "CONF",
    tiebreaker_depth = tiebreaker_depth,
    playoff_seeds = playoff_seeds,
    verbosity = user_verbosity
  )

  # PLAYOFFS ----------------------------------------------------------------
  if (sim_include > 0L){
    # Identify the number of the last regular season week
    max_reg_week <- standings$max_reg_week[[1]]

    # bye count (per conference)
    num_byes <- 2^ceiling(log(playoff_seeds, 2)) - playoff_seeds

    # first playoff week
    first_playoff_week <- max_reg_week + 1L

    # final week of season (Super Bowl week)
    week_max <- max_reg_week +
      ceiling(log(num_teams * uniqueN(standings$conf), 2))

    n_playoff_games <- c(
      "WC" = 2^3 - num_byes,
      "DIV" = 2^2,
      "CON" = 2^1,
      "SB" = 2^0
    )

    playoff_summand <- c(
      "WC" = 1L,
      "DIV" = 2L,
      "CON" = 3L,
      "SB" = 4L
    )

    playoff_games <- data.table(
      "game_type" = c(
        rep("WC", n_playoff_games[["WC"]]),
        rep("DIV", n_playoff_games[["DIV"]]),
        rep("CON", n_playoff_games[["CON"]]),
        rep("SB", n_playoff_games[["SB"]])
      ),
      "max_reg_week" = 18L,
      "away_team" = NA_character_,
      "home_team" = NA_character_,
      "away_rest" = NA_integer_,
      "home_rest" = NA_integer_,
      "location" = NA_integer_,
      "result" = NA_integer_
    )
    playoff_games[,
      week := max_reg_week + playoff_summand[game_type]
    ]
    playoff_games[, max_reg_week := NULL]
  }

  # p(sprintf("Finished sim chunk %g", chunk))

  list("teams" = standings, "games" = games)
}
