simulate_chunk <- function(chunk,
                           nsims,
                           nchunks,
                           games_sim_vec,
                           teams_sim_vec,
                           sim_games,
                           sim_teams,
                           weeks_to_simulate,
                           tiebreaker_depth,
                           verbosity,
                           playoff_seeds,
                           byes_per_conf,
                           sim_include,
                           compute_results,
                           p,
                           ...){

  sim_games <- sim_games[,
    sim := games_sim_vec[data.table::inrange(
      games_sim_vec,
      lower = ceiling(nsims / nchunks) * (chunk - 1L) + 1L,
      upper = ceiling(nsims / nchunks) * chunk
    )]
  ]

  sim_teams <- sim_teams[,
    sim := teams_sim_vec[data.table::inrange(
      teams_sim_vec,
      lower = ceiling(nsims / nchunks) * (chunk - 1L) + 1L,
      upper = ceiling(nsims / nchunks) * chunk
    )]
  ]

  # REMAINDER OF REGULAR SEASON ---------------------------------------------
  reg_season_weeks <- base::setdiff(weeks_to_simulate, playoff_weeks())
  if (length(reg_season_weeks) > 0L) {
    if (verbosity > 0L){
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
        teams = sim_teams,
        games = sim_games,
        week_num = week_num,
        ...
      )
      sim_teams <- return_value[["teams"]]
      sim_games <- return_value[["games"]]
    }
  }


  # STANDINGS AFTER REG SEASON ----------------------------------------------
  user_verbosity <- switch (verbosity,
    "0L" = "NONE",
    "1L" = "MIN",
    "2L" = "MAX",
  )
  # We need conference ranks to identify playoff teams
  # sim_games includes games with missing results, i.e. post season, remove them
  # for the standings calculation
  standings <- nfl_standings(
    games = sim_games[!is.na(result)],
    # this is an undocumented feature that make standings return the h2h and
    # double games tables in the standings attributes. We need those
    # to add draft order later
    in_sim = if (sim_include > 1L) TRUE else NULL,
    ranks = "CONF",
    tiebreaker_depth = tiebreaker_depth,
    playoff_seeds = playoff_seeds,
    verbosity = user_verbosity
  )

  # If we need draft order, the above will return h2h and double games tables
  # in attributes. Extract them here and then remove them from standings.
  if (sim_include > 1L){
    h2h <- attr(standings, "h2h") |> data.table::setDT()
    data.table::setattr(standings, "h2h", NULL)
  }

  standings[is.na(conf_rank) | conf_rank > playoff_seeds, exit := "REG"]

  # PLAYOFFS ----------------------------------------------------------------
  post_season_weeks <- base::setdiff(playoff_weeks(), weeks_to_simulate)
  if (sim_include > 0L && length(post_season_weeks) > 0L){
    if (verbosity > 0L){
      report(
        "CHUNK #{.val {chunk}}: Start simulation of post season \\
        {cli::qty(length(post_season_weeks))} week{?s} \\
        {.val {post_season_weeks}}",
        wrap = TRUE
      )
    }

    # Create an identifier that is used to fill home and away teams, and to
    # identify losers
    standings[is.na(exit), playoff_id := paste(sim, conf, conf_rank, sep = "-")]
    # This vector lists all playoff teams across simulations. It's a named vector
    # where the above created playoff_id is the name. That's how we can identify
    # teams across all sims and playoff rounds
    playoff_teams <- standings[is.na(exit), setNames(team, playoff_id)]

    for (week_num in post_season_weeks) {
      if (verbosity > 1L){
        report("Simulate post season week {.val {week_num}}")
      }

      # Fill participants of current playoff week
      # make sure to check if they are not already there!!!
      #
      if (week_num == "WC"){
        remaining_teams <- standings[is.na(exit)]
        sim_games[week_num, home_round_id := paste(sim, home_round_id, sep = "-"), on = "week"]
        sim_games[week_num, away_round_id := paste(sim, away_round_id, sep = "-"), on = "week"]

        sim_games[week_num, home_team := playoff_teams[home_round_id], on = "week"]
        sim_games[week_num, away_team := playoff_teams[away_round_id], on = "week"]

      } else {
        # Use standings to identify teams that should play this round
        remaining_teams <- standings[is.na(exit)]
        # We need an identifier in games data that can be used to fill
        # names of home and away teams. After the WC round, the identifier can be
        # defined as "teams 1-N" of that playoff round. The number of games is
        # predefined.
        # If we have 1-N as identifier, we need to reassign ranks of the remaining
        # teams based on their conference rank.
        remaining_teams[, new_rank := frankv(conf_rank), by = c("sim", "conf")]
        remaining_teams[, round_id := paste(sim, conf, new_rank, sep = "-")]

        round_teams <- remaining_teams[, setNames(team, round_id)]

        # We can describe the home_round_id and away_round_id as
        # "The top .N teams by conf_rank and conference are home teams, and the
        # top .N + .N teams (in reversed order) are away_teams.
        # Consider DIV round, this translates to
        # home_round_id = sim-conf-1/2
        # away_round_id = sim-conf-4/3
        sim_games[week_num,
                  home_round_id := paste(sim, conf, seq_len(.N), sep = "-"),
                  by = c("sim", "conf"),
                  on = "week"]
        sim_games[week_num,
                  away_round_id := paste(sim, conf, rev(seq_len(.N) + .N), sep = "-"),
                  by = c("sim", "conf"),
                  on = "week"]

        if (week_num == "SB"){
          sim_games[week_num,
                    home_round_id := paste(sim, "AFC", 1, sep = "-"),
                    on = "week"]
          sim_games[week_num,
                    away_round_id := paste(sim, "NFC", 1, sep = "-"),
                    on = "week"]
        }

        # Use above created IDs to fill the matchups
        sim_games[week_num, home_team := round_teams[home_round_id], on = "week"]
        sim_games[week_num, away_team := round_teams[away_round_id], on = "week"]

        # adjust rest of bye week teams
        if (week_num == "DIV") {
          sim_games[week == "DIV" & grepl("AFC-1|NFC-1", home_round_id), home_rest := 14L]
        }
      }

      return_value <- compute_results(
        teams = sim_teams,
        games = sim_games,
        week_num = week_num,
        ...
      )
      sim_teams <- return_value[["teams"]]
      sim_games <- return_value[["games"]]

      round_loser <- sim_games[week_num, on = "week"][, loser := fifelse(result < 0, home_team, away_team)]
      round_loser <- merge(
        round_loser,
        standings[, list(sim, loser = team, loser_playoff_id = playoff_id)],
        by = c("sim", "loser"),
        all.x = TRUE
      )
      standings[
        is.na(exit) & (playoff_id %in% round_loser$loser_playoff_id),
        exit := sims_exit_translate_to("INT")[week_num]]

      if (week_num == "SB"){
        # Only SB winners remain with no exit value. Set it here
        standings[is.na(exit), exit := sims_exit_translate_to("INT")["SB_WIN"]]
      }
    }

  # Remove helper variables
  standings[, playoff_id := NULL]
  sim_games[, c("home_round_id", "away_round_id", "conf") := NULL]

  }

  sim_games[, week := old_week]
  sim_games[, old_week := NULL]

  # DRAFT ORDER -------------------------------------------------------------
  if (sim_include > 1L){
    if (verbosity > 0L) report("Compute Draft Order")

    standings <- add_draft_ranks(
      standings = standings,
      h2h = h2h,
      dg = NULL,
      tiebreaker_depth = tiebreaker_depth,
      verbosity = user_verbosity
    )
    standings <- standings[order(sim, division, div_rank)]
  }


  # STATUS UPDATE AND RETURN ------------------------------------------------
  p(sprintf("Finished sim chunk %g", chunk))
  list("standings" = standings, "games" = sim_games)
}
