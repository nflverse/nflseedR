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

  # sim_games and sim_teams have no sim identifier yet. Add it here
  # we have to copy sim_games and sim_teams because otherwise all changes to
  # those tables in one round would affect the input to the next round.
  # This happens when running the sims with a sequential future plan which is
  # the same as running it with purrr::map. We need to make sure that the input
  # doesn't change. It doesn't happen in non sequential future plans because
  # in these cases the tables are copied anyways.
  if (is_sequential()){
    sim_games <- data.table::copy(sim_games)
    sim_teams <- data.table::copy(sim_teams)
  }
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
        "1L" = 4L,
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
  user_verbosity <- switch (as.character(verbosity),
    "0" = "NONE",
    "1" = "MIN",
    "2" = "MAX"
  )
  # We need conference ranks to identify playoff teams
  # sim_games includes games with missing results, i.e. post season, remove them
  # for the standings calculation
  # NOTE: in case all regular season games are done before we enter the sims,
  # we could calculate standings based on one season only and replicate it like
  # we do with sim_games. However, we would have to pass standings, and h2h and
  # write a bunch of code to handle that situation. Yes, it would be faster to
  # simulate postseason only, but the speed increase isn't really worth the effort.
  standings <- nfl_standings(
    games = sim_games[!is.na(result)],
    # this is an undocumented feature that make standings return the h2h table
    # in the standings attributes. We need it to add draft_ranks later
    in_sim = if (sim_include > 1L) TRUE else NULL,
    ranks = "CONF",
    tiebreaker_depth = tiebreaker_depth,
    # If user asks for draft_ranks, then we must calculate all conf_ranks in order
    # to use them for draft_ranks
    playoff_seeds = if (sim_include > 1L) 16L else playoff_seeds,
    verbosity = user_verbosity
  )

  # If we need draft ranks, the above will return the h2h table
  # in attributes. Extract it here and then remove it from standings.
  if (sim_include > 1L){
    h2h <- data.table::setDT(attr(standings, "h2h"))
    data.table::setattr(standings, "h2h", NULL)
  }

  # We use the exit variable to identify teams playing playoffs.
  standings[is.na(conf_rank) | conf_rank > playoff_seeds, exit := sims_exit_translate_to("INT")["REG"]]

  # If sims_games includes already finished playoff games, then we have to fill
  # the exit variable of the losers. Otherwise the wrong teams could be filled
  # in the matchups.
  po_results <- sim_games[week %in% playoff_weeks() & !is.na(result)]

  if (nrow(po_results) > 0){
    # identify losers and create a lookup vector of the playoff round they lost
    po_results[, loser := fifelse(result > 0, away_team, home_team)]
    po_losers <- po_results[, setNames(game_type, paste(sim, loser, sep = "-"))]
    # use the lookup vector to set the exit of playoff losers
    standings[is.na(exit), exit := sims_exit_translate_to("INT")[unname(po_losers[paste(sim, team, sep = "-")])]]
  }

  # PLAYOFFS ----------------------------------------------------------------
  post_season_weeks <- base::intersect(playoff_weeks(), weeks_to_simulate)
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

      # It is possible - but unlikely - that the user provided a games table
      # where some or all playoff matchups are already filled. In that case we
      # can skip filling participants. That's why we look up all home and away
      # teams and fill only if one is missing
      home_teams <- sim_games[week_num, home_team, on = "week"]
      away_teams <- sim_games[week_num, away_team, on = "week"]

      if (any(is.na(home_teams), is.na(away_teams))) {
        # Compute vector of teams participating in the playoff round
        # Use standings to identify teams that should play this round
        remaining_teams <- standings[is.na(exit)]

        if (week_num == "WC"){
          round_teams <- remaining_teams[, setNames(team, playoff_id)]
        } else {
          # We need an identifier in games data that can be used to fill
          # names of home and away teams. After the WC round, the identifier can be
          # defined as "teams 1-N" of that playoff round. The number of games is
          # predefined.
          # If we have 1-N as identifier, we need to reassign ranks of the remaining
          # teams based on their conference rank.
          remaining_teams[, new_rank := frankv(conf_rank), by = c("sim", "conf")]
          remaining_teams[, round_id := paste(sim, conf, new_rank, sep = "-")]

          round_teams <- remaining_teams[, setNames(team, round_id)]
        }

        # Prepare Matchup IDs
        if (week_num == "WC"){
          # For Wildcard, the playoff games template has prefilled matchup IDs
          # All we need to do is to prepend the sim number
          sim_games[week_num, home_round_id := paste(sim, home_round_id, sep = "-"), on = "week"]
          sim_games[week_num, away_round_id := paste(sim, away_round_id, sep = "-"), on = "week"]

        } else if (week_num != "SB") {
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

          # adjust rest of bye week teams
          if (week_num == "DIV") {
            sim_games[week == "DIV" & grepl("AFC-1|NFC-1", home_round_id), home_rest := 14L]
          }

        } else {
          # this is SB
          # AFC always listed as home team. Shouldn't matter anyways as
          # location defaults to "Neutral"
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
      }

      return_value <- compute_results(
        teams = sim_teams,
        games = sim_games,
        week_num = week_num,
        ...
      )
      sim_teams <- return_value[["teams"]]
      sim_games <- return_value[["games"]]

      # Each Playoff round, we compute the losing teams and set their "exit" value
      # to the current round. sim_games doesn't hold unique identifiers across
      # the complete playoffs (just the current round). That's why we have to
      # merge the playoff_id to make sure we set the exit value for the right teams
      round_loser <- sim_games[week_num, on = "week"][, loser := fifelse(result < 0, home_team, away_team)]
      round_loser <- merge(
        round_loser,
        standings[, list(sim, loser = team, loser_playoff_id = playoff_id)],
        by = c("sim", "loser"),
        all.x = TRUE
      )
      # We use current week_num for the exit value, but that's a factor
      # To be able to calculate draft_ranks later on, we need to make the exit
      # value an integer. sims_exit_translate_to("INT") does this for us
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

  # Simulation is done at this point. We get rid of the temporarily created
  # factor variable "week" and replace it with the integer variant we got
  # from user. We gotta do this anyways and we need week to be a numeric value
  # in the below draft_ranks function.
  sim_games[, week := old_week]
  sim_games[, old_week := NULL]

  # DRAFT RANKS -------------------------------------------------------------
  if (sim_include > 1L){
    if (verbosity > 0L) report("Compute Draft Order")

    standings <- add_draft_ranks(
      standings = standings,
      h2h = h2h,
      dg = NULL,
      tiebreaker_depth = tiebreaker_depth,
      verbosity = verbosity
    )
    # nfl_standings sorts the output by sim, division and draft rank
    # let's do this here
    standings <- standings[order(sim, division, div_rank)]
  }


  # STATUS UPDATE AND RETURN ------------------------------------------------
  p(sprintf("Finished sim chunk %g", chunk))
  list("standings" = standings, "games" = sim_games)
}
