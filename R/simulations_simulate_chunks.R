# The function nfl_simulations() maps over chunks. furrr scans it for globals,
# which is quick for this small wrapper (and slow for simulate_chunk()). The
# workers find simulate_chunk() in the nflseedR namespace.
simulate_chunk_future <- function(chunk, ...) simulate_chunk(chunk, ...)

simulate_chunk <- function(chunk,
                           chunk_size,
                           base_games,
                           base_teams,
                           weeks_to_simulate,
                           tiebreaker_depth,
                           verbosity,
                           playoff_seeds,
                           byes_per_conf,
                           sim_include,
                           compute_results,
                           p,
                           ...){

  # NULL means the default function (see nfl_simulations())
  compute_results <- compute_results %||% nflseedR_compute_results

  # Repeat the games and teams of one season for every simulation of this
  # chunk. Subsetting creates new tables, so the input stays unchanged. This
  # matters for sequential plans where all chunks share the same input.
  sims <- as.integer((chunk - 1L) * chunk_size) + seq_len(chunk_size)
  game_number <- nrow(base_games)
  sim_games <- base_games[rep(seq_len(game_number), times = chunk_size)]
  sim_games[, sim := rep(sims, each = game_number)]
  team_number <- nrow(base_teams)
  sim_teams <- base_teams[rep(seq_len(team_number), times = chunk_size)]
  sim_teams[, sim := rep(sims, each = team_number)]

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
  # We need conference ranks to identify playoff teams. Standings use the
  # played games only (sim_games includes post season games without results).
  # The games are encoded once and reused for the draft ranks.
  # NOTE: in case all regular season games are done before we enter the sims,
  # we could calculate standings based on one season only and replicate it like
  # we do with sim_games. However, we would have to pass standings, and h2h and
  # write a bunch of code to handle that situation. Yes, it would be faster to
  # simulate postseason only, but the speed increase isn't really worth the effort.
  encoded <- sims_encode_played_games(sim_games, sims)
  if (verbosity > 0L) report("Compute standings")
  # Standings after the complete regular season, so records compare by
  # percentage only (D13)
  standings <- standings_run(
    encoded,
    sims = sims,
    season_complete = TRUE,
    ranks = "CONF",
    depth = standings_depth(tiebreaker_depth),
    # If user asks for draft_ranks, then we must calculate all conf_ranks in order
    # to use them for draft_ranks
    playoff_seeds = if (sim_include > 1L) 16L else playoff_seeds,
    verbosity = user_verbosity
  )

  # We use the exit variable to identify teams playing playoffs.
  standings[is.na(conf_rank) | conf_rank > playoff_seeds, exit := sims_exit_translate_to("INT")["REG"]]

  # Teams are identified across simulations by an integer key of sim and team
  standings_key <- sims_team_key(standings$sim, standings$team)

  # If sims_games includes already finished playoff games, then we have to fill
  # the exit variable of the losers. Otherwise the wrong teams could be filled
  # in the matchups.
  po_results <- sim_games[week %in% playoff_weeks() & !is.na(result)]

  if (nrow(po_results) > 0){
    # identify losers and look up the playoff round they lost
    po_losers <- po_results[, sims_team_key(sim, fifelse(result > 0, away_team, home_team))]
    lost_in <- po_results$game_type[match(standings_key, po_losers)]
    no_exit <- is.na(standings$exit)
    standings[no_exit, exit := sims_exit_translate_to("INT")[lost_in[no_exit]]]
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

    # Matchups are filled through slots of simulation, conference and seed
    standings_sim <- match(standings$sim, sims)
    standings_conf <- chmatch(standings$conf, c("AFC", "NFC"))

    for (week_num in post_season_weeks) {
      if (verbosity > 1L){
        report("Simulate post season week {.val {week_num}}")
      }

      week_rows <- sims_week_rows(sim_games$week, week_num)

      # It is possible - but unlikely - that the user provided a games table
      # where some or all playoff matchups are already filled. In that case we
      # can skip filling participants. That's why we look up all home and away
      # teams and fill only if one is missing
      if (anyNA(sim_games$home_team[week_rows]) || anyNA(sim_games$away_team[week_rows])) {
        # Use standings to identify teams that should play this round. In the
        # Wild Card round, a team's seed is its conference rank. After that,
        # the remaining teams are reseeded 1-N by conference rank.
        remaining <- which(is.na(standings$exit))
        if (week_num == "WC") {
          seed <- standings$conf_rank[remaining]
        } else {
          by_rank <- remaining[order(
            standings_sim[remaining],
            standings_conf[remaining],
            standings$conf_rank[remaining]
          )]
          seed <- integer(length(remaining))
          seed[match(by_rank, remaining)] <- data.table::rowid(
            standings_sim[by_rank],
            standings_conf[by_rank]
          )
        }
        slot <- sims_playoff_slot(
          standings_sim[remaining],
          standings_conf[remaining],
          seed
        )
        round_teams <- rep(NA_character_, chunk_size * 2L * 16L)
        round_teams[slot[!is.na(slot)]] <- standings$team[remaining[!is.na(slot)]]

        game_sim <- match(sim_games$sim[week_rows], sims)
        if (week_num == "WC"){
          # For Wildcard, the playoff games template has prefilled matchup IDs
          # like "AFC-2"
          home_id <- sim_games$home_round_id[week_rows]
          away_id <- sim_games$away_round_id[week_rows]
          home_slot <- sims_playoff_slot(
            game_sim,
            chmatch(sub("-.*", "", home_id), c("AFC", "NFC")),
            as.integer(sub(".*-", "", home_id))
          )
          away_slot <- sims_playoff_slot(
            game_sim,
            chmatch(sub("-.*", "", away_id), c("AFC", "NFC")),
            as.integer(sub(".*-", "", away_id))
          )
        } else if (week_num != "SB") {
          # The top .N teams by conf_rank and conference are home teams, and
          # the top .N + .N teams (in reversed order) are away teams.
          # Consider DIV round, this translates to
          # home seeds 1/2 and away seeds 4/3
          game_conf <- chmatch(sim_games$conf[week_rows], c("AFC", "NFC"))
          game_number <- data.table::rowid(game_sim, game_conf)
          n_games <- data.table(game_sim, game_conf)[,
            n := .N,
            by = c("game_sim", "game_conf")
          ][["n"]]
          home_slot <- sims_playoff_slot(game_sim, game_conf, game_number)
          away_slot <- sims_playoff_slot(
            game_sim,
            game_conf,
            2L * n_games + 1L - game_number
          )

          # adjust rest of bye week teams
          if (week_num == "DIV") {
            bye_rows <- week_rows[game_number == 1L & !is.na(game_conf)]
            sim_games[bye_rows, home_rest := 14L]
          }

        } else {
          # this is SB
          # AFC always listed as home team. Shouldn't matter anyways as
          # location defaults to "Neutral"
          home_slot <- sims_playoff_slot(game_sim, 1L, 1L)
          away_slot <- sims_playoff_slot(game_sim, 2L, 1L)
        }

        sim_games[week_rows, home_team := round_teams[home_slot]]
        sim_games[week_rows, away_team := round_teams[away_slot]]
      }

      return_value <- compute_results(
        teams = sim_teams,
        games = sim_games,
        week_num = week_num,
        ...
      )
      sim_teams <- return_value[["teams"]]
      sim_games <- return_value[["games"]]

      # Each Playoff round, we compute the losing teams and set their "exit"
      # value to the current round. We use current week_num for the exit
      # value, but that's a factor. To be able to calculate draft_ranks later
      # on, we need to make the exit value an integer.
      # sims_exit_translate_to("INT") does this for us
      week_rows <- sims_week_rows(sim_games$week, week_num)
      round_games <- sim_games[week_rows]
      round_losers <- round_games[, sims_team_key(sim, fifelse(result < 0, home_team, away_team))]
      standings[
        is.na(exit) & standings_key %in% round_losers,
        exit := sims_exit_translate_to("INT")[week_num]]

      if (week_num == "SB"){
        # Only SB winners remain with no exit value. Set it here
        standings[is.na(exit), exit := sims_exit_translate_to("INT")["SB_WIN"]]
      }
    }

    # Remove helper variables
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

    standings <- standings_add_draft_ranks(
      standings = standings,
      encoded = encoded,
      sims = sims,
      tiebreaker_depth = tiebreaker_depth
    )
  }


  # STATUS UPDATE AND RETURN ------------------------------------------------
  p(sprintf("Finished sim chunk %g", chunk))
  list("standings" = standings, "games" = sim_games)
}
