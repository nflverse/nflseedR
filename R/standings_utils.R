standings_double_games <- function(g, verbosity){
  if (verbosity == 2L) report("Clean Home/Away in Games Data")
  setDT(g)
  if (attr(g, "has_scores") == TRUE){
    away <- g[,list(sim, game_type, week, team = away_team, opp = home_team, score = away_score, result = -result)]
    home <- g[,list(sim, game_type, week, team = home_team, opp = away_team, score = home_score, result)]
  } else {
    away <- g[,list(sim, game_type, week, team = away_team, opp = home_team, result = -result)]
    home <- g[,list(sim, game_type, week, team = home_team, opp = away_team, result)]
  }
  out <- rbind(away, home)
  out[, outcome := fcase(
    is.na(result), NA_real_,
    result > 0, 1,
    result < 0, 0,
    default = 0.5
  )]
  setindexv(out, "game_type")
  out
}

standings_h2h <- function(gd, verbosity){
  if (verbosity == 2L) report("Calculate Head-to-Head Data")
  if( !is.data.table(gd) ) setDT(gd)
  out <- gd[
    "REG",
    list(
      h2h_games = .N,
      h2h_wins = sum(outcome),
      h2h_pd = sum(result)
    ),
    by = c("sim", "team", "opp"),
    on = "game_type"
  ]
}

# A games files is valid if we can perform all necessary steps in the tiebreaking
# process.
standings_validate_games <- function(games){
  if( !is.data.table(games) ) setDT(games)
  games_names <- colnames(games)
  required_vars <- c(
    "game_type",
    "week",
    "away_team",
    "home_team",
    "result"
  )
  if (all(c("sim", "season") %in% games_names)){
    cli::cli_alert_warning(
      "The {.arg games} argument includes both {.val sim} and {.val season}. \\
      Will group by {.val sim}. Please adjust {.arg games} if that is not \\
      what you want.",
      wrap = TRUE
    )
    # drop season column
    games$season <- NULL
    games_names <- colnames(games)
  }
  uses_sim <- all(c("sim", required_vars) %in% games_names)
  uses_season <- all(c("season", required_vars) %in% games_names)
  has_scores <- all(c("away_score", "home_score") %in% games_names)
  setattr(games, "uses_season", uses_season)
  setattr(games, "has_scores", has_scores)
  if( !any(uses_sim, uses_season) ){
    cli::cli_abort(
      "The {.arg games} argument has to be a table including one of the \\
      identifiers {.val sim} or {.val season} as well as \\
      all of the following variables: {.val {required_vars}}!"
    )
  }
  if ( any(is.na(games$result)) ){
    cli::cli_abort(
      "The {.arg games} table includes {.val NA} results! Please fix and rerun."
    )
  }
  if (uses_season) colnames(games)[colnames(games) == "season"] <- "sim"

  games
}

finalize_standings <- function(standings, games){
  if ("div_rank" %chin% colnames(standings)){
    standings <- standings[order(sim, division, div_rank)]
  } else {
    standings <- standings[order(sim, division)]
  }
  if (attributes(games)[["uses_season"]]){
    colnames(standings)[colnames(standings) == "sim"] <- "season"
  }
  if ("exit" %chin% colnames(standings)){
    standings[, exit := sims_exit_translate_to("CHAR")[as.character(exit)]]
  }
  # Conference Point Differential is a deep tiebreaker. We don't need to return it
  standings[, conf_pd := NULL]
  standings
}
