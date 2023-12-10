standings_double_games <- function(g){
  setDT(g)
  away <- g[,list(sim, game_type, week, team = away_team, opp = home_team, result = -result)]
  home <- g[,list(sim, game_type, week, team = home_team, opp = away_team, result)]
  out <- rbind(away, home)
  out[, outcome := fcase(
    is.na(result), NA_real_,
    result > 0, 1,
    result < 0, 0,
    default = 0.5
  )]
  out
}

standings_h2h <- function(gd, verbose = FALSE){
  report("Calculate Head-to-Head Data")
  if( !is.data.table(gd) ) setDT(gd)
  out <- gd[game_type == "REG", list(
    h2h_games = .N,
    h2h_wins = sum(outcome)
  ), keyby = c("sim", "team", "opp")]
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
  standings <- standings[order(sim, division, div_rank)]
  if (attributes(games)[["uses_season"]]){
    colnames(standings)[colnames(standings) == "sim"] <- "season"
  }
  standings
}
