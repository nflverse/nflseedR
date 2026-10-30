# A games files is valid if we can perform all necessary steps in the tiebreaking
# process.
standings_validate_games <- function(games, call = rlang::caller_env()) {
  if (!is.data.table(games)) {
    setDT(games)
  }
  games_names <- colnames(games)
  required_vars <- c(
    "game_type",
    "week",
    "away_team",
    "home_team",
    "result"
  )
  if (all(c("sim", "season") %in% games_names)) {
    cli::cli_warn(
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
  if (!any(uses_sim, uses_season)) {
    cli::cli_abort(
      "The {.arg games} argument has to be a table including one of the \\
      identifiers {.val sim} or {.val season} as well as \\
      all of the following variables: {.val {required_vars}}!",
      call = call
    )
  }
  # D9 in dev/cpp-standings-engine.md: only the 32 current teams
  valid_teams <- standings_league()$team
  if (
    anyNA(chmatch(games$away_team, valid_teams)) ||
      anyNA(chmatch(games$home_team, valid_teams))
  ) {
    invalid_teams <- setdiff(
      unique(c(games$away_team, games$home_team)),
      valid_teams
    )
    cli::cli_abort(
      c(
        "The {.arg games} table includes invalid team abbreviations: \\
        {.val {invalid_teams}}.",
        "i" = "Valid abbreviations are the teams in \\
        {.code nflseedR::divisions} without \\
        {.val {c('LAR', 'OAK', 'SD', 'STL')}}.",
        "i" = "{.fun nflreadr::clean_team_abbrs} converts old abbreviations."
      ),
      call = call
    )
  }
  if (any(is.na(games$result))) {
    cli::cli_abort(
      "The {.arg games} table includes {.val NA} results! Please fix and rerun.",
      call = call
    )
  }
  if (uses_season) {
    colnames(games)[colnames(games) == "season"] <- "sim"
  }

  games
}

finalize_standings <- function(standings, games) {
  # Keep a stable column order no matter which tiebreakers were needed
  setcolorder(standings, c("sim", "team", "conf", "division"))
  if ("div_rank" %chin% colnames(standings)) {
    standings <- standings[order(sim, division, div_rank)]
  } else {
    standings <- standings[order(sim, division)]
  }
  if (attributes(games)[["uses_season"]]) {
    colnames(standings)[colnames(standings) == "sim"] <- "season"
  }
  if ("exit" %chin% colnames(standings)) {
    standings[, exit := sims_exit_translate_to("CHAR")[as.character(exit)]]
  }
  # Conference Point Differential is a deep tiebreaker. We don't need to return it
  standings[, conf_pd := NULL]
  standings
}
