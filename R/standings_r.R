# The data.table implementation of nfl_standings() before the C++ engine.
# Internal and unchanged, so the C++ engine can be compared with it (see
# dev/cpp-standings-engine.md).
nfl_standings2 <- function(
  games,
  ...,
  ranks = c("CONF", "DIV", "DRAFT", "NONE"),
  tiebreaker_depth = c("SOS", "PRE-SOV", "POINTS", "RANDOM"),
  playoff_seeds = NULL,
  verbosity = c("MIN", "MAX", "NONE")
) {
  # VALIDATE INPUT ----------------------------------------------------------
  games <- standings_validate_games(games)
  ranks <- rlang::arg_match(ranks)
  tiebreaker_depth <- rlang::arg_match(tiebreaker_depth)
  tiebreaker_depth <- switch(
    tiebreaker_depth,
    "RANDOM" = 0L,
    "PRE-SOV" = 1L,
    "SOS" = 2L,
    "POINTS" = 3L
  )
  verbosity <- rlang::arg_match(verbosity)
  verbosity <- switch(verbosity, "MIN" = 1L, "MAX" = 2L, "NONE" = 0L)
  if (!is.null(playoff_seeds) && !inrange(playoff_seeds, 1L, 16L)) {
    cli::cli_abort(
      "The {.arg playoff_seeds} argument must be in range {.pkg 1} - {.pkg 16}"
    )
  }

  # INITIATE STANDINGS WITHOUT ANY RANKINGS ---------------------------------
  if (verbosity > 0L) {
    report("Initiate Standings & Tiebreaking Data")
  }
  dg <- standings_double_games(games, verbosity = verbosity)
  standings <- standings_init(dg, verbosity = verbosity)
  h2h <- standings_h2h(dg, verbosity = verbosity)

  if (ranks == "NONE") {
    return(finalize_standings(standings, games))
  }

  # DIVISION RANKS ----------------------------------------------------------
  if (verbosity > 0L) {
    report("Compute Division Ranks")
  }
  standings <- add_div_ranks(
    standings = standings,
    h2h = h2h,
    tiebreaker_depth = tiebreaker_depth,
    verbosity = verbosity
  )
  if (ranks == "DIV") {
    return(finalize_standings(standings, games))
  }

  # CONFERENCE RANKS --------------------------------------------------------
  if (verbosity > 0L) {
    report("Compute Conference Ranks")
  }
  standings <- add_conf_ranks(
    standings = standings,
    h2h = h2h,
    tiebreaker_depth = tiebreaker_depth,
    playoff_seeds = playoff_seeds,
    verbosity = verbosity
  )
  if (ranks == "CONF") {
    out <- finalize_standings(standings, games)
    dots <- list(...)
    in_sim <- if ("in_sim" %in% names(dots)) dots$in_sim else FALSE
    if (isTRUE(in_sim)) {
      data.table::setattr(out, "h2h", h2h)
    }
    return(out)
  }

  # DRAFT ORDER -------------------------------------------------------------
  if (verbosity > 0L) {
    report("Compute Draft Order")
  }
  standings <- add_draft_ranks(
    standings = standings,
    h2h = h2h,
    dg = dg,
    tiebreaker_depth = tiebreaker_depth,
    verbosity = verbosity
  )

  return(finalize_standings(standings, games))
}
