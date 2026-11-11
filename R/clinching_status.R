# Clinching and elimination status of every team (dev/playoff-status.md). The
# work happens in the C++ engine (src/status.cpp). Internal until the
# exported nfl_clinching_status() is added.
clinching_status_compute <- function(
  games,
  status = c("DIV", "PLAYOFFS", "1SEED"),
  call = rlang::caller_env()
) {
  status <- rlang::arg_match(status, multiple = TRUE, error_call = call)
  games <- standings_validate_games(
    games,
    allow_missing_results = TRUE,
    call = call
  )
  league <- standings_league()
  sims <- sort(unique(games$sim))
  played <- !is.na(games$result)
  # Cancelled games are already removed (P3), so every regular season game
  # without a result is still to be played
  remaining <- !played & games$game_type == "REG"
  encode <- function(rows) {
    standings_encode_games(rows, sims, league, has_scores = FALSE, call = call)
  }
  out <- clinching_status_cpp(
    played = encode(games[played]),
    # Results of remaining games are placeholders
    remaining = encode(games[remaining][, result := 0]),
    seeds = clinching_status_seeds(sims, attr(games, "uses_season")),
    goals = c("DIV", "PLAYOFFS", "1SEED") %in% status
  )

  team <- out$team + 1L
  cols <- list(
    sim = sims[out$sim],
    team = league$team[team],
    conf = league$conf[team],
    division = league$division[team],
    wins = out$wins,
    losses = out$losses,
    ties = out$ties,
    games_left = out$games_left
  )
  goals <- c(
    DIV = "div_status",
    PLAYOFFS = "playoffs_status",
    `1SEED` = "seed1_status"
  )
  for (goal in status) {
    cols[[goals[[goal]]]] <- clinching_status_labels(out[[goals[[goal]]]])
  }
  if (attr(games, "uses_season")) {
    names(cols)[1] <- "season"
  }
  out <- setDT(cols)
  setorderv(out, c(names(cols)[1], "division", "team"))
  out[]
}

# Playoff seeds per conference: 6 through 2019, 7 since 2020. With `sim`
# instead of `season`, the current 7 seeds.
clinching_status_seeds <- function(sims, uses_season) {
  if (uses_season) fifelse(sims <= 2019, 6L, 7L) else rep(7L, length(sims))
}

# Status codes of src/engine/status.hpp
clinching_status_labels <- function(code) {
  c("clinched", "alive", "eliminated", "undecided")[code]
}
