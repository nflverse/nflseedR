# Clinching and elimination status of every team (dev/playoff-status.md). The
# work happens in the C++ engine (src/status.cpp). Internal until the
# exported nfl_clinching_status() is added.
#
# With `scenarios = TRUE`, returns a list of the status table and the
# scenarios (clinching_status_scenarios()). `verify = TRUE` (tests) checks
# every clinch and elimination that needed tie proofs against the engine.
clinching_status_compute <- function(
  games,
  status = c("DIV", "PLAYOFFS", "1SEED"),
  scenarios = FALSE,
  verify = FALSE,
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
    goals = c("DIV", "PLAYOFFS", "1SEED") %in% status,
    scenarios = scenarios,
    verify = verify
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
  status_table <- setDT(cols)
  setorderv(status_table, c(names(cols)[1], "division", "team"))
  if (!scenarios) {
    return(status_table[])
  }
  list(
    status = status_table[],
    scenarios = clinching_status_scenarios(
      out,
      games[remaining],
      sims,
      league,
      uses_season = attr(games, "uses_season")
    )
  )
}

# One row per scenario and remaining game of its season: the remaining games
# (in the format of `games`) with the results of the scenario. `scenario`
# numbers the scenarios, `proves` says what they prove and `coin_toss` if a
# coin toss was needed. Scenarios of seasons without remaining games have no
# rows.
clinching_status_scenarios <- function(
  out,
  remaining,
  sims,
  league,
  uses_season
) {
  # Rows of each season's remaining games, in the order the C++ engine got
  # them
  rows <- split(seq_len(nrow(remaining)), factor(remaining$sim, sims))
  sim_id <- out$scenario_sim
  n_games <- lengths(rows)[sim_id]
  scenario <- rep(seq_along(sim_id), n_games)
  games <- remaining[unlist(rows[sim_id], use.names = FALSE)]
  games[, result := out$scenario_result]
  goals <- c("DIV", "PLAYOFFS", "1SEED")
  cols <- list(
    scenario = scenario,
    sim = sims[sim_id][scenario],
    team = league$team[out$scenario_team + 1L][scenario],
    goal = goals[out$scenario_goal + 1L][scenario],
    proves = c("reach", "miss")[out$scenario_proves][scenario],
    coin_toss = (out$scenario_coin_toss == 1L)[scenario]
  )
  games[, sim := NULL]
  out <- cbind(setDT(cols), games)
  if (uses_season) {
    setnames(out, "sim", "season")
  }
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
