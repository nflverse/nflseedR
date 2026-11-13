#' Compute NFL Clinching and Elimination Status
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Determine for every team whether it has clinched, can still reach, or has
#' been eliminated from a division title, a playoff berth and the No. 1 seed,
#' given the games played so far.
#'
#' @inheritParams nfl_standings
#' @param games A data frame of played and remaining games, like
#'   [nflreadr::load_schedules()]. It needs the variables `sim` or `season`,
#'   `game_type`, `week`, `away_team`, `home_team` and `result` (see
#'   [nfl_standings()]). Games with a `result` are played, regular season
#'   games with `result = NA` are still to be played. To get the status after
#'   a given week, set `result` of all later games to `NA`.
#'
#'   Cancelled games must be removed from `games`, otherwise they count as
#'   games still to be played.
#' @param status One or more of `"DIV"`, `"PLAYOFFS"` and `"1SEED"`: the
#'   goals to compute.
#'   - `"DIV"`: win the division.
#'   - `"PLAYOFFS"`: make the playoffs, as division winner or wild card.
#'   - `"1SEED"`: be the No. 1 seed of the conference.
#'
#'   Only the requested goals are computed. `"PLAYOFFS"` takes the most time.
#' @param scenarios If `TRUE`, the result gets the attribute `"scenarios"`,
#'   the results of the remaining games that prove the answers (see Value).
#' @param search If `TRUE`, answers the proofs leave open are searched by
#'   fixing the results of remaining games one at a time, within a fixed
#'   budget (see Details). This decides more teams but takes more time, mostly
#'   with many games left. With `FALSE` (default), the function says how many
#'   statuses are `"undecided"` and whether the search might help.
#' @param verbosity One of `"MIN"`, `"MAX"`, or `"NONE"`.
#'   - `"MIN"` (default): Prints the main steps.
#'   - `"MAX"`: Also prints all steps of the tiebreaking process of the
#'     current standings and how many teams got each status.
#'   - `"NONE"`: No status reports.
#'
#' @details
#' For every goal, the function answers two questions: can the team still
#' reach the goal, and can it still miss it, over all possible results (win,
#' loss or tie) of the remaining games. The status follows from the answers:
#'
#' | Can reach | Can miss | Status |
#' |---|---|---|
#' | no | yes | `"eliminated"` |
#' | yes | no | `"clinched"` |
#' | yes | yes | `"alive"` |
#' | not proven | | `"undecided"` |
#'
#' Every answer is proven, either by showing that no results of the remaining
#' games can do it, or by a concrete set of results run through the standings
#' (see `scenarios`). Where tiebreakers decide, the function goes through the
#' ways the teams of the conference can end above, level with or below the
#' team, and proves who wins each of these ties. It never tries every
#' combination of results, so a few teams can be `"undecided"` when the
#' answer depends on tiebreakers it can't settle in advance.
#'
#' With `search = TRUE`, an open answer is searched further: the function
#' fixes the result of one remaining game at a time (home win, away win, tie)
#' and proves the answer for each case, up to 200 cases per answer. This
#' helps most with 5 or fewer games left per team.
#'
#' Tiebreakers through strength of schedule are applied as in
#' [nfl_standings()]. Coin tosses and tiebreakers based on points can't be
#' known before they happen, so both of their outcomes count as possible. A
#' goal that only a coin toss decides after the last game is `"undecided"`.
#'
#' Seasons through 2019 have 6 playoff seeds per conference, later seasons 7.
#' With `sim` instead of `season`, 7 seeds are used.
#'
#' The results are compared with the official clinching flags of the NFL for
#' every week of 2006 to 2025: no status contradicts them. With
#' `search = TRUE`, every team clinches or is eliminated in the same week as
#' in the NFL. Without it, two teams are `"undecided"` a week longer.
#'
#' @return The current standings: the output of [nfl_standings()] for the
#'   games played so far (with conference ranks and tiebreakers through
#'   strength of schedule), followed by `games_left` (regular season games
#'   still to be played) and one status column per requested goal:
#'   `div_status`, `playoffs_status` and `seed1_status`. Each status is one of
#'   `"clinched"`, `"alive"`, `"eliminated"` or `"undecided"`. Like
#'   [nfl_standings()], there is only a row for teams that have played at least
#'   one game, and the rows are sorted the same way.
#'
#'   With `scenarios = TRUE`, the attribute `"scenarios"` is a data.table with
#'   one row per scenario and remaining game of its season: `scenario` (an
#'   id), `season` (or `sim`), `team`, `goal` (`"DIV"`, `"PLAYOFFS"` or
#'   `"1SEED"`), `proves` (`"reach"` or `"miss"`: the team reaches or misses
#'   the goal with these results), `coin_toss` (`TRUE` if it does so only
#'   with a particular coin toss), followed by the columns of `games` with the
#'   results of the scenario. Played games together with the rows of one
#'   scenario can go straight into [nfl_standings()]. Seasons without
#'   remaining games have no rows.
#' @export
#'
#' @seealso For the implemented tiebreakers, see
#'   <https://nflseedr.com/articles/tiebreaker.html>
#'
#' @examples
#' \donttest{
#' try({#to avoid CRAN test problems
#'   games <- nflreadr::load_schedules(2023)
#' })
#' # Status after week 15
#' games$result[games$week > 15] <- NA
#' status <- nflseedR::nfl_clinching_status(games)
#' status
#' }
nfl_clinching_status <- function(
  games,
  ...,
  status = c("DIV", "PLAYOFFS", "1SEED"),
  scenarios = FALSE,
  search = FALSE,
  verbosity = c("MIN", "MAX", "NONE")
) {
  rlang::check_dots_empty()
  status <- rlang::arg_match(status, multiple = TRUE)
  verbosity <- rlang::arg_match(verbosity)
  if (!rlang::is_bool(scenarios)) {
    cli::cli_abort("{.arg scenarios} must be {.code TRUE} or {.code FALSE}.")
  }
  if (!rlang::is_bool(search)) {
    cli::cli_abort("{.arg search} must be {.code TRUE} or {.code FALSE}.")
  }
  if (verbosity != "NONE") {
    report("Compute clinching status")
  }
  status_table <- clinching_status_compute(
    games,
    status = status,
    scenarios = scenarios,
    search = search
  )
  out <- clinching_status_standings(games, status_table, verbosity = verbosity)
  if (verbosity == "MAX") {
    clinching_status_report(out)
  }
  if (!search && verbosity != "NONE") {
    clinching_status_search_hint(status_table)
  }
  out
}

# Games left per team up to which the search decided most open answers
# (dev/playoff-status.md, milestone 9)
clinching_status_search_games_left <- 5L

# search = FALSE: a hint if statuses of teams with games left are undecided
clinching_status_search_hint <- function(status_table) {
  cols <- intersect(
    c("div_status", "playoffs_status", "seed1_status"),
    names(status_table)
  )
  open <- status_table[games_left > 0L]
  undecided <- rowSums(as.matrix(open[, cols, with = FALSE]) == "undecided")
  n <- sum(undecided)
  if (n == 0L) {
    return(invisible())
  }
  left <- range(open$games_left[undecided > 0L])
  left <- if (left[1] == left[2]) left[1] else paste0(left[1], " to ", left[2])
  max_left <- clinching_status_search_games_left
  cli::cli_inform(c(
    "i" = "{n} status{?es} {?is/are} {.val undecided} (teams with {left} \\
      games left). {.code search = TRUE} might decide {?it/them}.",
    " " = "The search helps most with {max_left} or fewer games left per \\
      team."
  ))
}

# Current standings (nfl_standings() of the played games) with the columns
# `games_left` and the status columns of `status_table` added. Only teams
# that have played a game have a row.
clinching_status_standings <- function(
  games,
  status_table,
  verbosity,
  call = rlang::caller_env()
) {
  id <- names(status_table)[1]
  played <- as.data.table(games)
  played <- played[!is.na(played$result)]
  # clinching_status_compute() already warned about both `sim` and `season`
  if (id == "sim") {
    played$season <- NULL
  }
  # A season is complete when no games are left, not when the last week has
  # a played game
  complete <- status_table[, list(complete = all(games_left == 0L)), by = id]
  sims <- sort(unique(played[[id]]))
  standings <- standings_compute(
    played,
    ranks = "CONF",
    tiebreaker_depth = "SOS",
    playoff_seeds = NULL,
    verbosity = verbosity,
    season_complete = complete$complete[match(sims, complete[[id]])],
    call = call
  )
  cols <- setdiff(names(status_table), c(id, "team", names(standings)))
  standings[
    status_table,
    on = c(id, "team"),
    (cols) := mget(paste0("i.", cols))
  ]
  setattr(standings, "scenarios", attr(status_table, "scenarios"))
  standings[]
}

# The work of nfl_clinching_status() after argument checks
# (dev/playoff-status.md). The C++ engine is in src/status.cpp.
# `verify = TRUE` (tests) checks every clinch and elimination that needed
# tie proofs against the engine.
clinching_status_compute <- function(
  games,
  status = c("DIV", "PLAYOFFS", "1SEED"),
  scenarios = FALSE,
  search = FALSE,
  verify = FALSE,
  call = rlang::caller_env()
) {
  status <- rlang::arg_match(status, multiple = TRUE, error_call = call)
  # A plain data.table copy: tibbles keep their class through setDT(), and
  # the user's table stays as it is
  games <- as.data.table(games)
  # Playoff games that aren't played yet don't matter (and their teams may
  # be unknown). `$` because the columns are only validated below: a missing
  # column gives zero rows here and the validation error below
  games <- games[!(games$game_type != "REG" & is.na(games$result))]
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
    search = search,
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
  setattr(
    status_table,
    "scenarios",
    clinching_status_scenarios(
      out,
      games[remaining],
      sims,
      league,
      uses_season = attr(games, "uses_season")
    )
  )
  status_table[]
}

# verbosity = "MAX": how many teams got each status per goal
clinching_status_report <- function(status) {
  cols <- intersect(
    c("div_status", "playoffs_status", "seed1_status"),
    names(status)
  )
  for (col in cols) {
    counts <- table(factor(
      status[[col]],
      c("clinched", "alive", "eliminated", "undecided")
    ))
    report(
      "{sub('_status', '', col)}: \\
      {paste(names(counts), counts, sep = ' x ', collapse = ', ')}"
    )
  }
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
