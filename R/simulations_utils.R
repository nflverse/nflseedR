playoff_weeks <- function() c("WC", "DIV", "CON", "SB")
playoff_summands <- function(){
  setNames(
    seq_along(playoff_weeks()),
    playoff_weeks()
  )
}

sims_exit_translate_to <- function(to = c("INT", "CHAR")){
  to <- rlang::arg_match(to)
  translation_vec <- c(
    "REG"    = 0L,
    "WC"     = 1L,
    "DIV"    = 2L,
    "CON"    = 3L,
    "SB"     = 4L,
    "SB_WIN" = 5L
  )
  if (to == "INT"){
    translation_vec
  } else {
    setNames(names(translation_vec), translation_vec)
  }
}

sims_calculate_chunk_size <- function(nsims, nchunks) ceiling(nsims / nchunks)

sims_check_chunk_size <- function(nsims, nchunks, chunk_size){
  resulting_sims <- nchunks * chunk_size
  if (resulting_sims != nsims){
    cli::cli_abort(
      "Can't simulate {.pkg {prettyNum(nsims, big.mark = ' ')}} \\
      {cli::qty(nsims)}season{?s} in {.val {nchunks}} equally sized
      chunk{?s} of size {.pkg {prettyNum(chunk_size, big.mark = ' ')}}. \\
      Please make sure that the number of seasons can be evenly distributed \\
      over the number of chunks."
    )
  }
  invisible(NULL)
}

# Integer key of a team in a simulation. Team ids are 1..32, so keys are unique
sims_team_key <- function(sim, team) {
  sim * 32L + chmatch(team, standings_league()$team)
}

# Position of a playoff seed in a vector of 16 seeds per conference (1 AFC,
# 2 NFC) and simulation (1..chunk size)
sims_playoff_slot <- function(sim, conf, seed) {
  ((sim - 1L) * 2L + conf - 1L) * 16L + seed
}

# Rows of a week. `week` is a factor in simulations. Comparing the integer
# codes in C++ avoids turning millions of rows into strings and is about 4x
# faster than which() on the codes.
sims_week_rows <- function(week, week_num) {
  if (is.factor(week)) {
    code <- match(as.character(week_num), levels(week))
    if (is.na(code)) integer() else positions_of_cpp(week, code)
  } else {
    which(week == week_num)
  }
}

# Name of a built-in compute_results function, NULL for custom functions.
# nfl_simulations() sends built-in functions to the workers by name, so furrr
# doesn't scan them for globals.
sims_builtin_results <- function(fun) {
  if (identical(fun, nflseedR_compute_results)) {
    "nflseedR"
  } else if (identical(fun, cpp_compute_results)) {
    "cpp"
  }
}

# The compute_results function of a name from sims_builtin_results(), or the
# function itself
sims_results_fun <- function(x) {
  if (!is.character(x)) {
    return(x)
  }
  switch(x, nflseedR = nflseedR_compute_results, cpp = cpp_compute_results)
}

# Turn all character columns of `dt` into factors (by reference) and return
# their names. Levels are in order of appearance, so no sorting is needed.
# Parallel workers return integer codes much faster than strings.
sims_encode_strings <- function(dt) {
  cols <- names(dt)[vapply(dt, is.character, logical(1))]
  for (col in cols) {
    x <- dt[[col]]
    levels <- unique(x)
    levels <- levels[!is.na(levels)]
    codes <- chmatch(x, levels)
    data.table::set(dt, j = col, value = factor_from_codes(codes, levels))
  }
  cols
}

factor_from_codes <- function(codes, levels) {
  structure(codes, levels = levels, class = "factor")
}

# Turn the columns `cols` of `dt` back into strings (by reference)
sims_decode_strings <- function(dt, cols) {
  for (col in cols) {
    x <- dt[[col]]
    data.table::set(dt, j = col, value = levels(x)[as.integer(x)])
  }
  invisible(dt)
}

# Played games of a simulation chunk, encoded for the standings engine
sims_encode_played_games <- function(games, sims) {
  played <- which(!is.na(games$result))
  vars <- c("sim", "game_type", "away_team", "home_team", "result")
  games <- lapply(stats::setNames(vars, vars), \(var) games[[var]][played])
  league <- standings_league()
  encoded <- standings_encode_games(games, sims, league, has_scores = FALSE)
  if (anyNA(encoded$away) || anyNA(encoded$home)) {
    standings_check_teams(games$away_team, games$home_team)
  }
  encoded
}

sims_validate_games <- function(games){
  setDT(games)

  # Check required columns --------------------------------------------------
  games_names <- colnames(games)
  required_vars <- c(
    "game_type", "week", "away_team", "home_team",
    "away_rest", "home_rest", "location", "result"
  )
  uses_sim <- all(c("sim", required_vars) %chin% games_names)
  uses_season <- all(c("season", required_vars) %chin% games_names)
  setattr(games, "uses_season", uses_season)
  # we want the season information in attributes for stuff like summary
  if (uses_season) setattr(games, "season", unique(games[["season"]]))

  if( !any(uses_sim, uses_season) ){
    cli::cli_abort(
      "The {.arg games} argument has to be a table including one of the \\
      identifiers {.val sim} or {.val season} as well as \\
      all of the following variables: {.val {required_vars}}!"
    )
  }
  games <- games[, required_vars, with = FALSE]
  games[, old_week := week]
  # Make week a factor so we can filter postseason weeks correctly
  games[, week := fifelse(game_type == "REG", as.character(week), game_type)]
  games[, week := factor(week, levels = unique(week))]

  # Error on too many seasons -----------------------------------------------
  unique_seasons <- if (uses_sim){
    data.table::uniqueN(games[["sim"]])
  } else if (uses_season){
    data.table::uniqueN(games[["season"]])
  } else {
    1L
  }
  if (length(unique_seasons) > 1){
    cli::cli_abort(
      "The identifiers {.val sim} or {.val season} in your  \\
      {.arg games} argument consist of the following unique values: \\
      {.val {unique_seasons}}. {.code nfl_simulations} can only handle one \\
      unique value."
    )
  }

  games
}

sims_compute_playoff_dummy <- function(num_byes){

  n_playoff_games <- c(
    "WC" = 2^3 - num_byes * 2L,
    "DIV" = 2^2,
    "CON" = 2^1,
    "SB" = 2^0
  )

  game_type <- c(
    rep("WC", n_playoff_games[["WC"]]),
    rep("DIV", n_playoff_games[["DIV"]]),
    rep("CON", n_playoff_games[["CON"]]),
    rep("SB", n_playoff_games[["SB"]])
  )

  conf <- c(
    rep("AFC", n_playoff_games[["WC"]] / 2),
    rep("NFC", n_playoff_games[["WC"]] / 2),
    rep("AFC", n_playoff_games[["DIV"]] / 2),
    rep("NFC", n_playoff_games[["DIV"]] / 2),
    rep("AFC", n_playoff_games[["CON"]] / 2),
    rep("NFC", n_playoff_games[["CON"]] / 2),
    NA_character_
  )

  playoff_games <- data.table(
    "game_type" = game_type,
    "week" = as.factor(game_type),
    "conf" = conf,
    "away_team" = NA_character_,
    "home_team" = NA_character_,
    "away_rest" = 7L, # only bye teams have 14 days rest in div round
    "home_rest" = 7L, # only bye teams have 14 days rest in div round
    "location" = "Home", # we don't expect a neutral site playoff game
    "result" = NA_integer_
  )

  setindexv(playoff_games, "game_type")

  wc_home_seeds <- seq(1 + num_byes, length.out = n_playoff_games[["WC"]] / 2)
  wc_away_seeds <- rev(wc_home_seeds + n_playoff_games[["WC"]] / 2)

  # add ids to fill WC games
  playoff_games["WC", home_round_id := paste(conf, wc_home_seeds, sep = "-"), on = "game_type"]
  playoff_games["WC", away_round_id := paste(conf, wc_away_seeds, sep = "-"), on = "game_type"]

  # adjust location and rest default values for SB
  # this means that SB is always simulated as neutral site game although there
  # is a chance that a team can play a home SB. Doesn't happen too often and the
  # home field advantage shouldn't be overestimated anyways
  playoff_games["SB", location := "Neutral", on = "game_type"]
  playoff_games["SB", away_rest := 14L, on = "game_type"]
  playoff_games["SB", home_rest := 14L, on = "game_type"]
  playoff_games
}


#' Compute NFL Game Results in Season Simulations
#'
#' This is the default nflseedR function to compute game results in season
#' simulations.
#'
#' @inherit simulations_verify_fct
#' @param week_num The week of a NFL season for which the function should
#' compute results.
#' @param ... Additional parameters used in the function. It is possible to pass
#' the argument `elo` to the function. This must be a named vector in which the
#' names correspond to the team abbreviations and the values correspond to the
#' initial elo ratings, which are then updated after each week based on the
#' results and transported to the next week.
#'
#' @details
#' This function implements a variant of 538's elo model initially coded
#' by Lee Sharpe (in nflseedR 1.0) and for performance rewritten
#' by Sebastian Carl (in nflseedR 2.0).
#'
#' @return A list of updated `teams` and `games` tables.
#' @export
#' @keywords internal
#' @examples
#' g <- nflseedR::sims_games_example
#' # The functions expects the variable "sim" instead of "season". The example
#' # teams hold simulations 1 and 2, so we simulate the games as sim 1
#' g$sim <- 1L
#' t <- nflseedR::sims_teams_example
#'
#' # Week 6 has a game without result
#' out <- nflseedR_compute_results(
#'   teams = t,
#'   games = g,
#'   week_num = "6"
#' )
#'
#' str(out, max.level = 2)
nflseedR_compute_results <- function(teams, games, week_num, ...) {
  # this example estimates at PK/0 and 50%
  # estimate = is the median spread expected (positive = home team favored)
  # wp = is the probability of the team winning the game
  #
  # only simulate games through week week_num
  # only simulate games with is.na(result)
  # result = how many points home team won by

  # round out (away from zero)
  round_out <- function(x) {
    x <- x[!is.na(x)]
    x[x < 0] <- floor(  x[x < 0])
    x[x > 0] <- ceiling(x[x > 0])
    as.integer(x)
  }

  if (!data.table::is.data.table(games)) data.table::setDT(games)
  if (!data.table::is.data.table(teams)) data.table::setDT(teams)

  # get elo if not in teams data already
  # elo is expected to be a named vector of elo ratings where
  # names are NFL team abbreviations
  if (!"elo" %in% colnames(teams)) {
    # Query arguments in dots and see if elo is in there
    # if not, set it to random values
    args <- list(...)
    if ("elo" %in% names(args)) {
      # pull from custom arguments
      ratings <- args$elo
    } else {
      # if custom elo is missing in dots, start everyone at a random elo
      ratings <- setNames(
        rnorm(length(unique(teams$team)), 1500, 150),
        unique(teams$team)
      )
    }
    # set() adds the column by reference like `:=`
    data.table::set(teams, j = "elo", value = unname(ratings[teams$team]))
  }

  # Row numbers of the current week's games. We compute them once and work
  # on these rows only.
  idx <- which(games$week == week_num)

  # Rows of the away and home teams in the teams data. A row is identified by
  # sim and team, so elo values of one team aren't mixed across simulations.
  # We combine both into one integer key and match() the keys.
  team_ids <- unique(teams$team)
  team_key <- function(sim, team) {
    sim * length(team_ids) + data.table::chmatch(team, team_ids)
  }
  teams_key <- team_key(teams$sim, teams$team)
  away_row <- match(team_key(games$sim[idx], games$away_team[idx]), teams_key)
  home_row <- match(team_key(games$sim[idx], games$home_team[idx]), teams_key)

  # create elo diff
  elo_diff <- teams$elo[home_row] - teams$elo[away_row] +
    (games$home_rest[idx] - games$away_rest[idx]) / 7 * 25
  # adjust elo diff for location = HOME
  # we estimate roughly 25 elo points per actual point
  # with ~1.8 points home field advantage leads, we get 25 * 1.8 = 45 elo points
  home <- games$location[idx] %in% "Home"
  elo_diff[home] <- elo_diff[home] + 45
  # adjust elo_diff for postseason game types
  post <- games$game_type[idx] %in% c("WC", "DIV", "CON", "SB")
  elo_diff[post] <- elo_diff[post] * 1.2
  # create wp and estimate
  wp <- 1 / (10^(-elo_diff / 400) + 1)
  estimate <- elo_diff / 25

  # simulate results of games that don't have one yet. set() changes games
  # by reference like `:=`
  missing <- is.na(games$result[idx])
  if (any(missing)) {
    data.table::set(
      games,
      i = idx[missing],
      j = "result",
      value = round_out(stats::rnorm(sum(missing), estimate[missing], 13))
    )
  }
  result <- games$result[idx]

  # compute elo shift
  outcome <- data.table::fcase(
    is.na(result), NA_real_,
    result > 0, 1,
    result < 0, 0,
    default = 0.5
  )
  elo_input <- data.table::fcase(
    is.na(result), NA_real_,
    result > 0, elo_diff * 0.001 + 2.2,
    result < 0, -elo_diff * 0.001 + 2.2,
    default = 1.0
  )
  elo_mult <- log(pmax(abs(result), 1) + 1.0) * 2.2 / elo_input
  elo_shift <- 20 * elo_mult * (outcome - wp)

  # apply elo shift in teams data to transport new elo values to next week.
  # Teams that didn't play that week (or aren't in the teams data) keep their
  # elo (shift 0)
  team_shift <- numeric(nrow(teams))
  found <- !is.na(away_row)
  team_shift[away_row[found]] <- -elo_shift[found]
  found <- !is.na(home_row)
  team_shift[home_row[found]] <- elo_shift[found]
  team_shift[is.na(team_shift)] <- 0
  data.table::set(teams, j = "elo", value = teams$elo + team_shift)

  list("teams" = teams, "games" = games)
}

#' Compute NFL Game Results in Season Simulations with C++
#'
#' A C++ port of [nflseedR_compute_results()]. It returns the same results
#' for the same random seed and is about 2 times faster, which makes
#' [nfl_simulations()] about a third faster. Use it with
#' `nfl_simulations(compute_results = cpp_compute_results)`.
#'
#' @inheritParams nflseedR_compute_results
#' @inherit nflseedR_compute_results return
#' @export
#' @keywords internal
#' @examples
#' g <- nflseedR::sims_games_example
#' g$sim <- 1L
#' t <- nflseedR::sims_teams_example
#'
#' out <- cpp_compute_results(
#'   teams = t,
#'   games = g,
#'   week_num = "6"
#' )
#'
#' str(out, max.level = 2)
cpp_compute_results <- function(teams, games, week_num, ...) {
  if (!data.table::is.data.table(games)) {
    data.table::setDT(games)
  }
  if (!data.table::is.data.table(teams)) {
    data.table::setDT(teams)
  }

  # Same elo start values as nflseedR_compute_results()
  if (!"elo" %in% colnames(teams)) {
    args <- list(...)
    if ("elo" %in% names(args)) {
      ratings <- args$elo
    } else {
      ratings <- stats::setNames(
        stats::rnorm(length(unique(teams$team)), 1500, 150),
        unique(teams$team)
      )
    }
    data.table::set(teams, j = "elo", value = unname(ratings[teams$team]))
  }

  idx <- sims_week_rows(games$week, week_num)
  if (length(idx) == 0L) {
    return(list("teams" = teams, "games" = games))
  }

  team_ids <- unique(teams$team)
  out <- elo_week_cpp(
    game_sim = as.integer(games$sim[idx]),
    away = data.table::chmatch(games$away_team[idx], team_ids),
    home = data.table::chmatch(games$home_team[idx], team_ids),
    rest_diff = as.double(games$home_rest[idx] - games$away_rest[idx]),
    home_field = games$location[idx] %chin% "Home",
    postseason = games$game_type[idx] %chin% c("WC", "DIV", "CON", "SB"),
    result = as.integer(games$result[idx]),
    team_sim = as.integer(teams$sim),
    team = data.table::chmatch(teams$team, team_ids),
    elo = as.double(teams$elo)
  )

  if (anyNA(games$result[idx])) {
    data.table::set(games, i = idx, j = "result", value = out$result)
  }
  data.table::set(teams, j = "elo", value = out$elo)
  list("teams" = teams, "games" = games)
}
