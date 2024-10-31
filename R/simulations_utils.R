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

sims_validate_games <- function(games){
  setDT(games)

  # Check required columns --------------------------------------------------
  games_names <- colnames(games)
  required_vars <- c(
    "game_type", "week", "away_team", "home_team",
    "away_rest", "home_rest", "location", "result"
  )
  uses_sim <- all(c("sim", required_vars) %in% games_names)
  uses_season <- all(c("season", required_vars) %in% games_names)
  setattr(games, "uses_season", uses_season)
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


#' @export
#' @noRd
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

  games_indices <- data.table::indices(games)
  if (is.null(games_indices) || !"week" %chin% games_indices){
    data.table::setindexv(games, c("week", "location", "game_type"))
  }

  # get elo if not in teams data already
  # elo is expected to be a named vector of elo ratings where
  # names are NFL team abbreviations
  if (!"elo" %chin% colnames(teams)) {
    # Query arguments in dots and see if elo is in there
    # if not, set it to random values
    args <- list(...)
    if ("elo" %chin% names(args)) {
      # pull from custom arguments
      ratings <- args$elo
      teams[, elo := ratings[team]]
    } else {
      # if custom elo is missing in dots, start everyone at a random elo
      ratings <- setNames(
        rnorm(length(unique(teams$team)), 1500, 150),
        unique(teams$team)
      )
      teams[, elo := ratings[team]]
    }
  }

  # At this point, there is a column named "elo" in the teams data
  # We use that column to create a elo lookup vector that we use to
  # add those elo ratings to home and away teams in the games data
  # Names of that vector are built off sim and team to make sure
  # we don't mix elo values of one team across simulations
  ratings <- teams[, setNames(elo, paste(sim, team, sep = "-"))]

  # fill elo values of home and away teams
  games[week_num, away_elo := ratings[paste(sim, away_team, sep = "-")], on = "week"]
  games[week_num, home_elo := ratings[paste(sim, home_team, sep = "-")], on = "week"]

  # create elo diff
  games[week_num, elo_diff := home_elo - away_elo + (home_rest - away_rest) / 7 * 25, on = "week"]
  # adjust elo diff for location = HOME
  games[list(week_num, "Home"), elo_diff := elo_diff + 20, on = c("week", "location")]
  # adjust elo_diff for game type = REG
  games[list(week_num, "REG"), elo_diff := elo_diff * 1.2, on = c("week", "game_type")]
  # create wp and estimate
  games[week_num, `:=`(wp = 1 / (10^(-elo_diff / 400) + 1),
                       estimate = elo_diff / 25), on = "week"]
  # adjust result in current week
  games[week_num == week & is.na(result),
        result := round_out(rnorm(.N, estimate, 13))]
  # compute elo shift
  games[week_num, `:=`(
    outcome = data.table::fcase(
      is.na(result), NA_real_,
      result > 0, 1,
      result < 0, 0,
      default = 0.5
    ),
    elo_input = data.table::fcase(
      is.na(result), NA_real_,
      result > 0, elo_diff * 0.001 + 2.2,
      result < 0, -elo_diff * 0.001 + 2.2,
      default = 1.0
    )
  ), on = "week"]
  games[week_num, elo_mult := log(pmax(abs(result), 1) + 1.0) * 2.2 / elo_input, on = "week"]
  games[week_num, elo_shift := 20 * elo_mult * (outcome - wp), on = "week"]

  # Build elo_shift vector based on results if home and away teams
  elo_change_away <- games[week_num, setNames(-elo_shift, paste(sim, away_team, sep = "-")), on = "week"]
  elo_change_home <- games[week_num, setNames(elo_shift,  paste(sim, home_team, sep = "-")), on = "week"]
  elo_change <- c(elo_change_away, elo_change_home)

  # drop helper columns
  drop_cols <- c("away_elo", "home_elo", "elo_diff", "wp", "estimate",
                 "outcome", "elo_input", "elo_mult", "elo_shift")
  games[, (drop_cols) := NULL]

  # apply elo shift in teams data to transport new elo values to next week
  teams[, elo_shift := elo_change[paste(sim, team, sep = "-")]]
  # teams that didn't play that week are missing in elo_change. Their shift
  # value will be NA. We set it to 0 to be able to add the shift for all teams
  teams[, elo_shift := data.table::fifelse(is.na(elo_shift), 0, elo_shift)]
  teams[, elo := elo + elo_shift]
  # remove the shift variable for this round
  teams[, elo_shift := NULL]

  list("teams" = teams, "games" = games)
}
