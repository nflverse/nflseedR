# nfl_standings() after argument matching. The work happens in the C++ engine
# (src/, dev/cpp-standings-engine.md). R validates, encodes teams and seasons
# as integers and decodes the result.
standings_compute <- function(
  games,
  ranks,
  tiebreaker_depth,
  playoff_seeds,
  verbosity,
  season_complete = NULL,
  call = rlang::caller_env()
) {
  games <- standings_validate_games(games, call = call)
  depth <- standings_depth(tiebreaker_depth)
  if (!is.null(playoff_seeds) && !inrange(playoff_seeds, 1L, 16L)) {
    cli::cli_abort(
      "The {.arg playoff_seeds} argument must be in range {.pkg 1} - {.pkg 16}",
      call = call
    )
  }
  has_scores <- attr(games, "has_scores")
  if (depth == 3L && !has_scores) {
    cli::cli_abort(
      "{.code tiebreaker_depth = \"POINTS\"} needs the variables \\
      {.val home_score} and {.val away_score} in {.arg games}.",
      call = call
    )
  }
  if (verbosity != "NONE") {
    report("Compute standings")
  }

  league <- standings_league()
  sims <- sort(unique(games$sim))
  standings_run(
    standings_encode_games(games, sims, league, has_scores, call = call),
    sims = sims,
    season_complete = season_complete %||%
      standings_season_complete(games, sims),
    ranks = ranks,
    depth = depth,
    playoff_seeds = playoff_seeds,
    verbosity = verbosity,
    uses_season = attr(games, "uses_season"),
    integer_scores = has_scores &&
      is.integer(games$away_score) &&
      is.integer(games$home_score)
  )
}

# Run the C++ engine on encoded games (standings_encode_games()) and decode
# the result. nfl_simulations() calls it directly to skip validating and
# encoding the games again.
standings_run <- function(
  encoded,
  sims,
  season_complete,
  ranks,
  depth,
  playoff_seeds,
  verbosity,
  uses_season = FALSE,
  integer_scores = FALSE
) {
  has_scores <- encoded$has_scores
  league <- standings_league()
  out <- standings_cpp(
    encoded,
    season_complete = as.integer(rep_len(season_complete, length(sims))),
    ranks = match(ranks, c("NONE", "DIV", "CONF", "DRAFT")) - 1L,
    depth = depth,
    playoff_seeds = playoff_seeds %||% 16L
  )

  # Build all columns as a list and convert once. Every `[.data.table` call
  # costs time that matters for single seasons
  team <- out$team + 1L
  cols <- list(
    sim = sims[out$sim],
    team = league$team[team],
    conf = league$conf[team],
    division = league$division[team],
    games = out$games,
    wins = out$wins,
    true_wins = out$true_wins,
    losses = out$losses,
    ties = out$ties
  )
  if (has_scores) {
    as_score <- if (integer_scores) as.integer else identity
    cols$pf <- as_score(out$pf)
    cols$pa <- as_score(out$pa)
    cols$pd <- cols$pf - cols$pa
  }
  cols[c("win_pct", "div_pct", "conf_pct", "sov", "sos")] <-
    out[c("win_pct", "div_pct", "conf_pct", "sov", "sos")]
  if (ranks != "NONE") {
    cols$div_rank <- out$div_rank
    cols$div_tie_broken_by <- standings_labels(out$div_label, "div")
  }
  if (ranks %in% c("CONF", "DRAFT")) {
    cols$conf_rank <- out$conf_rank
    cols$conf_tie_broken_by <- standings_labels(out$conf_label, "conf")
  }
  if (ranks == "DRAFT") {
    cols$exit <- unname(sims_exit_translate_to("CHAR"))[out$exit + 1L]
    cols$draft_rank <- out$draft_rank
    cols$draft_tie_broken_by <- standings_labels(out$draft_label, "draft")
  }
  if (uses_season) {
    names(cols)[1] <- "season"
  }
  standings <- setDT(cols)
  if (verbosity == "MAX") {
    standings_report_labels(standings)
  }
  standings[]
}

# Draft ranks for standings computed inside nfl_simulations(). Division and
# conference ranks (all 16) and the integer `exit` are already there.
# `encoded` are the played games that standings_run() got, `sims` their
# season ids.
standings_add_draft_ranks <- function(
  standings,
  encoded,
  sims,
  tiebreaker_depth
) {
  # Draft ranks only use regular season games
  reg <- encoded$type == 0L
  vars <- c("sim", "type", "away", "home", "result")
  encoded[vars] <- lapply(encoded[vars], \(x) x[reg])
  out <- draft_ranks_cpp(
    encoded,
    row_sim = match(standings$sim, sims),
    row_team = chmatch(standings$team, standings_league()$team) - 1L,
    div_rank = as.integer(standings$div_rank),
    conf_rank = as.integer(standings$conf_rank),
    exit = as.integer(standings$exit),
    depth = standings_depth(tiebreaker_depth)
  )
  standings[, `:=`(
    draft_rank = out$draft_rank,
    draft_tie_broken_by = standings_labels(out$draft_label, "draft")
  )]
  standings[]
}

# Games as the C++ engine expects them: dense season ids, 0-based team ids,
# integer game types. One list, unpacked by make_games() in src/bindings.cpp
standings_encode_games <- function(
  games,
  sims,
  league,
  has_scores,
  call = rlang::caller_env()
) {
  list(
    sim = match(games$sim, sims),
    type = standings_game_type(games$game_type, call = call),
    away = chmatch(games$away_team, league$team) - 1L,
    home = chmatch(games$home_team, league$team) - 1L,
    result = as.double(games$result),
    away_score = if (has_scores) as.double(games$away_score) else double(),
    home_score = if (has_scores) as.double(games$home_score) else double(),
    has_scores = has_scores,
    n_sims = length(sims),
    team_division = league$division_id,
    team_conference = league$conf_id
  )
}

# Net wins break ties of equal percentages only during the regular season
# (D13). A season is complete once its final regular season week (17 through
# 2020, 18 since 2021) or a playoff game has been played. With `sim` instead of
# `season`, the year is unknown and the current 18 weeks are assumed.
standings_season_complete <- function(games, sims) {
  # Plain vectors because `games` can also be a tibble (nflreadr)
  reg <- games$game_type == "REG"
  last_week <- data.table(sim = games$sim[reg], week = games$week[reg])[,
    list(week = max(week)),
    by = "sim"
  ]
  last_week <- last_week$week[match(sims, last_week$sim)]
  final_week <- if (attr(games, "uses_season")) {
    fifelse(sims <= 2020, 17L, 18L)
  } else {
    18L
  }
  playoffs <- sims %in% games$sim[!reg]
  playoffs | (!is.na(last_week) & last_week >= final_week)
}

standings_depth <- function(tiebreaker_depth) {
  match(tiebreaker_depth, c("RANDOM", "PRE-SOV", "SOS", "POINTS")) - 1L
}

# Cache of standings_league()
the <- new.env(parent = emptyenv())

# The 32 valid teams in alphabetical order with 0-based division and
# conference ids (also alphabetical), as the C++ engine expects them. Built
# once per session.
standings_league <- function() {
  the$league <- the$league %||% standings_league_build()
  the$league
}

standings_league_build <- function() {
  league <- nflseedR::divisions[
    !nflseedR::divisions$team %in% c("LAR", "OAK", "STL", "SD"),
    c("team", "conf", "division")
  ]
  league <- league[order(league$team), ]
  league$division_id <- match(league$division, sort(unique(league$division))) -
    1L
  league$conf_id <- match(league$conf, sort(unique(league$conf))) - 1L
  league
}

standings_game_type <- function(game_type, call = rlang::caller_env()) {
  type <- chmatch(game_type, c("REG", "WC", "DIV", "CON", "SB")) - 1L
  if (anyNA(type)) {
    invalid <- unique(game_type[is.na(type)])
    cli::cli_abort(
      "{.arg games} includes invalid {.val game_type} values: {.val {invalid}}",
      call = call
    )
  }
  type
}

# Turn label codes of the C++ engine (step * 10 + number of tied teams) into
# text. Step ids are defined in src/engine/types.hpp.
standings_labels <- function(code, procedure = c("div", "conf", "draft")) {
  procedure <- rlang::arg_match(procedure)
  steps <- c(
    "1" = "Head-To-Head Win PCT",
    "2" = switch(procedure, conf = "Head-To-Head Sweep", "Head-To-Head"),
    "3" = "Division Win PCT",
    "4" = "Common Games Win PCT",
    "5" = "Conference Win PCT",
    "6" = "SOV",
    "7" = "SOS",
    "8" = "Conference Points Rank",
    "9" = "League Points Rank",
    "10" = "Common Games Point Differential",
    "11" = "Conference Point Differential",
    "12" = switch(
      procedure,
      conf = "League Point Differential",
      "Point Differential"
    ),
    "13" = "Coin Toss",
    "14" = "Division Tiebreaker",
    "15" = "Conference Tiebreaker"
  )
  # Build the text once per distinct code
  codes <- unique(code)
  step <- unname(steps[as.character(codes %/% 10L)])
  n <- codes %% 10L
  text <- fifelse(n > 0L, paste0(step, " (", n, ")"), step, na = NA_character_)
  text[match(code, codes)]
}

# verbosity = "MAX": how often each tiebreaker decided a rank
standings_report_labels <- function(standings) {
  cols <- intersect(
    c("div_tie_broken_by", "conf_tie_broken_by", "draft_tie_broken_by"),
    names(standings)
  )
  for (col in cols) {
    counts <- table(standings[[col]])
    if (length(counts) == 0L) {
      next
    }
    report(
      "{toupper(sub('_tie_broken_by', '', col))} ties broken by: \\
      {paste0(names(counts), ' x ', counts, collapse = ', ')}"
    )
  }
}
