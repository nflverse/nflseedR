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
  # Filter with a logical expression instead of a join on "game_type" because
  # data.table's optimized grouping (GForce) is not available in joins
  out <- gd[
    game_type == "REG",
    list(
      h2h_games = .N,
      h2h_wins = sum(outcome),
      h2h_pd = sum(result)
    ),
    by = c("sim", "team", "opp")
  ]
  setkeyv(out, c("sim", "team", "opp"))
  out
}

# Rank `x` (a list of columns) within groups `by` (a list of columns) with
# ties.method = "min". Same result as
# `dt[, frankv(x, ties.method = "min"), by = by]` but without calling frankv()
# once per group: a row's rank within its group equals its rank across all
# groups minus the number of rows in groups that sort before its group.
frank_by <- function(by, x) {
  frankv(c(by, x), ties.method = "min") - frankv(by, ties.method = "min") + 1L
}

# Minimum of `x` within groups `by` (a list of columns), expanded to all rows.
# Same result as `dt[, min(x), by = by]` but uses data.table's optimized
# grouping (GForce).
min_by <- function(x, by) {
  dt <- data.table(group = frankv(by, ties.method = "dense"), x = x)
  dt[, min_x := min(x), by = "group"][["min_x"]]
}

# Identify the winner and the losers of every tie in `group` where a higher
# `value` is better. A tie has a winner only if exactly one team has the best
# value. Every team without the best value loses. Missing values are worse
# than any other value and tie with each other. Same result as
# `frankv(-value, ties.method = "max") == 1` and
# `frankv(-value, ties.method = "dense") != 1` by group, but without calling
# frankv() once per group. Returns a list of the logical vectors
# `tie_winner` and `tie_loser`.
tie_winner_loser <- function(group, value) {
  dt <- data.table(group = group, value = value, best = value[NA_integer_])
  # data.table evaluates `max()` once even if no rows are selected. Skip this
  # case to avoid warnings from `max()` of an empty vector
  if (!all(is.na(value))) {
    group_best <- dt[!is.na(value), list(best = max(value)), by = "group"]
    dt[group_best, best := i.best, on = "group"]
  }
  # If all values of a group are missing, all teams share the best value
  dt[, is_best := is.na(best) | (!is.na(value) & value == best)]
  dt[, n_best := sum(is_best), by = "group"]
  list(
    tie_winner = dt[["is_best"]] & dt[["n_best"]] == 1L,
    tie_loser = !dt[["is_best"]]
  )
}

# Add all non-key columns of `i` to `x` by reference (left join on `on`).
# Same result as merge(x, i, by = on, all.x = TRUE) when x is sorted by `on`,
# but without copying x.
join_cols <- function(x, i, on = c("sim", "team")) {
  cols <- setdiff(names(i), on)
  idx <- i[x, on = on, which = TRUE, mult = "first"]
  x[, (cols) := lapply(cols, function(col) i[[col]][idx])]
  x
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
