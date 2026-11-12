# Diagnostics for cases where nfl_clinching_status() is later than the NFL.
# Used for milestones 6 and 7 of dev/playoff-status.md. Not part of the
# package (dev/ is in .Rbuildignore).
#
#   source("dev/clinching-diagnostics.R")
#   cases <- later_cases()                  # the open cases of the snapshot
#   x <- cases[1]
#   single_team_fixes(x)   # which competitor fixed to win/lose out proves it
#   sides <- tie_sides(x)  # who ends level with X and on which side
#   sides$teams[mixed == TRUE] # both sides: gap 3, no per-team side exists
#   show_completion(x$season, x$nfl, winners = c("PHI", "ATL"))

devtools::load_all(quiet = TRUE)
library(data.table)

diag_games <- function() {
  as.data.table(readRDS("tests/testthat/games_2006_2025.rds"))[
    game_type == "REG"
  ]
}

# Cases of the NFL comparison snapshot where we're later than the NFL
later_cases <- function() {
  fread("tests/testthat/_snaps/clinching_status/nfl_status_differences.csv")[
    ours_in_nfl_week == "undecided"
  ]
}

# The state after `last_week`: later results set to NA
state_after <- function(season_id, last_week) {
  g <- diag_games()[season == season_id]
  g[week > last_week, result := NA]
  g[]
}

# All remaining games of `team` won (or lost)
fix_team <- function(g, team, win) {
  g <- copy(g)
  s <- if (win) 3 else -3
  g[is.na(result) & home_team == team, result := s]
  g[is.na(result) & away_team == team, result := -s]
  g[]
}

goal_column <- c(
  div = "div_status",
  playoffs = "playoffs_status",
  eliminated = "playoffs_status",
  seed1 = "seed1_status"
)
goal_target <- c(
  div = "clinched",
  playoffs = "clinched",
  eliminated = "eliminated",
  seed1 = "clinched"
)
goal_status <- c(
  div = "DIV",
  playoffs = "PLAYOFFS",
  eliminated = "PLAYOFFS",
  seed1 = "1SEED"
)

proven <- function(g, x, goal) {
  s <- clinching_status_compute(g, status = goal_status[[goal]])
  s[s$team == x][[goal_column[[goal]]]] == goal_target[[goal]]
}

# Fix each conference competitor's remaining games (win out, lose out) in the
# NFL's week and report the fixes that prove the case. Any fix is a valid
# restriction (a true clinch holds in every completion), but a fix that
# touches X's own games changes X's worst case, so read those with care.
single_team_fixes <- function(case) {
  g <- state_after(case$season, case$nfl)
  league <- standings_league()
  conf <- league$conf[league$team == case$team]
  left <- unique(c(g[is.na(result), home_team], g[is.na(result), away_team]))
  teams <- setdiff(intersect(league$team[league$conf == conf], left), case$team)
  fixes <- CJ(team = teams, win = c(TRUE, FALSE))
  fixes[,
    proves := mapply(
      \(t, w) proven(fix_team(g, t, w), case$team, case$goal),
      team,
      win
    )
  ]
  fixes[proves == TRUE, paste0(team, ifelse(win, " wins out", " loses out"))]
}

# Random completions in the NFL's week with X at its worst case (loses out for
# a clinch, wins out for an elimination). For every team that ends level with
# X: in how many of these ties it ranks ahead of X. `mixed` teams rank on both
# sides, so no per-team side exists (gap 3). `wrong` counts completions that
# contradict the NFL (should be 0). `steps` are the tiebreaking steps that
# placed X in those ties.
tie_sides <- function(case, n = 2000, tie_prob = 0.06) {
  x <- case$team
  clinch <- case$goal != "eliminated"
  g <- fix_team(state_after(case$season, case$nfl), x, win = !clinch)
  rem <- which(is.na(g$result))
  sims <- g[rep(seq_len(.N), n)][, sim := rep(seq_len(n), each = nrow(g))]
  sims[, result := as.numeric(result)]
  idx <- rep(rem, n) + rep((seq_len(n) - 1) * nrow(g), each = length(rem))
  sims[
    idx,
    result := sample(
      c(3, -3, 0),
      length(idx),
      TRUE,
      c((1 - tie_prob) / 2, (1 - tie_prob) / 2, tie_prob)
    )
  ]
  sims[, season := NULL]
  st <- as.data.table(nfl_standings(
    sims,
    verbosity = "NONE",
    playoff_seeds = 16
  ))
  xc <- st[team == x, conf][1]
  st <- st[conf == xc]
  xs <- st[
    team == x,
    list(
      sim,
      xpct = win_pct,
      xdiv = div_rank,
      xconf = conf_rank,
      xdivision = division,
      xstep = if (case$goal == "div") div_tie_broken_by else conf_tie_broken_by
    )
  ]
  seeds <- if (case$season >= 2020) 7L else 6L
  reached <- switch(
    case$goal,
    div = xs$xdiv == 1L,
    seed1 = xs$xconf == 1L,
    xs$xconf <= seeds
  )
  level <- merge(st[team != x], xs, by = "sim")[win_pct == xpct]
  if (case$goal == "div") {
    level <- level[division == xdivision][, ahead := div_rank < xdiv]
  } else {
    level[, ahead := conf_rank < xconf]
  }
  teams <- level[, list(ties = .N, ahead = sum(ahead)), by = "team"]
  teams[, mixed := ahead > 0 & ahead < ties]
  setorderv(teams, c("mixed", "ties"), c(-1L, -1L))
  steps <- xs[sim %in% level$sim, sub(" \\(.*", "", xstep)]
  list(
    wrong = if (clinch) sum(!reached) else sum(reached),
    tie_sims = uniqueN(level$sim),
    teams = teams[],
    steps = sort(table(steps), decreasing = TRUE)
  )
}

# One completion through the engine: `winners` win all their remaining games
# (in order, so later teams don't overwrite earlier ones), every other game
# goes to the home team. Prints the conference standings.
show_completion <- function(season_id, last_week, winners, conference = NULL) {
  g <- state_after(season_id, last_week)
  for (w in winners) {
    g <- fix_team(g, w, win = TRUE)
  }
  g[is.na(result), result := 3]
  seeds <- if (season_id >= 2020) 7L else 6L
  st <- as.data.table(nfl_standings(
    g,
    verbosity = "NONE",
    playoff_seeds = seeds
  ))
  if (!is.null(conference)) {
    st <- st[conf == conference]
  }
  st[order(conf, conf_rank, -win_pct)][, list(
    conf,
    team,
    wins,
    losses,
    ties,
    div_rank,
    div_tie_broken_by,
    conf_rank,
    conf_tie_broken_by
  )]
}
