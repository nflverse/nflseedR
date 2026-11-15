# TEMPORARY (2026-11-15, decided by the maintainer): remove this file once
# the work on the open No. 1 seed cases of week 10 of 2026 is done (the SOV
# gap, see "SOV with many open games (handoff)" in dev/playoff-status.md).
# It holds scenarios copied from an external site.
#
# The 3^7 results of seven week 10 games of 2026 and whether LAC is out of
# the race for the AFC's No. 1 seed in each, by nflplayoffscenarios.com and
# by clinching_status_compute(). Used for milestones 11 to 13. Not part of
# the package (dev/ is in .Rbuildignore).
#
#   source("dev/clinching-week10-2026.R")
#   w <- week10_compare()         # ours against the site, all 2,187 results
#   table(w$site, w$ours)
#   w[site == "eliminated" & ours == "undecided"]  # the open cases
#   week10_games(w$result[[412]]) # the games of one result, for
#                                 # clinching_status_compute()
#   before <- all_statuses()      # every week of 2006-2025 and week 10
#   # ... change the code, then:
#   changed_statuses(before, all_statuses())
#
# Build an optimized library first (load_all() compiles with -O0, which is
# about ten times slower): pkgbuild::clean_dll();
# pkgbuild::compile_dll(debug = FALSE); devtools::load_all(compile = FALSE)

devtools::load_all(quiet = TRUE)
library(data.table)

# "LAC is knocked out of 1 seed contention with:", the 50 scenarios of
# https://nflplayoffscenarios.com/2026/week/10/week-10-live-picture/ (read
# 2026-11-15, written before the week 10 games). Every scenario also has
# "LAC loss". Each is one way; LAC is out if any of them holds.
site_scenarios <- c(
  "BUF win + KC win + NE tie",
  "BUF win + KC win + MIA win",
  "BUF win + KC win/tie + LV win",
  "BUF win + KC win + LV tie + CLE tie",
  "BUF win + KC win + CLE win",
  "BUF win + KC tie + NE tie + LV tie + CLE tie",
  "BUF win + KC tie + NE tie + CLE win",
  "BUF win + KC tie + MIA win + LV tie + CLE tie",
  "BUF win + KC tie + MIA win + CLE win",
  "BUF win + KC tie + LV tie + CLE win",
  "BUF win + NE win",
  "BUF win + NE tie + MIA win",
  "BUF win + NE tie + LV win",
  "BUF win + NE tie + LV tie + CLE win",
  "BUF win + MIA win + LV win",
  "BUF win + MIA win + LV tie + CLE win",
  "BUF win + LV win + CLE win/tie",
  "BUF tie + KC win + NE win",
  "BUF tie + KC win + NE tie + LV win",
  "BUF tie + KC win + NE tie + LV tie + CLE tie",
  "BUF tie + KC win + NE tie + CLE win",
  "BUF tie + KC win + MIA win + LV tie + CLE tie",
  "BUF tie + KC win + MIA win + LV win",
  "BUF tie + KC win + MIA win + CLE win",
  "BUF tie + KC win + LV win + CLE win/tie",
  "BUF tie + KC win + LV tie + CLE win",
  "BUF tie + KC tie + NE win + LV tie + CLE tie",
  "BUF tie + KC tie + NE win + CLE win",
  "BUF tie + KC tie + NE tie + LV win + MIA win",
  "BUF tie + KC tie + NE tie + LV win + CLE tie",
  "BUF tie + KC tie + LV win + CLE win",
  "BUF tie + KC tie + MIA win + LV win + CLE tie",
  "BUF tie + KC tie + MIA win + LV tie + CLE win",
  "BUF tie + KC tie + NE tie + LV tie + CLE win",
  "BUF tie + NE win + LV win",
  "BUF tie + NE win + LV tie + CLE win",
  "BUF tie + NE tie + MIA win + LV win + CLE tie",
  "BUF tie + NE tie + LV win + CLE win",
  "BUF tie + MIA win + LV win + CLE win",
  "KC win + NE win + LV win",
  "KC win + NE win + LV tie + CLE tie",
  "KC win + NE win + CLE win",
  "KC win + NE tie + LV win + CLE win/tie",
  "KC win + NE tie + LV tie + CLE win",
  "KC win + MIA win + LV win + CLE win/tie",
  "KC win + MIA win + LV tie + CLE win",
  "KC tie + NE win + LV win + CLE tie",
  "KC tie + NE win + LV tie + CLE win",
  "KC tie + NE tie + MIA win + LV win + CLE win",
  "NE win + LV win + CLE win"
)

# The seven games the scenarios are about, by home team (the names
# games_2026_week_10() in tests/testthat/helpers.R takes), and the sign of
# the home result (home - away) when the scenario's team wins:
# LAC@BAL, BUF@NYJ, KC@ATL, NE@DET, MIA@IND, SEA@LV, HOU@CLE
site_games <- data.table(
  team = c("LAC", "BUF", "KC", "NE", "MIA", "LV", "CLE"),
  home = c("BAL", "NYJ", "ATL", "DET", "IND", "LV", "CLE"),
  win = c(-1L, -1L, -1L, -1L, -1L, 1L, 1L)
)

# All 3^7 results: one row per result, one column per home team with the
# home result (3 home win, 0 tie, -3 away win)
week10_grid <- function() {
  grid <- as.matrix(expand.grid(rep(list(c(3L, 0L, -3L)), nrow(site_games))))
  colnames(grid) <- site_games$home
  grid
}

# Whether the site has LAC out of the No. 1 seed race in each row of `grid`
site_eliminated <- function(grid = week10_grid()) {
  holds <- function(scenario) {
    # LAC loss: BAL wins at home
    ok <- grid[, "BAL"] == 3L
    for (condition in strsplit(scenario, " + ", fixed = TRUE)[[1]]) {
      parts <- strsplit(condition, " ", fixed = TRUE)[[1]]
      game <- site_games[team == parts[[1]]]
      results <- c(win = 3L * game$win, tie = 0L)[
        strsplit(parts[[2]], "/", fixed = TRUE)[[1]]
      ]
      ok <- ok & grid[, game$home] %in% results
    }
    ok
  }
  Reduce(`|`, lapply(site_scenarios, holds))
}

# The 2026 season after week 9 with one row of the grid as week 10 results
week10_games <- function(results) {
  games <- as.data.table(readRDS("tests/testthat/games_2026_week_9.rds"))
  rows <- match(names(results), games[week == 10, home_team])
  games[week == 10, result := replace(result, rows, as.integer(results))]
  games[]
}

# LAC's No. 1 seed status in every result (about 15s on 8 cores, optimized
# build), next to the site. Expect 339 site eliminations, never "alive" for
# one of them, and never "eliminated" for one of the others.
week10_compare <- function(search = FALSE, cores = 8L) {
  grid <- week10_grid()
  ours <- unlist(parallel::mclapply(
    seq_len(nrow(grid)),
    function(i) {
      s <- clinching_status_compute(
        week10_games(grid[i, ]),
        status = "1SEED",
        search = search
      )
      s[team == "LAC", seed1_status]
    },
    mc.cores = cores
  ))
  data.table(
    i = seq_len(nrow(grid)),
    site = ifelse(site_eliminated(grid), "eliminated", "not eliminated"),
    ours = ours,
    result = lapply(seq_len(nrow(grid)), \(i) grid[i, ])
  )
}

# Every status of every week of 2006-2025, with and without the search, and
# LAC's of week 10 of 2026: the baseline to compare a change with
all_statuses <- function(cores = 8L) {
  games <- as.data.table(readRDS("tests/testthat/games_2006_2025.rds"))[
    game_type == "REG"
  ]
  weeks <- unique(games[, list(season, week)])
  weekly <- function(search) {
    rbindlist(parallel::mclapply(
      seq_len(nrow(weeks)),
      function(i) {
        g <- games[season == weeks$season[i]]
        g[week > weeks$week[i], result := NA]
        s <- clinching_status_compute(g, search = search)
        s[, list(
          season,
          week = weeks$week[i],
          team,
          search = search,
          div_status,
          playoffs_status,
          seed1_status
        )]
      },
      mc.cores = cores
    ))
  }
  list(
    weekly = rbind(weekly(FALSE), weekly(TRUE)),
    week10 = week10_compare(cores = cores)
  )
}

# What changed between two results of all_statuses(). Sound changes only
# turn "undecided" into something else, and week 10 never contradicts the
# site.
changed_statuses <- function(before, after) {
  weekly <- merge(
    melt(before$weekly, id.vars = c("season", "week", "team", "search")),
    melt(after$weekly, id.vars = c("season", "week", "team", "search")),
    by = c("season", "week", "team", "search", "variable"),
    suffixes = c("_before", "_after")
  )
  list(
    weekly = weekly[value_before != value_after],
    week10 = table(
      site = after$week10$site,
      before = before$week10$ours,
      after = after$week10$ours
    )
  )
}
