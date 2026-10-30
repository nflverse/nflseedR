test_that("standings works", {
  g <- try(nflreadr::load_schedules(2008:2023), silent = TRUE)
  skip_if(inherits(g, "try-error"))

  standings <- nflseedR::nfl_standings(
    g,
    ranks = "DRAFT",
    verbosity = "NONE"
  ) |>
    data.table::setDF()
  expect_snapshot_value(standings, style = "json2", variant = "standings")

  # trigger warning related to presence of sim and season
  g$sim <- g$season * 10
  expect_warning(
    standings <- nflseedR::nfl_standings(g, ranks = "NONE", verbosity = "NONE"),
    regexp = 'The `games` argument includes both "sim" and "season".'
  )
})

test_that("standings of played games are stable", {
  g <- data.table::as.data.table(nflseedR::sims_games_example)
  g <- g[!is.na(result)]
  standings <- nflseedR::nfl_standings(
    g,
    ranks = "DRAFT",
    verbosity = "NONE"
  ) |>
    data.table::setDF()
  expect_snapshot_value(standings, style = "json2", variant = "standings")
})

# Synthetic tie-heavy seasons that reach the deep tiebreakers. These snapshots
# are the reference for the C++ standings engine (dev/cpp-standings-engine.md)
test_that("deep tiebreakers are stable", {
  games <- load_deep_tiebreaker_games()
  for (depth in c("POINTS", "SOS", "PRE-SOV")) {
    standings <- with_reference_seed(
      20261030,
      nfl_standings(
        data.table::copy(games),
        ranks = "DRAFT",
        tiebreaker_depth = depth,
        verbosity = "NONE"
      )
    )
    expect_snapshot_file(
      standings_csv(standings),
      paste0("deep_tiebreakers_", tolower(depth), ".csv")
    )
  }
})

test_that("deep tiebreaker games reach the deep tiebreakers", {
  games <- load_deep_tiebreaker_games()
  standings <- with_reference_seed(
    20261030,
    nfl_standings(
      data.table::copy(games),
      ranks = "DRAFT",
      tiebreaker_depth = "POINTS",
      verbosity = "NONE"
    )
  )
  expect_contains(
    standings$div_tie_broken_by,
    c(
      "SOV (2)",
      "SOS (2)",
      "Conference Points Rank (2)",
      "League Points Rank (2)",
      "Common Games Point Differential (2)",
      "Point Differential (2)",
      "Coin Toss"
    )
  )
  expect_contains(
    standings$conf_tie_broken_by,
    c(
      "Division Tiebreaker",
      "Head-To-Head Sweep (4)",
      "Common Games Win PCT (4)",
      "SOV (2)",
      "SOS (2)",
      "Conference Points Rank (2)",
      "League Points Rank (2)",
      "Conference Point Differential (2)",
      "League Point Differential (2)",
      "Coin Toss"
    )
  )
  expect_contains(
    standings$draft_tie_broken_by,
    c(
      "Division Tiebreaker",
      "Conference Tiebreaker",
      "Head-To-Head (2)",
      "Common Games Win PCT (2)",
      "SOV (2)",
      "Coin Toss"
    )
  )
})

test_that("standings of a partly played week 1 are stable", {
  games <- load_deep_tiebreaker_games()
  games <- games[sim == 1L & week == 1L][1:6]
  standings <- with_reference_seed(
    20261030,
    nfl_standings(games, ranks = "DRAFT", verbosity = "NONE")
  )
  expect_identical(nrow(standings), 12L)
  expect_snapshot_file(standings_csv(standings), "partial_week_1.csv")
})

test_that("large draft ties abort the draft tiebreaker loop", {
  # Documents a bug in the R implementation (D10 in
  # dev/cpp-standings-engine.md). The C++ engine has to rank these seasons
  games <- data.table::as.data.table(
    readRDS(test_path("draft_tiebreaker_loop_games.rds"))
  )
  expect_snapshot(
    with_reference_seed(
      1L,
      nfl_standings(games, ranks = "DRAFT", verbosity = "NONE")
    ),
    error = TRUE
  )
})
