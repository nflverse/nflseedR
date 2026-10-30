# nfl_standings2() is the data.table implementation from before the C++
# engine. Its snapshots document the old behavior for comparisons.

test_that("nfl_standings2() of 2006-2025 is stable", {
  standings <- with_reference_seed(
    20261030,
    nfl_standings2(load_games_2006_2025(), ranks = "DRAFT", verbosity = "NONE")
  )
  expect_snapshot_file(standings_csv(standings), "games_2006_2025.csv")
})

test_that("nfl_standings2() deep tiebreakers are stable", {
  for (depth in c("POINTS", "SOS", "PRE-SOV")) {
    standings <- with_reference_seed(
      20261030,
      nfl_standings2(
        load_deep_tiebreaker_games(),
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

test_that("large draft ties abort nfl_standings2()", {
  # Fixed in nfl_standings() (D10 in dev/cpp-standings-engine.md)
  games <- data.table::as.data.table(
    readRDS(test_path("draft_tiebreaker_loop_games.rds"))
  )
  expect_snapshot(
    with_reference_seed(
      1L,
      nfl_standings2(games, ranks = "DRAFT", verbosity = "NONE")
    ),
    error = TRUE
  )
})
