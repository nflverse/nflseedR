test_that("standings works", {
  g <- try(nflreadr::load_schedules(2008:2023), silent = TRUE)
  skip_if(inherits(g, "try-error"))

  standings <- nflseedR::nfl_standings(g, ranks = "DRAFT", verbosity = "NONE") |>
    data.table::setDF()
  expect_snapshot_value(standings, style = "json2", variant = "standings")

  # trigger warning related to presence of sim and season
  g$sim <- g$season * 10
  expect_warning(
    standings <- nflseedR::nfl_standings(g, ranks = "NONE", verbosity = "NONE"),
    regexp = 'The `games` argument includes both "sim" and "season".'
  )
})
