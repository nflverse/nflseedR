test_that("regular season computation works", {
  # see also data-raw/verify_regular_season_fct.R for more details
  # load 2016:2023 standings
  standings <- readRDS("standings_2016_2023.rds")

  # calculate matchups
  regular_seasons <- lapply(
    sort(unique(standings$season)),
    function(year, standings) {
      s <- standings[season == year]
      nflseedR::nfl_regular_season(s)
    },
    standings = standings
  ) |>
    data.table::rbindlist()

  # create a games object from matchups by filtering down to "home" locations
  # only and renaming team to home and opp to away
  games <- regular_seasons[
    "home",
    list(season, home = team, away = opp, home_matchup_type = opp_type),
    on = "location"
  ]

  # reorder to ensure it's always as expected
  setorder(games, season, home, away)
  # can't snapshot a data.table
  setDF(games)

  expect_snapshot_value(games, style = "json2", cran = TRUE)
})
