test_that("season simulator works", {
  g <- load_sharpe_games()
  skip_if_not(nrow(g) > 0, message = NULL)
  # dplyr v1.1.1 introduced a warning about many-to-many relationships
  # that completely explodes in Lee's code
  # for the moment, we skip tests if that version of dplyr is installed
  skip_if_not(packageVersion("dplyr") < "1.1.1")

  sim <- nflseedR::simulate_nfl(
     nfl_season = 2020,
     fresh_season = TRUE,
     simulations = 4,
     sims_per_round = 2
   )

  expect_type(sim, "list")
  expect_named(sim, c("teams", "games", "overall", "team_wins", "game_summary", "sim_params"))
})
