test_that("season simulator works", {
  g <- load_sharpe_games()
  skip_if_not(nrow(g) > 0, message = NULL)
  sim <- nflseedR::simulate_nfl(
     nfl_season = 2020,
     fresh_season = TRUE,
     simulations = 4,
     sims_per_round = 2
   )

  expect_type(sim, "list")
  expect_named(sim, c("teams", "games", "overall", "team_wins"))
})
