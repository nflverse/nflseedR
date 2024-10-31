test_that("simulations works", {
  if (!is_sequential()) future::plan(future::sequential)
  set.seed(5, "L'Ecuyer-CMRG")
  sim <- nflseedR::nfl_simulations(
    games = nflseedR::sims_games_example,
    simulations = 4,
    chunks = 1,
    sim_include = "DRAFT",
    verbosity = "NONE"
  )
  expect_named(sim, c("standings", "games", "overall", "team_wins", "game_summary", "sim_params"))

  standings <- sim[["standings"]]
  expect_snapshot_value(standings, style = "json2", variant = "sims")

  games <-  sim[["games"]]
  expect_snapshot_value(games, style = "json2", variant = "sims")

  overall <-  sim[["overall"]]
  expect_snapshot_value(overall, style = "json2", variant = "sims")

  team_wins <-  sim[["team_wins"]]
  expect_snapshot_value(team_wins, style = "json2", variant = "sims")

  game_summary <-  sim[["game_summary"]]
  expect_snapshot_value(game_summary, style = "json2", variant = "sims")

  sim_params <-  sim[["sim_params"]]
  expect_snapshot_value(sim_params, style = "json2", variant = "sims")
})
