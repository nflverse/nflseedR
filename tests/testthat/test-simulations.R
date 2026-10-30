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
  sim_params[["nflseedR_version"]] <- NULL
  sim_params[["finished_at"]] <- NULL
  expect_snapshot_value(sim_params, style = "json2", variant = "sims")
})

test_that("simulations can skip games and game_summary", {
  if (!is_sequential()) future::plan(future::sequential)
  run <- function(return_games) {
    set.seed(5, "L'Ecuyer-CMRG")
    nflseedR::nfl_simulations(
      games = nflseedR::sims_games_example,
      simulations = 4,
      chunks = 2,
      return_games = return_games,
      verbosity = "NONE"
    )
  }
  with_games <- run(TRUE)
  without_games <- run(FALSE)
  expect_named(without_games, names(with_games))
  expect_null(without_games$games)
  expect_null(without_games$game_summary)
  expect_identical(without_games$standings, with_games$standings)
  expect_identical(without_games$overall, with_games$overall)
  expect_identical(without_games$team_wins, with_games$team_wins)
  expect_identical(without_games$sim_params$return_games, FALSE)
})

test_that("simulations check return_games", {
  expect_snapshot(
    nflseedR::nfl_simulations(
      games = nflseedR::sims_games_example,
      return_games = NA
    ),
    error = TRUE
  )
})

test_that("simulations results with many tiebreakers are stable", {
  if (!is_sequential()) future::plan(future::sequential)
  games <- data.table::as.data.table(nflseedR::sims_games_example)
  games <- games[game_type == "REG"][week >= 6L, result := NA_integer_]
  set.seed(7, "L'Ecuyer-CMRG")
  sim <- nflseedR::nfl_simulations(
    games = games,
    simulations = 200,
    chunks = 1,
    sim_include = "DRAFT",
    verbosity = "NONE"
  )
  standings <- data.table::as.data.table(sim$standings)

  ranks <- standings[, list(
    div_rank = sum(div_rank),
    conf_rank = sum(conf_rank),
    draft_rank = sum(draft_rank)
  ), keyby = "team"]
  expect_snapshot_value(as.data.frame(ranks), style = "json2", variant = "sims")

  tiebreakers <- lapply(
    standings[, list(div_tie_broken_by, conf_tie_broken_by, draft_tie_broken_by)],
    \(x) as.list(table(x))
  )
  expect_snapshot_value(tiebreakers, style = "json2", variant = "sims")
  expect_snapshot_value(sim$overall, style = "json2", variant = "sims")
})

test_that("simulations results with pre-SOV tiebreakers are stable", {
  if (!is_sequential()) future::plan(future::sequential)
  games <- data.table::as.data.table(nflseedR::sims_games_example)
  games <- games[game_type == "REG"][week >= 10L, result := NA_integer_]
  set.seed(8, "L'Ecuyer-CMRG")
  sim <- nflseedR::nfl_simulations(
    games = games,
    simulations = 200,
    chunks = 1,
    sim_include = "POST",
    tiebreaker_depth = "PRE-SOV",
    playoff_seeds = 6,
    byes_per_conf = 2,
    verbosity = "NONE"
  )
  standings <- data.table::as.data.table(sim$standings)

  ranks <- standings[, list(
    div_rank = sum(div_rank),
    conf_rank = sum(conf_rank, na.rm = TRUE)
  ), keyby = "team"]
  expect_snapshot_value(as.data.frame(ranks), style = "json2", variant = "sims")
  expect_snapshot_value(sim$overall, style = "json2", variant = "sims")
})

test_that("team_wins matches over and under probabilities of simulated wins", {
  if (!is_sequential()) future::plan(future::sequential)
  games <- data.table::as.data.table(nflseedR::sims_games_example)
  games <- games[game_type == "REG"][week >= 10L, result := NA_integer_]
  set.seed(9, "L'Ecuyer-CMRG")
  sim <- nflseedR::nfl_simulations(
    games = games,
    simulations = 50,
    chunks = 1,
    sim_include = "REG",
    verbosity = "NONE"
  )
  standings <- data.table::as.data.table(sim$standings)

  expected <- data.table::CJ(
    team = unique(standings$team),
    wins = seq(0, max(standings$games), 0.5)
  )
  expected[, `:=`(
    over_prob = mapply(\(t, w) mean(standings[team == t, true_wins] > w), team, wins),
    under_prob = mapply(\(t, w) mean(standings[team == t, true_wins] < w), team, wins)
  )]
  expect_equal(sim$team_wins, expected, ignore_attr = TRUE)
})

test_that("simulations split sims over chunks and keep the input", {
  if (!is_sequential()) future::plan(future::sequential)
  games <- data.table::as.data.table(nflseedR::sims_games_example)
  input <- data.table::copy(games)
  set.seed(10, "L'Ecuyer-CMRG")
  sim <- suppressMessages(nflseedR::nfl_simulations(
    games = games,
    simulations = 6,
    chunks = 3,
    verbosity = "NONE"
  ))

  expect_identical(
    as.list(table(sim$standings$sim)),
    as.list(table(rep(1:6, each = 32)))
  )
  expect_identical(
    as.list(table(sim$games$sim)),
    as.list(table(rep(1:6, each = nrow(games))))
  )
  expect_equal(games, input, ignore_attr = TRUE)
})

test_that("simulations keep given playoff results", {
  if (!is_sequential()) future::plan(future::sequential)
  reg <- data.table::as.data.table(nflseedR::sims_games_example)
  reg <- reg[game_type == "REG"]
  set.seed(11, "L'Ecuyer-CMRG")
  season <- nflseedR::nfl_simulations(
    games = reg,
    simulations = 1,
    chunks = 1,
    sim_include = "POST",
    verbosity = "NONE"
  )
  games <- data.table::as.data.table(season$games)[game_type %in% c("REG", "WC")]
  wc_losers <- games[game_type == "WC", fifelse(result > 0, away_team, home_team)]

  sim <- nflseedR::nfl_simulations(
    games = games,
    simulations = 20,
    chunks = 2,
    verbosity = "NONE"
  )
  standings <- data.table::as.data.table(sim$standings)
  sim_games <- data.table::as.data.table(sim$games)

  expect_all_equal(standings[team %chin% wc_losers, exit], "WC")
  expect_identical(nrow(sim_games[game_type == "WC"]), 20L * 6L)
  expect_false(anyNA(sim_games[game_type != "REG", c(away_team, home_team)]))
  expect_identical(sim_games[game_type == "DIV" & home_rest == 14L, .N], 20L * 2L)
  expect_identical(standings[exit == "SB_WIN", .N], 20L)
})

test_that("simulations work with custom compute_results functions", {
  if (!is_sequential()) future::plan(future::sequential)
  # Home teams always win by 3
  set.seed(12, "L'Ecuyer-CMRG")
  sim <- suppressMessages(nflseedR::nfl_simulations(
    games = nflseedR::sims_games_example,
    compute_results = function(teams, games, week_num, ...) {
      games[week == week_num & is.na(result), result := 3L]
      list(teams = teams, games = games)
    },
    simulations = 4,
    chunks = 2,
    verbosity = "NONE"
  ))
  games <- data.table::as.data.table(sim$games)
  simulated <- is.na(nflseedR::sims_games_example$result)

  expect_all_equal(games$result[rep(simulated, 4)], 3L)
  expect_identical(sum(sim$standings$exit == "SB_WIN"), 4L)
})

test_that("simulations with cpp_compute_results() match the default", {
  if (!is_sequential()) future::plan(future::sequential)
  run <- function(fun) {
    set.seed(5, "L'Ecuyer-CMRG")
    nflseedR::nfl_simulations(
      games = nflseedR::sims_games_example,
      compute_results = fun,
      simulations = 6,
      chunks = 2,
      verbosity = "NONE"
    )
  }
  expect_equal(
    run(cpp_compute_results)[c("standings", "games")],
    run(nflseedR_compute_results)[c("standings", "games")]
  )
})

test_that("simulations check teams and playoff seeds before simulating", {
  games <- data.table::as.data.table(nflseedR::sims_games_example)
  games[home_team == "LV", home_team := "OAK"]
  expect_snapshot(
    nflseedR::nfl_simulations(games, simulations = 2, chunks = 1),
    error = TRUE
  )
  expect_snapshot(
    nflseedR::nfl_simulations(
      nflseedR::sims_games_example,
      playoff_seeds = 17,
      simulations = 2,
      chunks = 1
    ),
    error = TRUE
  )
})
