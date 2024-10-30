test_that("verification finds problems", {

  # default function should not error and return TRUE
  expect_true(simulations_verify_fct(nflseedR_compute_results))

  # Missing args
  user_fun <- function(...) {}
  expect_error(simulations_verify_fct(user_fun))

  # No list
  user_fun <- function(teams, games, week_num, ...) {}
  expect_error(simulations_verify_fct(user_fun))

  # incomplete list
  user_fun <- function(teams, games, week_num, ...) {list()}
  expect_error(simulations_verify_fct(user_fun))

  # sims and teams miss rows and columns
  user_fun <- function(teams, games, week_num, ...) {
    list(
      teams = teams[-1,-(1:2)],
      games = games[-1,-(2:3)]
    )
  }
  expect_error(simulations_verify_fct(user_fun))

  # mess up results
  user_fun <- function(teams, games, week_num, ...) {
    setDT(games)
    games[week == 2, result := NA_integer_]
    games[week == 3, result := 6L]
    list(
      teams = teams,
      games = games
    )
  }
  expect_error(simulations_verify_fct(user_fun))

  # mess up SB
  user_fun <- function(teams, games, week_num, ...) {
    setDT(games)
    if (week_num == "SB"){
      games[week == week_num, result := 0L]
    } else {
      games[week == week_num & is.na(result), result := 3L]
    }
    list(
      teams = teams,
      games = games
    )
  }
  expect_error(simulations_verify_fct(user_fun))
})
