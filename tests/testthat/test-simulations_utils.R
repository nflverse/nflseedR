new_compute_results_input <- function() {
  games <- data.table::as.data.table(nflseedR::sims_games_example)
  games <- data.table::rbindlist(list(
    data.table::copy(games)[, sim := 1L],
    data.table::copy(games)[, sim := 2L]
  ))
  games[week == 10L, result := NA_integer_]
  teams <- data.table::as.data.table(nflseedR::sims_teams_example)
  teams[, elo := seq(1300, 1700, length.out = .N)]
  list(games = games, teams = teams)
}

test_that("nflseedR_compute_results() matches elo by sim and team", {
  input <- new_compute_results_input()
  set.seed(4)
  ordered <- nflseedR_compute_results(
    teams = data.table::copy(input$teams),
    games = data.table::copy(input$games),
    week_num = 10L
  )

  shuffled_teams <- input$teams[rev(seq_len(.N))]
  set.seed(4)
  shuffled <- nflseedR_compute_results(
    teams = shuffled_teams,
    games = data.table::copy(input$games),
    week_num = 10L
  )

  expect_identical(shuffled$games$result, ordered$games$result)
  expect_identical(
    data.table::setkey(shuffled$teams, sim, team)$elo,
    data.table::setkey(ordered$teams, sim, team)$elo
  )
})

test_that("nflseedR_compute_results() transports elo shifts correctly", {
  input <- new_compute_results_input()
  old_teams <- data.table::copy(input$teams)
  set.seed(5)
  out <- nflseedR_compute_results(
    teams = input$teams,
    games = input$games,
    week_num = 10L
  )
  week_games <- out$games[week == 10L]
  played <- old_teams[,
    team %chin% c(week_games$away_team, week_games$home_team)
  ]

  expect_false(anyNA(week_games$result))
  # teams on bye keep their rating, the others gain or lose elo
  expect_identical(out$teams$elo[!played], old_teams$elo[!played])
  expect_identical(sum(out$teams$elo[played] == old_teams$elo[played]), 0L)
  # elo is zero sum within each simulation
  expect_equal(
    out$teams[, list(elo = sum(elo)), keyby = "sim"]$elo,
    old_teams[, list(elo = sum(elo)), keyby = "sim"]$elo
  )
})
