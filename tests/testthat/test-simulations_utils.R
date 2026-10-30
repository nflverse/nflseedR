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

test_that("nflseedR_compute_results() matches elo in randomly ordered teams", {
  input <- new_compute_results_input()
  set.seed(4)
  ordered <- nflseedR_compute_results(
    teams = data.table::copy(input$teams),
    games = data.table::copy(input$games),
    week_num = 10L
  )

  set.seed(3)
  random_teams <- input$teams[sample(.N)]
  set.seed(4)
  random <- nflseedR_compute_results(
    teams = random_teams,
    games = data.table::copy(input$games),
    week_num = 10L
  )

  expect_identical(random$games$result, ordered$games$result)
  expect_identical(
    data.table::setkey(random$teams, sim, team)$elo,
    data.table::setkey(ordered$teams, sim, team)$elo
  )
})

test_that("nflseedR_compute_results() works outside the nflseedR namespace", {
  # Users copy this function as an example, so it must not use internals
  copied <- nflseedR_compute_results
  environment(copied) <- globalenv()
  input <- new_compute_results_input()
  set.seed(4)
  expected <- nflseedR_compute_results(
    teams = data.table::copy(input$teams),
    games = data.table::copy(input$games),
    week_num = "10"
  )
  set.seed(4)
  out <- copied(
    teams = data.table::copy(input$teams),
    games = data.table::copy(input$games),
    week_num = "10"
  )
  expect_identical(out, expected)
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

test_that("sims_encode_strings() and sims_decode_strings() round trip", {
  games <- data.table::as.data.table(nflseedR::sims_games_example)
  games[c(2, 5), home_team := NA_character_]
  input <- data.table::copy(games)

  cols <- sims_encode_strings(games)
  expect_identical(cols, c("game_type", "away_team", "home_team", "location"))
  expect_s3_class(games$home_team, "factor")

  combined <- data.table::rbindlist(list(games, games[rev(seq_len(.N))]))
  sims_decode_strings(combined, cols)
  expect_identical(combined, rbind(input, input[rev(seq_len(.N))]))
})
