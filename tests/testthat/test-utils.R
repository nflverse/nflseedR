test_that("is_single_digit_numeric works", {
  expect_true(is_single_digit_numeric(1234))
  expect_false(is_single_digit_numeric(c(1, 2, 3, 4)))
})

test_that("is_sequential works", {
  skip_on_cran()

  future::plan("sequential")
  expect_true(is_sequential())
  future::plan("multisession")
  expect_false(is_sequential())
})

test_that("report works", {
  expect_message(report("this is a message"))
})

test_that("double_games works", {
  g <- data.frame(
    sim = 2020,
    game_type = "REG",
    week = 16L,
    away_team = "ABC",
    home_team = "XYZ",
    result = 33
  )

  d <- data.table(
    sim = c(2020, 2020),
    game_type = c("REG", "REG"),
    week = c(16L, 16L),
    team = c("ABC", "XYZ"),
    opp = c("XYZ", "ABC"),
    result = c(-33, 33),
    outcome = c(0, 1)
  ) %>% tibble::as_tibble()
  expect_identical(double_games(g), d)
})

test_that("h2h works", {
  g <- data.frame(
    sim = 2020,
    game_type = "REG",
    week = 16L,
    away_team = "ABC",
    home_team = "XYZ",
    result = 33
  )

  h2h <- compute_h2h(double_games(g))

  exp <- data.table(
    sim = c(2020, 2020),
    team = c("ABC", "XYZ"),
    opp = c("XYZ", "ABC"),
    h2h_games = 1L,
    h2h_wins = c(0, 1),
    h2h_played = 1,
    key = c("sim", "team", "opp")
  )
  expect_identical(h2h, exp)
})
