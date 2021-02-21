test_that("is_single_digit_numeric works", {
  skip_on_cran()

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

test_that("sim_info and report works", {
  skip_on_cran()

  expect_message(sim_info("this is a message"))
  expect_message(report("this is a message"))
})

test_that("double_games works", {
  skip_on_cran()

  g <- data.frame(
    sim = 2020,
    game_type = "REG",
    week = 16L,
    away_team = "ABC",
    home_team = "XYZ",
    result = 33
  )

  d <- data.frame(
    sim = c(2020, 2020),
    game_type = c("REG", "REG"),
    week = c(16L, 16L),
    team = c("ABC", "XYZ"),
    opp = c("XYZ", "ABC"),
    result = c(-33, 33),
    outcome = c(0, 1)
  )
  expect_identical(double_games(g), d)
})
