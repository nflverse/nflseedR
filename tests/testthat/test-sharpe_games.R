test_that("load_sharpe_games works", {
  skip_on_cran()

  g <- load_sharpe_games()
  expect_true(is_tibble(g))
})
