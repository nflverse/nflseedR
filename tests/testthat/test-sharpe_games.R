test_that("load_sharpe_games works", {
  g <- load_sharpe_games()
  skip_if_not(nrow(g) > 0, message = NULL)
  expect_true(is_tibble(g))
})
