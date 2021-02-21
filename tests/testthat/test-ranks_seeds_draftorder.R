source("helpers.R")

test_that("compute_division_ranks() works for multiple seasons", {
  skip_on_cran()

  ref <- readRDS("reference_div_ranks.rds")
  div_ranks <- load_test_games() %>%
    nflseedR::compute_division_ranks()

  expect_identical(ref, div_ranks$standings)
})

test_that("compute_conference_seeds() works for multiple seasons", {
  skip_on_cran()

  ref <- readRDS("reference_conf_seeds.rds")
  conf_seeds <- load_test_games() %>%
    nflseedR::compute_division_ranks() %>%
    nflseedR::compute_conference_seeds(h2h = .$h2h, playoff_seeds = 6)

  expect_identical(ref, conf_seeds$standings)
})

test_that("compute_draft_order() works for multiple seasons", {
  skip_on_cran()

  ref <- readRDS("reference_draft_order.rds")
  games <- load_test_games()
  draft_order <- games %>%
    nflseedR::compute_division_ranks() %>%
    nflseedR::compute_conference_seeds(h2h = .$h2h, playoff_seeds = 6) %>%
    nflseedR::compute_draft_order(games = games, h2h = .$h2h)

  expect_identical(ref, draft_order)
})
