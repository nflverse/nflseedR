source("helpers.R")

test_that("compute_division_ranks() works for multiple seasons", {
  g <- load_test_games()
  skip_if_not(nrow(g) > 0, message = NULL)
  # dplyr v1.1.1 introduced a warning about many-to-many relationships
  # that completely explodes in Lee's code
  # for the moment, we skip tests if that version of dplyr is installed
  skip_if_not(packageVersion("dplyr") < "1.1.1")

  ref <- readRDS("reference_div_ranks.rds")

  div_ranks <- g %>%
    compute_division_ranks()

  expect_identical(div_ranks$standings, ref)
})

test_that("compute_conference_seeds() works for multiple seasons", {
  g <- load_test_games()
  skip_if_not(nrow(g) > 0, message = NULL)
  # dplyr v1.1.1 introduced a warning about many-to-many relationships
  # that completely explodes in Lee's code
  # for the moment, we skip tests if that version of dplyr is installed
  skip_if_not(packageVersion("dplyr") < "1.1.1")

  ref <- readRDS("reference_conf_seeds.rds")

  conf_seeds <- g %>%
    compute_division_ranks() %>%
    compute_conference_seeds(h2h = .$h2h, playoff_seeds = 6)

  expect_identical(conf_seeds$standings, ref)
})

test_that("compute_draft_order() works for multiple seasons", {
  g <- load_test_games()
  skip_if_not(nrow(g) > 0, message = NULL)
  # dplyr v1.1.1 introduced a warning about many-to-many relationships
  # that completely explodes in Lee's code
  # for the moment, we skip tests if that version of dplyr is installed
  skip_if_not(packageVersion("dplyr") < "1.1.1")

  ref <- readRDS("reference_draft_order.rds")

  draft_order <- g %>%
    compute_division_ranks() %>%
    compute_conference_seeds(h2h = .$h2h, playoff_seeds = 6) %>%
    compute_draft_order(games = g, h2h = .$h2h)

  expect_identical(draft_order, ref)
})
