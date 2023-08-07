test_that("compute_division_ranks() works for multiple seasons", {
  g <- load_test_games()

  div_ranks <- compute_division_ranks(g)
  div_ranks <- div_ranks$standings %>% strip_nflverse_attributes()

  exp <- load_reference("div")

  expect_identical(div_ranks, exp)
})

test_that("compute_conference_seeds() works for multiple seasons", {
  g <- load_test_games()
  conf_seeds <- g %>%
    compute_division_ranks() %>%
    compute_conference_seeds(playoff_seeds = 6)
  conf_seeds <- conf_seeds$standings %>% strip_nflverse_attributes()

  exp <- load_reference("conf")

  expect_identical(conf_seeds, exp)
})

test_that("compute_draft_order() works for multiple seasons", {
  g <- load_test_games()
  draft_order <- g %>%
    compute_division_ranks() %>%
    compute_conference_seeds(playoff_seeds = 6) %>%
    compute_draft_order(games = g)
  draft_order <- strip_nflverse_attributes(draft_order)

  exp <- load_reference("draft")

  expect_identical(draft_order, exp)
})
