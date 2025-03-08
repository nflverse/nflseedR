library(dplyr, warn.conflicts = FALSE)

games <-
  nflseedR::load_sharpe_games() |>
  dplyr::filter(season %in% 2014:2019) |>
  dplyr::select(sim = season, game_type, week, away_team, home_team, result)

div_ranks <- games |>
  nflseedR::compute_division_ranks() |>
  purrr::pluck("standings")

conf_seeds <- games |>
  nflseedR::compute_division_ranks() |>
  nflseedR::compute_conference_seeds(h2h = .$h2h, playoff_seeds = 6) |>
  purrr::pluck("standings")

draft_order <- games |>
  nflseedR::compute_division_ranks() |>
  nflseedR::compute_conference_seeds(h2h = .$h2h, playoff_seeds = 6) |>
  nflseedR::compute_draft_order(games = games, h2h = .$h2h)

saveRDS(div_ranks,   "tests/testthat/reference_div_ranks.rds")
saveRDS(conf_seeds,  "tests/testthat/reference_conf_seeds.rds")
saveRDS(draft_order, "tests/testthat/reference_draft_order.rds")
