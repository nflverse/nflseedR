sims_teams_example <- nflseedR::divisions |>
  dplyr::filter(!team %in% c("LAR", "STL", "SD", "OAK")) |>
  strip_nflverse_attributes() |>
  tibble::as_tibble()
sims_teams_example <- sims_teams_example[rep(seq_len(nrow(sims_teams_example)), 2), ] |>
  dplyr::mutate(sim = rep(1:2, each = nrow(sims_teams_example))) |>
  select(sim, dplyr::everything())

sims_games_example <- nflreadr::load_schedules(2022) |>
  # dplyr::filter(game_type == "REG") |>
  dplyr::select(
    "season", "game_type", "week", "away_team", "home_team",
    "away_rest", "home_rest", "location", "result", "game_id"
  )
reset_ids <- sims_games_example |>
  dplyr::slice_sample(n = 30) |>
  dplyr::pull(game_id)
sims_games_example <- sims_games_example |>
  dplyr::mutate(
    result = data.table::fifelse(game_id %in% reset_ids, NA_integer_, result),
    result = data.table::fifelse(game_type == "SB", NA_integer_, result)
  ) |>
  dplyr::select(-"game_id") |>
  strip_nflverse_attributes() |>
  tibble::as_tibble()

usethis::use_data(sims_teams_example, sims_games_example, overwrite = TRUE)

rm(sims_teams_example, sims_games_example, reset_ids)
