sims_teams_example <- nflseedR::divisions |>
  dplyr::filter(!team %in% c("LAR", "STL", "SD", "OAK"))
sims_teams_example <- sims_teams_example[rep(seq_len(nrow(sims_teams_example)), 2), ] |>
  dplyr::mutate(sim = rep(1:2, each = nrow(sims_teams_example))) |>
  select(sim, dplyr::everything())

sims_games_example <- nflreadr::load_schedules(2022) |>
  dplyr::filter(game_type == "REG")
reset_ids <- sims_games_example |>
  dplyr::slice_sample(n = 30) |>
  dplyr::pull(game_id)
sims_games_example <- sims_games_example |>
  dplyr::mutate(
    result = data.table::fifelse(game_id %in% reset_ids, NA_integer_, result)
  )
sims_games_example <- nflseedR:::sims_validate_games(sims_games_example) |>
  tibble::as_tibble()

usethis::use_data(sims_teams_example, sims_games_example, overwrite = TRUE)

rm(sims_teams_example, sims_games_example, reset_ids)
