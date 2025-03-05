
# Standings ---------------------------------------------------------------
dictionary_standings <- jsonlite::read_json(
  "data-raw/dictionary_standings.json",
  simplifyVector = TRUE
) |>
  dplyr::mutate_all(stringr::str_squish)
usethis::use_data(dictionary_standings, overwrite = TRUE)

# Games -------------------------------------------------------------------
dictionary_games <- jsonlite::read_json(
  "data-raw/dictionary_games.json",
  simplifyVector = TRUE
) |>
  dplyr::mutate_all(stringr::str_squish)
usethis::use_data(dictionary_games, overwrite = TRUE)


# Overall -----------------------------------------------------------------
dictionary_overall <- jsonlite::read_json(
  "data-raw/dictionary_overall.json",
  simplifyVector = TRUE
) |>
  dplyr::mutate_all(stringr::str_squish)
usethis::use_data(dictionary_overall, overwrite = TRUE)


# team_wins ---------------------------------------------------------------
dictionary_team_wins <- jsonlite::read_json(
  "data-raw/dictionary_team_wins.json",
  simplifyVector = TRUE
) |>
  dplyr::mutate_all(stringr::str_squish)
usethis::use_data(dictionary_team_wins, overwrite = TRUE)


# game_summary ------------------------------------------------------------
dictionary_game_summary <- jsonlite::read_json(
  "data-raw/dictionary_game_summary.json",
  simplifyVector = TRUE
) |>
  dplyr::mutate_all(stringr::str_squish)
usethis::use_data(dictionary_game_summary, overwrite = TRUE)

