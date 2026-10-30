# Build tests/testthat/games_2006_2025.rds: all games of the 2006-2025
# seasons for the standings snapshots. Old team abbreviations are converted
# to the current ones.

g <- nflreadr::load_schedules(2006:2025) |>
  dplyr::mutate(
    dplyr::across(c(away_team, home_team), nflreadr::clean_team_abbrs)
  ) |>
  dplyr::select(
    season,
    week,
    game_type,
    away_team,
    home_team,
    result,
    home_score,
    away_score
  )

# Drop nflverse attributes (timestamp etc.) so the file only changes with the
# data
g <- data.frame(as.list(g))

saveRDS(g, "tests/testthat/games_2006_2025.rds", version = 2)
