load_test_games <- function(){
  g <- nflreadr::load_schedules(2014:2019)

  if (!nrow(g) > 0) return(tibble::tibble())

  g |>
    dplyr::select(sim = season, game_type, week, away_team, home_team, result)
}
