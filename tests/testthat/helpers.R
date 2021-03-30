load_test_games <- function(){
  g <- nflseedR::load_sharpe_games()

  if (!nrow(g) > 0) return(tibble::tibble())

  g %>%
    dplyr::filter(season %in% 2014:2019) %>%
    dplyr::select(sim = season, game_type, week, away_team, home_team, result)
}
