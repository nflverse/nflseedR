load_test_games <- function(){
  nflseedR::load_sharpe_games() %>%
    dplyr::filter(season %in% 2014:2019) %>%
    dplyr::select(sim = season, game_type, week, away_team, home_team, result)
}
