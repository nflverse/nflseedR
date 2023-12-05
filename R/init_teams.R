init_teams <- function(games_doubled){
  setDT(games_doubled)
  team_records <-
    merge(
      games_doubled["REG", on = "game_type"],
      data.table(nflseedR::divisions)[,sdiv:=NULL],
      by = "team",
      sort = FALSE
    )[,
      list(
        games = .N,
        wins = sum(outcome),
        true_wins = sum(outcome == 1),
        losses = sum(outcome == 0),
        ties = sum(outcome == 0.5),
        win_pct = sum(outcome) / .N
      ), by = c("sim", "conf", "division", "team")]

  # add in tiebreaker info
  teams <- team_records %>%
    merge(
      games_doubled["REG", on = "game_type"],
      by = c("sim", "team"),
      sort = FALSE
    ) %>%
    merge(
      team_records,
      by.x = c("sim", "opp"),
      by.y = c("sim", "team"),
      suffixes = c("", "_opp"),
      sort = FALSE
    )
  teams[, div_game := fifelse(division == division_opp, 1, 0)]
  teams[, conf_game := fifelse(conf == conf_opp, 1, 0)]
  teams <- teams[, list(
    div_pct = fifelse(
      sum(div_game) == 0, 0.5,
      sum(div_game * outcome) / sum(div_game)
    ),
    conf_pct = fifelse(
      sum(conf_game) == 0, 0.5,
      sum(conf_game * outcome) / sum(conf_game)
    ),
    sov = fifelse(
      sum(outcome == 1) == 0, 0,
      sum(wins_opp * (outcome == 1)) / sum(games_opp * (outcome == 1))
    ),
    sos = sum(wins_opp) / sum(games_opp)
  ), by = c("sim", "conf", "division", "team", "games", "wins",
            "true_wins", "losses", "ties", "win_pct")][order(sim, conf, division, team)]
  teams
}
