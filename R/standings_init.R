standings_init <- function(games_doubled, verbosity){
  if (verbosity == 2L) report("Compute Raw Standings")
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
  standings <- team_records %>%
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
  standings[, div_game := fifelse(division == division_opp, 1, 0)]
  standings[, conf_game := fifelse(conf == conf_opp, 1, 0)]
  standings <- standings[, list(
    div_pct = fifelse(
      sum(div_game) == 0, 0,
      sum(div_game * outcome) / sum(div_game)
    ),
    conf_pct = fifelse(
      sum(conf_game) == 0, 0,
      sum(conf_game * outcome) / sum(conf_game)
    ),
    sov = fifelse(
      sum(outcome == 1) == 0, 0,
      sum(wins_opp * (outcome == 1)) / sum(games_opp * (outcome == 1))
    ),
    sos = sum(wins_opp) / sum(games_opp)
  ), by = c("sim", "conf", "division", "team", "games", "wins",
            "true_wins", "losses", "ties", "win_pct")]

  # In simulations, we need to know the maximum regular season week as this has
  # changed over the time. We compute the max week by sim and join it
  # to the teams data
  max_reg_week <- games_doubled[
    game_type == "REG",
    list(max_reg_week = max(week)),
    by = "sim"
  ]
  standings <- merge(standings, max_reg_week, keyby = "sim")
  standings
}
