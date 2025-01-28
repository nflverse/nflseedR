standings_init <- function(games_doubled, verbosity){
  if (verbosity == 2L) report("Compute Raw Standings")

  has_score <- TRUE

  if(!"score" %chin% colnames(games_doubled)){
    games_doubled[, score := 3L]
    has_score <- FALSE
  }

  games_doubled <- games_doubled[,`:=`(
    div_game = div_vec[team] == div_vec[opp],
    conf_game = conf_vec[team] == conf_vec[opp]
  )]
  team_records <- games_doubled[
    "REG",
    list(
      games = .N,
      wins = sum(outcome),
      true_wins = sum(outcome == 1),
      losses = sum(outcome == 0),
      ties = sum(outcome == 0.5),
      pf = sum(score),
      pa = sum(score - result),
      win_pct = sum(outcome) / .N,
      div_pct = fifelse(
        sum(div_game) == 0, 0,
        sum(div_game * outcome) / sum(div_game)
      ),
      conf_pct = fifelse(
        sum(conf_game) == 0, 0,
        sum(conf_game * outcome) / sum(conf_game)
      ),
      conf_pd = fifelse(
        sum(conf_game) == 0, NA_integer_,
        sum(conf_game * result)
      )
    ),
    by = c("sim", "team"),
    on = "game_type"
  ]
  team_records[, `:=`(
    division = div_vec[team],
    conf = conf_vec[team]
  )]

  opp_info <- merge(
    games_doubled["REG", list(sim, team, opp, outcome), on = "game_type"],
    team_records[,list(sim, opp = team, wins_opp = wins, games_opp = games)],
    by = c("sim", "opp")
  )[, list(
    sov = fifelse(
      sum(outcome == 1) == 0, 0,
      sum(wins_opp * (outcome == 1)) / sum(games_opp * (outcome == 1))
    ),
    sos = sum(wins_opp) / sum(games_opp)
  ),
  by = c("sim", "team")]

  standings <- merge(
    team_records, opp_info, by = c("sim", "team")
  )

  if (has_score){
    standings[,pd := pf - pa]
    standings <- standings[,list(
      sim, conf, division, team, games, wins, true_wins, losses, ties, pf, pa, pd,
      win_pct, div_pct, conf_pct, sov, sos, conf_pd
    )]
  } else {
    standings <- standings[,list(
      sim, conf, division, team, games, wins, true_wins, losses, ties, win_pct,
      div_pct, conf_pct, sov, sos, conf_pd
    )]
  }

  # In simulations, we need to know the maximum regular season week as this has
  # changed over the time. We compute the max week by sim and join it
  # to the teams data
  # max_reg_week <- games_doubled[
  #   game_type == "REG",
  #   list(max_reg_week = max(week)),
  #   by = "sim"
  # ]
  # standings <- merge(standings, max_reg_week, keyby = "sim")
  standings
}
