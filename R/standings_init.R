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
  # Precompute all summands as columns so the grouped sums below are simple
  # column sums. That allows data.table to use its optimized grouping (GForce)
  # which is much faster than evaluating expressions once per group.
  reg_games <- games_doubled[game_type == "REG"]
  reg_games[, `:=`(
    true_win = outcome == 1,
    loss = outcome == 0,
    tie = outcome == 0.5,
    score_allowed = score - result,
    div_outcome = div_game * outcome,
    conf_outcome = conf_game * outcome,
    conf_result = conf_game * result
  )]
  team_records <- reg_games[
    ,
    list(
      games = .N,
      wins = sum(outcome),
      true_wins = sum(true_win),
      losses = sum(loss),
      ties = sum(tie),
      pf = sum(score),
      pa = sum(score_allowed),
      div_games = sum(div_game),
      div_wins = sum(div_outcome),
      conf_games = sum(conf_game),
      conf_wins = sum(conf_outcome),
      conf_pd = sum(conf_result)
    ),
    by = c("sim", "team")
  ]
  team_records[, `:=`(
    win_pct = wins / games,
    div_pct = fifelse(div_games == 0, 0, div_wins / div_games),
    conf_pct = fifelse(conf_games == 0, 0, conf_wins / conf_games),
    conf_pd = fifelse(conf_games == 0, NA_integer_, conf_pd)
  )]
  team_records[, `:=`(
    division = div_vec[team],
    conf = conf_vec[team]
  )]

  opp_info <- merge(
    reg_games[, list(sim, team, opp, outcome)],
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
