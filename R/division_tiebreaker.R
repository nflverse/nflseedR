break_div_ties_by_h2h <- function(teams, h2h, n_tied){
  ties <- compute_tied_teams(teams, n_tied)

  h2h_games_played <- merge(
    ties[, list(sim, team, division, div_rank)],
    ties[, list(sim, division, opp = team, div_rank)],
    by = c("sim", "division", "div_rank"),
    allow.cartesian = TRUE
  )[team != opp]

  h2h_win_pct <- merge(
    h2h_games_played, h2h, by = c("sim", "team", "opp")
  )[, list(h2h_win_pct = sum(h2h_wins) / sum(h2h_games)), by = c("sim", "team")]

  teams <- merge(teams, h2h_win_pct, by = c("sim", "team"), all.x = TRUE)
  # If a tied team didn't play any h2h vs. other tied teams, it misses in h2h_win_pct
  # After the merge, that team's h2h_win_pct will remain NA, but should be 0
  # This is something that can happen at early stages in the season
  teams[
    div_rank_counter == n_tied & is.na(h2h_win_pct),
    h2h_win_pct := 0,
    by = c("sim", "division")
  ]
  teams[
    div_rank_counter == n_tied,
    div_rank := min(div_rank) - 1 + frank(list(div_rank, -h2h_win_pct), ties.method = "min"),
    by = c("sim", "division")
  ]
  teams[
    div_rank_counter == n_tied,
    div_rank := min(div_rank) - 1 + frank(list(div_rank, -h2h_win_pct), ties.method = "min"),
    by = c("sim", "division")
  ]
  teams[, div_rank_counter := .N, by = c("sim", "division", "div_rank")]
  teams[!is.na(h2h_win_pct) & div_rank_counter == 1, div_tie_broken_by := paste0("Head-To-Head Win PCT (", n_tied, ")")]
  teams <- teams[,!c("h2h_win_pct")]
  teams
}

break_div_ties_by_div_win_pct <- function(teams, n_tied){
  ties <- compute_tied_teams(teams, n_tied)

  teams[
    div_rank_counter == n_tied,
    div_rank := min(div_rank) - 1 + frank(list(div_rank, -div_pct), ties.method = "min"),
    by = c("sim", "division")
  ]
  teams[
    div_rank_counter == n_tied,
    div_tie_broken_by := paste0("Division Win PCT (", n_tied, ")")
  ]
  teams[, div_rank_counter := .N, by = c("sim", "division", "div_rank")]
  teams[div_rank_counter > 1, div_tie_broken_by := NA_character_]
  teams
}

break_div_ties_by_common_win_pct <- function(teams, h2h, n_tied){
  ties <- compute_tied_teams(teams, n_tied)

  common_win_pct <- merge(
    ties[, list(sim, division, team, div_rank)], h2h, by = c("sim", "team"), all.y = FALSE
  )[
    , common := as.integer(.N == n_tied),
    by = c("sim", "division", "opp", "div_rank")
  ][
    , list(common_win_pct = sum(common * h2h_wins) / sum(common * h2h_games)),
    by = c("sim", "team")
  ]
  common_win_pct[is.nan(common_win_pct), common_win_pct := 0]

  teams <- merge(teams, common_win_pct, by = c("sim", "team"), all.x = TRUE)
  # If a tied team didn't play any common games, it misses in common_win_pct
  # After the merge, that team's common_win_pct will remain NA, but should be 0
  # This is something that can happen at early stages in the season
  teams[
    div_rank_counter == n_tied & is.na(common_win_pct),
    common_win_pct := 0,
    by = c("sim", "division")
  ]
  teams[
    div_rank_counter == n_tied,
    div_rank := min(div_rank) - 1 + frank(list(div_rank, -common_win_pct), ties.method = "min"),
    by = c("sim", "division")
  ]
  teams[, div_rank_counter := .N, by = c("sim", "division", "div_rank")]
  teams[!is.na(common_win_pct) & div_rank_counter == 1, div_tie_broken_by := paste0("Common Games Win PCT (", n_tied, ")")]
  teams <- teams[,!c("common_win_pct")]
  teams
}

break_div_ties_by_conf_win_pct <- function(teams, n_tied){
  ties <- compute_tied_teams(teams, n_tied)

  teams[
    div_rank_counter == n_tied,
    div_rank := min(div_rank) - 1 + frank(list(div_rank, -conf_pct), ties.method = "min"),
    by = c("sim", "division")
  ]
  teams[
    div_rank_counter == n_tied,
    div_tie_broken_by := paste0("Conference Win PCT (", n_tied, ")")
  ]
  teams[, div_rank_counter := .N, by = c("sim", "division", "div_rank")]
  teams[div_rank_counter > 1, div_tie_broken_by := NA_character_]
  teams
}

break_div_ties_by_sov <- function(teams, n_tied){
  ties <- compute_tied_teams(teams, n_tied)

  teams[
    div_rank_counter == n_tied,
    div_rank := min(div_rank) - 1 + frank(list(div_rank, -sov), ties.method = "min"),
    by = c("sim", "division")
  ]
  teams[
    div_rank_counter == n_tied,
    div_tie_broken_by := paste0("SOV (", n_tied, ")")
  ]
  teams[, div_rank_counter := .N, by = c("sim", "division", "div_rank")]
  teams[div_rank_counter > 1, div_tie_broken_by := NA_character_]
  teams
}

break_div_ties_by_sos <- function(teams, n_tied){
  ties <- compute_tied_teams(teams, n_tied)

  teams[
    div_rank_counter == n_tied,
    div_rank := min(div_rank) - 1 + frank(list(div_rank, -sos), ties.method = "min"),
    by = c("sim", "division")
  ]
  teams[
    div_rank_counter == n_tied,
    div_tie_broken_by := paste0("SOS (", n_tied, ")")
  ]
  teams[, div_rank_counter := .N, by = c("sim", "division", "div_rank")]
  teams[div_rank_counter > 1, div_tie_broken_by := NA_character_]
  teams
}

compute_tied_teams <- function(teams, n_tied) teams[div_rank_counter == n_tied]

tie_break_done <- function(teams, n_tied) all(teams$div_rank_counter < n_tied)
