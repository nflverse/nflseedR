# https://www.nfl.com/standings/tie-breaking-procedures
add_div_ranks <- function(standings,
                          h2h,
                          tiebreaker_depth,
                          verbosity){
  # Set ranks by win percentage in descending order by sim and division.
  # If ties method is "random", data.table will break all ties randomly
  # and we won't need any further tie-breaking methods
  dt_ties_method <- if (tiebreaker_depth == "RANDOM") "random" else "min"
  standings[
    , div_rank := frankv(-win_pct, ties.method = dt_ties_method),
    by = c("sim", "division")
  ]

  # If tiebreaker_depth == "RANDOM", all ties are broken at this stage. We add
  # tiebreaker information to the tied teams.
  if (tiebreaker_depth == "RANDOM") {
    standings[, div_rank_counter := .N, by = c("sim", "division", "win_pct")]
    standings[
      div_rank_counter > 1,
      div_tie_broken_by := "Coin Toss",
    ]
  }

  # Count division ranks by sim and division. If each rank only exists once,
  # then there are no ties that need to be broken
  standings[, div_rank_counter := .N, by = c("sim", "division", "div_rank")]

  # enter tie breaking procedure only if there are actual ties,
  # i.e. a division rank exists more than once per sim and division
  if ( any(standings$div_rank_counter > 1) ) {
    # 3 or 4-Team ties need to go through all these steps until at least 2 tied
    # teams remain. If that's the case, we have to jump back to the beginning
    # of the process with the 2 remaining teams. That's why we have to loop over
    # this process and check the number of tied teams after each step.
    # A 3 iterations for loop is fine. No need to go crazy about it.
    for (tied_teams in 4:2) {

      if (div_tie_break_done(standings, tied_teams)) next

      # Head To Head ------------------------------------------------------------
      if (verbosity == 2L) report("DIV ({tied_teams}): Head-to-Head Win PCT")
      standings <- break_div_ties_by_h2h(standings = standings, h2h = h2h, n_tied = tied_teams)
      if (div_tie_break_done(standings, tied_teams)) next

      # Division Record ---------------------------------------------------------
      if (verbosity == 2L) report("DIV ({tied_teams}): Division Win PCT")
      standings <- break_div_ties_by_div_win_pct(standings = standings, n_tied = tied_teams)
      if (div_tie_break_done(standings, tied_teams)) next

      # Common Games Win Pct ----------------------------------------------------
      if (verbosity == 2L) report("DIV ({tied_teams}): Common Games Win PCT")
      standings <- break_div_ties_by_common_win_pct(standings = standings, h2h = h2h, n_tied = tied_teams)
      if (div_tie_break_done(standings, tied_teams)) next

      # Conference Win PCT ------------------------------------------------------
      if (verbosity == 2L) report("DIV ({tied_teams}): Conference Win PCT")
      standings <- break_div_ties_by_conf_win_pct(standings = standings, n_tied = tied_teams)
      if (div_tie_break_done(standings, tied_teams)) next

      # SOV ---------------------------------------------------------------------
      if (verbosity == 2L) report("DIV ({tied_teams}): Strength of Victory")
      standings <- break_div_ties_by_sov(standings = standings, n_tied = tied_teams)
      if (div_tie_break_done(standings, tied_teams)) next

      # SOS ---------------------------------------------------------------------
      if (verbosity == 2L) report("DIV ({tied_teams}): Strength of Schedule")
      standings <- break_div_ties_by_sos(standings = standings, n_tied = tied_teams)
      if (div_tie_break_done(standings, tied_teams)) next
    }

    # We've worked through all implemented tie-breakers.
    # If there are still ties, we break them randomly
    if ( any(standings$div_rank_counter > 1) ) {
      if (verbosity == 2L) report("DIV    : Coin Toss")
      standings[
        div_rank_counter > 1,
        div_rank := min(div_rank) - 1 + frank(list(div_rank, -win_pct), ties.method = "random"),
        by = c("sim", "division")
      ]
      standings[
        div_rank_counter > 1,
        div_tie_broken_by := "Coin Toss",
      ]
    }
  }

  # Finally, the div_rank_counter can be removed
  standings <- standings[,!c("div_rank_counter")]
  standings
}

break_div_ties_by_h2h <- function(standings, h2h, n_tied){
  ties <- div_compute_tied_teams(standings, n_tied)

  h2h_games_played <- merge(
    ties[, list(sim, team, division, div_rank)],
    ties[, list(sim, division, opp = team, div_rank)],
    by = c("sim", "division", "div_rank"),
    allow.cartesian = TRUE
  )[team != opp]

  h2h_win_pct <- merge(
    h2h_games_played, h2h, by = c("sim", "team", "opp")
  )[, list(h2h_win_pct = sum(h2h_wins) / sum(h2h_games)), by = c("sim", "team")]

  standings <- merge(standings, h2h_win_pct, by = c("sim", "team"), all.x = TRUE)
  # If a tied team didn't play any h2h vs. other tied teams, it misses in h2h_win_pct
  # After the merge, that team's h2h_win_pct will remain NA, but should be 0
  # This is something that can happen at early stages in the season
  standings[
    div_rank_counter == n_tied & is.na(h2h_win_pct),
    h2h_win_pct := 0,
    by = c("sim", "division")
  ]
  standings[
    div_rank_counter == n_tied,
    div_rank := min(div_rank) - 1 + frank(list(div_rank, -h2h_win_pct), ties.method = "min"),
    by = c("sim", "division")
  ]
  standings[, div_rank_counter := .N, by = c("sim", "division", "div_rank")]
  standings[!is.na(h2h_win_pct) & div_rank_counter == 1, div_tie_broken_by := paste0("Head-To-Head Win PCT (", n_tied, ")")]
  standings <- standings[,!c("h2h_win_pct")]
  standings
}

break_div_ties_by_div_win_pct <- function(standings, n_tied){
  ties <- div_compute_tied_teams(standings, n_tied)

  standings[
    div_rank_counter == n_tied,
    div_rank := min(div_rank) - 1 + frank(list(div_rank, -div_pct), ties.method = "min"),
    by = c("sim", "division")
  ]
  standings[
    div_rank_counter == n_tied,
    div_tie_broken_by := paste0("Division Win PCT (", n_tied, ")")
  ]
  standings[, div_rank_counter := .N, by = c("sim", "division", "div_rank")]
  standings[div_rank_counter > 1, div_tie_broken_by := NA_character_]
  standings
}

break_div_ties_by_common_win_pct <- function(standings, h2h, n_tied){
  ties <- div_compute_tied_teams(standings, n_tied)

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

  standings <- merge(standings, common_win_pct, by = c("sim", "team"), all.x = TRUE)
  # If a tied team didn't play any common games, it misses in common_win_pct
  # After the merge, that team's common_win_pct will remain NA, but should be 0
  # This is something that can happen at early stages in the season
  standings[
    div_rank_counter == n_tied & is.na(common_win_pct),
    common_win_pct := 0,
    by = c("sim", "division")
  ]
  standings[
    div_rank_counter == n_tied,
    div_rank := min(div_rank) - 1 + frank(list(div_rank, -common_win_pct), ties.method = "min"),
    by = c("sim", "division")
  ]
  standings[, div_rank_counter := .N, by = c("sim", "division", "div_rank")]
  standings[!is.na(common_win_pct) & div_rank_counter == 1, div_tie_broken_by := paste0("Common Games Win PCT (", n_tied, ")")]
  standings <- standings[,!c("common_win_pct")]
  standings
}

break_div_ties_by_conf_win_pct <- function(standings, n_tied){
  ties <- div_compute_tied_teams(standings, n_tied)

  standings[
    div_rank_counter == n_tied,
    div_rank := min(div_rank) - 1 + frank(list(div_rank, -conf_pct), ties.method = "min"),
    by = c("sim", "division")
  ]
  standings[
    div_rank_counter == n_tied,
    div_tie_broken_by := paste0("Conference Win PCT (", n_tied, ")")
  ]
  standings[, div_rank_counter := .N, by = c("sim", "division", "div_rank")]
  standings[div_rank_counter > 1, div_tie_broken_by := NA_character_]
  standings
}

break_div_ties_by_sov <- function(standings, n_tied){
  ties <- div_compute_tied_teams(standings, n_tied)

  standings[
    div_rank_counter == n_tied,
    div_rank := min(div_rank) - 1 + frank(list(div_rank, -sov), ties.method = "min"),
    by = c("sim", "division")
  ]
  standings[
    div_rank_counter == n_tied,
    div_tie_broken_by := paste0("SOV (", n_tied, ")")
  ]
  standings[, div_rank_counter := .N, by = c("sim", "division", "div_rank")]
  standings[div_rank_counter > 1, div_tie_broken_by := NA_character_]
  standings
}

break_div_ties_by_sos <- function(standings, n_tied){
  ties <- div_compute_tied_teams(standings, n_tied)

  standings[
    div_rank_counter == n_tied,
    div_rank := min(div_rank) - 1 + frank(list(div_rank, -sos), ties.method = "min"),
    by = c("sim", "division")
  ]
  standings[
    div_rank_counter == n_tied,
    div_tie_broken_by := paste0("SOS (", n_tied, ")")
  ]
  standings[, div_rank_counter := .N, by = c("sim", "division", "div_rank")]
  standings[div_rank_counter > 1, div_tie_broken_by := NA_character_]
  standings
}

div_compute_tied_teams <- function(standings, n_tied) standings[div_rank_counter == n_tied]

div_tie_break_done <- function(standings, n_tied) all(standings$div_rank_counter < n_tied)
