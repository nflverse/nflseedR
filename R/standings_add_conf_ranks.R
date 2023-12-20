# https://www.nfl.com/standings/tie-breaking-procedures
add_conf_ranks <- function(standings,
                           h2h,
                           tiebreaker_depth,
                           playoff_seeds,
                           verbosity){
  # Set ranks by win percentage in descending order by sim and conference.
  # If ties method is "random", data.table will break all ties randomly
  # and we won't need any further tie-breaking methods
  dt_ties_method <- if (tiebreaker_depth == "RANDOM") "random" else "min"
  standings[
    div_rank == 1,
    conf_rank := frankv(-win_pct, ties.method = dt_ties_method),
    by = c("sim", "conf")
  ]
  standings[
    div_rank != 1,
    conf_rank := 4 + frankv(-win_pct, ties.method = dt_ties_method),
    by = c("sim", "conf")
  ]

  # If tiebreaker_depth == "RANDOM", all ties are broken at this stage. We add
  # tiebreaker information to the tied teams.
  if (tiebreaker_depth == "RANDOM") {
    standings[, conf_rank_counter := .N, by = c("sim", "conf", "win_pct")]
    standings[
      conf_rank_counter > 1,
      conf_tie_broken_by := "Coin Toss",
    ]
    standings[, tied_for := NA_character_]
  }

  # If the user supplied a number of playoff seeds, we will set the lower
  # conference ranks to a random value and remove it when the tiebreakers are
  # done
  if (!is.null(playoff_seeds)){
    standings[
      conf_rank > playoff_seeds,
      conf_rank := 50L + frankv(-win_pct, ties.method = "random"),
      by = c("sim", "conf")
    ]
  }

  # Count conference ranks by sim and conference. If each rank only exists once,
  # then there are no ties that need to be broken
  standings <- conf_count_ranks(standings)

  # Do this only if any ties exist
  if ( any(standings$draft_rank_counter > 1) ){
    if(verbosity == 2L) report("Break CONF ties")

    # If all tied clubs are from the same division, we can apply
    # division tiebreakers, i.e. the div_rank
    # We do this here before any tiebreaking starts
    standings <- break_conf_ties_by_division(standings, verbosity = verbosity)

    # enter tie breaking procedure only if there are actual ties,
    # i.e. a conference rank exists more than once per sim and conference
    # and tied teams don't share the same division
    # conference tie breakers allow only one team to advance in any tie-breaking
    # step. So if there are ties with more than 2 teams, we have to do the 4:3:2
    # loop multiple times. We could calculate the number of loops as the maximum
    # of conf_rank_counter numbers. But it is easier to loop over this thing in a
    # while loop.

    # We add a loop counter to avoid infinite loops
    while_counter <- 0L

    while ( any(standings$conf_rank_counter > 1) ) {

      while_counter <- while_counter + 1L

      if (while_counter > 12L){
        cli::cli_abort("Entered infinite loop in conference tiebreaking procedure")
      }

      # Add a helper variable to summarize information on tied teams
      # We use this as grouping vartiable in subsequent functions
      standings[
        conf_rank_counter > 1,
        tied_for := paste0(sim, " ", conf, " #", conf_rank, " (n = ", conf_rank_counter, ")")
      ]

      # NOTE: The system of the below code is as follows
      # All teams that are eliminated in any tiebreaking step, either through
      # division reduction or through actual tiebreakers, will get their
      # `conf_rank` increased by 1 (they lost, so they won't get that rank)
      # `conf_rank_counter` set to NA_integer_
      # After the 4:3:2 tiebreaking loop, we count conf_ranks again. If all ties
      # are broken, there won't be any counter > 1 and we are done.

      # If multiple teams from one division are part of a tiebreaker, we have to
      # make multiple rounds and start with the highest div rank.
      # Teams losing at this stage get their counter set to NA and rank incremented
      standings <- conf_apply_division_reduction(standings, verbosity = verbosity)

      # Since we allow only one team per tie, there can never be more than 4
      # tied teams during a tiebreaking process. That's why we have to loop over
      # the number of tied teams and check the number of tied teams after each step.
      # Every tiebreaking function updates the conf_rank_counter and the conf_rank
      # of eliminated or winning teams.
      # As soon as at least one team is eliminated, we have to restart with the lower
      # number of tied teams.
      for (tied_teams in 4:2) {

        if (conf_tie_break_done(standings, tied_teams)) next

        # Head To Head ------------------------------------------------------------
        if (verbosity == 2L) report("CONF ({tied_teams}): Head-to-Head Sweep")
        standings <- break_conf_ties_by_h2h(standings = standings, h2h = h2h, n_tied = tied_teams)
        if (conf_tie_break_done(standings, tied_teams)) next

        # Conference Win PCT ------------------------------------------------------
        if (verbosity == 2L) report("CONF ({tied_teams}): Conference Win PCT")
        standings <- break_conf_ties_by_conf_win_pct(standings = standings, n_tied = tied_teams)
        if (conf_tie_break_done(standings, tied_teams)) next

        # Common Games Win Pct ----------------------------------------------------
        if (verbosity == 2L) report("CONF ({tied_teams}): Common Games Win PCT")
        standings <- break_conf_ties_by_common_win_pct(standings = standings, h2h = h2h, n_tied = tied_teams)
        if (conf_tie_break_done(standings, tied_teams)) next

        if (tiebreaker_depth == "SOS"){

          # SOV ---------------------------------------------------------------------
          if (verbosity == 2L) report("CONF ({tied_teams}): Strength of Victory")
          standings <- break_conf_ties_by_sov(standings = standings, n_tied = tied_teams)
          if (conf_tie_break_done(standings, tied_teams)) next

          # SOS ---------------------------------------------------------------------
          if (verbosity == 2L) report("CONF ({tied_teams}): Strength of Schedule")
          standings <- break_conf_ties_by_sos(standings = standings, n_tied = tied_teams)
          if (conf_tie_break_done(standings, tied_teams)) next

        }

        # Coin Flip ---------------------------------------------------------------
        if (verbosity == 2L) report("CONF ({tied_teams}): Coin Toss")
        standings <- break_conf_ties_by_coinflip(standings = standings, n_tied = tied_teams)

      } # end of tied teams loop

      # The round of ties is broken and we have set the counter of the eliminated
      # teams to NA during the process.
      # We've also increased the possible conf rank of the eliminated teams by 1,
      # so now we need to recount all ranks and break ties again, if necessary.
      standings <- conf_count_ranks(standings)

      # At this spot, we might have remaining ties within one division where we can
      # apply the division tiebreaker. We do this here to avoid another round of
      # the loop
      standings <- break_conf_ties_by_division(standings, verbosity = verbosity)
      standings[, tied_for := NULL]
    }# end of conf_rank_counter loop
  }# end of tie breaking

  # If the user supplied a number of playoff seeds, we have set the lower
  # conference ranks to a random value and now we remove it
  if (!is.null(playoff_seeds)){
    standings[
      conf_rank > playoff_seeds,
      conf_rank := NA_integer_
    ]
  }

  # Finally, the helper variables can be removed
  standings <- standings[, conf_rank_counter := NULL]
  standings
}

break_conf_ties_by_division <- function(standings, verbosity){
  # The variable conf_rank_shared_by_one_div will be TRUE if all teams that are
  # tied for one rank share the same division.
  standings[
    conf_rank_counter > 1,
    conf_rank_shared_by_one_div := uniqueN(division) == 1,
    by = c("sim", "conf", "conf_rank")
  ]

  if (any(standings$conf_rank_shared_by_one_div, na.rm = TRUE) & verbosity == 2L){
    report("CONF    : Division Rank")
  }
  # In this case, we can break the tie by ranking them through div_rank
  standings[
    conf_rank_counter > 1 & conf_rank_shared_by_one_div == TRUE,
    `:=`(
      conf_rank = conf_rank - 1 + frankv(div_rank, ties.method = "min"),
      conf_tie_broken_by = "Division Tiebreaker"
    ),
    by = c("sim", "conf", "conf_rank")
  ]
  # Remove the helper and update the counter because the tie is broken
  standings[, conf_rank_shared_by_one_div := NULL]
  standings <- conf_count_ranks(standings)
  standings
}

break_conf_ties_by_h2h <- function(standings, h2h, n_tied){
  # 1. Compute a head 2 head table of the tied teams
  ties <- standings[conf_rank_counter == n_tied]

  h2h_games_played <- merge(
    ties[, list(sim, team, conf, conf_rank)],
    ties[, list(sim, conf, opp = team, conf_rank)],
    by = c("sim", "conf", "conf_rank"),
    allow.cartesian = TRUE
  )[team != opp]

  # The variable h2h_sweep will be
  #  0.5 if a team didn't play all other tied teams or
  #      if a team did play all other teams but didn't sweep or got swept
  #  1 if a teams swept all other teams
  #  0 if a team got swept by all other teams
  h2h_table <- merge(
    h2h_games_played, h2h, by = c("sim", "team", "opp"), all.x = TRUE
  )[,
    list(
      h2h_sweep = sum(h2h_wins) / sum(h2h_games)
    ),
    by = c("sim", "team")
  ][
    inrange(h2h_sweep, 0, 1, incbounds = FALSE),  h2h_sweep := NA_real_
  ][
    is.na(h2h_sweep), h2h_sweep := 0.5
  ]

  # 2. Join the head 2 head table to the standings and
  # add the helper variables tie_winner and tie_loser
  standings <- merge(standings, h2h_table, by = c("sim", "team"), all.x = TRUE)
  standings[
    conf_rank_counter == n_tied,
    `:=`(
      tie_winner = frankv(-h2h_sweep, ties.method = "max") == 1,
      tie_loser = frankv(-h2h_sweep, ties.method = "dense") != 1
    ),
    by = "tied_for"
  ]
  standings[
    tie_loser == TRUE,
    `:=`(
      conf_rank_counter = NA_integer_,
      conf_rank = conf_rank + 1
    )
  ]
  standings[
    tie_winner == TRUE,
    `:=`(
      conf_rank_counter = 1L,
      conf_tie_broken_by = paste0("Head-To-Head Sweep (", n_tied, ")")
    )
  ]
  standings[, `:=`(h2h_sweep = NULL, tie_winner = NULL, tie_loser = NULL)]
  standings
}

break_conf_ties_by_conf_win_pct <- function(standings, n_tied){
  standings[
    conf_rank_counter == n_tied,
    `:=`(
      tie_winner = frankv(-conf_pct, ties.method = "max") == 1,
      tie_loser = frankv(-conf_pct, ties.method = "dense") != 1
    ),
    by = "tied_for"
  ]
  standings[
    tie_loser == TRUE,
    `:=`(
      conf_rank_counter = NA_integer_,
      conf_rank = conf_rank + 1
    )
  ]
  standings[
    tie_winner == TRUE,
    `:=`(
      conf_rank_counter = 1L,
      conf_tie_broken_by = paste0("Conference Win PCT (", n_tied, ")")
    )
  ]
  standings[, `:=`(tie_winner = NULL, tie_loser = NULL)]
  standings
}

break_conf_ties_by_common_win_pct <- function(standings, h2h, n_tied){
  ties <- standings[conf_rank_counter == n_tied]

  common_win_pct <- merge(
    ties[, list(sim, conf, team, conf_rank)], h2h, by = c("sim", "team"), all.y = FALSE
  )[,
    common := as.integer(.N == n_tied),
    by = c("sim", "conf", "opp", "conf_rank")
  ][,
    list(
      common_games = sum(common * h2h_games),
      common_win_pct = sum(common * h2h_wins) / sum(common * h2h_games)
    ),
    by = c("sim", "team")
  ]
  common_win_pct[is.nan(common_win_pct), common_win_pct := 0]

  standings <- merge(standings, common_win_pct, by = c("sim", "team"), all.x = TRUE)
  standings[
    conf_rank_counter == n_tied & common_games >= 4,
    `:=`(
      tie_winner = frankv(-common_win_pct, ties.method = "max") == 1,
      tie_loser = frankv(-common_win_pct, ties.method = "dense") != 1
    ),
    by = "tied_for"
  ]
  standings[
    tie_loser == TRUE,
    `:=`(
      conf_rank_counter = NA_integer_,
      conf_rank = conf_rank + 1
    )
  ]
  standings[
    tie_winner == TRUE,
    `:=`(
      conf_rank_counter = 1L,
      conf_tie_broken_by = paste0("Common Games Win PCT (", n_tied, ")")
    )
  ]
  standings[, `:=`(common_win_pct = NULL, common_games = NULL, tie_winner = NULL, tie_loser = NULL)]
  standings
}

break_conf_ties_by_sov <- function(standings, n_tied){
  standings[
    conf_rank_counter == n_tied,
    `:=`(
      tie_winner = frankv(-sov, ties.method = "max") == 1,
      tie_loser = frankv(-sov, ties.method = "dense") != 1
    ),
    by = "tied_for"
  ]
  standings[
    tie_loser == TRUE,
    `:=`(
      conf_rank_counter = NA_integer_,
      conf_rank = conf_rank + 1
    )
  ]
  standings[
    tie_winner == TRUE,
    `:=`(
      conf_rank_counter = 1L,
      conf_tie_broken_by = paste0("SOV (", n_tied, ")")
    )
  ]
  standings[, `:=`(tie_winner = NULL, tie_loser = NULL)]
  standings
}

break_conf_ties_by_sos <- function(standings, n_tied){
  standings[
    conf_rank_counter == n_tied,
    `:=`(
      tie_winner = frankv(-sos, ties.method = "max") == 1,
      tie_loser = frankv(-sos, ties.method = "dense") != 1
    ),
    by = "tied_for"
  ]
  standings[
    tie_loser == TRUE,
    `:=`(
      conf_rank_counter = NA_integer_,
      conf_rank = conf_rank + 1
    ),
    by = "tied_for"
  ]
  standings[
    tie_winner == TRUE,
    `:=`(
      conf_rank_counter = 1L,
      conf_tie_broken_by = paste0("SOS (", n_tied, ")")
    )
  ]
  standings[, `:=`(tie_winner = NULL, tie_loser = NULL)]
  standings
}

break_conf_ties_by_coinflip <- function(standings, n_tied){
  standings[
    conf_rank_counter == n_tied,
    `:=`(
      conf_rank = conf_rank - 1 + frank(list(conf_rank, -win_pct), ties.method = "random"),
      conf_tie_broken_by = "Coin Toss"
    ),
    by = "tied_for"
  ]
  standings
}

conf_tie_break_done <- function(standings, n_tied){
  # We set the counter of eliminated teams to NA.
  # That's why we have to remove NAs here
  all(standings$conf_rank_counter < n_tied, na.rm = TRUE)
}

conf_apply_division_reduction <- function(standings, verbosity){
  # If there is a conf rank where multiple teams from one division are tied for,
  # the variable apply_div_reduction will be TRUE for the lower division rank
  standings[
    conf_rank_counter > 1,
    apply_div_reduction := fifelse(div_rank != min(div_rank), TRUE, FALSE),
    by = c("sim", "conf_rank", "division")
  ]

  if (any(standings$apply_div_reduction == TRUE, na.rm = TRUE) & verbosity == 2L){
    report("CONF    : Apply Division Reduction")
  }
  # We increment the rank of the eliminated teams...
  standings[
    apply_div_reduction == TRUE,
    conf_rank := conf_rank + 1,
  ]
  # and count ranks again, because counters cannot be greater than 4
  standings <- conf_count_ranks(standings)
  # The counter counts NAs so we have to remove those to avoid a participation
  # of the eliminated teams in lower tier tiebreakers
  standings[
    apply_div_reduction == TRUE,
    conf_rank_counter := NA_integer_
  ]
  # Always remove helpers
  standings[, apply_div_reduction := NULL]
  standings
}

conf_count_ranks <- function(standings){
  standings[, conf_rank_counter := .N, by = c("sim", "conf", "conf_rank")]
}

