# https://www.nfl.com/standings/tie-breaking-procedures
add_draft_ranks <- function(standings,
                            h2h,
                            dg,
                            tiebreaker_depth,
                            playoff_seeds,
                            verbosity){
  dg[, sb_winner := fifelse(game_type == "SB" & result > 0, 1L, 0L, 0L)]
  exit <- dg[
    game_type != "REG",
    list(exit = max(week) + max(sb_winner) - 1L),
    by = c("sim", "team")
  ]
  standings <- merge(standings, exit, by = c("sim", "team"), all.x = TRUE)
  standings[is.na(exit), exit := 0L]

  # Set ranks by exit, win percentage, and sos in ascending order by sim.
  # If ties method is "random", data.table will break all ties randomly
  # and we won't need any further tie-breaking methods
  dt_ties_method <- if (tiebreaker_depth == "RANDOM") "random" else "min"
  standings[,
    draft_rank := frank(list(exit, win_pct, sos), ties.method = dt_ties_method),
    by = c("sim")
  ]

  # If tiebreaker_depth == "RANDOM", all ties are broken at this stage. We add
  # tiebreaker information to the tied teams.
  if (tiebreaker_depth == "RANDOM") {
    standings[, draft_rank_counter := .N, by = c("sim", "exit", "win_pct", "sos")]
    standings[
      draft_rank_counter > 1,
      draft_tie_broken_by := "Coin Toss",
    ]
    standings[, tied_for := NA_character_]
  }

  # Count draft ranks by sim. If each rank only exists once,
  # then there are no ties that need to be broken
  standings <- draft_count_ranks(standings)

  # Do this only if any ties exist
  if ( any(standings$draft_rank_counter > 1) ) {
    if(verbosity == 2L) report("Break DRAFT ties")

    # If all tied clubs are from the same division, or same conference, we can apply
    # division/conference tiebreakers, i.e. the div_rank or conf_rank
    # We do this here before any tiebreaking starts
    standings <- break_draft_ties_by_division(standings, verbosity = verbosity)
    standings <- break_draft_ties_by_conference(standings, verbosity = verbosity)

    # enter tie breaking procedure only if there are actual ties,
    # i.e. a draft rank exists more than once per sim
    # and tied teams don't share the same division or conference
    # draft tie breakers allow only one team to advance in any tie-breaking
    # step. So if there are ties with more than 2 teams, we have to do the 4:3:2
    # loop multiple times. We could calculate the number of loops as the maximum
    # of draft_rank_counter numbers. But it is easier to loop over this thing in a
    # while loop.

    # We add a loop counter to avoid infinite loops
    while_counter <- 0L

    while ( any(standings$draft_rank_counter > 1) ) {

      while_counter <- while_counter + 1L

      if (while_counter > 18L){
        cli::cli_abort("Entered infinite loop in draft tiebreaking procedure")
      }

      # Add a helper variable to summarize information on tied teams
      # We use this as grouping variable in subsequent functions
      standings[
        draft_rank_counter > 1,
        tied_for := paste0(sim, " #", draft_rank, " (n = ", draft_rank_counter, ")")
      ]

      # NOTE: The system of the below code is as follows
      # All teams that are eliminated in any tiebreaking step, either through
      # division/conference reduction or through actual tiebreakers, will get their
      # `draft_rank` increased by 1 (they lost, so they won't get that rank)
      # `draft_rank_counter` set to NA_integer_
      # After the 2 teams tiebreak, we count draft_ranks again. If all ties
      # are broken, there won't be any counter > 1 and we are done.

      # If multiple teams from one division/conference are part of a tiebreaker,
      # we have to make multiple rounds and start with the lowest div_rank/conf_rank.
      # Teams losing at this stage get their counter set to NA and rank incremented
      # This reduction ultimately means that we only ever have to perform a 2-team tiebreaker
      standings <- draft_apply_reduction(standings, verbosity = verbosity)

      # Since we allow only one team per division and conference, there can never
      # be more than 2 tied teams during a tiebreaking process.
      # That's why we loop over the value 2. The loop allows us to exit the process.
      # Every tiebreaking function updates the draft_rank_counter and the conf_rank
      # of eliminated or winning teams.
      for (tied_teams in 2) {

        if (draft_tie_break_done(standings, tied_teams)) next

        # Head To Head ------------------------------------------------------------
        if (verbosity == 2L) report("DRAFT ({tied_teams}): Head-to-Head Sweep")
        standings <- break_draft_ties_by_h2h(standings = standings, h2h = h2h, n_tied = tied_teams)
        if (draft_tie_break_done(standings, tied_teams)) next

        # Common Games Win Pct ----------------------------------------------------
        if (verbosity == 2L) report("DRAFT ({tied_teams}): Common Games Win PCT")
        standings <- break_draft_ties_by_common_win_pct(standings = standings, h2h = h2h, n_tied = tied_teams)
        if (draft_tie_break_done(standings, tied_teams)) next

        # SOV ---------------------------------------------------------------------
        if (verbosity == 2L) report("DRAFT ({tied_teams}): Strength of Victory")
        standings <- break_draft_ties_by_sov(standings = standings, n_tied = tied_teams)
        if (draft_tie_break_done(standings, tied_teams)) next

        # Coin Flip ---------------------------------------------------------------
        if (verbosity == 2L) report("DRAFT ({tied_teams}): Coin Toss")
        standings <- break_draft_ties_by_coinflip(standings = standings, n_tied = tied_teams)

      } # end of tied teams loop

      # The round of ties is broken and we have set the counter of the eliminated
      # teams to NA during the process.
      # We've also increased the possible draft rank of the eliminated teams by 1,
      # so now we need to recount all ranks and break ties again, if necessary.
      standings <- draft_count_ranks(standings)

      # At this spot, we might have remaining ties within one division/conference
      #  where we can apply the corresponding tiebreaker.
      #  We do this here to avoid another round of the loop
      standings <- break_draft_ties_by_division(standings, verbosity = verbosity)
      standings <- break_draft_ties_by_conference(standings, verbosity = verbosity)
      standings[, tied_for := NULL]
    }# end of draft_rank_counter loop
  }# end of tie breaking
  # Finally, the helper variables can be removed
  standings <- standings[, draft_rank_counter := NULL]
  standings
}

break_draft_ties_by_division <- function(standings, verbosity){
  # The variable draft_rank_shared_by_one_div will be TRUE if all teams that are
  # tied for one rank share the same division.
  standings[
    draft_rank_counter > 1,
    draft_rank_shared_by_one_div := uniqueN(division) == 1,
    by = c("sim", "draft_rank")
  ]

  if (any(standings$draft_rank_shared_by_one_div, na.rm = TRUE) & verbosity == 2L){
    report("DRAFT    : Division Rank")
  }
  # In this case, we can break the tie by ranking them through div_rank
  # lower div_rank wins higher draft_rank!
  standings[
    draft_rank_counter > 1 & draft_rank_shared_by_one_div == TRUE,
    `:=`(
      draft_rank = min(draft_rank) - 1 + frankv(div_rank, order = -1L, ties.method = "min"),
      draft_tie_broken_by = "Division Tiebreaker"
    ),
    by = c("sim", "draft_rank")
  ]
  # Remove the helper and update the counter because the tie is broken
  standings[, draft_rank_shared_by_one_div := NULL]
  standings <- draft_count_ranks(standings)
  standings
}

break_draft_ties_by_conference <- function(standings, verbosity){
  # The variable draft_rank_shared_by_one_conf will be TRUE if all teams that are
  # tied for one rank share the same conference.
  standings[
    draft_rank_counter > 1,
    draft_rank_shared_by_one_conf := uniqueN(conf) == 1,
    by = c("sim", "draft_rank")
  ]

  if (any(standings$draft_rank_shared_by_one_conf, na.rm = TRUE) & verbosity == 2L){
    report("DRAFT    : Conference Rank")
  }
  # In this case, we can break the tie by ranking them through conf_rank
  # lower conf_rank wins higher draft_rank!
  standings[
    draft_rank_counter > 1 & draft_rank_shared_by_one_conf == TRUE,
    `:=`(
      draft_rank = min(draft_rank) - 1 + frankv(conf_rank, order = -1L, ties.method = "min"),
      draft_tie_broken_by = "Conference Tiebreaker"
    ),
    by = c("sim", "draft_rank")
  ]
  # Remove the helper and update the counter because the tie is broken
  standings[, draft_rank_shared_by_one_conf := NULL]
  standings <- draft_count_ranks(standings)
  standings
}

break_draft_ties_by_h2h <- function(standings, h2h, n_tied){
  # 1. Compute a head 2 head table of the tied teams
  ties <- standings[draft_rank_counter == n_tied]

  h2h_games_played <- merge(
    ties[, list(sim, team, draft_rank)],
    ties[, list(sim, opp = team, draft_rank)],
    by = c("sim", "draft_rank"),
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
    draft_rank_counter == n_tied,
    `:=`(
      tie_winner = frankv(h2h_sweep, ties.method = "max") == 1,
      tie_loser = frankv(h2h_sweep, ties.method = "dense") != 1
    ),
    by = "tied_for"
  ]
  standings[
    tie_loser == TRUE,
    `:=`(
      draft_rank_counter = NA_integer_,
      draft_rank = draft_rank + 1
    )
  ]
  standings[
    tie_winner == TRUE,
    `:=`(
      draft_rank_counter = 1L,
      draft_tie_broken_by = paste0("Head-To-Head (", n_tied, ")")
    )
  ]
  standings[, c("h2h_sweep", "tie_winner", "tie_loser") := NULL]
  standings
}

break_draft_ties_by_common_win_pct <- function(standings, h2h, n_tied){
  ties <- standings[draft_rank_counter == n_tied]

  common_win_pct <- merge(
    ties[, list(sim, team, draft_rank)], h2h, by = c("sim", "team"), all.y = FALSE
  )[,
    common := as.integer(.N == n_tied),
    by = c("sim", "opp", "draft_rank")
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
    draft_rank_counter == n_tied & common_games >= 4,
    `:=`(
      tie_winner = frankv(common_win_pct, ties.method = "max") == 1,
      tie_loser = frankv(common_win_pct, ties.method = "dense") != 1
    ),
    by = "tied_for"
  ]
  standings[
    tie_loser == TRUE,
    `:=`(
      draft_rank_counter = NA_integer_,
      draft_rank = draft_rank + 1
    )
  ]
  standings[
    tie_winner == TRUE,
    `:=`(
      draft_rank_counter = 1L,
      draft_tie_broken_by = paste0("Common Games Win PCT (", n_tied, ")")
    )
  ]
  standings[, c("common_win_pct", "common_games", "tie_winner", "tie_loser") := NULL]
  standings
}

break_draft_ties_by_sov <- function(standings, n_tied){
  standings[
    draft_rank_counter == n_tied,
    `:=`(
      tie_winner = frankv(sov, ties.method = "max") == 1,
      tie_loser = frankv(sov, ties.method = "dense") != 1
    ),
    by = "tied_for"
  ]
  standings[
    tie_loser == TRUE,
    `:=`(
      draft_rank_counter = NA_integer_,
      draft_rank = draft_rank + 1
    )
  ]
  standings[
    tie_winner == TRUE,
    `:=`(
      draft_rank_counter = 1L,
      draft_tie_broken_by = paste0("SOV (", n_tied, ")")
    )
  ]
  standings[, c("tie_winner", "tie_loser") := NULL]
  standings
}

break_draft_ties_by_coinflip <- function(standings, n_tied){
  standings[
    draft_rank_counter == n_tied,
    `:=`(
      draft_rank = draft_rank - 1 + frank(list(exit, win_pct, sos), ties.method = "random"),
      draft_tie_broken_by = "Coin Toss"
    ),
    by = "tied_for"
  ]
  standings
}

draft_tie_break_done <- function(standings, n_tied){
  # We set the counter of eliminated teams to NA.
  # That's why we have to remove NAs here
  all(standings$draft_rank_counter < n_tied, na.rm = TRUE)
}

draft_apply_reduction <- function(standings, verbosity){
  # If there is a draft rank where multiple teams from one division are tied for,
  # the variable apply_div_reduction will be TRUE for the higher division rank
  standings[
    draft_rank_counter > 1,
    apply_div_reduction := fifelse(div_rank != max(div_rank), TRUE, FALSE),
    by = c("sim", "draft_rank", "division")
  ]
  # If there is a draft rank where multiple teams from one conference are tied for,
  # the variable apply_div_reduction will be TRUE for the higher conference rank
  standings[
    draft_rank_counter > 1,
    apply_conf_reduction := fifelse(conf_rank != max(conf_rank), TRUE, FALSE),
    by = c("sim", "draft_rank", "conf")
  ]

  if (any(standings$apply_div_reduction == TRUE, na.rm = TRUE) & verbosity == 2L){
    report("DRAFT    : Apply Division Reduction")
  }
  if (any(standings$apply_conf_reduction == TRUE, na.rm = TRUE) & verbosity == 2L){
    report("DRAFT    : Apply Conference Reduction")
  }

  # We increment the rank of the eliminated teams...
  standings[
    apply_div_reduction == TRUE | apply_conf_reduction == TRUE,
    draft_rank := draft_rank + 1,
  ]
  # and count ranks again, because counters cannot be greater than 4
  standings <- draft_count_ranks(standings)
  # The counter counts NAs so we have to remove those to avoid a participation
  # of the eliminated teams in lower tier tiebreakers
  standings[
    apply_div_reduction == TRUE | apply_conf_reduction == TRUE,
    draft_rank_counter := NA_integer_
  ]
  # Always remove helpers
  standings[, c("apply_div_reduction", "apply_conf_reduction") := NULL]
  standings
}

draft_count_ranks <- function(standings){
  standings[, draft_rank_counter := .N, by = c("sim", "draft_rank")]
}

