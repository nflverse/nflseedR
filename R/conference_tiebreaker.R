#### CONFERENCE TIEBREAKER ####

# break ties for next division rank
# u = teams which may be tied
# r = rank number to set
break_conference_ties <- function(u, r, h2h, tb_depth, .debug = FALSE) {

  # if any ties
  if (any(is.na(u$conf_rank)) && tb_depth > TIEBREAKERS_NONE) {

    # larger ties before smaller ties
    for (min_tied in 3:2)
    {

      # filter to ties
      tied <- u %>%
        filter(is.na(conf_rank)) %>%
        group_by(sim, conf) %>%
        mutate(tied_teams = n()) %>%
        ungroup()

      # any ties to break at this size?
      if (tied %>% filter(tied_teams >= min_tied) %>% nrow() == 0) next

      # if not all division winners, reduce to best per division
      if (tied %>% filter(div_winner) %>% nrow() == 0) {
        if (isTRUE(.debug)) report("CONF ({min_tied}): Best-in-division reduction")
        list[u, tied] <- tied %>%
          group_by(sim, conf, division) %>%
          mutate(value = case_when(
            max(tied_teams) < min_tied ~ NA_real_,
            TRUE ~ as.numeric(div_best_left)
          )) %>%
          ungroup() %>%
          process_conf_ties(u, r)

        # any ties to break at this size?
        if (tied %>% filter(tied_teams >= min_tied) %>% nrow() == 0) next
      }

      # head-to-head sweep
      if (isTRUE(.debug)) report("CONF ({min_tied}): Head-to-head Sweep")
      list[u, tied] <- tied %>%
        inner_join(tied %>% select(sim, conf, team, div_winner, div_best_left, win_pct),
          by = c("sim", "conf", "div_winner", "div_best_left", "win_pct"),
          suffix = c("", "_opp")
        ) %>%
        rename(opp = team_opp) %>%
        filter(team != opp) %>%
        left_join(h2h, by = c("sim", "team", "opp")) %>%
        group_by(
          sim, conf, division, div_winner, div_best_left, team,
          conf_pct, sov, sos, tied_teams
        ) %>%
        summarize(value = case_when(
          max(tied_teams, na.rm = TRUE) < min_tied ~ NA_real_, # not enough tied teams
          sum(h2h_games, na.rm = TRUE) < (max(tied_teams) - 1) ~ 0, # didn't play vs. each other tied team
          sum(h2h_wins, na.rm = TRUE) == 0 ~ -1, # got swept by other tied teams
          sum(h2h_wins, na.rm = TRUE) == (max(tied_teams) - 1) ~ 1, # swept other tied teams
          TRUE ~ 0, # split vs. other tied teams
        )) %>%
        ungroup() %>%
        process_conf_ties(u, r)

      # any ties to break at this size?
      if (tied %>% filter(tied_teams >= min_tied) %>% nrow() == 0) next

      # conference record
      if (isTRUE(.debug)) report("CONF ({min_tied}): Conference Record")
      list[u, tied] <- tied %>%
        mutate(value = case_when(
          tied_teams < min_tied ~ NA_real_,
          TRUE ~ conf_pct
        )) %>%
        process_conf_ties(u, r)

      # any ties to break at this size?
      if (tb_depth < TIEBREAKERS_NO_COMMON) next
      if (tied %>% filter(tied_teams >= min_tied) %>% nrow() == 0) next

      # common games
      if (isTRUE(.debug)) report("CONF ({min_tied}): Common Record")
      list[u, tied] <- tied %>%
        left_join(h2h, by = c("sim", "team")) %>%
        filter(h2h_played == 1) %>%
        group_by(sim, conf, opp) %>%
        mutate(common = (tied_teams == n())) %>%
        ungroup() %>%
        group_by(sim, conf, division, team, conf_pct, sov, sos, tied_teams) %>%
        summarize(value = case_when(
          max(tied_teams) < min_tied ~ NA_real_,
          sum(common) == 0 ~ 0.5,
          sum(common * h2h_games) < 4 ~ 0.5, # this only applies if 4+ games
          TRUE ~ sum(common * h2h_wins) / sum(common * h2h_games)
        )) %>%
        ungroup() %>%
        process_conf_ties(u, r)

      # any ties to break at this size?
      if (tied %>% filter(tied_teams >= min_tied) %>% nrow() == 0) next

      # strength of victory
      if (isTRUE(.debug)) report("CONF ({min_tied}): Strength of Victory")
      list[u, tied] <- tied %>%
        mutate(value = case_when(
          tied_teams < min_tied ~ NA_real_,
          TRUE ~ sov
        )) %>%
        process_conf_ties(u, r)

      # any ties to break at this size?
      if (tied %>% filter(tied_teams >= min_tied) %>% nrow() == 0) next

      # strength of schedule
      if (isTRUE(.debug)) report("CONF ({min_tied}): Strength of Schedule")
      list[u, tied] <- tied %>%
        mutate(value = case_when(
          tied_teams < min_tied ~ NA_real_,
          TRUE ~ sos
        )) %>%
        process_conf_ties(u, r)
    }
  }

  # break any remaning ties at random
  u <- u %>%
    mutate(coin_flip = sample(n())) %>%
    group_by(sim, conf, conf_rank, div_winner, win_pct) %>%
    mutate(conf_rank = case_when(
      !is.na(conf_rank) ~ conf_rank,
      coin_flip == max(coin_flip) ~ as.numeric(r),
      TRUE ~ NA_real_
    )) %>%
    ungroup() %>%
    filter(!is.na(conf_rank)) %>%
    rename(new_rank = conf_rank) %>%
    select(sim, team, new_rank)

  # return updates
  return(u)
}

process_conf_ties <- function(t, u, r = seed_num) {
  # value = max value for this
  # 0 = teams elimianted from tiebreaker
  t <- t %>%
    group_by(sim, conf) %>%
    mutate(tied = (value == max(value))) %>%
    mutate(tied_teams = ifelse(!is.na(sum(tied)), sum(tied), tied_teams)) %>%
    mutate(new_rank = case_when(
      !tied ~ as.numeric(0),
      sum(tied) == 1 & tied ~ as.numeric(r),
      TRUE ~ NA_real_
    )) %>%
    ungroup()
  u <- u %>%
    left_join(t %>% select(sim, team, new_rank),
      by = c("sim", "team")
    ) %>%
    mutate(conf_rank = ifelse(!is.na(new_rank), new_rank, conf_rank)) %>%
    filter(is.na(new_rank) | new_rank != 0) %>%
    select(-new_rank)
  t <- t %>%
    filter(is.na(new_rank)) %>%
    select(-value, -tied, -new_rank)
  return(list(u = u, tied = t))
}
