#### DIVISION TIEBREAKER ####

# break ties for next division rank
# u = teams which may be tied
# r = division rank to set
break_division_ties <- function(u, r, h2h, tb_depth, .debug = FALSE) {

  # any ties to break?
  if (any(is.na(u$div_rank)) && tb_depth > TIEBREAKERS_NONE) {

    # larger ties before smaller ties
    for (min_tied in 3:2)
    {

      # filter to ties
      tied <- u %>%
        filter(is.na(div_rank)) %>%
        group_by(sim, division) %>%
        mutate(tied_teams = n()) %>%
        ungroup()

      # any ties to break at this size?
      if (tied %>% filter(tied_teams >= min_tied) %>% nrow() == 0) next

      # head-to-head
      if (isTRUE(.debug)) report("DIV ({min_tied}): Head-to-head")
      list[u, tied] <- tied %>%
        inner_join(tied %>% select(sim, division, team, win_pct),
                   by = c("sim", "division", "win_pct"),
                   suffix = c("", "_opp")
        ) %>%
        rename(opp = team_opp) %>%
        filter(team != opp) %>%
        inner_join(h2h, by = c("sim", "team", "opp")) %>%
        group_by(sim, division, team, div_pct, conf_pct, sov, sos, tied_teams) %>%
        summarize(value = case_when(
          max(tied_teams) < min_tied ~ NA_real_,
          sum(h2h_games) == 0 ~ 0.5,
          TRUE ~ sum(h2h_wins) / sum(h2h_games)
        )) %>%
        ungroup() %>%
        process_div_ties(u, r)

      # any ties to break at this size?
      if (tied %>% filter(tied_teams >= min_tied) %>% nrow() == 0) next

      # division record
      if (isTRUE(.debug)) report("DIV ({min_tied}): Division Record")
      list[u, tied] <- tied %>%
        mutate(value = case_when(
          tied_teams < min_tied ~ NA_real_,
          TRUE ~ div_pct
        )) %>%
        process_div_ties(u, r)

      # any ties to break at this size?
      if (tb_depth < TIEBREAKERS_NO_COMMON) next
      if (tied %>% filter(tied_teams >= min_tied) %>% nrow() == 0) next

      # common games
      if (isTRUE(.debug)) report("DIV ({min_tied}): Common Record")
      list[u, tied] <- tied %>%
        inner_join(h2h, by = c("sim", "team")) %>%
        filter(h2h_played == 1) %>%
        group_by(sim, division, opp) %>%
        mutate(common = (tied_teams == n())) %>%
        ungroup() %>%
        group_by(sim, division, team, conf_pct, sov, sos, tied_teams) %>%
        summarize(value = case_when(
          max(tied_teams) < min_tied ~ NA_real_,
          sum(common) == 0 ~ 0.5,
          TRUE ~ sum(common*h2h_wins) / sum(common*h2h_games)
        )) %>%
        ungroup() %>%
        process_div_ties(u, r)

      # any ties to break at this size?
      if (tied %>% filter(tied_teams >= min_tied) %>% nrow() == 0) next

      # conference record
      if (isTRUE(.debug)) report("DIV ({min_tied}): Conference Record")
      list[u, tied] <- tied %>%
        mutate(value = case_when(
          tied_teams < min_tied ~ NA_real_,
          TRUE ~ conf_pct
        )) %>%
        process_div_ties(u, r)

      # any ties to break at this size?
      if (tied %>% filter(tied_teams >= min_tied) %>% nrow() == 0) next

      # strength of victory
      if (isTRUE(.debug)) report("DIV ({min_tied}): Strength of Victory")
      list[u, tied] <- tied %>%
        mutate(value = case_when(
          tied_teams < min_tied ~ NA_real_,
          TRUE ~ sov
        )) %>%
        process_div_ties(u, r)

      # any ties to break at this size?
      if (tied %>% filter(tied_teams >= min_tied) %>% nrow() == 0) next

      # strength of schedule
      if (isTRUE(.debug)) report("DIV ({min_tied}): Strength of Schedule")
      list[u, tied] <- tied %>%
        mutate(value = case_when(
          tied_teams < min_tied ~ NA_real_,
          TRUE ~ sos
        )) %>%
        process_div_ties(u, r)
    }
  }

  # break any remaning ties at random
  u <- u %>%
    mutate(coin_flip = sample(n())) %>%
    group_by(sim, division) %>%
    mutate(div_rank = case_when(
      !is.na(div_rank) ~ div_rank,
      coin_flip == max(coin_flip) ~ r,
      TRUE ~ NA_real_
    )) %>%
    ungroup() %>%
    filter(!is.na(div_rank)) %>%
    rename(new_rank = div_rank) %>%
    select(sim, team, new_rank)

  # return updates
  return(u)
}



process_div_ties <- function(t, u, r) {
  # value = max value for this
  # 0 = teams elimianted from tiebreaker
  t <- t %>%
    group_by(sim, division) %>%
    mutate(tied = (value == max(value))) %>%
    mutate(tied_teams = ifelse(!is.na(sum(tied)), sum(tied), tied_teams)) %>%
    mutate(new_rank = case_when(
      !tied ~ 0,
      sum(tied) == 1 & tied ~ r,
      TRUE ~ NA_real_
    )) %>%
    ungroup()
  u <- u %>%
    left_join(t %>% select(sim, team, new_rank),
              by = c("sim", "team")
    ) %>%
    mutate(div_rank = ifelse(!is.na(new_rank), new_rank, div_rank)) %>%
    filter(is.na(new_rank) | new_rank != 0) %>%
    select(-new_rank)
  t <- t %>%
    filter(is.na(new_rank)) %>%
    select(-value, -tied, -new_rank)
  return(list(u = u, tied = t))
}
