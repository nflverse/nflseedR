#### DRAFT TIEBREAKER ####

# break ties for next division rank
# u = teams which may be tied
# r = rank number to set
break_draft_ties <- function(u, r, h2h, tb_depth, .debug = FALSE) {

  # any ties to break?
  if (any(is.na(u$draft_order)) && tb_depth > TIEBREAKERS_NONE) {
    for (min_tied in 2)
    {

      # filter to ties
      tied <- u %>%
        group_by(sim) %>%
        mutate(tied_teams = n()) %>%
        ungroup()

      # any ties to break?
      if (tied %>% filter(tied_teams >= min_tied) %>% nrow() == 0) next

      # divisional tiebreakers
      if (isTRUE(.debug)) report(glue("DRAFT: Divisional Rank"))
      list[u, tied] <- tied %>%
        group_by(sim) %>%
        mutate(value = case_when(
          min(division) != max(division) ~ NA_real_,
          TRUE ~ -div_rank
        )) %>%
        ungroup() %>%
        process_draft_ties(u, r)

      # any ties to break?
      if (tied %>% filter(tied_teams >= min_tied) %>% nrow() == 0) next

      # conference tiebreakers
      if (isTRUE(.debug)) report(glue("DRAFT: Conference Rank"))
      list[u, tied] <- tied %>%
        mutate(
          div_winner = NA, # we don't care about div winners here
          conf_rank = NA_real_
        ) %>%
        group_by(sim, division) %>%
        mutate(div_best_left = (div_rank == min(div_rank))) %>%
        ungroup() %>%
        break_conference_ties(r, h2h = h2h, tb_depth = tb_depth, .debug = .debug) %>%
        right_join(tied, by = c("sim", "team"), multiple = "all") %>%
        group_by(sim) %>%
        mutate(value = case_when(
          min(conf) != max(conf) ~ NA_real_,
          !is.na(new_rank) ~ new_rank,
          TRUE ~ 0
        )) %>%
        ungroup() %>%
        select(-new_rank) %>%
        process_draft_ties(u, r)

      # any ties to break?
      if (tied %>% filter(tied_teams >= min_tied) %>% nrow() == 0) next

      # head-to-head sweep
      if (isTRUE(.debug)) report(glue("DRAFT: Head-to-head Sweep"))
      list[u, tied] <- tied %>%
        inner_join(tied %>% select(sim, team), by = c("sim"), suffix = c("", "_opp"), multiple = "all") %>%
        rename(opp = team_opp) %>%
        filter(team != opp) %>%
        inner_join(h2h, by = c("sim", "team", "opp"), multiple = "all") %>%
        group_by(sim, team, sov, tied_teams) %>%
        summarize(value = case_when(
          sum(h2h_games) < (max(tied_teams) - 1) ~ 0, # didn't play vs. each other tied team
          sum(h2h_wins) == 0 ~ -1, # got swept by other tied teams
          sum(h2h_wins) == (max(tied_teams) - 1) ~ 1, # swept other tied teams
          TRUE ~ 0, # won some, lost others
        )) %>%
        ungroup() %>%
        process_draft_ties(u, r)

      # any ties to break at this size?
      if (tb_depth < TIEBREAKERS_NO_COMMON) next
      if (tied %>% filter(tied_teams >= min_tied) %>% nrow() == 0) next

      # common games
      if (isTRUE(.debug)) report(glue("DRAFT: Common Record"))
      list[u, tied] <- tied %>%
        inner_join(h2h, by = c("sim", "team"), multiple = "all") %>%
        filter(h2h_played == 1) %>%
        group_by(sim, opp) %>%
        mutate(common = (tied_teams == n())) %>%
        ungroup() %>%
        group_by(sim, team, sov, tied_teams) %>%
        summarize(value = case_when(
          sum(common) == 0 ~ 0.5,
          sum(common * h2h_games) < 4 ~ 0.5, # this only applies if 4+ games
          TRUE ~ sum(common * h2h_wins) / sum(common * h2h_games)
        )) %>%
        ungroup() %>%
        process_draft_ties(u, r)

      # any ties to break at this size?
      if (tied %>% filter(tied_teams >= min_tied) %>% nrow() == 0) next

      # strength of victory
      if (isTRUE(.debug)) report(glue("DRAFT: Strength of Victory"))
      list[u, tied] <- tied %>%
        mutate(value = sov) %>%
        process_draft_ties(u, r)
    }
  }

  # break any remaining ties at random
  if (any(is.na(u$draft_order))) {
    u <- u %>%
      mutate(coin_flip = sample(n())) %>%
      group_by(sim) %>%
      mutate(draft_order = case_when(
        !is.na(draft_order) ~ as.numeric(draft_order),
        coin_flip == min(coin_flip) ~ as.numeric(r),
        TRUE ~ NA_real_
      )) %>%
      ungroup() %>%
      filter(!is.na(draft_order))
  }

  u <- u %>%
    rename(new_do = draft_order) %>%
    select(sim, team, new_do)

  # return updates
  return(u)
}

process_draft_ties <- function(t, u, d) {
  # value = min value for this
  # 0 = teams elimianted from tiebreaker
  t <- t %>%
    group_by(sim) %>%
    mutate(tied = (value == max(value))) %>%
    mutate(tied_teams = ifelse(!is.na(sum(tied)), sum(tied), tied_teams)) %>%
    mutate(new_do = case_when(
      !tied ~ 0,
      sum(tied) == 1 & tied ~ as.numeric(d),
      TRUE ~ NA_real_
    )) %>%
    ungroup()
  u <- u %>%
    left_join(t %>% select(sim, team, new_do),
      by = c("sim", "team"), multiple = "all"
    ) %>%
    mutate(draft_order = ifelse(!is.na(new_do), new_do, draft_order)) %>%
    filter(is.na(new_do) | new_do != 0) %>%
    select(-new_do)
  t <- t %>%
    filter(is.na(new_do)) %>%
    select(-value, -tied, -new_do)
  return(list(u = u, tied = t))
}
