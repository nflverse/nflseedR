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
        left_join(h2h, by = c("sim", "team", "opp")) %>%
        group_by(sim, division, team, div_pct, conf_pct, sov, sos, tied_teams) %>%
        summarize(value = case_when(
          max(tied_teams, na.rm = TRUE) < min_tied ~ NA_real_,
          sum(h2h_games, na.rm = TRUE) == 0 ~ 0.5,
          TRUE ~ sum(h2h_wins, na.rm = TRUE) / sum(h2h_games, na.rm = TRUE)
        )) %>%
        ungroup() %>%
        process_div_ties(u, r, tb_type = "Head-to-head")

      # any ties to break at this size?
      if (tied %>% filter(tied_teams >= min_tied) %>% nrow() == 0) next

      # division record
      if (isTRUE(.debug)) report("DIV ({min_tied}): Division Record")
      list[u, tied] <- tied %>%
        mutate(value = case_when(
          tied_teams < min_tied ~ NA_real_,
          TRUE ~ div_pct
        )) %>%
        process_div_ties(u, r, tb_type = "Division Record")

      # any ties to break at this size?
      if (tb_depth < TIEBREAKERS_NO_COMMON) next
      if (tied %>% filter(tied_teams >= min_tied) %>% nrow() == 0) next

      # common games
      if (isTRUE(.debug)) report("DIV ({min_tied}): Common Record")
      list[u, tied] <- tied %>%
        left_join(h2h, by = c("sim", "team")) %>%
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
        process_div_ties(u, r, tb_type = "Common Record")

      # any ties to break at this size?
      if (tied %>% filter(tied_teams >= min_tied) %>% nrow() == 0) next

      # conference record
      if (isTRUE(.debug)) report("DIV ({min_tied}): Conference Record")
      list[u, tied] <- tied %>%
        mutate(value = case_when(
          tied_teams < min_tied ~ NA_real_,
          TRUE ~ conf_pct
        )) %>%
        process_div_ties(u, r, tb_type = "Conference Record")

      # any ties to break at this size?
      if (tied %>% filter(tied_teams >= min_tied) %>% nrow() == 0) next

      if (tb_depth < TIEBREAKERS_THROUGH_SOS) next

      # strength of victory
      if (isTRUE(.debug)) report("DIV ({min_tied}): Strength of Victory")
      list[u, tied] <- tied %>%
        mutate(value = case_when(
          tied_teams < min_tied ~ NA_real_,
          TRUE ~ sov
        )) %>%
        process_div_ties(u, r, tb_type = "SOV")

      # any ties to break at this size?
      if (tied %>% filter(tied_teams >= min_tied) %>% nrow() == 0) next

      # strength of schedule
      if (isTRUE(.debug)) report("DIV ({min_tied}): Strength of Schedule")
      list[u, tied] <- tied %>%
        mutate(value = case_when(
          tied_teams < min_tied ~ NA_real_,
          TRUE ~ sos
        )) %>%
        process_div_ties(u, r, tb_type = "SOS")
    }
  }

  if(any(is.na(u$div_rank))){
    # break any remaining ties at random
    if (isTRUE(.debug)) report("DIV: Coinflip")
    u <- u %>%
      mutate(coin_flip = sample(n())) %>%
      group_by(sim, division) %>%
      mutate(
        div_rank = case_when(
          !is.na(div_rank) ~ div_rank,
          coin_flip == max(coin_flip) ~ r,
          TRUE ~ NA_real_
        ),
        tie_broken_by = case_when(
          !is.na(div_rank) ~ tie_broken_by,
          coin_flip == max(coin_flip) ~ "Coinflip",
          TRUE ~ NA_character_
        )
      ) %>%
      ungroup()
  }

  u <- u %>%
    filter(!is.na(div_rank)) %>%
    rename(new_rank = div_rank, tb_new = tie_broken_by) %>%
    select(sim, team, new_rank, tb_new)

  # return updates
  return(u)
}



process_div_ties <- function(t, u, r, tb_type = NA_character_) {
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
    mutate(
      div_rank = ifelse(!is.na(new_rank), new_rank, div_rank),
      tie_broken_by = ifelse(!is.na(new_rank), tb_type, tie_broken_by)
    ) %>%
    filter(is.na(new_rank) | new_rank != 0) %>%
    select(-new_rank)
  t <- t %>%
    filter(is.na(new_rank)) %>%
    select(-value, -tied, -new_rank)
  return(list(u = u, tied = t))
}


break_div_ties_by_h2h <- function(teams, h2h, n_tied){
  if(n_tied == 2 && FALSE) browser()
  ties <- compute_tied_teams(teams, n_tied)

  h2h_games_played <- merge(
    ties[, list(sim, team)],
    ties[, list(sim, opp = team)],
    by = c("sim"),
    allow.cartesian = TRUE
  )[team != opp]

  h2h_win_pct <- merge(
    h2h_games_played, h2h, by = c("sim", "team", "opp")
  )[, list(h2h_win_pct = sum(h2h_wins) / sum(h2h_games)), by = c("sim", "team")]

  teams <- merge(teams, h2h_win_pct, by = c("sim", "team"), all.x = TRUE)
  teams[
    div_rank_counter == n_tied,
    div_rank := min(div_rank) - 1 + frank(list(div_rank, -h2h_win_pct), ties.method = "min"),
    by = c("sim", "division")
  ]
  teams[, div_rank_counter := .N, by = c("sim", "division", "div_rank")]
  teams[!is.na(h2h_win_pct) & div_rank_counter == 1, div_tie_broken_by := "Head-To-Head Win PCT"]
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
    div_tie_broken_by := "Division Win PCT",
  ]
  teams[, div_rank_counter := .N, by = c("sim", "division", "div_rank")]
  teams[div_rank_counter > 1, div_tie_broken_by := NA_character_]
  teams
}

break_div_ties_by_common_win_pct <- function(teams, h2h, n_tied){
  ties <- compute_tied_teams(teams, n_tied)

  common_win_pct <- merge(
    ties[, list(sim, division, team)], h2h, by = c("sim", "team"), all.y = FALSE
  )[
    , common := as.integer(.N == n_tied),
    by = c("sim", "division", "opp")
  ][
    , list(common_win_pct = sum(common * h2h_wins) / sum(common * h2h_games)),
    by = c("sim", "team")
  ]

  teams <- merge(teams, common_win_pct, by = c("sim", "team"), all.x = TRUE)
  teams[
    div_rank_counter == n_tied,
    div_rank := min(div_rank) - 1 + frank(list(div_rank, -common_win_pct), ties.method = "min"),
    by = c("sim", "division")
  ]
  teams[, div_rank_counter := .N, by = c("sim", "division", "div_rank")]
  teams[!is.na(common_win_pct) & div_rank_counter == 1, div_tie_broken_by := "Common Games Win PCT"]
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
    div_tie_broken_by := "Conference Win PCT",
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
    div_tie_broken_by := "SOV",
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
    div_tie_broken_by := "SOS",
  ]
  teams[, div_rank_counter := .N, by = c("sim", "division", "div_rank")]
  teams[div_rank_counter > 1, div_tie_broken_by := NA_character_]
  teams
}

compute_tied_teams <- function(teams, n_tied) teams[div_rank_counter == n_tied]

tie_break_done <- function(teams, n_tied) all(teams$div_rank_counter < n_tied)
