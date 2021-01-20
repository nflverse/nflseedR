#' @export
compute_division_ranks <- function(games,
                                   teams,
                                   tiebreaker_depth = 3,
                                   .debug = FALSE
                                   ){
  # NOTE: Add message here with ui_stop but avoid usethis dependency
  stopifnot(tiebreaker_depth %in% 1:3)
  #
  # double games
  games_doubled <- double_games(games)

  teams <- divisions[rep(seq_len(nrow(divisions)), each = iter_sims_num), ] %>%
    mutate(sim = rep(iter_sims, nrow(divisions))) %>%
    select(sim, everything())

  # record of each team
  report("Calculating team data")
  teams <- teams %>%
    inner_join(games_doubled, by = c("sim", "team")) %>%
    filter(game_type == "REG") %>%
    group_by(sim, conf, division, team) %>%
    summarize(games = n(), wins = sum(outcome)) %>%
    ungroup()

  # add in tiebreaker info
  teams <- teams %>%
    inner_join(games_doubled, by = c("sim", "team")) %>%
    filter(game_type == "REG") %>%
    inner_join(teams,
               by = c("sim" = "sim", "opp" = "team"),
               suffix = c("", "_opp")
    ) %>%
    mutate(
      win_pct = wins / games,
      div_game = ifelse(division == division_opp, 1, 0),
      conf_game = ifelse(conf == conf_opp, 1, 0)
    ) %>%
    group_by(sim, conf, division, team, games, wins, win_pct) %>%
    summarize(
      div_pct = ifelse(sum(div_game) == 0, 0.5,
                       sum(div_game * outcome) / sum(div_game)
      ),
      conf_pct = ifelse(sum(conf_game) == 0, 0.5,
                        sum(conf_game * outcome) / sum(conf_game)
      ),
      sov = ifelse(sum(outcome == 1) == 0, 0,
                   sum(wins_opp * (outcome == 1)) /
                     sum(games_opp * (outcome == 1))
      ),
      sos = sum(wins_opp) / sum(games_opp)
    ) %>%
    ungroup()

  # below only if there are tiebreakers
  h2h <- NULL
  if (tiebreaker_depth > TIEBREAKERS_NONE) {
    report("Calculating head to head")
    h2h <- teams %>%
      select(sim, team) %>%
      inner_join(teams %>% select(sim, team),
                 by = "sim", suffix = c("", "_opp")
      ) %>%
      rename(opp = team_opp) %>%
      arrange(sim, team, opp) %>%
      left_join(games_doubled %>% filter(game_type == "REG"),
                by = c("sim", "team", "opp")
      ) %>%
      group_by(sim, team, opp) %>%
      summarize(
        h2h_games = sum(!is.na(outcome)),
        h2h_wins = sum(outcome, na.rm = TRUE),
        h2h_played = ifelse(h2h_games > 0, 1, 0)
      ) %>%
      ungroup()
  }

  #### FIND DIVISION RANKS ####

  # initialize division rank
  teams <- teams %>%
    mutate(div_rank = NA_real_)

  # determine division ranks
  dr <- 0
  while (any(is.na(teams$div_rank))) {
    # increment division rank
    dr <- dr + 1
    report(paste0("Calculating division rank #", dr))

    # update teams with this rank
    update <- teams %>%
      filter(is.na(div_rank)) %>%
      group_by(sim, division) %>%
      filter(win_pct == max(win_pct)) %>%
      mutate(div_rank = ifelse(n() == 1, dr, div_rank)) %>%
      ungroup() %>%
      break_division_ties(dr, h2h = h2h, tb_depth = tiebreaker_depth, .debug = .debug)

    # store updates
    teams <- teams %>%
      left_join(update, by = c("sim", "team")) %>%
      mutate(div_rank = ifelse(!is.na(new_rank), new_rank, div_rank)) %>%
      select(-new_rank)
  }

  return(teams)
}
