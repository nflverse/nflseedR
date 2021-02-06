simulate_round <- function(sim_round,
                           sim_rounds,
                           sims_per_round,
                           schedule,
                           simulations,
                           weeks_to_sim,
                           process_games,
                           ...,
                           tiebreaker_depth,
                           .debug,
                           playoff_seeds,
                           p
                           ){

  # start us off
  # report(glue("Beginning simulation round {sim_round} of {sim_rounds}"))

  # iteration sims
  iter_sims <- sims_per_round * (sim_round - 1) + seq_len(sims_per_round)
  iter_sims <- iter_sims[iter_sims <= simulations]
  iter_sims_num <- length(iter_sims)

  # games have copies per sim
  games <- schedule[rep(seq_len(nrow(schedule)), each = iter_sims_num), ] %>%
    mutate(sim = rep(iter_sims, nrow(schedule))) %>%
    select(sim, everything())

  #### SIMULATE REGULAR SEASON ####

  # simulate remaining regular season games
  for (week_num in weeks_to_sim)
  {
    # estimate and simulate games
    # report(glue("Processing Week {week_num}"))
    list[teams,games] <-
      process_games(teams, games, week_num, ...)
  }

  #### FIND DIVISIONAL STANDINGS AND PLAYOFF SEEDINGS ####

  standings_and_h2h <- games %>%
    compute_division_ranks(
      tiebreaker_depth = tiebreaker_depth,
      .debug = .debug
    ) %>%
    compute_conference_seeds(
      h2h = .$h2h,
      tiebreaker_depth = tiebreaker_depth,
      .debug = .debug,
      playoff_seeds = playoff_seeds
    )

  teams <- standings_and_h2h$standings
  h2h_df <- standings_and_h2h$h2h


  #### PLAYOFFS ####

  # week tracker
  week_num <- games %>%
    filter(game_type == "REG") %>%
    pull(week) %>%
    max()

  week_max <- week_num + 4L

  # identify playoff teams
  playoff_teams <- teams %>%
    filter(!is.na(seed)) %>%
    select(sim, conf, seed, team) %>%
    arrange(sim, conf, seed)

  # num teams tracker
  num_teams <- playoff_teams %>%
    group_by(sim, conf) %>%
    summarize(count = n()) %>%
    pull(count) %>%
    max()


  # playoff weeks
  while (num_teams > 1) {

    # inseed_numement week number
    week_num <- week_num + 1
    report(paste("Processing Playoffs Week", week_num))

    # seed_numeate games if they don't already exist
    if (!any(games$week == week_num)) {
      # teams playing this round
      add_teams <- playoff_teams %>%
        group_by(sim, conf) %>%
        slice((2^ceiling(log(num_teams, 2)) - num_teams + 1):num_teams) %>%
        mutate(round_rank = row_number()) %>%
        ungroup()

      # games to seed_numeate
      add_games <- add_teams %>%
        inner_join(add_teams, by = c("sim", "conf")) %>%
        filter(round_rank.x > round_rank.y) %>%
        filter(round_rank.x + round_rank.y == max(round_rank.x) + 1) %>%
        select(-conf, -seed.x, -seed.y, -round_rank.x, -round_rank.y) %>%
        rename(away_team = team.x, home_team = team.y) %>%
        mutate(
          week = week_num,
          game_type = case_when(
            week_max - week_num == 3 ~ "WC",
            week_max - week_num == 2 ~ "DIV",
            week_max - week_num == 1 ~ "CON",
            week_max - week_num == 0 ~ "SB",
            TRUE ~ NA_character_
          )
        )

      # add to games
      games <- bind_rows(games, add_games)
    }

    # process any new games
    list[teams,games] <-
      process_games(teams, games, week_num, ...)

    # record losers
    teams <- games %>%
      filter(week == week_num) %>%
      double_games() %>%
      filter(outcome == 0) %>%
      select(sim, team, outcome) %>%
      right_join(teams, by = c("sim", "team")) %>%
      mutate(exit = ifelse(!is.na(outcome), week_num, exit)) %>%
      select(-outcome)

    # if super bowl, record winner
    if (any(playoff_teams$conf == "SB")) {
      # super bowl winner exit is +1 to SB week
      teams <- games %>%
        filter(week == week_num) %>%
        double_games() %>%
        filter(outcome == 1) %>%
        select(sim, team, outcome) %>%
        right_join(teams, by = c("sim", "team")) %>%
        mutate(exit = ifelse(!is.na(outcome), week_num + 1, exit)) %>%
        select(-outcome)
    }

    # filter to winners or byes
    playoff_teams <- games %>%
      filter(week == week_num) %>%
      double_games() %>%
      right_join(playoff_teams, by = c("sim", "team")) %>%
      filter(is.na(result) | result > 0) %>%
      select(sim, conf, seed, team) %>%
      arrange(sim, conf, seed)

    # update number of teams
    num_teams <- playoff_teams %>%
      group_by(sim, conf) %>%
      summarize(count = n()) %>%
      pull(count) %>%
      max()

    # if at one team per conf, loop once more for the super bowl
    if (num_teams == 1 && !any(playoff_teams$conf == "SB")) {
      playoff_teams <- playoff_teams %>%
        mutate(conf = "SB", seed = rep(1:2, n() / 2))
      num_teams <- 2
    }
  } # end playoff loop

  #### DRAFT ORDER ####

  teams <- standings_and_h2h %>%
    compute_draft_order(
      games = games,
      h2h = h2h_df,
      tiebreaker_depth = tiebreaker_depth,
      .debug = .debug
    )

  p(sprintf("finished sim round %g", sim_round))

  list("teams" = teams, "games" = games)
}
