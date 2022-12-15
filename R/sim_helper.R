simulate_round <- function(sim_round,
                           sim_rounds,
                           sims_per_round,
                           schedule,
                           simulations,
                           weeks_to_sim,
                           process_games,
                           ...,
                           tiebreaker_depth,
                           test_week,
                           .debug,
                           playoff_seeds,
                           p,
                           sim_include) {

  # iteration sims
  iter_sims <- sims_per_round * (sim_round - 1) + seq_len(sims_per_round)
  iter_sims <- iter_sims[iter_sims <= simulations]
  iter_sims_num <- length(iter_sims)

  # games have copies per sim
  sched_rows <- nrow(schedule)
  games <- schedule[rep(seq_len(sched_rows), each = iter_sims_num), ] %>%
    mutate(sim = rep(iter_sims, sched_rows)) %>%
    select(sim, everything())

  # teams starts as divisions data
  teams <- nflseedR::divisions %>%
    filter(team %in% schedule$away_team | team %in% schedule$home_team)
  teams <- teams[rep(seq_len(nrow(teams)), iter_sims_num), ] %>%
    mutate(sim = rep(iter_sims, each = nrow(teams))) %>%
    select(sim, everything())

  # playoff seeds bounds checking
  max_seeds <- teams %>%
    group_by(sim, conf) %>%
    summarize(count=n()) %>%
    ungroup() %>%
    pull(count) %>%
    min()
  if (playoff_seeds < 1 || playoff_seeds > max_seeds) {
    stop("`playoff_seeds` must be between 1 and ",max_seeds)
  }

  # simulate remaining regular season games
  for (week_num in weeks_to_sim)
  {
    return_value <-
      simulate_week(teams = teams,
                    games = games,
                    week_num = week_num,
                    process_games = process_games,
                    test_week = test_week,
                    .debug = .debug,
                    ...)
    if (!is.null(test_week) && week_num == test_week) {
      return(return_value)
    }
    list[teams, games] <- return_value
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

  teams <- teams %>%
    inner_join(standings_and_h2h$standings,
      by = intersect(colnames(teams), colnames(standings_and_h2h$standings))
    )
  h2h_df <- standings_and_h2h$h2h

  #### PLAYOFFS ####
  if (sim_include != "REG"){# sim_include allows us to skip playoff simulation

    # week tracker
    week_num <- games %>%
      filter(game_type == "REG") %>%
      pull(week) %>%
      max()

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

    # bye count (per conference)
    num_byes <- 2^ceiling(log(num_teams, 2)) - num_teams

    # first playoff week
    first_playoff_week <- week_num + 1

    # final week of season (Super Bowl week)
    week_max <- week_num +
      ceiling(log(num_teams * length(unique(playoff_teams$conf)), 2))

    # playoff weeks
    for (week_num in first_playoff_week:week_max) {
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
          rename(away_team = team.x, home_team = team.y) %>%
          mutate(
            week = week_num,
            game_type = case_when(
              week_max - week_num == 3 ~ "WC",
              week_max - week_num == 2 ~ "DIV",
              week_max - week_num == 1 ~ "CON",
              week_max - week_num == 0 ~ "SB",
              TRUE ~ "POST"
            ),
            away_rest = case_when(
              conf == "SB" ~ 14,
              week_num == first_playoff_week + 1 & seed.x <= num_byes ~ 14,
              TRUE ~ 7
            ),
            home_rest = case_when(
              conf == "SB" ~ 14,
              week_num == first_playoff_week + 1 & seed.y <= num_byes ~ 14,
              TRUE ~ 7
            ),
            location = ifelse(conf == "SB", "Neutral", "Home")
          ) %>%
          select(-conf, -seed.x, -seed.y, -round_rank.x, -round_rank.y)

        # add to games
        games <- bind_rows(games, add_games)
      }

      # process any new games
      return_value <-
        simulate_week(teams = teams,
                      games = games,
                      week_num = week_num,
                      process_games = process_games,
                      test_week = test_week,
                      .debug = .debug,
                      ...)
      if (!is.null(test_week) && week_num == test_week) {
        return(return_value)
      }
      list[teams, games] <- return_value

      week_games_doubled <- games %>%
        filter(week == week_num) %>%
        double_games()

      # record losers
      teams <- week_games_doubled %>%
        filter(outcome == 0) %>%
        select(sim, team, outcome) %>%
        right_join(teams, by = c("sim", "team")) %>%
        mutate(exit = ifelse(!is.na(outcome), week_num, exit)) %>%
        select(-outcome)

      # if super bowl, record winner
      if (any(playoff_teams$conf == "SB")) {
        # super bowl winner exit is +1 to SB week
        teams <- week_games_doubled %>%
          filter(outcome == 1) %>%
          select(sim, team, outcome) %>%
          right_join(teams, by = c("sim", "team")) %>%
          mutate(exit = ifelse(!is.na(outcome), week_num + 1, exit)) %>%
          select(-outcome)
      }

      # filter to winners or byes
      playoff_teams <- week_games_doubled %>%
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
          mutate(conf = "SB", seed = 1)
        num_teams <- 2
      }
    } # end playoff loop
  }

  #### DRAFT ORDER ####
  if (sim_include == "DRAFT"){
    teams <- standings_and_h2h %>%
      compute_draft_order(
        games = games,
        h2h = h2h_df,
        tiebreaker_depth = tiebreaker_depth,
        .debug = .debug
      )
  } else {
    if (!is_tibble(teams)) teams <- teams$standings
    teams$draft_order <- NA_real_
    teams <- tibble::as_tibble(teams) %>%
      dplyr::select(
        dplyr::any_of(c(
          "sim", "team", "conf", "division", "games",
          "wins", "true_wins", "losses", "ties", "win_pct", "div_pct",
          "conf_pct", "sov", "sos", "div_rank", "seed", "exit", "draft_order"
        ))
      )
  }

  p(sprintf("finished sim round %g", sim_round))

  list("teams" = teams, "games" = games)
}
