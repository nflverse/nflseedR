test_that("clinched and eliminated teams really reached or missed the goal", {
  s <- clinching_status_weekly()
  expect_identical(
    s[div_status == "clinched", unique(div_rank)],
    1L
  )
  expect_false(1L %in% s[div_status == "eliminated", div_rank])
  expect_all_true(s[playoffs_status == "clinched", conf_rank <= seeds])
  expect_all_true(s[playoffs_status == "eliminated", conf_rank > seeds])
  expect_identical(s[seed1_status == "clinched", unique(conf_rank)], 1L)
  expect_false(1L %in% s[seed1_status == "eliminated", conf_rank])
})

test_that("clinching status follows the final standings once no games are left", {
  s <- clinching_status_weekly()
  final <- s[s[, .I[week == max(week)], by = "season"]$V1]
  expect_all_equal(final$games_left, 0L)
  expect_identical(final$div_status == "clinched", final$div_rank == 1L)
  expect_identical(
    final$playoffs_status == "clinched",
    final$conf_rank <= final$seeds
  )
  expect_identical(final$seed1_status == "clinched", final$conf_rank == 1L)
  expect_false(any(
    c(final$div_status, final$playoffs_status, final$seed1_status) ==
      "undecided"
  ))
})

test_that("clinching status is undecided when only a coin toss decides", {
  # A season of one week has no games left and lots of coin tosses
  games <- load_games_2006_2025()[season == 2023 & week == 1]
  s <- clinching_status_compute(games)
  status <- c(s$div_status, s$playoffs_status, s$seed1_status)
  expect_false("alive" %in% status)
  expect_contains(status, c("clinched", "eliminated", "undecided"))
})

test_that("clinching status follows the invariants", {
  s <- clinching_status_weekly()
  decided <- c("clinched", "eliminated")
  for (col in c("div_status", "playoffs_status", "seed1_status")) {
    changed <- s[,
      list(
        n = sum(shift(.SD[[1]]) %chin% decided & shift(.SD[[1]]) != .SD[[1]])
      ),
      by = c("season", "team"),
      .SDcols = col
    ]
    expect_all_equal(changed$n, 0L)
  }
  expect_all_true(s[div_status == "clinched", playoffs_status == "clinched"])
  expect_all_true(s[seed1_status == "clinched", div_status == "clinched"])
  expect_all_true(
    s[playoffs_status == "eliminated", div_status == "eliminated"]
  )
  expect_all_true(
    s[playoffs_status == "eliminated", seed1_status == "eliminated"]
  )
  counts <- s[,
    list(
      playoffs = sum(playoffs_status == "clinched"),
      seed1 = sum(seed1_status == "clinched")
    ),
    by = c("season", "week", "conf", "seeds")
  ]
  expect_all_true(counts$playoffs <= counts$seeds)
  expect_all_true(counts$seed1 <= 1L)
  div <- s[,
    list(n = sum(div_status == "clinched")),
    by = c("season", "week", "division")
  ]
  expect_all_true(div$n <= 1L)
})

test_that("first clinching and elimination weeks match the NFL", {
  s <- clinching_status_weekly()
  m <- merge(s, load_nfl_clinch_2006_2025(), by = c("season", "week", "team"))
  expect_identical(nrow(m), nrow(s))
  goals <- list(
    div = c("div_status", "clinched", "clinched_division"),
    playoffs = c("playoffs_status", "clinched", "clinched_playoff"),
    eliminated = c("playoffs_status", "eliminated", "clinched_eliminated"),
    seed1 = c("seed1_status", "clinched", "clinched_home_field")
  )
  first_week <- function(week, flag) {
    if (any(flag)) min(week[flag]) else NA_integer_
  }
  differences <- data.table::rbindlist(lapply(names(goals), function(goal) {
    g <- goals[[goal]]
    m[,
      list(
        goal = goal,
        ours = first_week(week, .SD[[1]] == g[2]),
        nfl = first_week(week, .SD[[2]]),
        # Our status in the NFL's first week
        ours_in_nfl_week = .SD[[1]][match(first_week(week, .SD[[2]]), week)]
      ),
      by = c("season", "team"),
      .SDcols = g[c(1, 3)]
    ]
  }))
  differences <- differences[
    !(is.na(ours) & is.na(nfl)) & (is.na(ours) | is.na(nfl) | ours != nfl)
  ]
  # Never later than the NFL. Earlier only for the cancelled 2022 BUF-CIN
  # game, which is missing from the remaining games.
  later <- differences[!is.na(nfl) & (is.na(ours) | ours > nfl)]
  expect_identical(nrow(later), 0L)
  earlier <- differences[!is.na(ours) & (is.na(nfl) | ours < nfl)]
  expect_setequal(earlier$team, c("BUF", "CIN"))
  expect_all_equal(earlier$season, 2022L)

  data.table::setorderv(differences, c("season", "team", "goal"))
  path <- tempfile(fileext = ".csv")
  data.table::fwrite(differences, path)
  expect_snapshot_file(path, "nfl_status_differences.csv")
})

test_that("clinching sees past a third team in a division tie", {
  # SEA can end 11-5 with SF and ARI: SF wins the division, SEA beats ARI
  games <- load_games_2006_2025()[season == 2013]
  games[week > 13, result := NA]
  s <- clinching_status_compute(games, status = "PLAYOFFS", verify = TRUE)
  expect_identical(s[team == "SEA", playoffs_status], "clinched")
})

test_that("division winners don't break wild card ties", {
  # SEA beats ATL at 9-7; WAS can end 9-7 too, but then wins its division
  games <- load_games_2006_2025()[season == 2015]
  games[week > 16, result := NA]
  s <- clinching_status_compute(games, status = "PLAYOFFS", verify = TRUE)
  expect_identical(s[team == "SEA", playoffs_status], "clinched")
})

test_that("clinching compares SOV and SOS of two tied teams directly", {
  # LA and SEA can end 10-7, LA has the better SOV whatever week 18 does
  s <- clinching_status_after(2024, 17, "DIV")
  expect_identical(
    s[team %chin% c("LA", "SEA"), div_status],
    c("clinched", "eliminated")
  )
})

test_that("clinching compares SOV of a tied team with games left", {
  # NE and NYJ can end 11-5, NE has the better SOV however NYJ gets there
  s <- clinching_status_after(2015, 14, "DIV")
  expect_identical(s[team == "NE", div_status], "clinched")
})

test_that("X only needs to beat one team of a division for a wild card", {
  # CAR loses to ARI but beats SF, and only one of them can take a wild card
  s <- clinching_status_after(2013, 16, "PLAYOFFS")
  expect_identical(s[team == "CAR", playoffs_status], "clinched")
})

test_that("wild card ties reduce divisions again after every pick", {
  # NYG ranks ahead of DAL, but after NYG's pick DAL beats ATL
  s <- clinching_status_after(2006, 15, "PLAYOFFS")
  expect_identical(s[team == "DAL", playoffs_status], "clinched")
})

test_that("teams that win every division tie never take a wild card", {
  # SF can end 10-7 with DAL, but then wins the NFC West over SEA
  s <- clinching_status_after(2022, 15, "PLAYOFFS")
  expect_identical(s[team == "DAL", playoffs_status], "clinched")
})

test_that("elimination sees which team represents a division", {
  # CIN only ends 8-8 by losing to PIT, which then ranks ahead of CIN
  s <- clinching_status_after(2012, 15, "PLAYOFFS")
  expect_identical(s[team == "NYJ", playoffs_status], "eliminated")
})

test_that("a tiebreaking step only counts where the earlier ones are equal", {
  # CAR can end 7-10 with CHI: CAR is never behind on common games and has
  # the better SOV whenever they are equal there
  s <- clinching_status_after(2021, 15, "PLAYOFFS")
  expect_identical(s[team == "CHI", playoffs_status], "eliminated")
})

test_that("a rival only takes a wild card if another rival can win", {
  # NO, ATL and CHI can end 10-6. ATL only takes a wild card if NO wins the
  # NFC South, and DAL only ends 10-6 by beating NYG, then wins its division.
  s <- clinching_status_after(2011, 14, "PLAYOFFS")
  expect_identical(s[team == "NO", playoffs_status], "clinched")
})

test_that("a division winner level with X counts as ahead of X", {
  # DAL can end 10-7 behind PHI on SOV. DAL beats NO, but NO can win the NFC
  # South at 10-7 while TB, which beat DAL, takes a wild card. The search
  # finds that completion.
  s <- clinching_status_after(2021, 15, "PLAYOFFS")
  expect_identical(s[team == "DAL", playoffs_status], "undecided")
  games <- load_games_2006_2025()[season == 2021]
  games[week > 15, result := NA]
  s <- clinching_status_compute(games, status = "PLAYOFFS", search = TRUE)
  expect_identical(s[team == "DAL", playoffs_status], "alive")
})

test_that("X is never last of a group of tied teams", {
  # SEA, ATL and MIN can all end 9-7 for two wild cards. SEA beats ATL in
  # some of these ties and MIN in the others, but is never behind both.
  s <- clinching_status_after(2015, 15, "PLAYOFFS")
  expect_identical(s[team == "SEA", playoffs_status], "clinched")
  # TB (8-8 at best) can beat some of the teams that end 8-8, never enough
  s <- clinching_status_after(2012, 15, "PLAYOFFS")
  expect_identical(s[team == "TB", playoffs_status], "eliminated")
})

test_that("a team level with X can be ahead only as its division's winner", {
  # GB can win the NFC North at 10-7 while MIN, which beat SF, is 10-7 too.
  # GB then takes the title, not a wild card.
  s <- clinching_status_after(2023, 14, "PLAYOFFS")
  expect_identical(s[team == "SF", playoffs_status], "clinched")
})

test_that("tie groups settle the division title and the No. 1 seed", {
  s <- clinching_status_after(2014, 14, c("DIV", "1SEED"))
  expect_identical(s[team == "SF", div_status], "eliminated")
  expect_identical(s[team == "SF", seed1_status], "eliminated")
})

test_that("tie groups know which teams end level with and below X", {
  s <- clinching_status_after(2024, 14, "PLAYOFFS")
  expect_identical(s[team == "PHI", playoffs_status], "clinched")
  s <- clinching_status_after(2008, 11, "DIV")
  expect_identical(s[team == "DET", div_status], "eliminated")
})

test_that("a tied team's other games bound its record against a group", {
  s <- clinching_status_after(2024, 13, "1SEED")
  expect_identical(s[team == "LA", seed1_status], "eliminated")
})

test_that("head-to-head wins of a tied division are shared out", {
  # LAC (8-9 at best) needs a 3-3 division record to win a four-way tie of
  # the AFC West. DEN ends 2-4, so KC or LV ends 4-2.
  games <- games_2026_week_10(
    c(BAL = 3, NYJ = -3, ATL = 3, DET = -3, IND = 0, LV = -3, CLE = 3)
  )
  s <- clinching_status_compute(games, status = "1SEED", verify = TRUE)
  expect_identical(s[team == "LAC", seed1_status], "eliminated")
})

test_that("only a single season gets the large budget for tie groups", {
  games <- games_2026_week_10(
    c(BAL = 3, NYJ = -3, ATL = 3, DET = -3, IND = 0, LV = -3, CLE = 3)
  )
  games[, season := NULL]
  games <- rbind(games[, sim := 1L], data.table::copy(games)[, sim := 2L])
  s <- clinching_status_compute(games, status = "1SEED")
  expect_identical(
    s[team == "LAC", seed1_status],
    c("undecided", "undecided")
  )
})

test_that("clinches and eliminations hold in completions of every tie group", {
  games <- load_games_2006_2025()[season == 2021]
  games[week > 15, result := NA]
  plain <- clinching_status_compute(games)
  with_reference_seed(20261113, {
    checked <- clinching_status_compute(games, group_samples = 4L)
  })
  expect_identical(
    attr(checked, "group_check"),
    c(complete = 34L, incomplete = 0L)
  )
  expect_equal(checked, plain, ignore_attr = "group_check")
})

test_that("the search settles answers the proofs leave open", {
  # DEN is out of the AFC West race after week 13, which the proofs only
  # show once the search fixes a few games
  games <- load_games_2006_2025()[season == 2017]
  games[week > 13, result := NA]
  s <- clinching_status_compute(
    games,
    status = "DIV",
    search = TRUE,
    verify = TRUE
  )
  expect_identical(s[team == "DEN", div_status], "eliminated")
})

test_that("search = FALSE (default) leaves open answers undecided", {
  s <- clinching_status_after(2017, 13, "DIV")
  expect_identical(s[team == "DEN", div_status], "undecided")
})

test_that("nfl_clinching_status() hints at the search", {
  games <- load_games_2006_2025()[season == 2015]
  games[week > 14, result := NA]
  s <- clinching_status_compute(games, search = FALSE)
  expect_snapshot(clinching_status_search_hint(s))
  # No games left: nothing for the search to do
  finished <- load_games_2006_2025()[season == 2015]
  expect_silent(
    clinching_status_search_hint(clinching_status_compute(finished))
  )
  expect_silent(
    nfl_clinching_status(games, search = FALSE, verbosity = "NONE")
  )
})

test_that("clinching status only computes the requested goals", {
  games <- load_games_2006_2025()[season == 2023]
  games[week > 15, result := NA]
  s <- clinching_status_compute(games, status = "PLAYOFFS")
  expect_named(
    s,
    c(
      "season",
      "team",
      "conf",
      "division",
      "wins",
      "losses",
      "ties",
      "games_left",
      "playoffs_status"
    )
  )
  expect_identical(nrow(s), 32L)
  expect_identical(
    s$playoffs_status,
    clinching_status_compute(games)$playoffs_status
  )
})

test_that("clinching status uses 6 seeds through 2019 and 7 since 2020", {
  expect_identical(
    clinching_status_seeds(c(2019L, 2020L), uses_season = TRUE),
    c(6L, 7L)
  )
  expect_identical(clinching_status_seeds(1:2, uses_season = FALSE), c(7L, 7L))
})

test_that("nfl_clinching_status() checks its arguments", {
  games <- load_games_2006_2025()[season == 2023]
  expect_snapshot(error = TRUE, {
    nfl_clinching_status(games, status = "WC", verbosity = "NONE")
    nfl_clinching_status(games, scenarios = NA, verbosity = "NONE")
    nfl_clinching_status(games, search = "no", verbosity = "NONE")
    nfl_clinching_status(games, "DIV", verbosity = "NONE")
    nfl_clinching_status(games, verbosity = "ALL")
  })
})

test_that("nfl_clinching_status() reports by verbosity", {
  games <- load_games_2006_2025()[season == 2023]
  expect_silent(nfl_clinching_status(games, verbosity = "NONE"))
  messages <- testthat::capture_messages(
    nfl_clinching_status(games, status = "DIV", verbosity = "MIN")
  )
  expect_length(messages, 2L)
  expect_match(messages[1], "Compute clinching status")
  expect_match(messages[2], "Compute standings")
  messages <- testthat::capture_messages(
    nfl_clinching_status(games, status = "DIV", verbosity = "MAX")
  )
  expect_gt(length(messages), 3L)
  expect_match(
    messages[length(messages)],
    "div: clinched x 8, alive x 0, eliminated x 24, undecided x 0"
  )
})

test_that("nfl_clinching_status() returns the current standings with status", {
  games <- load_games_2006_2025()[season %in% 2022:2023]
  games[week > 15, result := NA]
  out <- nfl_clinching_status(
    games,
    status = c("DIV", "1SEED"),
    verbosity = "NONE"
  )
  standings <- nfl_standings(games[!is.na(result)], verbosity = "NONE")
  expect_named(
    out,
    c(names(standings), "games_left", "div_status", "seed1_status")
  )
  expect_identical(out[, names(standings), with = FALSE], standings)
  status <- clinching_status_compute(games, status = c("DIV", "1SEED"))
  expect_identical(
    out[, list(season, team, games_left, div_status, seed1_status)],
    status[
      out,
      list(season, team, games_left, div_status, seed1_status),
      on = c("season", "team")
    ]
  )
})

test_that("nfl_clinching_status() only returns teams that have played", {
  games <- load_games_2006_2025()[season %in% 2022:2023]
  games[season == 2023 | week > 1, result := NA]
  out <- nfl_clinching_status(games, verbosity = "NONE")
  expect_identical(unique(out$season), 2022L)
  expect_setequal(
    out$team,
    games[!is.na(result), c(away_team, home_team)]
  )
  games[, result := NA]
  expect_identical(nrow(nfl_clinching_status(games, verbosity = "NONE")), 0L)
})

test_that("nfl_clinching_status() ranks unfinished seasons as unfinished", {
  # Weeks 1-2 and one game of the final week are played. Net wins break ties
  # of equal win percentages only during the season.
  games <- load_games_2006_2025()[season == 2023]
  games[week > 2 & !(week == 18 & home_team == "BAL"), result := NA]
  played <- games[!is.na(result)]
  out <- with_reference_seed(1, nfl_clinching_status(games, verbosity = "NONE"))
  in_season <- with_reference_seed(
    1,
    standings_compute(
      played,
      ranks = "CONF",
      tiebreaker_depth = "SOS",
      playoff_seeds = NULL,
      verbosity = "NONE",
      season_complete = FALSE
    )
  )
  expect_identical(out[, names(in_season), with = FALSE], in_season)
  complete <- with_reference_seed(1, nfl_standings(played, verbosity = "NONE"))
  expect_false(identical(in_season$team, complete$team))
})

test_that("nfl_clinching_status() takes tibbles and leaves them as they are", {
  games <- load_games_2006_2025()[season == 2023]
  games[week > 15, result := NA]
  tbl <- tibble::as_tibble(games)
  before <- data.table::copy(tbl)
  expect_identical(
    nfl_clinching_status(tbl, verbosity = "NONE"),
    nfl_clinching_status(games, verbosity = "NONE")
  )
  expect_identical(tbl, before)
})

test_that("nfl_clinching_status() ignores playoff games not played yet", {
  games <- load_games_2006_2025()[season == 2023]
  games[week > 15, result := NA]
  with_playoffs <- rbind(
    games,
    data.table::data.table(
      season = 2023L,
      week = 19L,
      game_type = "WC",
      away_team = NA_character_,
      home_team = NA_character_,
      result = NA_integer_
    ),
    fill = TRUE
  )
  expect_identical(
    nfl_clinching_status(with_playoffs, verbosity = "NONE"),
    nfl_clinching_status(games, verbosity = "NONE")
  )
})

test_that("scenarios really reach or miss the goal", {
  games <- load_games_2006_2025()[game_type == "REG"]
  checked <- 0L
  for (season_id in c(2012L, 2019L, 2023L)) {
    season_games <- games[season == season_id]
    for (last_week in sort(unique(season_games$week))) {
      g <- data.table::copy(season_games)
      g[week > last_week, result := NA]
      scenarios <- attr(
        clinching_status_compute(g, scenarios = TRUE, search = TRUE),
        "scenarios"
      )
      if (nrow(scenarios) == 0L) {
        next
      }
      # A coin toss in nfl_standings() can go either way
      meta <- unique(scenarios[
        coin_toss == FALSE,
        list(scenario, team, goal, proves)
      ])
      played <- g[
        !is.na(result),
        list(week, game_type, away_team, home_team, result)
      ]
      full <- rbind(
        played[rep(seq_len(.N), nrow(meta))][,
          sim := rep(meta$scenario, each = nrow(played))
        ],
        scenarios[
          scenario %in% meta$scenario,
          list(sim = scenario, week, game_type, away_team, home_team, result)
        ],
        use.names = TRUE
      )
      standings <- nfl_standings(full, verbosity = "NONE")
      m <- merge(
        meta,
        standings[, list(scenario = sim, team, div_rank, conf_rank)],
        by = c("scenario", "team")
      )
      seeds <- clinching_status_seeds(season_id, uses_season = TRUE)
      m[,
        reached := data.table::fcase(
          goal == "DIV"      ,
          div_rank == 1L     ,
          goal == "PLAYOFFS" ,
          conf_rank <= seeds ,
          goal == "1SEED"    ,
          conf_rank == 1L
        )
      ]
      expect_identical(nrow(m), nrow(meta))
      expect_identical(m$reached, m$proves == "reach")
      checked <- checked + nrow(m)
    }
  }
  expect_gt(checked, 5000L)
})

test_that("clinching status returns scenarios in the format of games", {
  games <- load_games_2006_2025()[season == 2023]
  games[week > 16, result := NA]
  out <- nfl_clinching_status(games, scenarios = TRUE, verbosity = "NONE")
  scenarios <- attr(out, "scenarios")
  expect_identical(
    data.table::setattr(data.table::copy(out), "scenarios", NULL),
    nfl_clinching_status(games, verbosity = "NONE")
  )
  expect_named(
    scenarios,
    c(
      "scenario",
      "season",
      "team",
      "goal",
      "proves",
      "coin_toss",
      setdiff(names(games), "season")
    )
  )
  n_left <- games[game_type == "REG" & is.na(result), .N]
  expect_all_equal(scenarios[, .N, by = scenario]$N, n_left)
  expect_in(scenarios$result, c(-1L, 0L, 1L))
  # Alive teams have a scenario for both answers, clinched and eliminated
  # teams only for the possible one
  playoffs <- out[, list(team, status = playoffs_status)]
  proofs <- unique(scenarios[goal == "PLAYOFFS", list(team, proves)])
  reach <- proofs[proves == "reach", team]
  miss <- proofs[proves == "miss", team]
  expect_contains(reach, playoffs[status %in% c("alive", "clinched"), team])
  expect_contains(miss, playoffs[status %in% c("alive", "eliminated"), team])
  expect_false(any(reach %in% playoffs[status == "eliminated", team]))
  expect_false(any(miss %in% playoffs[status == "clinched", team]))
})

test_that("clinched and eliminated teams hold in random completions", {
  games <- load_games_2006_2025()[game_type == "REG"]
  violations <- 0L
  checked <- 0L
  with_reference_seed(20261111, {
    for (season_id in c(2008L, 2014L, 2022L)) {
      for (last_week in 10:16) {
        g <- games[season == season_id]
        g[week > last_week, result := NA]
        status <- clinching_status_compute(g, search = TRUE)
        decided <- data.table::melt(
          status[, list(
            team,
            DIV = div_status,
            PLAYOFFS = playoffs_status,
            `1SEED` = seed1_status
          )],
          id.vars = "team",
          variable.name = "goal",
          value.name = "status"
        )[status %chin% c("clinched", "eliminated")]
        left <- g[is.na(result)]
        played <- g[
          !is.na(result),
          list(week, game_type, away_team, home_team, result)
        ]
        # Random completions, and the worst case of every decided team
        worst <- unique(decided[, list(
          fix = team,
          win = status == "eliminated"
        )])
        specs <- rbind(
          data.table::data.table(fix = rep(NA_character_, 20L), win = NA),
          worst[rep(seq_len(.N), each = 3L)]
        )
        specs[, sim := .I]
        completions <- specs[,
          {
            outcome <- sample(
              c(-1L, 0L, 1L),
              nrow(left),
              replace = TRUE,
              prob = c(0.45, 0.1, 0.45)
            )
            if (!is.na(fix)) {
              outcome[left$home_team == fix] <- if (win) 1L else -1L
              outcome[left$away_team == fix] <- if (win) -1L else 1L
            }
            left[, list(
              week,
              game_type,
              away_team,
              home_team,
              result = outcome
            )]
          },
          by = sim
        ]
        full <- rbind(
          played[rep(seq_len(.N), nrow(specs))][,
            sim := rep(specs$sim, each = nrow(played))
          ],
          completions,
          use.names = TRUE
        )
        standings <- nfl_standings(full, verbosity = "NONE")
        seeds <- clinching_status_seeds(season_id, uses_season = TRUE)
        reached <- data.table::melt(
          standings[, list(
            sim,
            team,
            DIV = div_rank == 1L,
            PLAYOFFS = conf_rank <= seeds,
            `1SEED` = conf_rank == 1L
          )],
          id.vars = c("sim", "team"),
          variable.name = "goal",
          value.name = "reached"
        )
        m <- merge(reached, decided, by = c("team", "goal"))
        violations <- violations +
          m[(status == "clinched") != reached, .N]
        checked <- checked + nrow(m)
      }
    }
  })
  expect_identical(violations, 0L)
  expect_gt(checked, 20000L)
})
