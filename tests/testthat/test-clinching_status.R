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
  # Later than the NFL only where we can't decide yet. Earlier only for the
  # cancelled 2022 BUF-CIN game, which is missing from the remaining games.
  expect_all_equal(
    differences[!is.na(nfl) & (is.na(ours) | ours > nfl), ours_in_nfl_week],
    "undecided"
  )
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
    nfl_clinching_status(games, "DIV", verbosity = "NONE")
    nfl_clinching_status(games, verbosity = "ALL")
  })
})

test_that("nfl_clinching_status() reports by verbosity", {
  games <- load_games_2006_2025()[season == 2023]
  expect_silent(nfl_clinching_status(games, verbosity = "NONE"))
  messages <- testthat::capture_messages(
    nfl_clinching_status(games, status = "DIV", verbosity = "MAX")
  )
  expect_length(messages, 2L)
  expect_match(messages[1], "Compute clinching status")
  expect_match(
    messages[2],
    "div: clinched x 8, alive x 0, eliminated x 24, undecided x 0"
  )
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
        clinching_status_compute(g, scenarios = TRUE),
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
        status <- clinching_status_compute(g)
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
