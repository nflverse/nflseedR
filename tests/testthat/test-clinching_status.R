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
      seed1 = sum(seed1_status == "clinched"),
      seeds = seeds[1]
    ),
    by = c("season", "week", "conf")
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
    differences[!is.na(nfl) & (is.na(ours) | ours > nfl)]$ours_in_nfl_week,
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

test_that("clinching status rejects invalid goals", {
  games <- load_games_2006_2025()[season == 2023]
  expect_snapshot(clinching_status_compute(games, status = "WC"), error = TRUE)
})
