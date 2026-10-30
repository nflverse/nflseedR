test_that("standings of 2006-2025 are stable", {
  standings <- with_reference_seed(
    20261030,
    nfl_standings(load_games_2006_2025(), ranks = "DRAFT", verbosity = "NONE")
  )
  expect_snapshot_file(standings_csv(standings), "games_2006_2025.csv")
})

# Official NFL standings of every week (built by
# data-raw/build_nfl_standings_2006_2025.R). The snapshot lists every team
# whose rank differs from the NFL.
test_that("weekly standings 2006-2025 match the NFL", {
  nfl <- load_nfl_standings_2006_2025()
  games <- weekly_games(
    load_games_2006_2025(),
    unique(nfl[, list(season, week)])
  )
  standings <- with_reference_seed(
    20261030,
    nfl_standings(
      games,
      ranks = "CONF",
      tiebreaker_depth = "POINTS",
      verbosity = "NONE"
    )
  )
  standings[, `:=`(season = sim %/% 100L, week = sim %% 100L)]
  m <- merge(
    standings,
    nfl,
    by = c("season", "week", "team"),
    suffixes = c("", "_nfl")
  )
  # The NFL also lists teams without games (2017 week 1)
  expect_identical(nrow(m), nrow(standings))
  expect_identical(m$true_wins, m$wins_nfl)
  expect_identical(m$losses, m$losses_nfl)
  expect_identical(m$ties, m$ties_nfl)

  # Ranks are missing until every team has played (2017 week 1)
  differences <- m[
    !is.na(div_rank) & (div_rank != div_rank_nfl | conf_rank != conf_rank_nfl),
    list(
      season,
      week,
      team,
      conf,
      division,
      record = paste(true_wins, losses, ties, sep = "-"),
      div_rank,
      div_rank_nfl,
      conf_rank,
      conf_rank_nfl,
      div_tie_broken_by,
      conf_tie_broken_by
    )
  ]
  path <- tempfile(fileext = ".csv")
  data.table::fwrite(differences, path)
  expect_snapshot_file(path, "nfl_weekly_differences.csv")
})

test_that("standings of a partly simulated season are stable", {
  games <- data.table::as.data.table(nflseedR::sims_games_example)
  standings <- with_reference_seed(
    20261030,
    nfl_standings(games[!is.na(result)], ranks = "DRAFT", verbosity = "NONE")
  )
  expect_snapshot_file(standings_csv(standings), "sims_games_example.csv")
})

# Synthetic tie-heavy seasons that reach every tiebreaker step (built by
# data-raw/build_deep_tiebreaker_games.R)
test_that("deep tiebreakers are stable", {
  games <- load_deep_tiebreaker_games()
  for (depth in c("POINTS", "SOS", "PRE-SOV", "RANDOM")) {
    standings <- with_reference_seed(
      20261030,
      nfl_standings(
        games,
        ranks = "DRAFT",
        tiebreaker_depth = depth,
        verbosity = "NONE"
      )
    )
    expect_snapshot_file(
      standings_csv(standings),
      paste0("deep_tiebreakers_", tolower(depth), ".csv")
    )
  }
})

test_that("deep tiebreaker games reach every tiebreaker step", {
  standings <- with_reference_seed(
    20261030,
    nfl_standings(
      load_deep_tiebreaker_games(),
      ranks = "DRAFT",
      tiebreaker_depth = "POINTS",
      verbosity = "NONE"
    )
  )
  steps <- function(x) unique(sub(" \\(\\d\\)$", "", stats::na.omit(x)))
  expect_setequal(
    steps(standings$div_tie_broken_by),
    c(
      "Head-To-Head Win PCT",
      "Division Win PCT",
      "Common Games Win PCT",
      "Conference Win PCT",
      "SOV",
      "SOS",
      "Conference Points Rank",
      "League Points Rank",
      "Common Games Point Differential",
      "Point Differential",
      "Coin Toss"
    )
  )
  expect_setequal(
    steps(standings$conf_tie_broken_by),
    c(
      "Division Tiebreaker",
      "Head-To-Head Sweep",
      "Conference Win PCT",
      "Common Games Win PCT",
      "SOV",
      "SOS",
      "Conference Points Rank",
      "League Points Rank",
      "Conference Point Differential",
      "League Point Differential",
      "Coin Toss"
    )
  )
  expect_setequal(
    steps(standings$draft_tie_broken_by),
    c(
      "Division Tiebreaker",
      "Conference Tiebreaker",
      "Head-To-Head",
      "Common Games Win PCT",
      "SOV",
      "League Points Rank",
      "Point Differential",
      "Coin Toss"
    )
  )
})

test_that("ranks and playoff_seeds return parts of the full standings", {
  # Real seasons without coin tosses, so the RNG doesn't matter
  games <- load_games_2006_2025()
  standings <- function(...) {
    nfl_standings(
      data.table::copy(games),
      ...,
      tiebreaker_depth = "POINTS",
      verbosity = "NONE"
    )
  }
  draft <- standings(ranks = "DRAFT")
  expect_identical(
    standings(ranks = "CONF"),
    draft[, season:conf_tie_broken_by]
  )
  expect_identical(
    standings(ranks = "DIV"),
    draft[, season:div_tie_broken_by]
  )
  expect_identical(
    standings(ranks = "NONE")[order(season, team)],
    draft[order(season, team), season:sos]
  )
  seeds_7 <- standings(ranks = "CONF", playoff_seeds = 7)
  expect_identical(
    seeds_7$conf_rank,
    data.table::fifelse(draft$conf_rank <= 7L, draft$conf_rank, NA_integer_)
  )
  # Draft ranks need all conference ranks
  expect_identical(standings(ranks = "DRAFT", playoff_seeds = 7), draft)
})

test_that("net wins only break ties during the regular season (D13)", {
  # 2022 schedule with the cancelled BUF-CIN game. Home teams win every game,
  # except that CIN and HOU (who don't meet) lose all of theirs
  games <- data.table::as.data.table(nflseedR::sims_games_example)[
    game_type == "REG",
    list(season, game_type, week, away_team, home_team)
  ]
  games[, result := 3L]
  games[home_team %in% c("CIN", "HOU"), result := -3L]
  ranks <- function(last_week) {
    nfl_standings(games[week <= last_week], verbosity = "NONE")[
      team %in% c("CIN", "HOU")
    ][order(conf_rank)]
  }

  # After week 17, CIN (0-15) is ahead of HOU (0-16) by net wins, no tie
  during <- ranks(17)
  expect_identical(during$team, c("CIN", "HOU"))
  expect_identical(during$conf_tie_broken_by, c(NA_character_, NA_character_))

  # After the final week, CIN (0-16) and HOU (0-17) are tied at 0%, so a
  # tiebreaker decides
  after <- ranks(18)
  expect_identical(after$win_pct, c(0, 0))
  expect_identical(after$conf_tie_broken_by, c("SOS (2)", NA))
})

test_that("regular seasons are complete after their final week", {
  games <- load_games_2006_2025()[game_type == "REG"]
  status <- function(g) {
    g <- standings_validate_games(data.table::copy(g))
    standings_season_complete(g, sort(unique(g$sim)))
  }
  expect_identical(status(games[season == 2019 & week <= 17]), TRUE)
  expect_identical(status(games[season == 2021 & week <= 17]), FALSE)
  expect_identical(status(games[season == 2021]), TRUE)
  # Without the year, 18 weeks are assumed
  sims <- games[season == 2019][, `:=`(sim = 1L, season = NULL)]
  expect_identical(status(sims), FALSE)
  expect_identical(
    status(rbind(sims, sims[1L][, `:=`(week = 18L, game_type = "WC")])),
    TRUE
  )
})

test_that("nfl_standings() works with nflreadr schedules", {
  # nflreadr returns tibbles that also inherit from data.table
  games <- load_games_2006_2025()[season == 2025]
  class(games) <- c(
    "nflverse_data",
    "tbl_df",
    "tbl",
    "data.table",
    "data.frame"
  )
  standings <- nfl_standings(games, ranks = "DIV", verbosity = "NONE")
  expect_identical(nrow(standings), 32L)
})

test_that("ranks are missing until every team has played", {
  games <- load_deep_tiebreaker_games()[sim == 1L & week == 1L][1:6]
  standings <- nfl_standings(games, ranks = "DRAFT", verbosity = "NONE")
  expect_identical(nrow(standings), 12L)
  expect_all_true(is.na(standings$div_rank))
  expect_all_true(is.na(standings$conf_rank))
  expect_all_true(is.na(standings$draft_rank))
})

test_that("large draft ties get valid draft ranks", {
  games <- data.table::as.data.table(
    readRDS(test_path("draft_tiebreaker_loop_games.rds"))
  )
  standings <- nfl_standings(games, ranks = "DRAFT", verbosity = "NONE")
  expect_identical(sort(standings$draft_rank), 1:32)
})

test_that("nfl_standings() checks its input", {
  games <- load_deep_tiebreaker_games()[sim == 1L]
  games[1L, away_team := "OAK"]
  expect_snapshot(nfl_standings(games, verbosity = "NONE"), error = TRUE)

  games <- load_deep_tiebreaker_games()[sim == 1L]
  games[, c("away_score", "home_score") := NULL]
  expect_snapshot(
    nfl_standings(games, tiebreaker_depth = "POINTS", verbosity = "NONE"),
    error = TRUE
  )

  games <- load_deep_tiebreaker_games()[sim == 1L]
  games[, season := 2022L]
  expect_warning(
    nfl_standings(games, ranks = "NONE", verbosity = "NONE"),
    regexp = 'The `games` argument includes both "sim" and "season".'
  )
})
