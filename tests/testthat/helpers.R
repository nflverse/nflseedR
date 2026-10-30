load_test_games <- function() {
  g <- nflreadr::load_schedules(2014:2019)

  if (!nrow(g) > 0) {
    return(tibble::tibble())
  }

  g |>
    dplyr::select(sim = season, game_type, week, away_team, home_team, result)
}

load_games_2006_2025 <- function() {
  data.table::as.data.table(readRDS(test_path("games_2006_2025.rds")))
}

load_nfl_standings_2006_2025 <- function() {
  data.table::as.data.table(readRDS(test_path("nfl_standings_2006_2025.rds")))
}

# Regular season games through `week` of `season` for every row of `weeks`, one
# sim per season and week (sim = season * 100 + week)
weekly_games <- function(games, weeks) {
  games <- games[game_type == "REG"]
  out <- weeks[,
    games[season == .BY$s & week <= .BY$wk],
    by = list(s = season, wk = week)
  ]
  out[, sim := s * 100L + wk]
  out[, c("s", "wk", "season") := NULL]
  out[]
}

load_deep_tiebreaker_games <- function() {
  data.table::as.data.table(readRDS(test_path("deep_tiebreaker_games.rds")))
}

# Write standings sorted by sim and team so snapshot diffs show changed teams
# instead of reordered rows
standings_csv <- function(standings) {
  path <- tempfile(fileext = ".csv")
  standings <- data.table::copy(standings)
  data.table::setorderv(standings, c(names(standings)[1], "team"))
  data.table::fwrite(standings, path)
  path
}

# Other test files can leave a different RNG kind behind (e.g. L'Ecuyer-CMRG
# from future), so pin the kinds to R's defaults to keep coin tosses stable
with_reference_seed <- function(seed, code) {
  withr::with_seed(
    seed,
    code,
    .rng_kind = "Mersenne-Twister",
    .rng_normal_kind = "Inversion",
    .rng_sample_kind = "Rejection"
  )
}
