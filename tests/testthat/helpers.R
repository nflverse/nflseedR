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

load_nfl_clinch_2006_2025 <- function() {
  data.table::as.data.table(readRDS(test_path("nfl_clinch_2006_2025.rds")))
}

# Clinching status after every week of 2006-2025 (results of later weeks set
# to NA), with the final ranks of the official standings. Cached, because
# several tests use it.
the_tests <- new.env(parent = emptyenv())
clinching_status_weekly <- function() {
  the_tests$weekly <- the_tests$weekly %||% clinching_status_weekly_build()
  data.table::copy(the_tests$weekly)
}

clinching_status_weekly_build <- function() {
  games <- load_games_2006_2025()[game_type == "REG"]
  weeks <- unique(games[, list(season, week)])
  status <- data.table::rbindlist(lapply(seq_len(nrow(weeks)), function(i) {
    g <- games[season == weeks$season[i]]
    g[week > weeks$week[i], result := NA]
    # verify: every clinch or elimination that needed tie proofs is checked
    # against the engine
    clinching_status_compute(g, verify = TRUE)[, week := weeks$week[i]]
  }))
  nfl <- load_nfl_standings_2006_2025()
  final <- nfl[,
    .SD[week == max(week), list(team, div_rank, conf_rank)],
    by = season
  ]
  out <- merge(status, final, by = c("season", "team"))
  out[, seeds := clinching_status_seeds(season, TRUE)]
  data.table::setorderv(out, c("season", "team", "week"))
  out[]
}
