load_test_games <- function() {
  g <- nflreadr::load_schedules(2014:2019)

  if (!nrow(g) > 0) {
    return(tibble::tibble())
  }

  g |>
    dplyr::select(sim = season, game_type, week, away_team, home_team, result)
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
