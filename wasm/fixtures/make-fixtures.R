# Fixtures for the Node tests of the wasm build (wasm/test): input games and
# the expected output of clinching_status_compute(), the table behind
# nfl_clinching_status(), as JSON.
#
# Usage (from the package root, with nflseedR installed or loaded):
#   Rscript wasm/fixtures/make-fixtures.R [out_dir]   (default: wasm/fixtures/json)
args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) > 0) args[[1]] else "wasm/fixtures/json"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

if (!"nflseedR" %in% loadedNamespaces()) {
  if (
    requireNamespace("pkgload", quietly = TRUE) && file.exists("DESCRIPTION")
  ) {
    pkgload::load_all(quiet = TRUE)
  } else {
    loadNamespace("nflseedR")
  }
}
`%||%` <- function(x, y) if (is.null(x)) y else x
compute <- get("clinching_status_compute", asNamespace("nflseedR"))
all_games <- readRDS("tests/testthat/games_2006_2025.rds")

# Games of `seasons`, with results after `last_week` of each season set to NA
cut_off <- function(seasons, last_week) {
  games <- all_games[all_games$season %in% seasons, ]
  last <- last_week[match(games$season, seasons)]
  games$result[games$week > last] <- NA
  games
}

fixtures <- list(
  list(name = "2024-week-15", games = cut_off(2024, 15)),
  # Ties in weeks 1 and 13
  list(name = "2022-week-14-ties", games = cut_off(2022, 14)),
  # 6 seeds, ties in weeks 7 and 8
  list(name = "2016-week-14-6-seeds", games = cut_off(2016, 14)),
  # 3 statuses undecided without the search, 1 with it
  list(name = "2023-week-15", games = cut_off(2023, 15)),
  list(name = "2023-week-15-search", games = cut_off(2023, 15), search = TRUE),
  list(
    name = "2021-week-17-scenarios",
    games = cut_off(2021, 17),
    scenarios = TRUE
  ),
  # Two seasons, one of them complete with playoff games, and only two goals
  list(
    name = "2018-2019-two-seasons",
    games = cut_off(2018:2019, c(15, 99)),
    status = c("PLAYOFFS", "1SEED")
  )
)

for (f in fixtures) {
  options <- list(
    # I(): stays an array in JSON even with one goal
    status = I(f$status %||% c("DIV", "PLAYOFFS", "1SEED")),
    scenarios = f$scenarios %||% FALSE,
    search = f$search %||% FALSE
  )
  status <- compute(
    f$games,
    status = options$status,
    scenarios = options$scenarios,
    search = options$search
  )
  scenarios <- attr(status, "scenarios")
  json <- list(
    name = f$name,
    nflseedr_version = as.character(utils::packageVersion("nflseedR")),
    options = options,
    games = f$games,
    expected = list(
      status = as.data.frame(status),
      scenarios = if (is.null(scenarios)) NULL else as.data.frame(scenarios)
    )
  )
  path <- file.path(out_dir, paste0(f$name, ".json"))
  jsonlite::write_json(
    json,
    path,
    auto_unbox = TRUE,
    na = "null",
    null = "null",
    digits = NA
  )
  cat(
    path,
    ":",
    nrow(status),
    "status rows,",
    NROW(scenarios),
    "scenario rows\n"
  )
}
