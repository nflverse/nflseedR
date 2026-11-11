# Benchmark nfl_clinching_status(). Numbers are in dev/playoff-status.md.
#
# The states after every week of 2006-2025 (345 calls, one season each), a
# single late-season state, and many simulated seasons in one call.

# load_all() compiles without optimization, so build an optimized library
pkgbuild::clean_dll()
pkgbuild::compile_dll(debug = FALSE)
devtools::load_all(compile = FALSE)
library(data.table)

games <- as.data.table(readRDS("tests/testthat/games_2006_2025.rds"))[
  game_type == "REG"
]
weeks <- unique(games[, list(season, week)])
states <- lapply(seq_len(nrow(weeks)), function(i) {
  g <- games[season == weeks$season[i]]
  g[week > weeks$week[i], result := NA]
  g
})
late <- states[[which(weeks$season == 2023 & weeks$week == 15)]]

status_of <- function(g, ...) {
  nfl_clinching_status(g, ..., verbosity = "NONE")
}

# 1,000 seasons in one call: the 2023 state after week 12 with the results
# of weeks 1-12 resampled per season
set.seed(20261111)
state_2023 <- states[[which(weeks$season == 2023 & weeks$week == 12)]]
n_sims <- 1000L
many <- state_2023[rep(seq_len(.N), n_sims)]
many[, sim := rep(seq_len(n_sims), each = nrow(state_2023))]
many[!is.na(result), result := sample(c(-3L, 3L), .N, replace = TRUE)]
many[, season := NULL]

results <- list(
  all_weeks = bench::mark(
    for (g in states) {
      status_of(g)
    },
    iterations = 3L,
    memory = FALSE
  ),
  late_state = bench::mark(status_of(late), iterations = 20L, memory = FALSE),
  late_state_playoffs = bench::mark(
    status_of(late, status = "PLAYOFFS"),
    iterations = 20L,
    memory = FALSE
  ),
  late_state_scenarios = bench::mark(
    status_of(late, scenarios = TRUE),
    iterations = 20L,
    memory = FALSE
  ),
  sims_1000 = bench::mark(status_of(many), iterations = 3L, memory = FALSE)
)
print(rbindlist(lapply(results, \(r) r[, c("min", "median")]), idcol = "case"))
