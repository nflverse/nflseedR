# Benchmark nfl_standings() (C++ engine) against nfl_standings2() (the old
# data.table implementation). Numbers are in dev/cpp-standings-engine.md.
#
# 10,000 simulated regular seasons on the 2022 schedule (2.7M game rows).

# load_all() compiles without optimization, so build an optimized library
pkgbuild::clean_dll()
pkgbuild::compile_dll(debug = FALSE)
devtools::load_all(compile = FALSE)
library(data.table)

set.seed(20261030)
schedule <- as.data.table(nflseedR::sims_games_example)[
  game_type == "REG",
  list(game_type, week, away_team, home_team)
]
n_sims <- 10000L
games <- schedule[rep(seq_len(nrow(schedule)), n_sims)]
games[, sim := rep(seq_len(n_sims), each = nrow(schedule))]
games[, result := as.integer(round(stats::rnorm(.N, mean = 1.5, sd = 13)))]
setcolorder(games, "sim")

standings_with <- function(engine, games, ranks) {
  fun <- switch(engine, r = nfl_standings2, cpp = nfl_standings)
  fun(copy(games), ranks = ranks, verbosity = "NONE")
}
engines <- c("r", "cpp")

all_seasons <- bench::press(
  engine = engines,
  ranks = c("DIV", "CONF", "DRAFT"),
  bench::mark(
    standings_with(engine, games, ranks),
    iterations = 3L,
    check = FALSE,
    memory = FALSE
  )
)

one_season <- bench::press(
  engine = engines,
  bench::mark(
    standings_with(engine, games[sim == 1L], "CONF"),
    iterations = 20L,
    check = FALSE,
    memory = FALSE
  )
)

print(all_seasons[, c("engine", "ranks", "min", "median")])
print(one_season[, c("engine", "min", "median")])
