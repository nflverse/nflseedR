# Benchmark cpp_compute_results() against nflseedR_compute_results() and check
# that both give the same output (all.equal(), see the dev notes). Numbers are in
# dev/cpp-standings-engine.md (section "cpp_compute_results()").

# load_all() compiles without optimization, so build an optimized library.
# Stale -O0 objects in src/ would be reused otherwise.
pkgbuild::clean_dll()
pkgbuild::compile_dll(debug = FALSE)
devtools::load_all(compile = FALSE)
library(data.table)

games <- as.data.table(nflseedR::sims_games_example)
games[, result := NA_integer_]

# The function alone: every week of a season, inputs prepared like
# simulate_chunk() does (factor week, sim column, teams repeated per sim)
function_inputs <- function(n) {
  g <- copy(games)[, week := as.factor(week)]
  g <- g[rep(seq_len(nrow(g)), times = n)][,
    sim := rep(seq_len(n), each = nrow(games))
  ]
  t <- as.data.table(nflseedR::divisions)[team %chin% games$home_team]
  t <- t[rep(seq_len(nrow(t)), times = n)][, sim := rep(seq_len(n), each = 32)]
  list(games = g, teams = t)
}
all_weeks <- function(fun, input) {
  teams <- copy(input$teams)
  games <- copy(input$games)
  set.seed(42)
  for (w in levels(games$week)) {
    out <- fun(teams, games, w)
    teams <- out$teams
    games <- out$games
  }
  list(teams = teams, games = games)
}

for (n in c(10000L, 50000L)) {
  input <- function_inputs(n)
  t_r <- system.time(r <- all_weeks(nflseedR_compute_results, input))[[3]]
  t_cpp <- system.time(cpp <- all_weeks(cpp_compute_results, input))[[3]]
  cat(sprintf(
    "function, %d sims: R %.2fs, C++ %.2fs, %.1fx, equal %s\n",
    n,
    t_r,
    t_cpp,
    t_r / t_cpp,
    isTRUE(all.equal(r, cpp))
  ))
}

# Complete simulations (regular season and playoffs), one chunk
simulate <- function(fun, n) {
  set.seed(3)
  nfl_simulations(
    games,
    compute_results = fun,
    simulations = n,
    chunks = 1,
    verbosity = "NONE"
  )
}
for (n in c(10000L, 50000L)) {
  t_r <- system.time(r <- simulate(nflseedR_compute_results, n))[[3]]
  t_cpp <- system.time(cpp <- simulate(cpp_compute_results, n))[[3]]
  cat(sprintf(
    "nfl_simulations(), %d sims: R %.2fs, C++ %.2fs, %.0f%% less time, equal %s\n",
    n,
    t_r,
    t_cpp,
    100 * (1 - t_cpp / t_r),
    isTRUE(all.equal(r[c("standings", "games")], cpp[c("standings", "games")]))
  ))
}
