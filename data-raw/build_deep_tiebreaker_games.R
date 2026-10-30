# Build tests/testthat/deep_tiebreaker_games.rds
#
# Real seasons almost never reach the deep tiebreakers (SOV and beyond), so
# the standings snapshots based on real games don't test them. This script
# synthesizes regular seasons on the 2022 schedule with tie-heavy results,
# computes standings with the current implementation, and keeps a small set
# of seasons that together reach every tiebreaker label. Prefer seasons with
# few coin tosses because coin tosses depend on the RNG implementation.
#
# The fixture is frozen as rds so the tests don't depend on this generator.
# Rerun only if you need to change the fixture, and review all snapshot
# changes afterwards.

devtools::load_all()
library(data.table)
# Forked workers inherit the code loaded by load_all()
future::plan(
  future::multicore,
  workers = max(1L, future::availableCores() - 2L)
)

set.seed(20261030)

schedule <- as.data.table(nflseedR::sims_games_example)[
  game_type == "REG",
  list(game_type, week, away_team, home_team)
]

# Each season draws its own tie probability and margin spread so the pool
# covers everything from "almost all games tied" to "few ties".
synthesize_seasons <- function(n, schedule) {
  n_games <- nrow(schedule)
  out <- schedule[rep(seq_len(n_games), n)]
  out[, sim := rep(seq_len(n), each = n_games)]
  p_tie <- sample(c(0.2, 0.4, 0.6, 0.8, 0.95, 0.98, 0.99), n, replace = TRUE)[
    out$sim
  ]
  max_margin <- sample(c(1L, 3L, 7L), n, replace = TRUE)[out$sim]
  is_tie <- stats::runif(nrow(out)) < p_tie
  margin <- ceiling(stats::runif(nrow(out)) * max_margin)
  sign <- sample(c(-1L, 1L), nrow(out), replace = TRUE)
  out[, result := as.integer(fifelse(is_tie, 0L, sign * margin))]
  # Few distinct scores produce equal points for and against, which is needed
  # to reach the steps after the combined points rankings.
  out[, away_score := sample(c(10L, 14L, 17L, 20L), .N, replace = TRUE)]
  out[, home_score := away_score + result]
  setcolorder(out, "sim")
  out[]
}

label_cols <- c(
  "div_tie_broken_by",
  "conf_tie_broken_by",
  "draft_tie_broken_by"
)

# The current implementation aborts some tie-heavy seasons ("Entered infinite
# loop in draft tiebreaking procedure") because its loop counter is too small
# for large ties. Find these seasons by bisection and skip them.
standings_or_failures <- function(games, depth) {
  s <- tryCatch(
    nfl_standings(
      copy(games),
      ranks = "DRAFT",
      tiebreaker_depth = depth,
      verbosity = "NONE"
    ),
    error = function(e) NULL
  )
  if (!is.null(s)) {
    return(list(standings = s, failed = integer()))
  }
  sims <- unique(games$sim)
  if (length(sims) == 1L) {
    return(list(standings = NULL, failed = sims))
  }
  half <- sims[seq_len(length(sims) %/% 2L)]
  a <- standings_or_failures(games[sim %in% half], depth)
  b <- standings_or_failures(games[!sim %in% half], depth)
  list(
    standings = rbind(a$standings, b$standings, fill = TRUE),
    failed = c(a$failed, b$failed)
  )
}

failed_sims <- integer()

season_labels <- function(games, depth) {
  chunks <- split(
    unique(games$sim),
    ceiling(seq_along(unique(games$sim)) / 250)
  )
  res <- furrr::future_map(
    chunks,
    \(sims) standings_or_failures(games[sim %in% sims], depth),
    .options = furrr::furrr_options(seed = TRUE, packages = "data.table")
  )
  failed_sims <<- union(failed_sims, unlist(lapply(res, `[[`, "failed")))
  s <- rbindlist(lapply(res, `[[`, "standings"), fill = TRUE)
  for (col in setdiff(label_cols, names(s))) {
    s[, (col) := NA_character_]
  }
  long <- melt(
    s[, c("sim", label_cols), with = FALSE],
    id.vars = "sim",
    variable.name = "type",
    value.name = "label",
    na.rm = TRUE
  )
  long[, key := paste(depth, type, label)]
  long[, list(sim, key, coin = grepl("Coin Toss", label))]
}

pool <- synthesize_seasons(10000, schedule)

# Large draft ties (many teams with equal win_pct and sos) trigger the loop
# counter bug, and bisecting many failures is slow. Drop those seasons upfront.
# Teams within each tie are placed one per loop round, so the bisection below
# only has to catch a few remaining failures.
records <- nfl_standings(copy(pool), ranks = "NONE", verbosity = "NONE")
draft_tie_size <- records[, .N, by = c("sim", "win_pct", "sos")][,
  list(max_tie = max(N)),
  by = "sim"
]
large_tie_sims <- draft_tie_size[max_tie > 12L, sim]
cli::cli_inform(
  "Dropping {length(large_tie_sims)} seasons with draft ties of more than 12 teams."
)
# Only some of these seasons fail, and whether they do depends on coin tosses.
# Find the first one that fails with the default arguments and seed 1, which is
# what the regression test uses. with_seed() leaves the RNG state of the main
# pipeline untouched.
for (i in large_tie_sims) {
  failing_example <- pool[sim == i]
  failed <- tryCatch(
    {
      withr::with_seed(
        1L,
        nfl_standings(
          copy(failing_example),
          ranks = "DRAFT",
          verbosity = "NONE"
        )
      )
      FALSE
    },
    error = function(e) TRUE
  )
  if (failed) break
}
pool <- pool[!sim %in% large_tie_sims]
labels <- rbindlist(lapply(c("POINTS", "SOS", "PRE-SOV"), \(d) {
  season_labels(pool, d)
}))
labels <- labels[!sim %in% failed_sims]
cli::cli_inform(
  "{length(failed_sims)} seasons fail in the current implementation."
)

coin_tosses <- labels[, list(coin = sum(coin)), by = "sim"]
season_keys <- labels[, list(keys = list(unique(key))), by = "sim"]
season_keys <- merge(season_keys, coin_tosses, by = "sim")

# Greedy set cover: every label (per depth) at least `min_hits` times. Pick the
# season with the most new hits, prefer fewer coin tosses on ties.
min_hits <- 2L
all_keys <- unique(labels$key)
hits <- setNames(integer(length(all_keys)), all_keys)
chosen <- integer()
repeat {
  needed <- names(hits)[hits < min_hits]
  if (length(needed) == 0L) {
    break
  }
  gain <- vapply(season_keys$keys, \(k) sum(k %in% needed), integer(1))
  gain[season_keys$sim %in% chosen] <- 0L
  if (max(gain) == 0L) {
    break
  }
  best <- order(-gain, season_keys$coin)[1]
  chosen <- c(chosen, season_keys$sim[best])
  k <- season_keys$keys[[best]]
  hits[k] <- hits[k] + 1L
}

cli::cli_inform(
  "Chose {length(chosen)} seasons covering {length(all_keys)} labels."
)
print(sort(hits))

fixture <- pool[sim %in% chosen]
fixture[, sim := match(sim, chosen)]
setorderv(fixture, c("sim", "week"))
fixture <- as.data.frame(fixture)

saveRDS(fixture, "tests/testthat/deep_tiebreaker_games.rds", version = 2)

# Keep one season that the current implementation can't rank as a regression
# case for the loop counter bug.
failing_example[, sim := 1L]
saveRDS(
  as.data.frame(failing_example),
  "tests/testthat/draft_tiebreaker_loop_games.rds",
  version = 2
)
