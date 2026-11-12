# Writes the states for the sanitizer driver: every week of 2006-2025 and
# 300 seasons with random results after week 12 and week 16 of 2023.
# Run from the package root (dev/sanitize-cpp.sh does).
suppressMessages(devtools::load_all(quiet = TRUE))
library(data.table)
out <- commandArgs(TRUE)[1]
games <- as.data.table(readRDS("tests/testthat/games_2006_2025.rds"))[
  game_type == "REG"
]
league <- standings_league()
weeks <- unique(games[, list(season, week)])
states <- lapply(seq_len(nrow(weeks)), function(i) {
  g <- games[season == weeks$season[i]]
  g[week > weeks$week[i], result := NA]
  g[, seeds := if (weeks$season[i] <= 2019) 6L else 7L]
  g
})
set.seed(1)
for (wk in c(12L, 16L)) {
  base <- games[season == 2023]
  for (i in 1:150) {
    g <- copy(base)
    g[, result := sample(c(-3, 3, 0), .N, TRUE, c(0.47, 0.47, 0.06))]
    g[week > wk, result := NA]
    g[, seeds := sample(6:7, 1)]
    states[[length(states) + 1L]] <- g
  }
}
con <- file(out, "w")
writeLines(
  c(
    as.character(length(states)),
    paste(league$division_id, collapse = " "),
    paste(league$conf_id, collapse = " ")
  ),
  con
)
for (g in states) {
  writeLines(paste(g$seeds[1], nrow(g)), con)
  writeLines(
    paste(
      chmatch(g$away_team, league$team) - 1L,
      chmatch(g$home_team, league$team) - 1L,
      ifelse(is.na(g$result), 9, sign(g$result))
    ),
    con
  )
}
close(con)
