# Build tests/testthat/nfl_standings_2006_2025.rds: the official NFL standings
# (division and conference ranks) of every week of the 2006-2025 regular
# seasons. Tests compare nfl_standings() with them.
#
# Needs the internal nflapi package, which only exists on the maintainer's
# machine. One request per season with a pause in between, to go easy on the
# API.

library(data.table)

fetch_season <- function(season) {
  Sys.sleep(2)
  x <- nflapi::nflapi_standings(season) |> nflapi::nflapi_standings_parse()
  x <- as.data.table(x)
  x[, season := season]
  x
}

# Keep only what the tests need. The API uses current team names
tidy_standings <- function(raw) {
  teams <- as.data.table(nflreadr::load_teams())[,
    list(team_full_name = team_name, team = team_abbr)
  ]
  out <- merge(raw, teams, by = "team_full_name")[,
    list(
      season,
      week,
      team,
      wins = overall_wins,
      losses = overall_losses,
      ties = overall_ties,
      div_rank = division_rank,
      conf_rank = conference_rank
    )
  ]
  setorderv(out, c("season", "week", "team"))
  as.data.frame(out)
}

if (sys.nframe() == 0L) {
  raw <- rbindlist(lapply(2006:2025, fetch_season), fill = TRUE)
  saveRDS(
    tidy_standings(raw),
    "tests/testthat/nfl_standings_2006_2025.rds",
    version = 2
  )
}
