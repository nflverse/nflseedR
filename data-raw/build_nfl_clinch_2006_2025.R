# Build tests/testthat/nfl_clinch_2006_2025.rds: the official NFL clinching
# flags of every team and week of the 2006-2025 regular seasons. Tests compare
# nfl_clinching_status() with them (see dev/playoff-status.md).
#
# Flags: clinched_division (division clinched), clinched_playoff (playoffs
# clinched), clinched_eliminated (playoffs eliminated) and clinched_home_field
# (No. 1 seed clinched).
#
# Needs the internal nflapi package, which only exists on the maintainer's
# machine. One request per season with a pause in between, to go easy on the
# API.

library(data.table)
source("data-raw/build_nfl_standings_2006_2025.R")

# Keep only what the tests need. The API uses current team names
tidy_clinch <- function(raw) {
  teams <- as.data.table(nflreadr::load_teams())[,
    list(team_full_name = team_name, team = team_abbr)
  ]
  out <- merge(raw, teams, by = "team_full_name")[,
    list(
      season,
      week,
      team,
      clinched_division,
      clinched_playoff,
      clinched_eliminated,
      clinched_home_field
    )
  ]
  setorderv(out, c("season", "week", "team"))
  as.data.frame(out)
}

if (sys.nframe() == 0L) {
  raw <- rbindlist(lapply(2006:2025, fetch_season), fill = TRUE)
  saveRDS(
    tidy_clinch(raw),
    "tests/testthat/nfl_clinch_2006_2025.rds",
    version = 2
  )
}
