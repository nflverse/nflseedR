# We test the output of nflseedR::nfl_regular_season in a snapshot test
# This file hosts code to verify that the current state is correct, and therefore
# the snapshot is correct.

# We want to verify that our functions does all rotations correctly. So we have
# to compute matchups of 8 seasons, including at least two seasons with 17 games.

# We do it for the 2017:2024 seasons, which means we need standings of the
# 2016:2023 seasons
games <- nflreadr::load_schedules(2016:2024) |>
  dplyr::mutate(
    # can't compute neutral site games so we overwrite them here
    location = "home"
  )

# to compare 2017:2024 season matchups, we need 2016:2023 standings
standings <- games |>
  dplyr::filter(dplyr::between(season, 2016, 2023)) |>
  nflseedR::nfl_standings(ranks = "DIV") |>
  dplyr::mutate(
    team = nflreadr::clean_team_abbrs(team)
  )
actual_matchups <- games |>
  dplyr::filter(game_type == "REG", season >= 2017) |>
  nflreadr::clean_homeaway() |>
  dplyr::mutate(
    # we need standardized team names for later comparison
    team = nflreadr::clean_team_abbrs(team),
    opponent = nflreadr::clean_team_abbrs(opponent)
  ) |>
  dplyr::select(season, team, opp = opponent, location) |>
  dplyr::arrange(season, team, opp)

# nflseedR::nfl_regular_season is designed to work with one season only
# there is no point in vectorizing it across seasons
regular_seasons <- lapply(
  sort(unique(standings$season)),
  function(year, standings) {
    s <- standings[season == year]
    nflseedR::nfl_regular_season(s)
  },
  standings = standings
) |>
  data.table::rbindlist()

# now we make an anti join. All matches in both tables will be removed
# ideally this has 0 rows which means we've done perfect
# In reality this has two lines. It's the 2022 W17 Bills at Bengals matchup
# that got cancelled (he Damar Hamlin game)
mismatches <- regular_seasons |>
  dplyr::anti_join(actual_matchups)
