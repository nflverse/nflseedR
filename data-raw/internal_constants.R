# internal constants
TIEBREAKERS_NONE        <- 1
TIEBREAKERS_NO_COMMON   <- 2
TIEBREAKERS_THROUGH_SOS <- 3

div_vec <- nflseedR::divisions$division |>
  rlang::set_names(nflseedR::divisions$team)

conf_vec <- nflseedR::divisions$conf |>
  rlang::set_names(nflseedR::divisions$team)

# See below code for rotation tables
usethis::use_data(
  TIEBREAKERS_NONE,
  TIEBREAKERS_NO_COMMON,
  TIEBREAKERS_THROUGH_SOS,
  div_vec,
  conf_vec,
  # opponent rotations
  intra_conf_rotation,
  cross_conf_rotation,
  cross_17th_rotation,
  # home advantage rotations
  full_div_rotation,
  div_rank_rotation,
  internal = TRUE,
  overwrite = TRUE
)


# Schedule Rotation Tables ------------------------------------------------

sched <- nflreadr::load_schedules(2020:2025)
cleaned <- sched |>
  filter(game_type == "REG") |>
  clean_homeaway() |>
  select(
    season, team, opponent
  ) |>
  left_join(
    nflseedR::divisions |> select(team, team_div = sdiv),
    by = "team"
  ) |>
  left_join(
    nflseedR::divisions |> select(opponent = team, opp_div = sdiv),
    by = "opponent"
  )

a <- cleaned |>
  filter(team_div != opp_div) |>
  count(season, team_div, opp_div) |>
  filter(n > 10) |>
  select(season, team_div, opp_div) |>
  mutate(
    type = case_when(
      substr(team_div, 1, 3) == substr(opp_div, 1, 3) ~ "intra",
      TRUE ~ "inter"
    )
  ) |>
  pivot_wider(names_from = type, values_from = opp_div) |>
  select(team_div, season, intra, inter) |>
  mutate(
    intra_index = mod(season, 3),
    inter_index = mod(season, 4)
  ) |>
  arrange(team_div, season)

# requires 4 opp divisions
cross_conf_rotation <- a |>
  filter(season <= 2023) |>
  mutate(
    index = season - 2020
  ) |>
  select(team_div, index, cross_conf_opp = inter)

# requires 3 opp divisions
intra_conf_rotation <- a |>
  filter(between(season, 2022, 2024)) |>
  mutate(
    index = season - 2022
  ) |>
  select(team_div, index, intra_conf_opp = intra)

cross_17th <- cleaned |>
  filter(team_div != opp_div) |>
  filter(substr(team_div, 1, 3) != substr(opp_div, 1, 3)) |>
  count(season, team, team_div, opp_div) |>
  filter(n == 1) |>
  distinct(season, team_div, opp_div) |>
  mutate(
    cross_17th_index = mod(season, 4)
  ) |>
  arrange(team_div, season, cross_17th_index)

# requires 4 opp divisions
cross_17th_rotation <- cross_17th |>
  filter(season <= 2024) |>
  select(team_div, index = cross_17th_index, cross_17th_opp = opp_div) |>
  arrange(team_div, index)

cleaned |>
  filter(team_div != opp_div) |>
  filter(substr(team_div, 1, 3) != substr(opp_div, 1, 3)) |>
  count(season, team, team_div, opp_div) |>
  filter(n == 1, team == "KC")


# Home Rotation -----------------------------------------------------------

# for full division matchups, home advantage rotates back and forth between teams
# this means it's a 6 year cycle intra conference and a 8 year cycle cross conference

# for division rank matchups intra conference, home advantage goes home, home, away, away
# by division (not by team). Since there's a full division matchup year in-between,
# this is also a 6 year cycle

# the 17th game rotation is as easy as AFC hosting in odd years and NFC in even years

scheds <- load_schedules(2010:2025) |> filter(game_type == "REG")
gd <- clean_homeaway(scheds) |> filter(season > 2010)
s <- nflseedR::nfl_standings(scheds, ranks = "DIV")
ns_all <- purrr::map(
  sort(unique(s$season)),
  function(year, standings) {
    s <- standings |> dplyr::filter(season == year)
    nfl_regular_season(s)
  },
  standings = s
) |>
  data.table::rbindlist() |>
  mutate(
    join = case_when(
      opp_type == "div_h" ~ "home",
      opp_type == "div_a" ~ "away",
      TRUE ~ "bla"
    ),
    team = clean_team_abbrs(team),
    opp = clean_team_abbrs(opp)
  ) |>
  select(-location)

df <- gd |>
  select(season, team, opp = opponent, location, div_game) |>
  arrange(season, team, opp) |>
  mutate(
    join = ifelse(div_game == 1, location, "bla"),
    team = clean_team_abbrs(team),
    opp = clean_team_abbrs(opp)
  ) |>
  left_join(
    ns_all, by = c("season", "team", "opp", "join")
  ) |>
  select(-div_game, -join) |>
  left_join(
    s |> distinct(team, team_div = division), by = "team"
  ) |>
  left_join(
    s |> distinct(opp = team, opp_div = division), by = "opp"
  ) |>
  mutate(
    # need to overwrite some neutral site games to home/away
    # because they count against the rotation
    location = case_when(
      team == "CLE" & season %in% c(2017, 2025) & location == "neutral" ~ "home",
      team == "MIN" & season %in% c(2017, 2025) & location == "neutral" ~ "away",
      team == "JAX" & season == 2018 & location == "neutral" ~ "home",
      team == "PHI" & season == 2018 & location == "neutral" ~ "away",
      team == "LV" & season == 2018 & location == "neutral" ~ "home",
      team == "SEA" & season == 2018 & location == "neutral" ~ "away",
      TRUE ~ location
    )
  )

full_div_rotation <- df |>
  filter_out(opp_type %in% c("div_h", "div_a", "div_rank", "cross_17th")) |>
  filter_out(location == "neutral") |>
  mutate(
    index = case_when(
      opp_type == "intra_conf" ~ season %% 6,
      opp_type == "cross_conf" ~ season %% 8,
      TRUE ~ NA_integer_
    )
  ) |>
  select(team, opp, location, index, opp_type) |>
  arrange(team, opp) |>
  distinct()

div_vctr <- setNames(nflseedR::divisions$sdiv, nflseedR::divisions$division)

div_rank_rotation <- df |>
  filter(opp_type == "div_rank") |>
  filter_out(location == "neutral") |>
  distinct(season, team_div, opp_div, location) |>
  mutate(
    index = season %% 6
  ) |>
  distinct(team_div, opp_div, location, index) |>
  arrange(team_div, opp_div, location) |>
  mutate(
    team_div = div_vctr[team_div],
    opp_div = div_vctr[opp_div],
  )

