# internal constants
TIEBREAKERS_NONE        <- 1
TIEBREAKERS_NO_COMMON   <- 2
TIEBREAKERS_THROUGH_SOS <- 3

div_vec <- nflseedR::divisions$division |>
  rlang::set_names(nflseedR::divisions$team)

conf_vec <- nflseedR::divisions$conf |>
  rlang::set_names(nflseedR::divisions$team)

usethis::use_data(
  TIEBREAKERS_NONE,
  TIEBREAKERS_NO_COMMON,
  TIEBREAKERS_THROUGH_SOS,
  div_vec,
  conf_vec,
  internal = TRUE,
  overwrite = TRUE
)

