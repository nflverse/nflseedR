# Changelog

## nflseedR 2.0.2

- Changed css styling of images in package documentation by CRAN
  request. ([\#61](https://github.com/nflverse/nflseedR/issues/61))

## nflseedR 2.0.1

CRAN release: 2025-08-18

- [`nflseedR_compute_results()`](https://nflseedr.com/reference/nflseedR_compute_results.md)
  correctly adjusts Elo difference for postseason games.
  ([\#54](https://github.com/nflverse/nflseedR/issues/54))
- [`nfl_standings()`](https://nflseedr.com/reference/nfl_standings.md)
  returns `exit` value consistent with
  [`nfl_simulations()`](https://nflseedr.com/reference/nfl_simulations.md)
  if argument `ranks = "DRAFT"`.
  ([\#56](https://github.com/nflverse/nflseedR/issues/56))
- [`nfl_standings_prettify()`](https://nflseedr.com/reference/nfl_standings_prettify.md)
  shows `exit` (if available).
  ([\#56](https://github.com/nflverse/nflseedR/issues/56))
- [`nfl_standings()`](https://nflseedr.com/reference/nfl_standings.md)
  now warns the user when the `games` argument includes both `"season"`
  and `"sim"` to avoid confusion.
  ([\#58](https://github.com/nflverse/nflseedR/issues/58))

## nflseedR 2.0.0

CRAN release: 2025-03-24

This is a major release that introduces a new generation of high
efficient standings and simulation code

### New Features

- New function
  [`nfl_standings()`](https://nflseedr.com/reference/nfl_standings.md)
  for high efficient standings calculation. The functions
  [`compute_division_ranks()`](https://nflseedr.com/reference/compute_division_ranks.md),
  [`compute_conference_seeds()`](https://nflseedr.com/reference/compute_conference_seeds.md),
  and
  [`compute_draft_order()`](https://nflseedr.com/reference/compute_draft_order.md)
  will be deprecated in a future release.
  ([\#45](https://github.com/nflverse/nflseedR/issues/45))
- New function
  [`nfl_simulations()`](https://nflseedr.com/reference/nfl_simulations.md)
  for a new, highly efficient approach to season simulations. This is a
  completely new design of the simulator, with the aim of achieving
  significantly faster run times and eliminating weaknesses in the old
  approach (in
  [`simulate_nfl()`](https://nflseedr.com/reference/simulate_nfl.md)).
  The introduction of this function is supplemented by the two new
  utility functions
  [`nflseedR_compute_results()`](https://nflseedr.com/reference/nflseedR_compute_results.md),
  and
  [`simulations_verify_fct()`](https://nflseedr.com/reference/simulations_verify_fct.md).
  These functions form the new standard for computing results (if the
  user does not have their own function for this) respectively allow
  verification of the functionality of their own functions instead of
  [`nflseedR_compute_results()`](https://nflseedr.com/reference/nflseedR_compute_results.md).
  It is planned that
  [`simulate_nfl()`](https://nflseedr.com/reference/simulate_nfl.md)
  will be deprecated in a future release so that the dependencies of
  nflseedR can be significantly reduced.
  ([\#47](https://github.com/nflverse/nflseedR/issues/47))
- New function
  [`nfl_standings_prettify()`](https://nflseedr.com/reference/nfl_standings_prettify.md)
  computes a [`gt::gt()`](https://gt.rstudio.com/reference/gt.html)
  table of the output created with
  [`nfl_standings()`](https://nflseedr.com/reference/nfl_standings.md).
  ([\#49](https://github.com/nflverse/nflseedR/issues/49))

### Bug Fixes and Minor Improvements

- [`nfl_standings()`](https://nflseedr.com/reference/nfl_standings.md)
  now supports `tiebreaker_depth = "POINTS"` which breaks ties using
  combined point ranks and point differentials. This means that all
  tiebreakers except net touchdowns are now implemented.
  ([\#47](https://github.com/nflverse/nflseedR/issues/47))
- Fixed a bug in
  [`nfl_standings()`](https://nflseedr.com/reference/nfl_standings.md)
  where the tie breaking procedure didn’t restart correctly after some
  teams were eliminated while some others remained tied.
  ([\#47](https://github.com/nflverse/nflseedR/issues/47))
- The `summary` method
  [`summary.nflseedR_simulation()`](https://nflseedr.com/reference/summary.nflseedR_simulation.md)
  explicitly sets the columns width of the logo column because those
  columns are hidden in some unclear scenarios.
- The `summary` method
  [`summary.nflseedR_simulation()`](https://nflseedr.com/reference/summary.nflseedR_simulation.md)
  now requires gt version v0.9.0 or higher to avoid warnings about
  deprecated arguments.
- Fixed error in
  [`simulate_nfl()`](https://nflseedr.com/reference/simulate_nfl.md)
  where it crashes because the “fake schedule” isn’t a tibble.
  ([\#43](https://github.com/nflverse/nflseedR/issues/43))
- The function `load_sharpe_games` has been deprecated. It was replaced
  a fairly long time ago by
  [`nflreadr::load_schedules()`](https://nflreadr.nflverse.com/reference/load_schedules.html).
  ([\#47](https://github.com/nflverse/nflseedR/issues/47))
- nflseedR now requires R 4.1 to allow the package to use R’s native
  pipe `|>` operator. This follows the [Tidyverse R version support
  rules](https://tidyverse.org/blog/2019/04/r-version-support/).
  ([\#48](https://github.com/nflverse/nflseedR/issues/48))

## nflseedR 1.2.0

CRAN release: 2023-01-05

- [`simulate_nfl()`](https://nflseedr.com/reference/simulate_nfl.md)
  gained the new argument `sim_include` to allow more access to what is
  actually being simulated. This makes it possible skip playoff
  simulation or the (possibly heavy) computation of draft order.
  ([\#34](https://github.com/nflverse/nflseedR/issues/34))
- The `summary` method
  [`summary.nflseedR_simulation()`](https://nflseedr.com/reference/summary.nflseedR_simulation.md)
  now hides columns where all values are `NA`. This is useful if
  [`simulate_nfl()`](https://nflseedr.com/reference/simulate_nfl.md)
  skips the postseason or draft order. The method also re-formats the
  number of simulations in the subtitle, e.g. from “10000” to “10k”
  (this requires scales \>= 1.2.0, but it is a good idea to update
  scales anyways).
  ([\#35](https://github.com/nflverse/nflseedR/issues/35))
- [`simulate_nfl()`](https://nflseedr.com/reference/simulate_nfl.md) now
  uses data.table to combine simulation rounds data. This is a
  significant performance improvement. The returned list `"sim_params"`
  now includes the package version of nflseedR (for debugging) and the
  current system time when the simulation was finished.
  ([\#36](https://github.com/nflverse/nflseedR/issues/36))
- Lots of internal improvements to reduce package dependencies and
  messaging. ([\#36](https://github.com/nflverse/nflseedR/issues/36))
- The `summary` method
  [`summary.nflseedR_simulation()`](https://nflseedr.com/reference/summary.nflseedR_simulation.md)
  now uses
  [`fmt_pct_special()`](https://nflseedr.com/reference/fmt_pct_special.md)
  to format probability strings to avoid the impression of
  overconfidence by rounding to integer percentages.
  ([\#37](https://github.com/nflverse/nflseedR/issues/37))

## nflseedR 1.1.0

CRAN release: 2022-07-07

- Added the data frame `game_summary` to the output of
  [`simulate_nfl()`](https://nflseedr.com/reference/simulate_nfl.md)
  which aggregates matchups across all simulated seasons
  ([\#24](https://github.com/nflverse/nflseedR/issues/24))
- Updated the description of some variables in
  [`load_sharpe_games()`](https://nflseedr.com/reference/load_sharpe_games.md)
  ([\#24](https://github.com/nflverse/nflseedR/issues/24))
- The output of
  [`simulate_nfl()`](https://nflseedr.com/reference/simulate_nfl.md) is
  now of class `nflseedR_simulation` which allows the implementation of
  a [`summary()`](https://rdrr.io/r/base/summary.html) method that
  computes a `gt()` table of the simulation summary data frame.
  ([\#26](https://github.com/nflverse/nflseedR/issues/26))
- The standings data frame calculated by
  [`compute_division_ranks()`](https://nflseedr.com/reference/compute_division_ranks.md)
  now outputs losses and ties as well.
  ([\#27](https://github.com/nflverse/nflseedR/issues/27))

## nflseedR 1.0.2

CRAN release: 2021-04-10

- Improved error handling of the function
  [`load_sharpe_games()`](https://nflseedr.com/reference/load_sharpe_games.md)
  for CRAN tests

## nflseedR 1.0.1

CRAN release: 2021-03-31

- Added support for a fake schedule for simulating the upcoming season
  before the NFL has released the official schedule.

- updated the documentation of the function
  [`load_sharpe_games()`](https://nflseedr.com/reference/load_sharpe_games.md)
  with the new columns `away_qb_id`, `home_qb_id`, `away_qb_name` and
  `home_qb_name`.

## nflseedR 1.0.0

- Initial release.
