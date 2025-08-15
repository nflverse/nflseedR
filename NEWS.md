# nflseedR 2.0.1

* `nflseedR_compute_results()` correctly adjusts Elo difference for postseason games. (#54)
* `nfl_standings()` returns `exit` value consistent with `nfl_simulations()` if argument `ranks = "DRAFT"`. (#56)
* `nfl_standings_prettify()` shows `exit` (if available). (#56)
* `nfl_standings()` now warns the user when the `games` argument includes both `"season"` and `"sim"` to avoid confusion. (#58)

# nflseedR 2.0.0

This is a major release that introduces a new generation of high efficient standings and simulation code

## New Features

* New function `nfl_standings()` for high efficient standings calculation. The functions `compute_division_ranks()`, `compute_conference_seeds()`, and `compute_draft_order()` will be deprecated in a future release. (#45)
* New function `nfl_simulations()` for a new, highly efficient approach to season simulations. This is a completely new design of the simulator, 
with the aim of achieving significantly faster run times and eliminating weaknesses in the old approach (in `simulate_nfl()`). 
The introduction of this function is supplemented by the two new utility functions `nflseedR_compute_results()`, and `simulations_verify_fct()`.
These functions form the new standard for computing results (if the user does not have their own function for this) 
respectively allow verification of the functionality of their own functions instead of `nflseedR_compute_results()`. 
It is planned that `simulate_nfl()` will be deprecated in a future release so that the dependencies of nflseedR can be significantly reduced. (#47)
* New function `nfl_standings_prettify()` computes a `gt::gt()` table of the output created with `nfl_standings()`. (#49)

## Bug Fixes and Minor Improvements

* `nfl_standings()` now supports `tiebreaker_depth = "POINTS"` which breaks ties using combined point ranks and point differentials. This means that all tiebreakers except net touchdowns are now implemented. (#47)
* Fixed a bug in `nfl_standings()` where the tie breaking procedure didn't restart correctly after some teams were eliminated while some others remained tied. (#47)
* The `summary` method `summary.nflseedR_simulation()` explicitly sets the columns width of the logo column because those columns are hidden in some unclear scenarios.
* The `summary` method `summary.nflseedR_simulation()` now requires gt version v0.9.0 or higher to avoid warnings about deprecated arguments.
* Fixed error in `simulate_nfl()` where it crashes because the "fake schedule" isn't a tibble. (#43)
* The function `load_sharpe_games` has been deprecated. It was replaced a fairly long time ago by `nflreadr::load_schedules()`. (#47)
* nflseedR now requires R 4.1 to allow the package to use R's native pipe `|>` operator. This follows the [Tidyverse R version support rules](https://www.tidyverse.org/blog/2019/04/r-version-support/). (#48)

# nflseedR 1.2.0

* `simulate_nfl()` gained the new argument `sim_include` to allow more access to what is actually being simulated. This makes it possible skip playoff simulation or the (possibly heavy) computation of draft order. (#34)
* The `summary` method `summary.nflseedR_simulation()` now hides columns where all values are `NA`. This is useful if `simulate_nfl()` skips the postseason or draft order. The method also re-formats the number of simulations in the subtitle, e.g. from "10000" to "10k" (this requires scales >= 1.2.0, but it is a good idea to update scales anyways). (#35)
* `simulate_nfl()` now uses data.table to combine simulation rounds data. This is a significant performance improvement. The returned list `"sim_params"` now includes the package version of nflseedR (for debugging) and the current system time when the simulation was finished. (#36)
* Lots of internal improvements to reduce package dependencies and messaging. (#36)
* The `summary` method `summary.nflseedR_simulation()` now uses `fmt_pct_special()` to format probability strings to avoid the impression of overconfidence by rounding to integer percentages. (#37)

# nflseedR 1.1.0

* Added the data frame `game_summary` to the output of `simulate_nfl()` which aggregates matchups across all simulated seasons (#24)
* Updated the description of some variables in `load_sharpe_games()` (#24)
* The output of `simulate_nfl()` is now of class `nflseedR_simulation` which allows the implementation of a `summary()` method that computes a `gt()` table of the simulation summary data frame. (#26)
* The standings data frame calculated by `compute_division_ranks()` now outputs losses and ties as well. (#27)

# nflseedR 1.0.2

* Improved error handling of the function `load_sharpe_games()` for CRAN tests

# nflseedR 1.0.1

* Added support for a fake schedule for simulating the upcoming season before the NFL has released the official schedule.

* updated the documentation of the function `load_sharpe_games()` with the new columns `away_qb_id`, `home_qb_id`, `away_qb_name` and `home_qb_name`.

# nflseedR 1.0.0

* Initial release.
