# nflseedR (development version)

* `simulate_nfl()` gained the new argument `sim_include` to allow more access to what is actually being simulated. This makes it possible skip playoff simulation or the (possibly heavy) computation of draft order. (#34)
* The `summary` method `summary.nflseedR_simulation()` now hides columns where all values are `NA`. This is useful if `simulate_nfl()` skips the postseason or draft order. The method also reformats the number of simulations in the subtitle, e.g. from "10000" to "10k" (this requires scales >= 1.2.0, but it is a good idea to update scales anyways). (#35)
* `simulate_nfl()` now uses data.table to combine simulation rounds data. This is a significant performance improvement. The returned list `"sim_params"` now includes the package version of nflseedR (for debugging) and the current system time when the simulation was finished. (#36)
* Lots of internal improvements to reduce package dependencies and messaging. (#36)

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
