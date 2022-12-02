# nflseedR (development version)

* `simulate_nfl()` gained the new argument `sim_include` to allow more access to what is actually being simulated. This makes it possible skip playoff simulation or the (possibly heavy) computation of draft order.

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
