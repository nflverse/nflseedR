# Verify Custom NFL Result Simulation Function

nflseedR supports custom functions to compute results in season
simulations through the argument `compute_results` in the season
simulation function
[nfl_simulations](https://nflseedr.com/reference/nfl_simulations.md). To
ensure that custom functions work as nflseedR expects them to, it is
recommended to verify their behavior. This function first checks the
structure of the output and then whether game results are changed as
expected. Whenever a problem is found, the function will error with a
hint to the problem (this means that you might be required to iterate
over all problems until the function stops erroring). See below detail
section for more information on expected behavior.

## Usage

``` r
simulations_verify_fct(
  compute_results,
  ...,
  games = nflseedR::sims_games_example,
  teams = nflseedR::sims_teams_example
)
```

## Arguments

- compute_results:

  A function to compute results of games. See below detail section for
  more information on expected behavior.

- ...:

  Further arguments passed on to `compute_results`.

- games:

  An NFL schedule where some results are missing. `compute_results` is
  supposed to compute those results on a weekly base. Defaults to
  [sims_games_example](https://nflseedr.com/reference/sims_games_example.md).
  Please see this example to understand the required data structure.

- teams:

  A list of teams by simulation number. This is usually calculated
  automatically and not user facing. It can be used to "transport" team
  information like elo ratings from one simulated week to the next.
  Defaults to
  [sims_teams_example](https://nflseedr.com/reference/sims_teams_example.md).
  Please see this example to understand the required data structure.

## Value

Returns `TRUE` invisibly if no problems are found.

## Details

The following sections detail the requirements for the `compute_results`
function. If anything is unclear, please see the source code of
nflseedR's default function `nflseedR_compute_results`.

### Required Function Arguments of `compute_results`

The function passed to `compute_results` is required to support the
arguments `"teams"`, `"games"`, and `"week_num"`. The two leading ones
are already described above. The latter is a factor with a length of 1,
which identifies the current week. Regular season weeks are labeled
`"1"`, `"2"`, etc. Playoff weeks are labeled `"WC"`, `"DIV"`, `"CON"`,
and `"SB"`.

### Required Output Structure of `compute_results`

The function passed to `compute_results` is required to return a list of
the two objects `"teams"` and `"games"` as passed to it in the arguments
of the same name. The function must not remove rows or columns. So the
last line of `compute_results` usually looks like

    list("teams" = teams, "games" = games)

### Required Behavior of `compute_results` when Computing Game Results

nflseedR calls `compute_results` for every week where a `result` is
missing in `games`. The variable `result` is defined as the point
differential between the home team and the away team. If the home team
loses, the value is therefore \< 0, if it wins \> 0 and if it ties == 0.
To support elo-based simulations, this is done in a loop so that elo
ratings can be updated based on the results and "transported" from week
to week. You can "transport" ratings or other information by joining
them to the `"teams"` table. This behavior requires that
`compute_results` only changes the results of the current week - called
`week_num`. And only if there is not already a result. So
`compute_results` must only compute a result when

    week == week_num & is.na(result)

For the playoffs, there is also the special case that matches cannot end
in a tie (`result == 0`). In most cases, ties are not simulated anyway
because they occur so rarely. But in the event that they are simulated,
they must not be in the playoffs.

## Examples

``` r
simulations_verify_fct(nflseedR_compute_results)
#> Warning: A shallow copy of this data.table was taken so that := can add or remove 1 columns by reference. At an earlier point, this data.table was copied by R (or was created manually using structure() or similar). Avoid names<- and attr<- which in R currently (and oddly) may copy the whole data.table. Use set* syntax instead to avoid copying: ?set, ?setnames and ?setattr. It's also not unusual for data.table-agnostic packages to produce tables affected by this issue. If this message doesn't help, please report your use case to the data.table issue tracker so the root cause can be fixed or this message improved.
#> ✔ No problems found!
```
