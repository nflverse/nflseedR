# Compute NFL Game Results in Season Simulations with C++

A C++ port of
[`nflseedR_compute_results()`](https://nflseedr.com/reference/nflseedR_compute_results.md).
It returns the same results for the same random seed and is about 2
times faster, which makes
[`nfl_simulations()`](https://nflseedr.com/reference/nfl_simulations.md)
about a third faster. Use it with
`nfl_simulations(compute_results = cpp_compute_results)`.

## Usage

``` r
cpp_compute_results(teams, games, week_num, ...)
```

## Arguments

- teams:

  A list of teams by simulation number. This is usually calculated
  automatically and not user facing. It can be used to "transport" team
  information like elo ratings from one simulated week to the next.
  Defaults to
  [sims_teams_example](https://nflseedr.com/reference/sims_teams_example.md).
  Please see this example to understand the required data structure.

- games:

  An NFL schedule where some results are missing. `compute_results` is
  supposed to compute those results on a weekly base. Defaults to
  [sims_games_example](https://nflseedr.com/reference/sims_games_example.md).
  Please see this example to understand the required data structure.

- week_num:

  The week of a NFL season for which the function should compute
  results.

- ...:

  Additional parameters used in the function. It is possible to pass the
  argument `elo` to the function. This must be a named vector in which
  the names correspond to the team abbreviations and the values
  correspond to the initial elo ratings, which are then updated after
  each week based on the results and transported to the next week.

## Value

A list of updated `teams` and `games` tables.

## Examples

``` r
g <- nflseedR::sims_games_example
g$sim <- 1L
t <- nflseedR::sims_teams_example

out <- cpp_compute_results(
  teams = t,
  games = g,
  week_num = "6"
)

str(out, max.level = 2)
#> List of 2
#>  $ teams:Classes ‘data.table’ and 'data.frame':  64 obs. of  6 variables:
#>   ..$ sim     : int [1:64] 1 1 1 1 1 1 1 1 1 1 ...
#>   ..$ team    : chr [1:64] "ARI" "ATL" "BAL" "BUF" ...
#>   ..$ conf    : chr [1:64] "NFC" "NFC" "AFC" "AFC" ...
#>   ..$ division: chr [1:64] "NFC West" "NFC South" "AFC North" "AFC East" ...
#>   ..$ sdiv    : chr [1:64] "NFCW" "NFCS" "AFCN" "AFCE" ...
#>   ..$ elo     : num [1:64] 1357 1424 1622 1432 1151 ...
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x5650dcb24580> 
#>  $ games:Classes ‘data.table’ and 'data.frame':  284 obs. of  10 variables:
#>   ..$ season   : int [1:284] 2022 2022 2022 2022 2022 2022 2022 2022 2022 2022 ...
#>   ..$ game_type: chr [1:284] "REG" "REG" "REG" "REG" ...
#>   ..$ week     : int [1:284] 1 1 1 1 1 1 1 1 1 1 ...
#>   ..$ away_team: chr [1:284] "BUF" "NO" "CLE" "SF" ...
#>   ..$ home_team: chr [1:284] "LA" "ATL" "CAR" "CHI" ...
#>   ..$ away_rest: int [1:284] 7 7 7 7 7 7 7 7 7 7 ...
#>   ..$ home_rest: int [1:284] 7 7 7 7 7 7 7 7 7 7 ...
#>   ..$ location : chr [1:284] "Home" "Home" "Home" "Home" ...
#>   ..$ result   : int [1:284] NA -1 -2 NA -3 -3 0 NA -15 6 ...
#>   ..$ sim      : int [1:284] 1 1 1 1 1 1 1 1 1 1 ...
#>   ..- attr(*, ".internal.selfref")=<pointer: 0x5650dcb24580> 
```
