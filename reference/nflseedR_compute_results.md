# Compute NFL Game Results in Season Simulations

This is the default nflseedR function to compute game results in season
simulations.

## Usage

``` r
nflseedR_compute_results(teams, games, week_num, ...)
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

## Details

This function implements a variant of 538's elo model initially coded by
Lee Sharpe (in nflseedR 1.0) and for performance rewritten by Sebastian
Carl (in nflseedR 2.0).

## Examples

``` r
g <- nflseedR::sims_games_example
# The functions expects the variable "sim" instead of "season"
g$sim <- g$season
t <- nflseedR::sims_teams_example

out <- nflseedR_compute_results(
  teams = t,
  games = g,
  week_num = 5L
)

str(out, max.level = 2)
#> List of 2
#>  $ teams:Classes ‘data.table’ and 'data.frame':  64 obs. of  6 variables:
#>   ..$ sim     : int [1:64] 1 1 1 1 1 1 1 1 1 1 ...
#>   ..$ team    : chr [1:64] "ARI" "ATL" "BAL" "BUF" ...
#>   ..$ conf    : chr [1:64] "NFC" "NFC" "AFC" "AFC" ...
#>   ..$ division: chr [1:64] "NFC West" "NFC South" "AFC North" "AFC East" ...
#>   ..$ sdiv    : chr [1:64] "NFCW" "NFCS" "AFCN" "AFCE" ...
#>   ..$ elo     : num [1:64] 1619 1772 1359 1629 1593 ...
#>   ..- attr(*, ".internal.selfref")=<externalptr> 
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
#>   ..$ sim      : int [1:284] 2022 2022 2022 2022 2022 2022 2022 2022 2022 2022 ...
#>   ..- attr(*, ".internal.selfref")=<externalptr> 
#>   ..- attr(*, "index")= int(0) 
#>   .. ..- attr(*, "__week__location__game_type")= int [1:284] 1 2 3 4 5 6 7 8 9 10 ...
#>   .. .. ..- attr(*, "starts")= int [1:27] 1 17 33 49 64 65 80 81 95 109 ...
#>   .. .. ..- attr(*, "maxgrpn")= int 16
#>   .. .. ..- attr(*, "anyna")= int 0
#>   .. .. ..- attr(*, "anyinfnan")= int 0
#>   .. .. ..- attr(*, "anynotascii")= int 0
#>   .. .. ..- attr(*, "anynotutf8")= int 0
```
