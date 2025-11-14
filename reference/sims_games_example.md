# Example Games Data used in NFL Simulations

Example Games Data used in NFL Simulations

## Usage

``` r
sims_games_example
```

## Format

A data frame with 284 rows and 9 variables containing NFL schedule
information.

## Details

Please see `data-raw/sim_examples.R` for the code to create this data.

## Examples

``` r
str(sims_games_example)
#> Classes ‘data.table’ and 'data.frame':   284 obs. of  9 variables:
#>  $ season   : int  2022 2022 2022 2022 2022 2022 2022 2022 2022 2022 ...
#>  $ game_type: chr  "REG" "REG" "REG" "REG" ...
#>  $ week     : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ away_team: chr  "BUF" "NO" "CLE" "SF" ...
#>  $ home_team: chr  "LA" "ATL" "CAR" "CHI" ...
#>  $ away_rest: int  7 7 7 7 7 7 7 7 7 7 ...
#>  $ home_rest: int  7 7 7 7 7 7 7 7 7 7 ...
#>  $ location : chr  "Home" "Home" "Home" "Home" ...
#>  $ result   : int  NA -1 -2 NA -3 -3 0 NA -15 6 ...
```
