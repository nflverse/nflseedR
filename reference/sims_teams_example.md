# Example Teams Data used in NFL Simulations

Example Teams Data used in NFL Simulations

## Usage

``` r
sims_teams_example
```

## Format

A data frame with 64 rows and 5 variables containing team name and
division information.

## Details

Please see `data-raw/sim_examples.R` for the code to create this data.

## Examples

``` r
str(sims_teams_example)
#> Classes ‘data.table’ and 'data.frame':   64 obs. of  5 variables:
#>  $ sim     : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ team    : chr  "ARI" "ATL" "BAL" "BUF" ...
#>  $ conf    : chr  "NFC" "NFC" "AFC" "AFC" ...
#>  $ division: chr  "NFC West" "NFC South" "AFC North" "AFC East" ...
#>  $ sdiv    : chr  "NFCW" "NFCS" "AFCN" "AFCE" ...
```
