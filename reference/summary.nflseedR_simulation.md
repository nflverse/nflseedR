# Compute Pretty Simulations Summary Table

Uses the R package gt to create a pretty html table of the nflseedR
simulation summary data frame.

## Usage

``` r
# S3 method for class 'nflseedR_simulation'
summary(object, ...)
```

## Arguments

- object:

  an object for which a summary is desired.

- ...:

  additional arguments passed on to the methods (currently not used).

## Output of below example

![](figures/summary_tbl.png)

## Examples

``` r
# \donttest{
library(nflseedR)
# set seed for recreation,
# internal parallelization requires a L'Ecuyer-CMRG random number generator
set.seed(19980310, kind = "L'Ecuyer-CMRG")

# Simulate the season 20 times in 1 round
sim <- nflseedR::simulate_nfl(
  nfl_season = 2021,
  fresh_season = TRUE,
  simulations = 20
)
#> ℹ 12:41:03 | Loading games data
#> ℹ 12:41:03 | Beginning simulation of 20 seasons in 1 round
#> ℹ 12:41:10 | Combining simulation data
#> ℹ 12:41:10 | Aggregating across simulations
#> ℹ 12:41:10 | DONE!

# Create Summary Tables
tbl <- summary(sim)

# The output of tbl is given in the above image.
# }
```
