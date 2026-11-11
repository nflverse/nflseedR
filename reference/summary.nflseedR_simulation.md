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
#> Warning: future::availableCores() is deprecated. Please use the identical parallelly::availableCores() instead [pkgdown::build_site_github_pages() -> pkgdown::build_site() -> pkgdown::build_site_local() -> pkgdown::build_reference() -> pkgdown::unwrap_purrr_error() -> base::withCallingHandlers() -> purrr::map() -> purrr::map_() -> purrr::with_indexed_errors() -> base::withCallingHandlers() -> purrr::call_with_cleanup() -> pkgdown::.f() -> base::withCallingHandlers() -> pkgdown::data_reference_topic() -> pkgdown::run_examples() -> pkgdown::highlight_examples() -> downlit::evaluate_and_highlight() -> evaluate::evaluate() -> base::withRestarts() -> withRestartList() -> withOneRestart() -> doWithOneRestart() -> withRestartList() -> withOneRestart() -> doWithOneRestart() -> evaluate::with_handlers() -> base::eval() -> eval() -> base::withCallingHandlers() -> base::withVisible() -> base::eval() -> eval() -> nflseedR::simulate_nfl() -> nflseedR::is_single_digit_numeric() -> future::availableCores()]
#> ℹ 17:27:01 | Loading games data
#> ℹ 17:27:01 | Beginning simulation of 20 seasons in 1 round
#> ℹ 17:27:09 | Combining simulation data
#> ℹ 17:27:09 | Aggregating across simulations
#> ℹ 17:27:09 | DONE!

# Create Summary Tables
tbl <- summary(sim)

# The output of tbl is given in the above image.
# }
```
