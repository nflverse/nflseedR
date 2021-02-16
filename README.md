
<!-- README.md is generated from README.Rmd. Please edit that file -->

# **nflseedR** <img src='man/figures/logo.png' align="right" style="width:25%;min-width:120px;max-width:100%;"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/leesharpe/nflseedR/workflows/R-CMD-check/badge.svg)](https://github.com/leesharpe/nflseedR/actions)
<!-- badges: end -->

## Motivation

The goal of nflseedR is to allow NFL modelers to simulate NFL seasons
using the model, by taking the hard work of navigating the complex rules
for division ranking, playoff seeding, and draft order off your plate
and let you focus on your model. This can also aid in sports betting,
such as betting on futures or win totals.

The package can run thousands of simulations of the NFL regular season,
based on a model you input. Within each simulated season, it will
calculate the division standings and playoff seedings for you. It will
also the generate the playoff games and simulate these as well, and
calculate the order for next year’s NFL draft. These can be used to
examine the probability of team making the playoffs or winning the Super
Bowl, based on your model.

The season simulations will take all completed games into account
already, and only simulate from there forward, including if run during
the playoffs. It can also be run as a fresh season, wiping away results
and simulating from scratch.

The season simulation code for nflseedR was developed by Lee Sharpe
([@LeeSharpeNFL](https://twitter.com/leesharpenfl)) and building it as
package was developed by Sebastian Carl
([@mrcaseb](https://twitter.com/mrcaseb)).

## Installation

<!-- You can install the released version of nflseedR from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("nflseedR") -->
<!-- ``` -->

You can install nflseedR from [GitHub](https://github.com/) with:

``` r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("leesharpe/nflseedR")
```

## Get Started

With nflseedR it’s possible to [simulate complete
seasons](https://leesharpe.github.io/nflseedR/articles/articles/nflsim.html)
or use it’s [seeding
functions](https://leesharpe.github.io/nflseedR/articles/articles/nflseedR.html)
in custom simulations.
