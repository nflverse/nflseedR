
<!-- README.md is generated from README.Rmd. Please edit that file -->

# **nflseedR** <img src='man/figures/logo.png' align="right" width="25%" min-width="120px"/>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version-last-release/nflseedR)](https://CRAN.R-project.org/package=nflseedR)
[![CRAN
downloads](http://cranlogs.r-pkg.org/badges/grand-total/nflseedR)](https://CRAN.R-project.org/package=nflseedR)
[![Dev
status](https://img.shields.io/github/r-package/v/nflverse/nflseedR/master?label=dev%20version&style=flat-square&logo=github)](https://nflseedr.com/)
[![R-CMD-check](https://github.com/nflverse/nflseedR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nflverse/nflseedR/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![nflverse
support](https://img.shields.io/discord/789805604076126219?color=7289da&label=nflverse%20support&logo=discord&logoColor=fff&style=flat-square)](https://discord.com/invite/5Er2FBnnQa)
<!-- badges: end -->

## Motivation

The goal of nflseedR is to allow NFL modelers to simulate NFL seasons
using their models, and taking off their plate the work of tracking the
schedule, navigating the complex rules for division ranking, playoff
seeding, and draft order. This can also aid in sports betting, such as
betting on futures or win totals.

The package can run thousands of Monte Carlo style simulations of the
NFL regular season, based on a model you input. Within each simulated
season, it will calculate the division standings and playoff seedings
for you. It will also the generate the playoff games and simulate these
as well, and calculate the order for next year’s NFL draft. These can be
used to examine the probability of team making the playoffs or winning
the Super Bowl, based on your model.

The season simulations will take all completed games into account
already, and only simulate from there forward, including if run during
the playoffs. It can also be run as a fresh season, wiping away results
and simulating from scratch.

The season simulation code for nflseedR was developed by Lee Sharpe
([@LeeSharpeNFL](https://twitter.com/leesharpenfl)) and building it as
package was developed by Sebastian Carl
([@mrcaseb](https://twitter.com/mrcaseb)). New high efficient standings
code was developed by Sebastian Carl
([@mrcaseb](https://twitter.com/mrcaseb))

## Installation

The easiest way to get nflseedR is to install it from
[CRAN](https://cran.r-project.org/package=nflseedR) with:

``` r
install.packages("nflseedR")
```

To get a bug fix or to use a feature from the development version, you
can install the development version of nflseedR either from
[GitHub](https://github.com/nflverse/nflseedR) with

``` r
if (!require("pak")) install.packages("pak")
pak::pak("nflverse/nflseedR")
```

or prebuilt from the [development repo](https://nflverse.r-universe.dev)
with:

``` r
install.packages("nflseedR", repos = c("https://nflverse.r-universe.dev", getOption("repos")))
```

## Get Started

With nflseedR it’s possible to [simulate complete
seasons](https://nflseedr.com/articles/articles/nflsim.html) or use it’s
[seeding
functions](https://nflseedr.com/articles/articles/nflseedR.html) in
custom simulations.
