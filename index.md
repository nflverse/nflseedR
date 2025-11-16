# **nflseedR**

## Motivation

The goal of nflseedR is to allow NFL modelers to simulate NFL seasons
using their models, and taking off their plate the work of tracking the
schedule, navigating the complex rules for division ranking, playoff
seeding, and draft order. This can also aid in sports betting, such as
betting on futures or win totals.

The package can run thousands of Monte Carlo style simulations of the
NFL regular season, based on a model you input. Within each simulated
season, it will calculate the division standings and playoff seedings
for you. It will also generate the playoff games and simulate these as
well, and calculate the order for next year’s NFL draft. These can be
used to examine the probability of team making the playoffs or winning
the Super Bowl, based on your model.

The season simulations will take all completed games into account
already, and only simulate from there forward, including if run during
the playoffs.

The season simulation code for nflseedR 1.x was developed by Lee Sharpe
([@LeeSharpeNFL](https://x.com/LeeSharpeNFL)) and building it as package
was developed by Sebastian Carl ([@mrcaseb](https://mrcaseb.com)).

nflseedR 2.0 introduced high efficient standings and simulation
functionality which was developed by Sebastian Carl
([@mrcaseb](https://mrcaseb.com))

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
if (!requireNamespace("pak")) install.packages("pak")
pak::pak("nflverse/nflseedR")
```

or prebuilt from the [development repo](https://nflverse.r-universe.dev)
with:

``` r
install.packages("nflseedR", repos = c("https://nflverse.r-universe.dev", getOption("repos")))
```

## Get Started

With nflseedR it’s possible to

- calculate NFL standings including deep tie breakers and to
- simulate complete NFL seasons.

For more info, please see **[“Getting started with
nflseedR”](https://nflseedr.com/articles/nflseedR.html)**
