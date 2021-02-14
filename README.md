
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nflseedR <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->
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
([@LeeSharpeNFL](<https://twitter.com/leesharpenfl>)) and building it
as package was developed by Sebastian Carl
([@mrcaseb](<https://twitter.com/mrcaseb>)).

## Installation

<!-- You can install the released version of nflseedR from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("nflseedR") -->
<!-- ``` -->

You can install nflseedR from [GitHub](https://github.com/) with:

``` r
if (!requireNamespace("remotes", quietly = TRUE)) {install.packages("remotes")}
remotes::install_github("leesharpe/nflseedR")
```

## Run a Quick Simulation

Let's do a quick simualation! We'll apply the `fresh_season = TRUE` option,
which blanks out all of the game results and simulates everything from scratch.
This is a Monte Carlo style of simulation, meaning that there are seasons
simulated.

By default, 1000 simulations are done. 

``` r
> library(nflseedR)
> sims <- simulate_nfl(fresh_season = TRUE)
* 2021-02-13 13:54:04: Loading games data
* 2021-02-13 13:54:22: Beginning simulation of 1000 seasons in 1 rounds
* 2021-02-13 13:56:22: Combining simulation data
* 2021-02-13 13:56:22: Aggregating across simulations
```

The output contains a lot of pre-aggregated information, as well as the individual
results from each game of each simulation. For example, let's look at the overall
results for the Bears:

*Note: This is example randomized output, your output will differ.*

```
> library(tidyverse)
> sims$overall %>% filter(team == "CHI")
# A tibble: 1 x 11
  conf  division  team   wins playoff  div1 seed1 won_conf won_sb draft1 draft5
  <chr> <chr>     <chr> <dbl>   <dbl> <dbl> <dbl>    <dbl>  <dbl>  <dbl>  <dbl>
1 NFC   NFC North CHI    10.6   0.696 0.301 0.076    0.082  0.042      0  0.002
```

We can see the Bears got 10.6 wins on average. They made the playoffs 70%
of the time, and won the division 30% of the time. They won the Super Bowl in 4%,
and have no realistic shot at the #1 or even a top 5 draft pick. The `teams`
section of the output will show how a team did in each simulated season. 

```
> sims$teams %>% filter(team == "CHI")
# A tibble: 1,000 x 16
     sim team  conf  division  games  wins true_wins win_pct div_pct conf_pct   sov   sos div_rank  seed  exit draft_order
   <dbl> <chr> <chr> <chr>     <int> <dbl>     <int>   <dbl>   <dbl>    <dbl> <dbl> <dbl>    <dbl> <dbl> <dbl>       <dbl>
 1     1 CHI   NFC   NFC North    16    14        14   0.875   1        0.833 0.491 0.496        1     1    22          32
 2     2 CHI   NFC   NFC North    16    12        12   0.75    0.667    0.667 0.438 0.508        2     5    19          27
 3     3 CHI   NFC   NFC North    16     9         9   0.562   0.667    0.5   0.396 0.531        2    NA    17          18
 4     4 CHI   NFC   NFC North    16     9         9   0.562   1        0.667 0.444 0.512        2     6    18          23
 5     5 CHI   NFC   NFC North    16     9         9   0.562   0.5      0.5   0.431 0.535        3    NA    17          18
 6     6 CHI   NFC   NFC North    16    10        10   0.625   0.833    0.583 0.381 0.531        2     7    18          23
 7     7 CHI   NFC   NFC North    16    12        12   0.75    0.667    0.667 0.432 0.531        2     5    18          23
 8     8 CHI   NFC   NFC North    16    13        13   0.812   1        0.833 0.476 0.527        1     3    19          28
 9     9 CHI   NFC   NFC North    16     8         8   0.5     0.5      0.583 0.469 0.531        3     7    18          19
10    10 CHI   NFC   NFC North    16    10        10   0.625   0.5      0.667 0.388 0.531        3     6    18          22
```

Oh hey, the Bears got the 32nd draft pick and Week "22" exit, which both mean they
won the Super Bowl! Let's check the individual games and see how that unfolded.

```
> sims$games %>% filter(sim == 1, game_type != "REG")
# A tibble: 13 x 9
     sim game_type  week away_team home_team away_rest home_rest location result
   <dbl> <chr>     <int> <chr>     <chr>         <dbl>     <dbl> <chr>     <int>
 1     1 WC           18 MIA       JAX               7         7 Home        -51
 2     1 WC           18 PIT       LAC               7         7 Home         -4
 3     1 WC           18 BAL       CIN               7         7 Home        -19
 4     1 WC           18 SF        ARI               7         7 Home         13
 5     1 WC           18 DET       NO                7         7 Home        -18
 6     1 WC           18 DAL       NYG               7         7 Home         17
 7     1 DIV          19 PIT       MIA               7         7 Home          2
 8     1 DIV          19 BAL       NYJ               7        14 Home         10
 9     1 DIV          19 ARI       NYG               7         7 Home         10
10     1 DIV          19 DET       CHI               7        14 Home          1
11     1 CON          20 MIA       NYJ               7         7 Home          9
12     1 CON          20 NYG       CHI               7         7 Home          3
13     1 SB           21 CHI       NYJ              14        14 Neutral     -22
```

Coming off the playoff bye, the Bears squeaked out a 1 point victory over the Lions,
won the NFC Conference Championship by 3 points, but then decisively took care of
the Jets in the Super Bowl.

As you may have gathered at this point, the default simulation code picks a random
Elo for every team, and uses those as the starting Elo ratings for all 32 teams.
However, the Elo will adjust indepedently within each simulation as each week is
simulated.
