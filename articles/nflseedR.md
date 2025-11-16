# Get started with nflseedR

## Preface

nflseedR essentially performs two tasks:

1.  Calculation of NFL standings with
    [`nfl_standings()`](https://nflseedr.com/reference/nfl_standings.md)
    based on game results of one or more seasons, especially taking into
    account the comprehensive and sometimes complicated tie-breaking
    procedures for division ranks, conference seeds and the draft order.
    Read [this article](https://nflseedr.com/articles/tiebreaker.html)
    for further information on the implemented tie-breakers.
2.  Running thousands of simulations (Monte Carlo style) of an NFL
    season with
    [`nfl_simulations()`](https://nflseedr.com/reference/nfl_simulations.md).
    The standings from point 1 and especially the conference seeds are
    needed to determine playoff participants. Basically, the first point
    only exists because we need it to carry out the simulations.

The actual core of a simulation is the generation of game results based
on any information that the user deems important. This is why nflseedR
is virtually extensible. By default, a simple ELO model is implemented
that works with initial starting ELO values and updates them from week
to week based on game results. However, the user can write their own
function for calculating game results and pass it to nflseedR together
with any additional data that may be required.

## Usage

### Standings

We need real or simulated match data to determine standings. The
required variables are specified in the documentation of the function
[`nfl_standings()`](https://nflseedr.com/reference/nfl_standings.md).

Here are games data from the 2023 and 2024 seasons.

``` r
games <- nflreadr::load_schedules(2023:2024)
```

We can pass this data directly to nflseedR and calculate standings. It
defaults to compute division ranks as well as conference ranks for all
teams and it applies tiebreakers through strength of schedule.

``` r
standings <- nflseedR::nfl_standings(games, ranks = "DRAFT")
#> ℹ 12:41:13 | Initiate Standings & Tiebreaking Data
#> ℹ 12:41:13 | Compute Division Ranks
#> ℹ 12:41:13 | Compute Conference Ranks
#> ℹ 12:41:13 | Compute Draft Order
# Let's view the structure of the output
str(standings, max.level = 1, width = 50, strict.width = "cut")
#> Classes 'data.table' and 'data.frame':   64 obs. of  24 variables:
#>  $ season             : int  2023 2023 2023 2023..
#>  $ team               : chr  "BUF" "MIA" "NYJ" "..
#>  $ conf               : chr  "AFC" "AFC" "AFC" "..
#>  $ division           : chr  "AFC East" "AFC Ea"..
#>  $ games              : int  17 17 17 17 17 17 1..
#>  $ wins               : num  11 11 7 4 13 11 10 ..
#>  $ true_wins          : int  11 11 7 4 13 11 10 ..
#>  $ losses             : int  6 6 10 13 4 6 7 8 7..
#>  $ ties               : int  0 0 0 0 0 0 0 0 0 0..
#>  $ pf                 : int  451 496 268 236 483..
#>  $ pa                 : int  311 391 355 366 280..
#>  $ pd                 : int  140 105 -87 -130 20..
#>  $ win_pct            : num  0.647 0.647 0.412 0..
#>  $ div_pct            : num  0.667 0.667 0.333 0..
#>  $ conf_pct           : num  0.583 0.583 0.333 0..
#>  $ sov                : num  0.471 0.358 0.454 0..
#>  $ sos                : num  0.471 0.45 0.502 0...
#>  $ div_rank           : int  1 2 3 4 1 2 3 4 1 2..
#>  $ div_tie_broken_by  : chr  "Head-To-Head Win "..
#>  $ conf_rank          : int  2 6 13 16 1 5 7 8 4..
#>  $ conf_tie_broken_by : chr  "Head-To-Head Swee"..
#>  $ exit               : chr  "DIV" "WC" "REG" ""..
#>  $ draft_rank         : int  28 21 10 3 30 23 20..
#>  $ draft_tie_broken_by: chr  NA NA NA NA ...
#>  - attr(*, ".internal.selfref")=<externalptr>
```

nflseedR also provides functionality to create a “pretty” html table
using the [gt](https://gt.rstudio.com) package. Use
[`nfl_standings_prettify()`](https://nflseedr.com/reference/nfl_standings_prettify.md)
with the output of
[`nfl_standings()`](https://nflseedr.com/reference/nfl_standings.md) to
create the table. It allows grouping by division, conference or overall
and it can sort by division rank, conference rank (seed), and draft
rank.

The default groups by division and sorts by division rank.

``` r
# It doesn't allow more than one season
s <- standings[season == 2024]
nflseedR::nfl_standings_prettify(s)
```

[TABLE]

But we can also do things like ordering the complete league by draft
rank.

``` r
nflseedR::nfl_standings_prettify(s, grp_by = "nfl", order_by = "draft_rank")
```

[TABLE]

Please note that
[`nfl_standings_prettify()`](https://nflseedr.com/reference/nfl_standings_prettify.md)
returns a [`gt::gt()`](https://gt.rstudio.com/reference/gt.html) table
so you can change it according to your own preferences.

### Simulations

With nflseedR 2.0, we have rethought and implemented the execution of
simulations from scratch. Particular attention was paid to flexibility
and performance. As the usage of the new function
[`nfl_simulations()`](https://nflseedr.com/reference/nfl_simulations.md)
differs from the old function
[`simulate_nfl()`](https://nflseedr.com/reference/simulate_nfl.md), we
will keep both variants for the time being and maintain two separate
articles explaining how to use them.

It is strongly recommended to switch to
[`nfl_simulations()`](https://nflseedr.com/reference/nfl_simulations.md)
because it is far superior to the old implementation in practically
every respect, especially in terms of performance.

- Go to [this article](https://nflseedr.com/articles/nflsim2.html) for a
  detailed explanation of how to use
  [`nfl_simulations()`](https://nflseedr.com/reference/nfl_simulations.md)
- Go to [this article](https://nflseedr.com/articles/nflsim.html) for a
  detailed explanation of how to use
  [`simulate_nfl()`](https://nflseedr.com/reference/simulate_nfl.md)
