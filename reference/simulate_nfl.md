# Simulate an NFL Season

This function simulates a given NFL season multiple times using custom
functions to estimate and simulate game results and computes the outcome
of the given season including playoffs and draft order. It is possible
to run the function in parallel processes by calling the appropriate
[plan](https://future.futureverse.org/reference/plan.html). Progress
updates can be activated by calling
[handlers](https://progressr.futureverse.org/reference/handlers.html)
before the start of the simulations. Please see the below given section
"Details" for further information.

## Usage

``` r
simulate_nfl(
  nfl_season = NULL,
  process_games = NULL,
  ...,
  playoff_seeds = ifelse(nfl_season >= 2020, 7, 6),
  if_ended_today = FALSE,
  fresh_season = FALSE,
  fresh_playoffs = FALSE,
  tiebreaker_depth = 3,
  test_week = NULL,
  simulations = 1000,
  sims_per_round = max(ceiling(simulations/future::availableCores() * 2), 100),
  .debug = FALSE,
  print_summary = FALSE,
  sim_include = c("DRAFT", "REG", "POST")
)
```

## Arguments

- nfl_season:

  Season to simulate

- process_games:

  A function to estimate and simulate the results of games. Uses team,
  schedule, and week number as arguments.

- ...:

  Additional parameters passed on to the function `process_games`.

- playoff_seeds:

  Number of playoff teams per conference (increased in 2020 from 6 to
  7).

- if_ended_today:

  Either `TRUE` or `FALSE`. If TRUE, ignore remaining regular season
  games and cut to playoffs based on current regular season data.

- fresh_season:

  Either `TRUE` or `FALSE`. Whether to blank out all game results and
  simulate the the season from scratch (TRUE) or take game results so
  far as a given and only simulate the rest (FALSE).

- fresh_playoffs:

  Either `TRUE` or `FALSE`. Whether to blank out all playoff game
  results and simulate the postseason from scratch (TRUE) or take game
  results so far as a given and only simulate the rest (FALSE).

- tiebreaker_depth:

  A single value equal to 1, 2, or 3. The default is 3. The value
  controls the depth of tiebreakers that shall be applied. The deepest
  currently implemented tiebreaker is strength of schedule. The
  following values are valid:

  tiebreaker_depth = 1

  :   Break all ties with a coinflip. Fastest variant.

  tiebreaker_depth = 2

  :   Apply head-to-head and division win percentage tiebreakers. Random
      if still tied.

  tiebreaker_depth = 3

  :   Apply all tiebreakers through strength of schedule. Random if
      still tied.

- test_week:

  Aborts after the simulator reaches this week and returns the results
  from your process games call.

- simulations:

  Equals the number of times the given NFL season shall be simulated

- sims_per_round:

  The number of `simulations` can be split into multiple rounds and be
  processed parallel. This parameter controls the number of simulations
  per round. The default value determines the number of locally
  available cores and calculates the number of simulations per round to
  be equal to half of the available cores (various benchmarks showed
  this results in optimal performance).

- .debug:

  Either `TRUE` or `FALSE`. Controls whether additional messages are
  printed to the console showing what the tie-breaking algorithms are
  currently performing.

- print_summary:

  If `TRUE`, prints the summary statistics to the console.

- sim_include:

  One of `"REG"`, `"POST"`, `"DRAFT"` (the default). Simulation will
  behave as follows:

  REG

  :   Simulate the regular season and compute standings, division ranks,
      and playoff seeds

  POST

  :   Do REG + simulate the postseason

  DRAFT

  :   Do POST + compute draft order

## Value

An `nflseedR_simulation` object containing a list of 6 data frames data
frames with the results of all simulated games, the final standings in
each simulated season (incl. playoffs and draft order), summary
statistics across all simulated seasons, and the simulation parameters.
For a full list, please see [the package
website](https://nflseedr.com/articles/articles/nflsim.html#simulation-output).

## Details

### More Speed Using Parallel Processing

We recommend choosing a default parallel processing method and saving it
as an environment variable in the R user profile to make sure all
futures will be resolved with the chosen method by default. This can be
done by following the below given steps.

First, run the following line and the user profile should be opened
automatically. If you haven't saved any environment variables yet, this
will be an empty file.

    usethis::edit_r_environ()

In the opened file add the next line, then save the file and restart
your R session. Please note that this example sets "multisession" as
default. For most users this should be the appropriate plan but please
make sure it truly is.

    R_FUTURE_PLAN="multisession"

After the session is freshly restarted please check if the above method
worked by running the next line. If the output is `FALSE` you
successfully set up a default non-sequential
[`future::plan()`](https://future.futureverse.org/reference/plan.html).
If the output is `TRUE` all functions will behave like they were called
with [`purrr::map()`](https://purrr.tidyverse.org/reference/map.html)
and NOT in multisession.

    inherits(future::plan(), "sequential")

For more information on possible plans please see [the future package
Readme](https://github.com/futureverse/future/blob/develop/README.md).

### Get Progress Updates while Functions are Running

Most nflfastR functions are able to show progress updates using
[`progressr::progressor()`](https://progressr.futureverse.org/reference/progressor.html)
if they are turned on before the function is called. There are at least
two basic ways to do this by either activating progress updates globally
(for the current session) with

    progressr::handlers(global = TRUE)

or by piping the function call into
[`progressr::with_progress()`](https://progressr.futureverse.org/reference/with_progress.html):

    simulate_nfl(2020, fresh_season = TRUE) |>
      progressr::with_progress()

For more information how to work with progress handlers please see
[progressr::progressr](https://progressr.futureverse.org/reference/progressr.html).

## See also

The examples [on the package
website](https://nflseedr.com/articles/articles/nflsim.html)

The method
[`summary.nflseedR_simulation()`](https://nflseedr.com/reference/summary.nflseedR_simulation.md)
that creates a pretty html summary table.

## Examples

``` r
# \donttest{
library(nflseedR)

# Activate progress updates
# progressr::handlers(global = TRUE)

# Parallel processing can be activated via the following line
# future::plan("multisession")

try({#to avoid CRAN test problems
# Simulate the season 4 times in 2 rounds
sim <- nflseedR::simulate_nfl(
  nfl_season = 2020,
  fresh_season = TRUE,
  simulations = 4,
  sims_per_round = 2
)

# Overview output
dplyr::glimpse(sim)
})
#> ℹ 14:45:58 | Loading games data
#> ℹ Computation in multiple rounds can be accelerated with parallel processing.
#> ℹ You should consider calling a `future::plan()`. Please see the function
#>   documentation for further information.
#> ℹ Will go on sequentially...
#> ℹ 14:45:58 | Beginning simulation of 4 seasons in 2 rounds
#> ℹ 14:46:06 | Combining simulation data
#> ℹ 14:46:06 | Aggregating across simulations
#> ℹ 14:46:06 | DONE!
#> List of 6
#>  $ teams       :Classes ‘data.table’ and 'data.frame':   128 obs. of  18 variables:
#>   ..$ sim        : num [1:128] 1 1 1 1 1 1 1 1 1 1 ...
#>   ..$ team       : chr [1:128] "BUF" "MIA" "NE" "NYJ" ...
#>   ..$ conf       : chr [1:128] "AFC" "AFC" "AFC" "AFC" ...
#>   ..$ division   : chr [1:128] "AFC East" "AFC East" "AFC East" "AFC East" ...
#>   ..$ games      : int [1:128] 16 16 16 16 16 16 16 16 16 16 ...
#>   ..$ wins       : num [1:128] 14 11 7 5 5 5 6 10 5 14 ...
#>   ..$ true_wins  : int [1:128] 14 11 7 5 5 5 6 10 5 14 ...
#>   ..$ losses     : int [1:128] 2 5 9 11 11 11 10 6 11 2 ...
#>   ..$ ties       : int [1:128] 0 0 0 0 0 0 0 0 0 0 ...
#>   ..$ win_pct    : num [1:128] 0.875 0.688 0.438 0.312 0.312 ...
#>   ..$ div_pct    : num [1:128] 0.833 0.5 0.333 0.333 0.333 ...
#>   ..$ conf_pct   : num [1:128] 0.833 0.583 0.5 0.333 0.333 ...
#>   ..$ sov        : num [1:128] 0.469 0.426 0.455 0.388 0.4 ...
#>   ..$ sos        : num [1:128] 0.469 0.488 0.492 0.547 0.512 ...
#>   ..$ div_rank   : num [1:128] 1 2 3 4 3 4 2 1 4 1 ...
#>   ..$ seed       : num [1:128] 2 6 NA NA NA NA NA 3 NA 1 ...
#>   ..$ exit       : num [1:128] 19 19 17 17 17 17 17 18 17 20 ...
#>   ..$ draft_order: num [1:128] 28 26 13 11 6 5 12 22 9 30 ...
#>   ..- attr(*, ".internal.selfref")=<externalptr> 
#>  $ games       :Classes ‘data.table’ and 'data.frame':   1076 obs. of  9 variables:
#>   ..$ sim      : num [1:1076] 1 2 1 2 1 2 1 2 1 2 ...
#>   ..$ game_type: chr [1:1076] "REG" "REG" "REG" "REG" ...
#>   ..$ week     : int [1:1076] 1 1 1 1 1 1 1 1 1 1 ...
#>   ..$ away_team: chr [1:1076] "HOU" "HOU" "SEA" "SEA" ...
#>   ..$ home_team: chr [1:1076] "KC" "KC" "ATL" "ATL" ...
#>   ..$ away_rest: num [1:1076] 7 7 7 7 7 7 7 7 7 7 ...
#>   ..$ home_rest: num [1:1076] 7 7 7 7 7 7 7 7 7 7 ...
#>   ..$ location : chr [1:1076] "Home" "Home" "Home" "Home" ...
#>   ..$ result   : int [1:1076] 5 3 -6 -4 14 10 19 18 -12 -23 ...
#>   ..- attr(*, ".internal.selfref")=<externalptr> 
#>  $ overall     : tibble [32 × 11] (S3: tbl_df/tbl/data.frame)
#>   ..$ conf    : chr [1:32] "AFC" "AFC" "AFC" "AFC" ...
#>   ..$ division: chr [1:32] "AFC East" "AFC East" "AFC East" "AFC East" ...
#>   ..$ team    : chr [1:32] "BUF" "MIA" "NE" "NYJ" ...
#>   ..$ wins    : num [1:32] 9.25 9.25 7 8.5 7.25 ...
#>   ..$ playoff : num [1:32] 0.5 0.75 0 0.25 0.25 0.5 0.5 0.75 0 0.75 ...
#>   ..$ div1    : num [1:32] 0.5 0.25 0 0.25 0.25 0 0.5 0.25 0 0.75 ...
#>   ..$ seed1   : num [1:32] 0 0.25 0 0.25 0.25 0 0 0 0 0.25 ...
#>   ..$ won_conf: num [1:32] 0.25 0.25 0 0.25 0 0 0 0 0 0 ...
#>   ..$ won_sb  : num [1:32] 0 0 0 0 0 0 0 0 0 0 ...
#>   ..$ draft1  : num [1:32] 0 0 0 0 0 0 0.25 0 0 0 ...
#>   ..$ draft5  : num [1:32] 0 0.25 0 0 0 0.5 0.25 0 0.25 0 ...
#>  $ team_wins   : tibble [1,056 × 4] (S3: tbl_df/tbl/data.frame)
#>   ..$ team      : chr [1:1056] "ARI" "ARI" "ARI" "ARI" ...
#>   ..$ wins      : num [1:1056] 0 0.5 1 1.5 2 2.5 3 3.5 4 4.5 ...
#>   ..$ over_prob : num [1:1056] 1 1 1 1 1 1 0.5 0.5 0.5 0.5 ...
#>   ..$ under_prob: num [1:1056] 0 0 0 0 0 0 0 0.5 0.5 0.5 ...
#>  $ game_summary: tibble [308 × 11] (S3: tbl_df/tbl/data.frame)
#>   ..$ game_type      : chr [1:308] "REG" "REG" "REG" "REG" ...
#>   ..$ week           : int [1:308] 1 1 1 1 1 1 1 1 1 1 ...
#>   ..$ away_team      : chr [1:308] "ARI" "CHI" "CLE" "DAL" ...
#>   ..$ home_team      : chr [1:308] "SF" "DET" "BAL" "LA" ...
#>   ..$ away_wins      : int [1:308] 2 4 2 0 1 1 3 1 2 2 ...
#>   ..$ home_wins      : int [1:308] 2 0 2 4 3 3 1 3 2 2 ...
#>   ..$ ties           : int [1:308] 0 0 0 0 0 0 0 0 0 0 ...
#>   ..$ result         : num [1:308] 4.75 -19 -0.25 10.25 8.5 ...
#>   ..$ games_played   : int [1:308] 4 4 4 4 4 4 4 4 4 4 ...
#>   ..$ away_percentage: num [1:308] 0.5 1 0.5 0 0.25 0.25 0.75 0.25 0.5 0.5 ...
#>   ..$ home_percentage: num [1:308] 0.5 0 0.5 1 0.75 0.75 0.25 0.75 0.5 0.5 ...
#>  $ sim_params  :List of 14
#>   ..$ nfl_season      : num 2020
#>   ..$ playoff_seeds   : num 7
#>   ..$ if_ended_today  : logi FALSE
#>   ..$ fresh_season    : logi TRUE
#>   ..$ fresh_playoffs  : logi FALSE
#>   ..$ tiebreaker_depth: num 3
#>   ..$ test_week       : NULL
#>   ..$ simulations     : num 4
#>   ..$ sims_per_round  : num 2
#>   ..$ .debug          : logi FALSE
#>   ..$ print_summary   : logi FALSE
#>   ..$ sim_include     : chr "DRAFT"
#>   ..$ nflseedR_version:Classes 'package_version', 'numeric_version'  hidden list of 1
#>   .. ..$ : int [1:4] 2 0 2 9000
#>   ..$ finished_at     : POSIXct[1:1], format: "2026-03-22 14:46:06"
#>  - attr(*, "class")= chr "nflseedR_simulation"
# }
```
