# Simulate an NFL Season

Simulate NFL games based on a user provided games/schedule object that
holds matchups with and without results. Missing results are computed
using the argument `compute_results` and possible further arguments to
`compute_results` in `...` (please see
[simulations_verify_fct](https://nflseedr.com/reference/simulations_verify_fct.md)
for further information.).

It is possible to let the function calculate playoff participants and
simulate the post-season. The code is also developed for maximum
performance and allows parallel computation by splitting the number of
simulations into chunks and calling the appropriate
[future::plan](https://future.futureverse.org/reference/plan.html).
Progress updates can be activated by calling
[progressr::handlers](https://progressr.futureverse.org/reference/handlers.html)
before the start of the simulations. Please see the below given section
"Details" for further information.

## Usage

``` r
nfl_simulations(
  games,
  compute_results = nflseedR_compute_results,
  ...,
  playoff_seeds = 7L,
  simulations = 10000L,
  chunks = 8L,
  byes_per_conf = 1L,
  tiebreaker_depth = c("SOS", "PRE-SOV", "RANDOM"),
  sim_include = c("DRAFT", "REG", "POST"),
  verbosity = c("MIN", "MAX", "NONE")
)
```

## Arguments

- games:

  A data frame containing real or simulated game scores. Outside of
  simulations, this is simply the output of
  [nflreadr::load_schedules](https://nflreadr.nflverse.com/reference/load_schedules.html).
  The following variables are required as a minimum:

  sim or season

  :   A season or simulation ID. Normally 1 - n simulated seasons. If
      both sim and season are included, a warning is triggered and work
      continues with sim.

  game_type

  :   One of 'REG', 'WC', 'DIV', 'CON', 'SB' indicating if a game was a
      regular season game or one of the playoff rounds.

  week

  :   The week of the corresponding NFL season.

  away_team

  :   Team abbreviation of the away team (please see
      [`divisions`](https://nflseedr.com/reference/divisions.md) for
      valid team abbreviations).

  home_team

  :   Team abbreviation of the home team (please see
      [`divisions`](https://nflseedr.com/reference/divisions.md) for
      valid team abbreviations).

  result

  :   Equals home score - away score.

  If tiebreakers beyond SOS are to be used, then the actual scores of
  the home (`home_score`) and away (`away_score`) teams must also be
  available.

- compute_results:

  Defaults to the nflseedR function `nflseedR_compute_results`. A
  function to compute results of games. Uses team, schedule, and week
  number as arguments. Please see
  [simulations_verify_fct](https://nflseedr.com/reference/simulations_verify_fct.md)
  for further information.

- ...:

  Additional parameters passed on to the function `compute_results`.

- playoff_seeds:

  If `NULL` (the default), will compute all 16 conference ranks. This
  means, the function applies conference tiebreakers to all conference
  ranks. For better performance, it is possible to set this to a value
  \< 16 to make the function skip tiebreakers of those conference ranks.

- simulations:

  Equals the number of times the given NFL season shall be simulated

- chunks:

  The number of chunks `simulations` should be split into and
  potentially be processed parallel. This parameter controls the number
  of simulations per chunk. There is no obvious way to determine the
  ideal number of chunks in advance because there are too many
  dependencies on the hardware. Too many chunks can be just as slow as
  too few. It is therefore up to the user to determine the optimum
  number themselves.

- byes_per_conf:

  The number of teams with a playoff bye week per conference. This
  number influences the number of wildcard games that are simulated.

- tiebreaker_depth:

  One of `"SOS"`, `"PRE-SOV"`, `"POINTS"` or `"RANDOM"`. Controls which
  tiebreakers are to be applied. The implemented tiebreakers are
  documented here <https://nflseedr.com/articles/tiebreaker.html>. The
  values mean:

  - `"SOS"` (default): Apply all tiebreakers through Strength of
    Schedule. If there are still remaining ties, break them through coin
    toss.

  - `"PRE-SOV"`: Apply all tiebreakers before Strength of Victory. If
    there are still remaining ties, break them through coin toss. Why
    Pre SOV? It's the first tiebreaker that requires knowledge of how
    OTHER teams played.

  - `"POINTS"`: Apply all tiebreakers through point differential. If
    there are still remaining ties, break them through coin toss. This
    will go beyond SOS and requires knowledge of points scored and
    points allowed. As this is not usually part of season simulations,
    caution is advised in this case. These tiebreakers should only be
    used if the scores are real or are deliberately simulated.

  - `"RANDOM"`: Breaks all tiebreakers with a coin toss. I don't really
    know, why I allow this...

- sim_include:

  One of `"REG"`, `"POST"`, `"DRAFT"` (the default). Simulation will
  behave as follows:

  - `"REG"`: Simulate the regular season and compute standings, division
    ranks, and playoff seeds

  - `"POST"`: Do `"REG"` + simulate the postseason

  - `"DRAFT"` (default): Do `"POST"` + compute draft order

- verbosity:

  One of `"MIN"`, `"MAX"`, or `"NONE"` allowing the user to set the
  grade of verbosity of status reports. They mean:

  - `"MIN"` (default): Prints main steps of the process.

  - `"MAX"`: Prints all steps of the complete tiebreaking process.

  - `"NONE"`: No status reports at all. Do this to maximize the
    performance.

## Value

An `nflseedR_simulation` object containing a list of 6 data frames with
the results of all simulated games, the final standings in each
simulated season, summary statistics across all simulated seasons, and
the simulation parameters. For a full list, please see [the package
website](https://nflseedr.com/articles/articles/nflsim2.html#simulation-output).

## Details

### More Speed Using Parallel Processing

We recommend choosing a default parallel processing method and saving it
as an environment variable in the R user profile to make sure all
futures will be resolved with the chosen method by default. This can be
done by following the below given steps.

First, run the below line and the user profile should be opened
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
and **NOT** in multisession.

    inherits(future::plan(), "sequential")

For more information on possible plans please see [the future package
Readme](https://github.com/futureverse/future/blob/develop/README.md).

### Get Progress Updates while Functions are Running

nflseedR is able to show progress updates using
[`progressr::progressor()`](https://progressr.futureverse.org/reference/progressor.html)
if they are turned on before the function is called. There are at least
two basic ways to do this by either activating progress updates globally
(for the current session) with

    progressr::handlers(global = TRUE)

or by piping the function call into
[`progressr::with_progress()`](https://progressr.futureverse.org/reference/with_progress.html):

    nflseedR::nfl_simulations(
      games = nflseedR::sims_games_example,
      simulations = 4,
      chunks = 2
    ) |>
      progressr::with_progress()

For more information how to work with progress handlers please see
[progressr::progressr](https://progressr.futureverse.org/reference/progressr.html).

### Reproducible Random Number Generation (RNG)

It is to be expected that some form of random number generation is
required in the function in argument `compute_results`. For better
performance, nflseedR uses the furrr package to parallelize chunks.
furrr functions are guaranteed to generate the exact same sequence of
random numbers given the same initial seed if, and only if, the initial
seed is of the type "L'Ecuyer-CMRG". So if you want a consistent seed to
be used across all chunks, you must ensure that the correct type is
specified in `set.seed`, e.g. with the following code

    set.seed(5, "L'Ecuyer-CMRG")

It is sufficient to set the seed before nfl_simulations is called. To
check that the type has been set correctly, you can use the following
code.

    RNGkind()
    "L'Ecuyer-CMRG" "Inversion"     "Rejection"

    # Should be a integer vector of length 7
    .Random.seed
    10407  1157214768 -1674567567 -1532971138 -1249749529  1302496508  -253670963

For more information, please see the section "Reproducible random number
generation (RNG)" in
[furrr::furrr_options](https://furrr.futureverse.org/reference/furrr_options.html).

## See also

The examples [on the package
website](https://nflseedr.com/articles/articles/nflsim2.html)

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

sim <- nflseedR::nfl_simulations(
  games = nflseedR::sims_games_example,
  simulations = 4,
  chunks = 2
)
#> ℹ Computation in multiple chunks can be accelerated with parallel processing.
#> ℹ You should consider calling a `future::plan()`. Please see the function
#>   documentation for further information.
#> ℹ Will go on sequentially...
#> This message is displayed once every 8 hours.
#> ℹ 12:40:48 | Start simulation of 4 seasons in 2 chunks with a chunk size of 2.
#> ℹ 12:40:49 | CHUNK #1: Start simulation of regular season weeks "1", "2", "3",
#> …, "17", and "18"
#> ℹ 12:40:49 | Initiate Standings & Tiebreaking Data
#> ℹ 12:40:49 | Compute Division Ranks
#> ℹ 12:40:49 | Compute Conference Ranks
#> ℹ 12:40:49 | CHUNK #1: Start simulation of post season weeks "WC" and "SB"
#> ℹ 12:40:49 | Compute Draft Order
#> ℹ 12:40:49 | CHUNK #2: Start simulation of regular season weeks "1", "2", "3",
#> …, "17", and "18"
#> ℹ 12:40:49 | Initiate Standings & Tiebreaking Data
#> ℹ 12:40:49 | Compute Division Ranks
#> ℹ 12:40:50 | Compute Conference Ranks
#> ℹ 12:40:50 | CHUNK #2: Start simulation of post season weeks "WC" and "SB"
#> ℹ 12:40:50 | Compute Draft Order
#> ℹ 12:40:50 | Combine simulation data
#> ℹ 12:40:50 | Aggregate across simulations
#> ℹ 12:40:50 | DONE!

# Overview output
str(sim, max.level = 3)
#> List of 6
#>  $ standings   :'data.frame':    128 obs. of  20 variables:
#>   ..$ sim               : int [1:128] 1 1 1 1 1 1 1 1 1 1 ...
#>   ..$ team              : chr [1:128] "BUF" "NE" "NYJ" "MIA" ...
#>   ..$ conf              : chr [1:128] "AFC" "AFC" "AFC" "AFC" ...
#>   ..$ division          : chr [1:128] "AFC East" "AFC East" "AFC East" "AFC East" ...
#>   ..$ games             : int [1:128] 16 17 17 17 17 16 17 17 17 17 ...
#>   ..$ wins              : num [1:128] 12 8 8 6 12 11 9 7 9 8 ...
#>   ..$ true_wins         : int [1:128] 12 8 8 6 12 11 9 7 9 8 ...
#>   ..$ losses            : int [1:128] 4 9 9 11 5 5 8 10 8 9 ...
#>   ..$ ties              : int [1:128] 0 0 0 0 0 0 0 0 0 0 ...
#>   ..$ win_pct           : num [1:128] 0.75 0.471 0.471 0.353 0.706 ...
#>   ..$ div_pct           : num [1:128] 0.667 0.5 0.5 0.333 0.833 ...
#>   ..$ conf_pct          : num [1:128] 0.818 0.5 0.5 0.333 0.667 ...
#>   ..$ sov               : num [1:128] 0.456 0.423 0.43 0.525 0.47 ...
#>   ..$ sos               : num [1:128] 0.474 0.495 0.528 0.544 0.491 ...
#>   ..$ div_rank          : int [1:128] 1 2 3 4 1 2 3 4 1 2 ...
#>   ..$ div_tie_broken_by : chr [1:128] NA "Head-To-Head Win PCT (2)" "Head-To-Head Win PCT (2)" NA ...
#>   ..$ conf_rank         : int [1:128] 1 9 10 13 2 5 7 11 4 8 ...
#>   ..$ conf_tie_broken_by: chr [1:128] NA "Division Tiebreaker" "Division Tiebreaker" "Conference Win PCT (2)" ...
#>   ..$ exit              : chr [1:128] "DIV" "REG" "REG" "REG" ...
#>   ..$ draft_rank        : int [1:128] 27 13 15 10 22 28 31 11 24 14 ...
#>  $ games       :'data.frame':    1136 obs. of  9 variables:
#>   ..$ game_type: chr [1:1136] "REG" "REG" "REG" "REG" ...
#>   ..$ week     : int [1:1136] 1 1 1 1 1 1 1 1 1 1 ...
#>   ..$ away_team: chr [1:1136] "BUF" "NO" "CLE" "SF" ...
#>   ..$ home_team: chr [1:1136] "LA" "ATL" "CAR" "CHI" ...
#>   ..$ away_rest: int [1:1136] 7 7 7 7 7 7 7 7 7 7 ...
#>   ..$ home_rest: int [1:1136] 7 7 7 7 7 7 7 7 7 7 ...
#>   ..$ location : chr [1:1136] "Home" "Home" "Home" "Home" ...
#>   ..$ result   : int [1:1136] -3 -1 -2 13 -3 -3 0 54 -15 6 ...
#>   ..$ sim      : int [1:1136] 1 1 1 1 1 1 1 1 1 1 ...
#>  $ overall     :'data.frame':    32 obs. of  11 variables:
#>   ..$ conf    : chr [1:32] "AFC" "AFC" "AFC" "AFC" ...
#>   ..$ division: chr [1:32] "AFC East" "AFC East" "AFC East" "AFC East" ...
#>   ..$ team    : chr [1:32] "BUF" "MIA" "NE" "NYJ" ...
#>   ..$ wins    : num [1:32] 11 6.5 8.25 8.5 11 11.5 7.25 9.25 3.25 4.5 ...
#>   ..$ playoff : num [1:32] 1 0 0 0 1 1 0 0.5 0 0 ...
#>   ..$ div1    : num [1:32] 1 0 0 0 0.5 0.5 0 0 0 0 ...
#>   ..$ seed1   : num [1:32] 0.25 0 0 0 0 0.25 0 0 0 0 ...
#>   ..$ won_conf: num [1:32] 0 0 0 0 0 0 0 0.5 0 0 ...
#>   ..$ won_sb  : num [1:32] 0 0 0 0 0 0 0 0.5 0 0 ...
#>   ..$ draft1  : num [1:32] 0 0 0 0 0 0 0 0 0.5 0 ...
#>   ..$ draft5  : num [1:32] 0 0 0 0 0 0 0 0 1 1 ...
#>  $ team_wins   :'data.frame':    1120 obs. of  4 variables:
#>   ..$ team      : chr [1:1120] "ARI" "ARI" "ARI" "ARI" ...
#>   ..$ wins      : num [1:1120] 0 0.5 1 1.5 2 2.5 3 3.5 4 4.5 ...
#>   ..$ over_prob : num [1:1120] 1 1 1 1 1 1 1 1 1 1 ...
#>   ..$ under_prob: num [1:1120] 0 0 0 0 0 0 0 0 0 0 ...
#>  $ game_summary:'data.frame':    284 obs. of  11 variables:
#>   ..$ game_type      : chr [1:284] "CON" "CON" "DIV" "DIV" ...
#>   ..$ week           : int [1:284] 21 21 20 20 20 20 1 1 1 1 ...
#>   ..$ away_team      : chr [1:284] "CIN" "SF" "CIN" "DAL" ...
#>   ..$ home_team      : chr [1:284] "KC" "PHI" "BUF" "SF" ...
#>   ..$ away_wins      : int [1:284] 0 0 4 0 0 0 4 3 4 0 ...
#>   ..$ home_wins      : int [1:284] 4 4 0 4 4 4 0 1 0 4 ...
#>   ..$ ties           : int [1:284] 0 0 0 0 0 0 0 0 0 0 ...
#>   ..$ result         : num [1:284] 3 24 -17 7 7 31 -15 4.75 -2 1 ...
#>   ..$ games_played   : int [1:284] 4 4 4 4 4 4 4 4 4 4 ...
#>   ..$ away_percentage: num [1:284] 0 0 1 0 0 0 1 0.75 1 0 ...
#>   ..$ home_percentage: num [1:284] 1 1 0 1 1 1 0 0.25 0 1 ...
#>  $ sim_params  :List of 10
#>   ..$ nfl_season      : int 2022
#>   ..$ playoff_seeds   : int 7
#>   ..$ simulations     : num 4
#>   ..$ chunks          : num 2
#>   ..$ byes_per_conf   : int 1
#>   ..$ tiebreaker_depth: chr "SOS"
#>   ..$ sim_include     : chr "DRAFT"
#>   ..$ verbosity       : chr "MIN"
#>   ..$ nflseedR_version:Classes 'package_version', 'numeric_version'  hidden list of 1
#>   .. ..$ : int [1:3] 2 0 2
#>   ..$ finished_at     : POSIXct[1:1], format: "2025-11-16 12:40:50"
#>  - attr(*, "class")= chr "nflseedR_simulation"
# }
```
