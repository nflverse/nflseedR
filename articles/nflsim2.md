# Simulating NFL seasons using nflseedR 2.0

nflseedR 2.0 introduced a new approach to NFL season simulations with
the new function
[`nfl_simulations()`](https://nflseedr.com/reference/nfl_simulations.md).
It is a new approach with a different api and a significant performance
boost compared to
[`simulate_nfl()`](https://nflseedr.com/reference/simulate_nfl.md).

The general idea is that the user only passes a table with NFL schedule
data to the function. Then all missing results are simulated. It is
possible to limit the simulations to the regular season or to simulate
playoffs afterwards. You can also switch on the determination of the
draft order if required.

What are the advantages of this approach?

1.  You have full control over which games are simulated.
2.  It is even easier to answer questions like “what if team x wins the
    next two games?”
3.  You can put together your own playoffs.
4.  Waiving postseason or draft order improves run time.

It is still possible to pass your own function for determining the
weekly results and all the additional data required for this to the
simulator. The user has full control and nflseedR takes care of
standings, tiebreakers and everything else with maximum efficiency.

## Function API

The detailed documentation of the function parameters can be found
directly in the function help page (click
[`nfl_simulations()`](https://nflseedr.com/reference/nfl_simulations.md)).
This section explains the big picture.

At its core, nflseedR only requires 2 things to run NFL simulations.

1.  The `games` of a season for which missing results are to be
    simulated.
2.  A function to `compute_results` of those games.

Simulations are performed on a weekly basis. The reason for this is that
Elo-based approaches have proven successful for NFL simulations. This
requires updating Elo values based on one week’s results and using the
new values for next week’s simulation. nflseedR provides a default
function for `compute_results` which is named `nflseedR_compute_results`
so that the simulator is functional right from the start.

If not provided by the user (we’ll discuss how to do this later), the
function picks a random Elo for every team, and uses those as the
starting Elo ratings for all teams. It will adjust Elo ratings within
each simulation as each week is simulated. (The Elo model used is
loosely based off of that of
[FiveThirtyEight](https://fivethirtyeight.com/methodology/how-our-nfl-predictions-work/).)

## Quick Start

Let’s do a quick run-through and look at the output. In the first step
we use nflseedR’s `compute_result` function and let this function
compute random Elo values at the beginning of the simulation. In this
case, all we need is a list of games. There is also an example of this
for demonstration purposes. We will now invoke it.

``` r
all_games <- nflseedR::sims_games_example |> dplyr::filter(game_type == "REG")
DT::datatable(all_games)
```

The example is the entire 2022 NFL regular season in which random
results were removed. Now we use this list of games and let nflseedR
simulate the missing results. Once the regular season is over, it
determines the playoff participants and then simulates the playoffs. For
this example, we choose 4 simulations in 2 chunks. Of course, 4
simulations is not a realistic number. Commonly 10k simulations are
performed, but the author strongly recommends that more than 10k
simulations are carried out. At least 25k, better 50k. Due to the
efficiency of the new simulator, this is no longer a problem.

``` r
sims <- nflseedR::nfl_simulations(
  games = all_games,
  simulations = 4,
  chunks = 2
)
#> ℹ 14:52:37 | Start simulation of 4 seasons in 2 chunks with a chunk size of 2.
#> ℹ 14:52:39 | CHUNK #1: Start simulation of regular season weeks "1", "2", "3",
#> …, "17", and "18"
#> ℹ 14:52:39 | Initiate Standings & Tiebreaking Data
#> ℹ 14:52:39 | Compute Division Ranks
#> ℹ 14:52:39 | Compute Conference Ranks
#> ℹ 14:52:39 | CHUNK #1: Start simulation of post season weeks "WC", "DIV",
#> "CON", and "SB"
#> ℹ 14:52:39 | Compute Draft Order
#> ℹ 14:52:39 | CHUNK #2: Start simulation of regular season weeks "1", "2", "3",
#> …, "17", and "18"
#> ℹ 14:52:39 | Initiate Standings & Tiebreaking Data
#> ℹ 14:52:39 | Compute Division Ranks
#> ℹ 14:52:39 | Compute Conference Ranks
#> ℹ 14:52:39 | CHUNK #2: Start simulation of post season weeks "WC", "DIV",
#> "CON", and "SB"
#> ℹ 14:52:39 | Compute Draft Order
#> ℹ 14:52:39 | Combine simulation data
#> ℹ 14:52:39 | Aggregate across simulations
#> ℹ 14:52:39 | DONE!
```

Before we take a closer look at the contents of a simulation, we will
first use this example in the next section to get an overview of the
output of an nflseedR simulation.

## Simulation Output

The output of
[`nfl_simulations()`](https://nflseedr.com/reference/nfl_simulations.md)
is a list of class `"nflseedR_simulation"` that holds five data frames
with simulation results as well as a list of parameters used in the
simulation. Here are the contents of each:

**standings**

One row per team per simulation. Variable names and their descriptions
are provided in the table below.

**games**

One row per game per simulation. Variable names and their descriptions
are provided in the table below.

**overall**

One row per team, aggregated across all simulations. Sorted by `conf`
then `division` then `team`. This is a good reference for looking at
futures bets, since this should represent your model’s chances of
various things happening for each team. Variable names and their
descriptions are provided in the table below.

**team_wins**

Each team has a row for 0 wins, 0.5 wins, 1 win, 1.5 wins, … , 15.5
wins, 16 wins. These are aggregated across all simulations and are
intending to represent your model’s probability for teams being below or
above a certain win total, making them an excellent reference for using
your model to make win total bets. Variable names and their descriptions
are provided in the table below.

**game_summary**

One row per game, aggregated across all simulations. Sorted by
`game_type` then `week` then `away_team` then `home_team`. Variable
names and their descriptions are provided in the table below.

**sim_params**

a list of parameters used in the simulation. The list contains the
following parameters which are described in
[`nfl_simulations()`](https://nflseedr.com/reference/nfl_simulations.md)

- `nfl_season`
- `playoff_seeds`
- `simulations`
- `chunks`
- `byes_per_conf`
- `tiebreaker_depth`
- `sim_include`
- `verbosity`
- `sims_per_round`
- `nflseedR_version`
- `finished_at`

## Realistic Example

In the quick start example above, we have simulated a few games of the
2022 season based on randomly generated Elo values. Now let’s run
through a slightly more realistic example. This time we simulate the
complete 2024 season and use market implied team ratings.

First of all, we need the games. We load the 2024 season and remove the
postseason, because we want to simulate the postseason as well. We also
remove all results because nflseedR only simulates missing results.

``` r
games <- nflreadr::load_schedules(2024) |> 
  dplyr::filter(game_type == "REG") |> 
  dplyr::mutate(
    result = NA_integer_,
    away_score = NA_integer_,
    home_score = NA_integer_
  )
```

Now we need the team ratings. The following table shows market implied
team spreads. These are from August 2024 and are based on all look-ahead
point spreads from various sports books at that time just before the
2024 season started. The number represents the points the corresponding
team would be favored by against an average team on a neutral field.

    #>  ARI  ATL  BAL  BUF  CAR  CHI  CIN  CLE  DAL  DEN  DET   GB  HOU  IND  JAX   KC 
    #> -2.4  0.0  3.9  2.1 -5.0 -0.2  2.9  1.1  2.1 -4.0  2.6  1.8  1.8 -0.6  0.1  4.5 
    #>   LA  LAC   LV  MIA  MIN   NE   NO  NYG  NYJ  PHI  PIT  SEA   SF   TB  TEN  WAS 
    #>  0.9 -0.7 -2.7  1.6 -2.1 -4.5 -2.2 -3.5  2.1  2.0 -0.6 -1.5  5.2 -1.8 -3.2 -3.2

We use a very simple approach to convert the spreads into Elo ratings.

``` r
team_ratings$elo <- 1500 + 25 * team_ratings$spread
```

The default `compute_results` function expects team ratings as named
vector.

``` r
team_ratings <- setNames(team_ratings$elo, team_ratings$team)
print(team_ratings)
#>  ARI  ATL  BAL  BUF  CAR  CHI  CIN  CLE  DAL  DEN  DET   GB  HOU  IND  JAX   KC 
#> 1440 1500 1598 1552 1375 1495 1572 1528 1552 1400 1565 1545 1545 1485 1502 1612 
#>   LA  LAC   LV  MIA  MIN   NE   NO  NYG  NYJ  PHI  PIT  SEA   SF   TB  TEN  WAS 
#> 1522 1482 1432 1540 1448 1388 1445 1412 1552 1550 1485 1462 1630 1455 1420 1420
```

We have the games and Elo ratings going into the season. We can now use
them to simulate, choose 50k simulations and turn off user messaging
with the `verbosity` argument for maximum speed. Don’t be afraid of 50k
simulations. At the time of writing, this code chunk takes less than 90
seconds to run on a 2022 MacBook Air with M2 chip.

``` r
# We set a seed for reproducible results
# Please see section "Reproducible Random Number Generation (RNG)" in the 
# help page of nfl_simulations for more details on the seed type "L'Ecuyer-CMRG"
set.seed(5, "L'Ecuyer-CMRG")

sims <- nflseedR::nfl_simulations(
  games = games,
  elo = team_ratings,
  simulations = 50000,
  chunks = 20,
  verbosity = "NONE"
)
#> ℹ 14:52:41 | Start simulation of 50 000 seasons in 20 chunks with a chunk size of 2 500.
#> ℹ 14:57:28 | DONE!
```

Please pay attention to how we pass the team ratings to the simulation.
The function
[`nfl_simulations()`](https://nflseedr.com/reference/nfl_simulations.md)
provides dots `...` as function argument. You can pass any R object as
argument to the dots as long as the argument is named. In your
`compute_results` function you then only have to search the dots for the
objects you need. The default function does exactly that. It searches
for the object named `elo`, and if this object exists, then these values
are used as initial Elo values and then updated from week to week. More
details can be found in the source code of the function
`nflseedR_compute_results` (see next chunk).

    #> This is the source code of `nflseedR_compute_results`. 
    #> Note that it is written in data.table for maximum performance and 
    #> consistency with the rest of the simulation code. 
    #> You are not required to write your own function in data.table.
    #> function(teams, games, week_num, ...) {
    #>   # this example estimates at PK/0 and 50%
    #>   # estimate = is the median spread expected (positive = home team favored)
    #>   # wp = is the probability of the team winning the game
    #>   #
    #>   # only simulate games through week week_num
    #>   # only simulate games with is.na(result)
    #>   # result = how many points home team won by
    #>   # round out (away from zero)
    #>   round_out <- function(x) {
    #>     x <- x[!is.na(x)]
    #>     x[x < 0] <- floor(  x[x < 0])
    #>     x[x > 0] <- ceiling(x[x > 0])
    #>     as.integer(x)
    #>   }
    #>   if (!data.table::is.data.table(games)) data.table::setDT(games)
    #>   if (!data.table::is.data.table(teams)) data.table::setDT(teams)
    #>   games_indices <- data.table::indices(games)
    #>   if (is.null(games_indices) || !"week" %chin% games_indices){
    #>     data.table::setindexv(games, c("week", "location", "game_type"))
    #>   }
    #>   # get elo if not in teams data already
    #>   # elo is expected to be a named vector of elo ratings where
    #>   # names are NFL team abbreviations
    #>   if (!"elo" %chin% colnames(teams)) {
    #>     # Query arguments in dots and see if elo is in there
    #>     # if not, set it to random values
    #>     args <- list(...)
    #>     if ("elo" %chin% names(args)) {
    #>       # pull from custom arguments
    #>       ratings <- args$elo
    #>       teams[, elo := ratings[team]]
    #>     } else {
    #>       # if custom elo is missing in dots, start everyone at a random elo
    #>       ratings <- setNames(
    #>         rnorm(length(unique(teams$team)), 1500, 150),
    #>         unique(teams$team)
    #>       )
    #>       teams[, elo := ratings[team]]
    #>     }
    #>   }
    #>   # At this point, there is a column named "elo" in the teams data
    #>   # We use that column to create a elo lookup vector that we use to
    #>   # add those elo ratings to home and away teams in the games data
    #>   # Names of that vector are built off sim and team to make sure
    #>   # we don't mix elo values of one team across simulations
    #>   ratings <- teams[, setNames(elo, paste(sim, team, sep = "-"))]
    #>   # fill elo values of home and away teams
    #>   games[list(week_num), away_elo := ratings[paste(sim, away_team, sep = "-")], on = "week"]
    #>   games[list(week_num), home_elo := ratings[paste(sim, home_team, sep = "-")], on = "week"]
    #>   # create elo diff
    #>   games[list(week_num), elo_diff := home_elo - away_elo + (home_rest - away_rest) / 7 * 25, on = "week"]
    #>   # adjust elo diff for location = HOME
    #>   games[list(week_num, "Home"), elo_diff := elo_diff + 20, on = c("week", "location")]
    #>   # adjust elo_diff for postseason game types
    #>   games[list(week_num, c("WC", "DIV", "CON", "SB")), elo_diff := elo_diff * 1.2, on = c("week", "game_type")]
    #>   # create wp and estimate
    #>   games[list(week_num), `:=`(wp = 1 / (10^(-elo_diff / 400) + 1),
    #>                        estimate = elo_diff / 25), on = "week"]
    #>   # adjust result in current week
    #>   games[list(week_num) == week & is.na(result),
    #>         result := round_out(rnorm(.N, estimate, 13))]
    #>   # compute elo shift
    #>   games[list(week_num), `:=`(
    #>     outcome = data.table::fcase(
    #>       is.na(result), NA_real_,
    #>       result > 0, 1,
    #>       result < 0, 0,
    #>       default = 0.5
    #>     ),
    #>     elo_input = data.table::fcase(
    #>       is.na(result), NA_real_,
    #>       result > 0, elo_diff * 0.001 + 2.2,
    #>       result < 0, -elo_diff * 0.001 + 2.2,
    #>       default = 1.0
    #>     )
    #>   ), on = "week"]
    #>   games[list(week_num), elo_mult := log(pmax(abs(result), 1) + 1.0) * 2.2 / elo_input, on = "week"]
    #>   games[list(week_num), elo_shift := 20 * elo_mult * (outcome - wp), on = "week"]
    #>   # Build elo_shift vector based on results if home and away teams
    #>   elo_change_away <- games[list(week_num), setNames(-elo_shift, paste(sim, away_team, sep = "-")), on = "week"]
    #>   elo_change_home <- games[list(week_num), setNames(elo_shift,  paste(sim, home_team, sep = "-")), on = "week"]
    #>   elo_change <- c(elo_change_away, elo_change_home)
    #>   # drop helper columns
    #>   drop_cols <- c("away_elo", "home_elo", "elo_diff", "wp", "estimate",
    #>                  "outcome", "elo_input", "elo_mult", "elo_shift")
    #>   games[, (drop_cols) := NULL]
    #>   # apply elo shift in teams data to transport new elo values to next week
    #>   teams[, elo_shift := elo_change[paste(sim, team, sep = "-")]]
    #>   # teams that didn't play that week are missing in elo_change. Their shift
    #>   # value will be NA. We set it to 0 to be able to add the shift for all teams
    #>   teams[, elo_shift := data.table::fifelse(is.na(elo_shift), 0, elo_shift)]
    #>   teams[, elo := elo + elo_shift]
    #>   # remove the shift variable for this round
    #>   teams[, elo_shift := NULL]
    #>   list("teams" = teams, "games" = games)
    #> }

The output contains a lot of pre-aggregated information, as well as the
individual results from each game of each simulation. For example, let’s
look at the overall results of the Chargers:

``` r
sims$overall |> dplyr::filter(team == "LAC") |> knitr::kable()
```

| conf | division | team | wins | playoff | div1 | seed1 | won_conf | won_sb | draft1 | draft5 |
|:-----|:---------|:-----|-----:|--------:|-----:|------:|---------:|-------:|-------:|-------:|
| AFC  | AFC West | LAC  | 8.56 |   0.418 | 0.22 | 0.043 |     0.04 |  0.018 |  0.026 |  0.145 |

We can see the Chargers got 8.6 wins on average. They made the playoffs
42% of simulations, won the division in 22%, won the Super Bowl in 2%,
and in 15% did they receive a top five draft pick. The `teams` section
of the output will show how a team did in each simulated season.

Now let nflseedR summarize the simulation for you by using
[`summary()`](https://rdrr.io/r/base/summary.html) with the nflseedR
simulation object. This will print a gt table.

``` r
summary(sims)
```

[TABLE]

## Use Your Own Model

### User-defined `compute_results`

But of course the real value of nflseedR is putting in your own model
into the simulator. To accomplish this, you can write your own
`compute_results` function which will determine the outcome of games
instead.

Now it gets important: **nflseedR will use your function during the
simulations without any checks**. The reason for this is that we want
maximum performance and such tests would slow us down. In addition,
repeated checks are not necessary if we have checked once in advance
that the function works as nflseedR expects it to. And this is exactly
what
[`simulations_verify_fct()`](https://nflseedr.com/reference/simulations_verify_fct.md)
is for. So if you want to use your own `compute_results` function, all
you need to do is check how it works in advance with
[`simulations_verify_fct()`](https://nflseedr.com/reference/simulations_verify_fct.md).
If there are no errors, then your function works exactly as nflseedR
needs it to simulate. Please see the documentation of
[`simulations_verify_fct()`](https://nflseedr.com/reference/simulations_verify_fct.md)
for a detailed description of expected `compute_results` behavior.

As an example for a custom `compute_results` function, here’s a very
stupid model that makes the team earlier alphabetically win by 3 points
90% of the time, and lose by 3 points the other 10% of the time.

``` r
stupid_games_model <- function(teams, games, week_num, ...) {
  # make the earlier alphabetical team win 90% of the time
  games <- games |> 
    dplyr::mutate(
      result = dplyr::case_when(
        !is.na(result) | week != week_num ~ result,
        away_team < home_team ~
          sample(c(-3, 3), dplyr::n(), prob = c(0.9, 0.1), replace = TRUE),
        away_team > home_team ~
          sample(c(-3, 3), dplyr::n(), prob = c(0.1, 0.9), replace = TRUE),
        TRUE ~ 0
      )
    )

  # return values
  list(teams = teams, games = games)
}
```

When you create this function, the first two inputs are data on the
teams (one row per team per sim), and data on the games (one row per
game per sim). The third argument is the week number currently being
simulated, as only one week is processed at a time.

Your function’s job - by whatever means you choose - is to update the
`result` column for that week’s games in each of the sims with the
number of points the home team won by (or lost by if negative, or 0 if
the game ended in a tie).

It returns both the `teams` and the `games` data. It does both because
this way you can store information in new columns by team or by game to
use in the next call. For example, the default function updates a team’s
Elo after the game, and stores it in the `teams` data. When the
simulator processes the next week, it uses the updated Elo rating to
inform the team’s next game.

We can verify that the function works as required

``` r
nflseedR::simulations_verify_fct(stupid_games_model)
#> ✔ No problems found!
```

Let’s run a simulation with `stupid_games_model` and see what happens:

``` r
sims2 <- nflseedR::nfl_simulations(
  games = games,
  compute_results = stupid_games_model,
  simulations = 500,
  chunks = 1,
  verbosity = "NONE"
)
#> ℹ You have provided your own function to `compute_results`, cool!
#> ℹ To maximize performance, `nfl_simulations()` does not control the output of
#>   your function during the simulation. Please use `simulations_verify_fct()` in
#>   advance to ensure that you do not get any unexpected results or errors.
#> ℹ 14:57:30 | Start simulation of 500 seasons in 1 chunk with a chunk size of 500.
#> 
#> ℹ 14:57:37 | DONE!
#> 
#> This message is displayed once every 8 hours.

sims2$overall |> 
  dplyr::arrange(team) |> 
  gt::gt_preview(top_n = 5, bottom_n = 5)
```

|       | conf | division  | team | wins  | playoff | div1  | seed1 | won_conf | won_sb | draft1 | draft5 |
|-------|------|-----------|------|-------|---------|-------|-------|----------|--------|--------|--------|
| 1     | NFC  | NFC West  | ARI  | 15.35 | 1.000   | 0.982 | 0.528 | 0.756    | 0.672  | 0.000  | 0.000  |
| 2     | NFC  | NFC South | ATL  | 15.32 | 1.000   | 0.912 | 0.406 | 0.194    | 0.176  | 0.000  | 0.000  |
| 3     | AFC  | AFC North | BAL  | 15.27 | 1.000   | 0.858 | 0.788 | 0.772    | 0.104  | 0.000  | 0.000  |
| 4     | AFC  | AFC East  | BUF  | 13.62 | 0.998   | 0.998 | 0.108 | 0.176    | 0.026  | 0.000  | 0.000  |
| 5     | NFC  | NFC South | CAR  | 12.89 | 0.980   | 0.088 | 0.018 | 0.030    | 0.008  | 0.000  | 0.000  |
| 6..27 |      |           |      |       |         |       |       |          |        |        |        |
| 28    | NFC  | NFC West  | SEA  | 3.31  | 0.000   | 0.000 | 0.000 | 0.000    | 0.000  | 0.014  | 0.476  |
| 29    | NFC  | NFC West  | SF   | 2.38  | 0.000   | 0.000 | 0.000 | 0.000    | 0.000  | 0.202  | 0.814  |
| 30    | NFC  | NFC South | TB   | 2.55  | 0.000   | 0.000 | 0.000 | 0.000    | 0.000  | 0.172  | 0.780  |
| 31    | AFC  | AFC South | TEN  | 2.58  | 0.000   | 0.000 | 0.000 | 0.000    | 0.000  | 0.162  | 0.780  |
| 32    | NFC  | NFC East  | WAS  | 1.69  | 0.000   | 0.000 | 0.000 | 0.000    | 0.000  | 0.354  | 0.922  |

As you might expect, the earliest alphabetical teams win a lot. The
Cardinals won the Super Bowl in 67% of seasons! Meanwhile, the teams at
the bottom alphabetically are virtually certain to be at the top of the
draft order with the Commanders picking 1st overall in 35% of seasons.

### Adding In Your Own Data

This is all well and good, you might be thinking, but your model works
off of other data not in the simulator! How can that work? This is where
we utilize R’s ability to have generic arguments.

The `...` in the function definition means that the function can be
called with any number of additional arguments. You can name these
whatever you want, as long as they’re not already the name of other
defined arguments.

When you call the
[`nfl_simulations()`](https://nflseedr.com/reference/nfl_simulations.md)
function, it too uses the `...` syntax, which allows you to pass in any
number of additional arguments to the function. The simulator will in
turn pass these on to *your* function that processes games.

For example, let’s slightly modify our last example:

``` r
library(rlang)
biased_games_model <- function(teams, games, week_num, ...) {
  
  # collect all arguments in dots
  args <- list(...)
  
  # if "best" or "worst" are in dots, assign them to best of worst
  # if not, then args$best and args$worst will be NULL and we replace 
  # them with empty strings
  best <- args$best %||% ""
  worst <- args$worst %||% ""

  # make the best team always win and the worst team always lose
  # otherwise, make the earlier alphabetical team win 90% of the time
  games <- games |>
    dplyr::mutate(
      result = dplyr::case_when(
        !is.na(result) | week != week_num ~ result,
        away_team == best | home_team == worst ~ -3,
        away_team == worst | home_team == best ~ 3,
        away_team < home_team ~
          sample(c(-3, 3), dplyr::n(), prob = c(0.9, 0.1), replace = TRUE),
        away_team > home_team ~
          sample(c(-3, 3), dplyr::n(), prob = c(0.1, 0.9), replace = TRUE),
        TRUE ~ 0
      )
    )
  
  # return values
  list(teams = teams, games = games)
}
```

This allows us to define `best` and `worst`, and use that information to
determine a result (in this case, have the best team always win and the
worst team always lose). While `best` and `worst` are in this example
single-length character vectors, they can be data frames or any other R
data type.

Let’s simulate using this:

``` r
sims3 <- nflseedR::nfl_simulations(
  games = games,
  compute_results = biased_games_model, 
  simulations = 500,
  chunks = 1,
  best = "CHI", 
  worst = "GB",
  verbosity = "NONE"
)
#> ℹ 14:57:37 | Start simulation of 500 seasons in 1 chunk with a chunk size of 500.
#> ℹ 14:57:45 | DONE!
sims3$overall |> 
  dplyr::arrange(-wins) |> 
  gt::gt_preview(top_n = 5, bottom_n = 5)
```

|       | conf | division  | team | wins  | playoff | div1  | seed1 | won_conf | won_sb | draft1 | draft5 |
|-------|------|-----------|------|-------|---------|-------|-------|----------|--------|--------|--------|
| 1     | NFC  | NFC North | CHI  | 17.00 | 1.000   | 1.000 | 0.996 | 1.000    | 1      | 0.000  | 0.000  |
| 2     | AFC  | AFC North | BAL  | 15.30 | 1.000   | 0.870 | 0.788 | 0.788    | 0      | 0.000  | 0.000  |
| 3     | NFC  | NFC South | ATL  | 15.28 | 1.000   | 0.944 | 0.004 | 0.000    | 0      | 0.000  | 0.000  |
| 4     | NFC  | NFC West  | ARI  | 14.55 | 1.000   | 0.966 | 0.000 | 0.000    | 0      | 0.000  | 0.000  |
| 5     | AFC  | AFC East  | BUF  | 13.65 | 0.992   | 0.988 | 0.116 | 0.162    | 0      | 0.000  | 0.000  |
| 6..27 |      |           |      |       |         |       |       |          |        |        |        |
| 28    | NFC  | NFC West  | SF   | 3.31  | 0.000   | 0.000 | 0.000 | 0.000    | 0      | 0.000  | 0.604  |
| 29    | NFC  | NFC South | TB   | 2.53  | 0.000   | 0.000 | 0.000 | 0.000    | 0      | 0.002  | 0.776  |
| 30    | AFC  | AFC North | PIT  | 2.47  | 0.000   | 0.000 | 0.000 | 0.000    | 0      | 0.000  | 0.682  |
| 31    | NFC  | NFC East  | WAS  | 1.57  | 0.000   | 0.000 | 0.000 | 0.000    | 0      | 0.020  | 0.898  |
| 32    | NFC  | NFC North | GB   | 0.00  | 0.000   | 0.000 | 0.000 | 0.000    | 0      | 0.978  | 1.000  |

And this shows exactly what we expect. By defining the Bears as the best
team, they always go 17-0, win the division, and win the Super Bowl.
Similarly, the Packers always go 0-17, and never make the playoffs.

### Passing Data in from One Week to the Next

Sometimes though you want your data to keep updating as the simulation
progresses. For example, the Elo-based model that nflseedR uses by
default updates each team’s Elo after each game. You can pass in the
starting Elo values per team, and as games are simulated, update the Elo
values for each team and store them in the `teams` data. This column
will be part of the `teams` data passed into your function when the
following week is simulated and your function is called.

Read the code and comments of the source of `nflseedR_compute_results`
for specific tips on doing this but here are good ones:

- You can add columns to `teams` and/or `games` if you want.
- When doing joins to do the above, do left joins to make sure no rows
  are removed.
- Remove any “helper” columns you generate along the way you don’t
  actually need before returning.
- Make sure any column doesn’t get blindly joined on so it has `.x` and
  `.y` versions in week 2 and R throws an error because an expected
  column name doesn’t exist.
