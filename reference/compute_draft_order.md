# Compute NFL Draft Order using Game Results and Divisional Rankings

Compute NFL Draft Order using Game Results and Divisional Rankings

## Usage

``` r
compute_draft_order(
  teams,
  games,
  h2h = NULL,
  tiebreaker_depth = 3,
  .debug = FALSE
)
```

## Arguments

- teams:

  The division standings data frame including playoff seeds as computed
  by
  [`compute_conference_seeds`](https://nflseedr.com/reference/compute_conference_seeds.md)

- games:

  A data frame containing real or simulated game scores. The following
  variables are required:

  sim

  :   A simulation ID. Normally 1 - n simulated seasons.

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

- h2h:

  A data frame that is used for head-to-head tiebreakers across the
  tie-breaking functions. It is computed by the function
  [`compute_division_ranks`](https://nflseedr.com/reference/compute_division_ranks.md).

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

- .debug:

  Either `TRUE` or `FALSE`. Controls whether additional messages are
  printed to the console showing what the tie-breaking algorithms are
  currently performing.

## Value

A data frame of standings including the final draft pick number and the
variable `exit` which indicates the week number of the teams final game
(Super Bowl Winner is one week higher).

## See also

The examples [on the package
website](https://nflseedr.com/articles/articles/nflseedR.html)

## Examples

``` r
# \donttest{
# Change some options for better output
old <- options(list(digits = 3, tibble.print_min = 64))
library(dplyr, warn.conflicts = FALSE)

try({#to avoid CRAN test problems
games <-
  nflseedR::load_sharpe_games() |>
  dplyr::filter(season %in% 2018:2019) |>
  dplyr::select(sim = season, game_type, week, away_team, home_team, result)

s <- games |> nflseedR::compute_division_ranks()
s <- nflseedR::compute_conference_seeds(s, h2h = s$h2h, playoff_seeds = 6)
nflseedR::compute_draft_order(s, games = games, h2h = s$h2h)
})
#> ℹ 13:58:20 | Calculating team data
#> ℹ 13:58:20 | Calculating head to head
#> ℹ 13:58:20 | Calculating division rank #1
#> ℹ 13:58:20 | Calculating division rank #2
#> ℹ 13:58:20 | Calculating division rank #3
#> ℹ 13:58:20 | Calculating division rank #4
#> ℹ 13:58:20 | Calculating seed #1
#> ℹ 13:58:20 | Calculating seed #2
#> ℹ 13:58:20 | Calculating seed #3
#> ℹ 13:58:21 | Calculating seed #4
#> ℹ 13:58:21 | Calculating seed #5
#> ℹ 13:58:21 | Calculating seed #6
#> ℹ 13:58:21 | Calculating draft order #32
#> ℹ 13:58:21 | Calculating draft order #31
#> ℹ 13:58:21 | Calculating draft order #30
#> ℹ 13:58:21 | Calculating draft order #29
#> ℹ 13:58:21 | Calculating draft order #28
#> ℹ 13:58:21 | Calculating draft order #27
#> ℹ 13:58:21 | Calculating draft order #26
#> ℹ 13:58:21 | Calculating draft order #25
#> ℹ 13:58:21 | Calculating draft order #24
#> ℹ 13:58:21 | Calculating draft order #23
#> ℹ 13:58:21 | Calculating draft order #22
#> ℹ 13:58:21 | Calculating draft order #21
#> ℹ 13:58:21 | Calculating draft order #20
#> ℹ 13:58:21 | Calculating draft order #19
#> ℹ 13:58:21 | Calculating draft order #18
#> ℹ 13:58:21 | Calculating draft order #17
#> ℹ 13:58:21 | Calculating draft order #16
#> ℹ 13:58:21 | Calculating draft order #15
#> ℹ 13:58:21 | Calculating draft order #14
#> ℹ 13:58:21 | Calculating draft order #13
#> ℹ 13:58:21 | Calculating draft order #12
#> ℹ 13:58:21 | Calculating draft order #11
#> ℹ 13:58:21 | Calculating draft order #10
#> ℹ 13:58:21 | Calculating draft order #9
#> ℹ 13:58:21 | Calculating draft order #8
#> ℹ 13:58:21 | Calculating draft order #7
#> ℹ 13:58:21 | Calculating draft order #6
#> ℹ 13:58:21 | Calculating draft order #5
#> ℹ 13:58:21 | Calculating draft order #4
#> ℹ 13:58:21 | Calculating draft order #3
#> ℹ 13:58:21 | Calculating draft order #2
#> ℹ 13:58:21 | Calculating draft order #1
#> # A tibble: 64 × 18
#>      sim team  conf  division games  wins true_wins losses  ties win_pct div_pct
#>    <int> <chr> <chr> <chr>    <int> <dbl>     <int>  <int> <int>   <dbl>   <dbl>
#>  1  2018 BUF   AFC   AFC East    16   6           6     10     0   0.375   0.333
#>  2  2018 MIA   AFC   AFC East    16   7           7      9     0   0.438   0.667
#>  3  2018 NE    AFC   AFC East    16  11          11      5     0   0.688   0.833
#>  4  2018 NYJ   AFC   AFC East    16   4           4     12     0   0.25    0.167
#>  5  2018 BAL   AFC   AFC Nor…    16  10          10      6     0   0.625   0.5  
#>  6  2018 CIN   AFC   AFC Nor…    16   6           6     10     0   0.375   0.167
#>  7  2018 CLE   AFC   AFC Nor…    16   7.5         7      8     1   0.469   0.583
#>  8  2018 PIT   AFC   AFC Nor…    16   9.5         9      6     1   0.594   0.75 
#>  9  2018 HOU   AFC   AFC Sou…    16  11          11      5     0   0.688   0.667
#> 10  2018 IND   AFC   AFC Sou…    16  10          10      6     0   0.625   0.667
#> 11  2018 JAX   AFC   AFC Sou…    16   5           5     11     0   0.312   0.167
#> 12  2018 TEN   AFC   AFC Sou…    16   9           9      7     0   0.562   0.5  
#> 13  2018 DEN   AFC   AFC West    16   6           6     10     0   0.375   0.333
#> 14  2018 KC    AFC   AFC West    16  12          12      4     0   0.75    0.833
#> 15  2018 LAC   AFC   AFC West    16  12          12      4     0   0.75    0.667
#> 16  2018 OAK   AFC   AFC West    16   4           4     12     0   0.25    0.167
#> 17  2018 DAL   NFC   NFC East    16  10          10      6     0   0.625   0.833
#> 18  2018 NYG   NFC   NFC East    16   5           5     11     0   0.312   0.167
#> 19  2018 PHI   NFC   NFC East    16   9           9      7     0   0.562   0.667
#> 20  2018 WAS   NFC   NFC East    16   7           7      9     0   0.438   0.333
#> 21  2018 CHI   NFC   NFC Nor…    16  12          12      4     0   0.75    0.833
#> 22  2018 DET   NFC   NFC Nor…    16   6           6     10     0   0.375   0.333
#> 23  2018 GB    NFC   NFC Nor…    16   6.5         6      9     1   0.406   0.25 
#> 24  2018 MIN   NFC   NFC Nor…    16   8.5         8      7     1   0.531   0.583
#> 25  2018 ATL   NFC   NFC Sou…    16   7           7      9     0   0.438   0.667
#> 26  2018 CAR   NFC   NFC Sou…    16   7           7      9     0   0.438   0.333
#> 27  2018 NO    NFC   NFC Sou…    16  13          13      3     0   0.812   0.667
#> 28  2018 TB    NFC   NFC Sou…    16   5           5     11     0   0.312   0.333
#> 29  2018 ARI   NFC   NFC West    16   3           3     13     0   0.188   0.333
#> 30  2018 LA    NFC   NFC West    16  13          13      3     0   0.812   1    
#> 31  2018 SEA   NFC   NFC West    16  10          10      6     0   0.625   0.5  
#> 32  2018 SF    NFC   NFC West    16   4           4     12     0   0.25    0.167
#> 33  2019 BUF   AFC   AFC East    16  10          10      6     0   0.625   0.5  
#> 34  2019 MIA   AFC   AFC East    16   5           5     11     0   0.312   0.333
#> 35  2019 NE    AFC   AFC East    16  12          12      4     0   0.75    0.833
#> 36  2019 NYJ   AFC   AFC East    16   7           7      9     0   0.438   0.333
#> 37  2019 BAL   AFC   AFC Nor…    16  14          14      2     0   0.875   0.833
#> 38  2019 CIN   AFC   AFC Nor…    16   2           2     14     0   0.125   0.167
#> 39  2019 CLE   AFC   AFC Nor…    16   6           6     10     0   0.375   0.5  
#> 40  2019 PIT   AFC   AFC Nor…    16   8           8      8     0   0.5     0.5  
#> 41  2019 HOU   AFC   AFC Sou…    16  10          10      6     0   0.625   0.667
#> 42  2019 IND   AFC   AFC Sou…    16   7           7      9     0   0.438   0.5  
#> 43  2019 JAX   AFC   AFC Sou…    16   6           6     10     0   0.375   0.333
#> 44  2019 TEN   AFC   AFC Sou…    16   9           9      7     0   0.562   0.5  
#> 45  2019 DEN   AFC   AFC West    16   7           7      9     0   0.438   0.5  
#> 46  2019 KC    AFC   AFC West    16  12          12      4     0   0.75    1    
#> 47  2019 LAC   AFC   AFC West    16   5           5     11     0   0.312   0    
#> 48  2019 OAK   AFC   AFC West    16   7           7      9     0   0.438   0.5  
#> 49  2019 DAL   NFC   NFC East    16   8           8      8     0   0.5     0.833
#> 50  2019 NYG   NFC   NFC East    16   4           4     12     0   0.25    0.333
#> 51  2019 PHI   NFC   NFC East    16   9           9      7     0   0.562   0.833
#> 52  2019 WAS   NFC   NFC East    16   3           3     13     0   0.188   0    
#> 53  2019 CHI   NFC   NFC Nor…    16   8           8      8     0   0.5     0.667
#> 54  2019 DET   NFC   NFC Nor…    16   3.5         3     12     1   0.219   0    
#> 55  2019 GB    NFC   NFC Nor…    16  13          13      3     0   0.812   1    
#> 56  2019 MIN   NFC   NFC Nor…    16  10          10      6     0   0.625   0.333
#> 57  2019 ATL   NFC   NFC Sou…    16   7           7      9     0   0.438   0.667
#> 58  2019 CAR   NFC   NFC Sou…    16   5           5     11     0   0.312   0.167
#> 59  2019 NO    NFC   NFC Sou…    16  13          13      3     0   0.812   0.833
#> 60  2019 TB    NFC   NFC Sou…    16   7           7      9     0   0.438   0.333
#> 61  2019 ARI   NFC   NFC West    16   5.5         5     10     1   0.344   0.167
#> 62  2019 LA    NFC   NFC West    16   9           9      7     0   0.562   0.5  
#> 63  2019 SEA   NFC   NFC West    16  11          11      5     0   0.688   0.5  
#> 64  2019 SF    NFC   NFC West    16  13          13      3     0   0.812   0.833
#> # ℹ 7 more variables: conf_pct <dbl>, sov <dbl>, sos <dbl>, div_rank <dbl>,
#> #   seed <dbl>, exit <dbl>, draft_order <dbl>

# Restore old options
options(old)
# }
```
