# Compute NFL Playoff Seedings using Game Results and Divisional Rankings

Compute NFL Playoff Seedings using Game Results and Divisional Rankings

## Usage

``` r
compute_conference_seeds(
  teams,
  h2h = NULL,
  tiebreaker_depth = 3,
  .debug = FALSE,
  playoff_seeds = 7
)
```

## Arguments

- teams:

  The division standings data frame as computed by
  [`compute_division_ranks`](https://nflseedr.com/reference/compute_division_ranks.md)

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

- playoff_seeds:

  Number of playoff teams per conference (increased in 2020 from 6 to
  7).

## Value

A data frame of division standings including playoff seeds and the week
in which the season ended for the respective team (`exit`).

A list of two data frames:

- standings:

  Division standings including playoff seeds.

- h2h:

  A data frame that is used for head-to-head tiebreakers across the
  tie-breaking functions.

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
s <- nflseedR::load_sharpe_games() |>
  dplyr::filter(season %in% 2019:2020) |>
  dplyr::select(sim = season, game_type, week, away_team, home_team, result) |>
  nflseedR::compute_division_ranks()
  nflseedR::compute_conference_seeds(s, h2h = s$h2h) |>
  purrr::pluck("standings")
})
#> Warning: `load_sharpe_games()` was deprecated in nflseedR 2.0.0.
#> ℹ Please use `nflreadr::load_schedules()` instead.
#> ℹ 12:24:45 | Calculating team data
#> ℹ 12:24:45 | Calculating head to head
#> ℹ 12:24:45 | Calculating division rank #1
#> ℹ 12:24:45 | Calculating division rank #2
#> ℹ 12:24:45 | Calculating division rank #3
#> ℹ 12:24:45 | Calculating division rank #4
#> ℹ 12:24:45 | Calculating seed #1
#> ℹ 12:24:45 | Calculating seed #2
#> ℹ 12:24:46 | Calculating seed #3
#> ℹ 12:24:46 | Calculating seed #4
#> ℹ 12:24:46 | Calculating seed #5
#> ℹ 12:24:46 | Calculating seed #6
#> ℹ 12:24:46 | Calculating seed #7
#> # A tibble: 64 × 17
#>      sim conf  division team  games  wins true_wins losses  ties win_pct div_pct
#>    <int> <chr> <chr>    <chr> <int> <dbl>     <int>  <int> <int>   <dbl>   <dbl>
#>  1  2019 AFC   AFC East BUF      16  10          10      6     0  0.625    0.5  
#>  2  2019 AFC   AFC East MIA      16   5           5     11     0  0.312    0.333
#>  3  2019 AFC   AFC East NE       16  12          12      4     0  0.75     0.833
#>  4  2019 AFC   AFC East NYJ      16   7           7      9     0  0.438    0.333
#>  5  2019 AFC   AFC Nor… BAL      16  14          14      2     0  0.875    0.833
#>  6  2019 AFC   AFC Nor… CIN      16   2           2     14     0  0.125    0.167
#>  7  2019 AFC   AFC Nor… CLE      16   6           6     10     0  0.375    0.5  
#>  8  2019 AFC   AFC Nor… PIT      16   8           8      8     0  0.5      0.5  
#>  9  2019 AFC   AFC Sou… HOU      16  10          10      6     0  0.625    0.667
#> 10  2019 AFC   AFC Sou… IND      16   7           7      9     0  0.438    0.5  
#> 11  2019 AFC   AFC Sou… JAX      16   6           6     10     0  0.375    0.333
#> 12  2019 AFC   AFC Sou… TEN      16   9           9      7     0  0.562    0.5  
#> 13  2019 AFC   AFC West DEN      16   7           7      9     0  0.438    0.5  
#> 14  2019 AFC   AFC West KC       16  12          12      4     0  0.75     1    
#> 15  2019 AFC   AFC West LAC      16   5           5     11     0  0.312    0    
#> 16  2019 AFC   AFC West OAK      16   7           7      9     0  0.438    0.5  
#> 17  2019 NFC   NFC East DAL      16   8           8      8     0  0.5      0.833
#> 18  2019 NFC   NFC East NYG      16   4           4     12     0  0.25     0.333
#> 19  2019 NFC   NFC East PHI      16   9           9      7     0  0.562    0.833
#> 20  2019 NFC   NFC East WAS      16   3           3     13     0  0.188    0    
#> 21  2019 NFC   NFC Nor… CHI      16   8           8      8     0  0.5      0.667
#> 22  2019 NFC   NFC Nor… DET      16   3.5         3     12     1  0.219    0    
#> 23  2019 NFC   NFC Nor… GB       16  13          13      3     0  0.812    1    
#> 24  2019 NFC   NFC Nor… MIN      16  10          10      6     0  0.625    0.333
#> 25  2019 NFC   NFC Sou… ATL      16   7           7      9     0  0.438    0.667
#> 26  2019 NFC   NFC Sou… CAR      16   5           5     11     0  0.312    0.167
#> 27  2019 NFC   NFC Sou… NO       16  13          13      3     0  0.812    0.833
#> 28  2019 NFC   NFC Sou… TB       16   7           7      9     0  0.438    0.333
#> 29  2019 NFC   NFC West ARI      16   5.5         5     10     1  0.344    0.167
#> 30  2019 NFC   NFC West LA       16   9           9      7     0  0.562    0.5  
#> 31  2019 NFC   NFC West SEA      16  11          11      5     0  0.688    0.5  
#> 32  2019 NFC   NFC West SF       16  13          13      3     0  0.812    0.833
#> 33  2020 AFC   AFC East BUF      16  13          13      3     0  0.812    1    
#> 34  2020 AFC   AFC East MIA      16  10          10      6     0  0.625    0.5  
#> 35  2020 AFC   AFC East NE       16   7           7      9     0  0.438    0.5  
#> 36  2020 AFC   AFC East NYJ      16   2           2     14     0  0.125    0    
#> 37  2020 AFC   AFC Nor… BAL      16  11          11      5     0  0.688    0.667
#> 38  2020 AFC   AFC Nor… CIN      16   4.5         4     11     1  0.281    0.167
#> 39  2020 AFC   AFC Nor… CLE      16  11          11      5     0  0.688    0.5  
#> 40  2020 AFC   AFC Nor… PIT      16  12          12      4     0  0.75     0.667
#> 41  2020 AFC   AFC Sou… HOU      16   4           4     12     0  0.25     0.333
#> 42  2020 AFC   AFC Sou… IND      16  11          11      5     0  0.688    0.667
#> 43  2020 AFC   AFC Sou… JAX      16   1           1     15     0  0.0625   0.167
#> 44  2020 AFC   AFC Sou… TEN      16  11          11      5     0  0.688    0.833
#> 45  2020 AFC   AFC West DEN      16   5           5     11     0  0.312    0.167
#> 46  2020 AFC   AFC West KC       16  14          14      2     0  0.875    0.667
#> 47  2020 AFC   AFC West LAC      16   7           7      9     0  0.438    0.5  
#> 48  2020 AFC   AFC West LV       16   8           8      8     0  0.5      0.667
#> 49  2020 NFC   NFC East DAL      16   6           6     10     0  0.375    0.333
#> 50  2020 NFC   NFC East NYG      16   6           6     10     0  0.375    0.667
#> 51  2020 NFC   NFC East PHI      16   4.5         4     11     1  0.281    0.333
#> 52  2020 NFC   NFC East WAS      16   7           7      9     0  0.438    0.667
#> 53  2020 NFC   NFC Nor… CHI      16   8           8      8     0  0.5      0.333
#> 54  2020 NFC   NFC Nor… DET      16   5           5     11     0  0.312    0.167
#> 55  2020 NFC   NFC Nor… GB       16  13          13      3     0  0.812    0.833
#> 56  2020 NFC   NFC Nor… MIN      16   7           7      9     0  0.438    0.667
#> 57  2020 NFC   NFC Sou… ATL      16   4           4     12     0  0.25     0.167
#> 58  2020 NFC   NFC Sou… CAR      16   5           5     11     0  0.312    0.167
#> 59  2020 NFC   NFC Sou… NO       16  12          12      4     0  0.75     1    
#> 60  2020 NFC   NFC Sou… TB       16  11          11      5     0  0.688    0.667
#> 61  2020 NFC   NFC West ARI      16   8           8      8     0  0.5      0.333
#> 62  2020 NFC   NFC West LA       16  10          10      6     0  0.625    0.5  
#> 63  2020 NFC   NFC West SEA      16  12          12      4     0  0.75     0.667
#> 64  2020 NFC   NFC West SF       16   6           6     10     0  0.375    0.5  
#> # ℹ 6 more variables: conf_pct <dbl>, sov <dbl>, sos <dbl>, div_rank <dbl>,
#> #   seed <dbl>, exit <dbl>

# Restore old options
options(old)
# }
```
