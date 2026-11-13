# Compute NFL Clinching and Elimination Status

**\[experimental\]**

Determine for every team whether it has clinched, can still reach, or
has been eliminated from a division title, a playoff berth and the No. 1
seed, given the games played so far.

## Usage

``` r
nfl_clinching_status(
  games,
  ...,
  status = c("DIV", "PLAYOFFS", "1SEED"),
  scenarios = FALSE,
  search = FALSE,
  verbosity = c("MIN", "MAX", "NONE")
)
```

## Arguments

- games:

  A data frame of played and remaining games, like
  [`nflreadr::load_schedules()`](https://nflreadr.nflverse.com/reference/load_schedules.html).
  It needs the variables `sim` or `season`, `game_type`, `week`,
  `away_team`, `home_team` and `result` (see
  [`nfl_standings()`](https://nflseedr.com/reference/nfl_standings.md)).
  Games with a `result` are played, regular season games with
  `result = NA` are still to be played. To get the status after a given
  week, set `result` of all later games to `NA`.

  Cancelled games must be removed from `games`, otherwise they count as
  games still to be played.

- ...:

  currently not used

- status:

  One or more of `"DIV"`, `"PLAYOFFS"` and `"1SEED"`: the goals to
  compute.

  - `"DIV"`: win the division.

  - `"PLAYOFFS"`: make the playoffs, as division winner or wild card.

  - `"1SEED"`: be the No. 1 seed of the conference.

  Only the requested goals are computed. `"PLAYOFFS"` takes the most
  time.

- scenarios:

  If `TRUE`, the result gets the attribute `"scenarios"`, the results of
  the remaining games that prove the answers (see Value).

- search:

  If `TRUE`, answers the proofs leave open are searched by fixing the
  results of remaining games one at a time, within a fixed budget (see
  Details). This decides more teams but takes more time, mostly with
  many games left. With `FALSE` (default), the function says how many
  statuses are `"undecided"` and whether the search might help.

- verbosity:

  One of `"MIN"`, `"MAX"`, or `"NONE"`.

  - `"MIN"` (default): Prints the main steps.

  - `"MAX"`: Also prints all steps of the tiebreaking process of the
    current standings and how many teams got each status.

  - `"NONE"`: No status reports.

## Value

The current standings: the output of
[`nfl_standings()`](https://nflseedr.com/reference/nfl_standings.md) for
the games played so far (with conference ranks and tiebreakers through
strength of schedule), followed by `games_left` (regular season games
still to be played) and one status column per requested goal:
`div_status`, `playoffs_status` and `seed1_status`. Each status is one
of `"clinched"`, `"alive"`, `"eliminated"` or `"undecided"`. Like
[`nfl_standings()`](https://nflseedr.com/reference/nfl_standings.md),
there is only a row for teams that have played at least one game, and
the rows are sorted the same way.

With `scenarios = TRUE`, the attribute `"scenarios"` is a data.table
with one row per scenario and remaining game of its season: `scenario`
(an id), `season` (or `sim`), `team`, `goal` (`"DIV"`, `"PLAYOFFS"` or
`"1SEED"`), `proves` (`"reach"` or `"miss"`: the team reaches or misses
the goal with these results), `coin_toss` (`TRUE` if it does so only
with a particular coin toss), followed by the columns of `games` with
the results of the scenario. Played games together with the rows of one
scenario can go straight into
[`nfl_standings()`](https://nflseedr.com/reference/nfl_standings.md).
Seasons without remaining games have no rows.

## Details

For every goal, the function answers two questions: can the team still
reach the goal, and can it still miss it, over all possible results
(win, loss or tie) of the remaining games. The status follows from the
answers:

|            |          |                |
|------------|----------|----------------|
| Can reach  | Can miss | Status         |
| no         | yes      | `"eliminated"` |
| yes        | no       | `"clinched"`   |
| yes        | yes      | `"alive"`      |
| not proven |          | `"undecided"`  |

Every answer is proven, either by showing that no results of the
remaining games can do it, or by a concrete set of results run through
the standings (see `scenarios`). Where tiebreakers decide, the function
goes through the ways the teams of the conference can end above, level
with or below the team, and proves who wins each of these ties. It never
tries every combination of results, so a few teams can be `"undecided"`
when the answer depends on tiebreakers it can't settle in advance.

With `search = TRUE`, an open answer is searched further: the function
fixes the result of one remaining game at a time (home win, away win,
tie) and proves the answer for each case, up to 200 cases per answer.
This helps most with 5 or fewer games left per team.

Tiebreakers through strength of schedule are applied as in
[`nfl_standings()`](https://nflseedr.com/reference/nfl_standings.md).
Coin tosses and tiebreakers based on points can't be known before they
happen, so both of their outcomes count as possible. A goal that only a
coin toss decides after the last game is `"undecided"`.

Seasons through 2019 have 6 playoff seeds per conference, later seasons
7. With `sim` instead of `season`, 7 seeds are used.

With a single season in `games`, the function puts more effort into ties
of many teams (mostly for the No. 1 seed with many games left), which
can take a few seconds. With more than one season, e.g. simulated
seasons, it gives up on them earlier, so more statuses are
`"undecided"`.

The results are compared with the official clinching flags of the NFL
for every week of 2006 to 2025, one season at a time: no status
contradicts them, and every team clinches or is eliminated in the same
week as in the NFL, with and without `search`.

## See also

For the implemented tiebreakers, see
<https://nflseedr.com/articles/tiebreaker.html>

## Examples

``` r
# \donttest{
try({#to avoid CRAN test problems
  games <- nflreadr::load_schedules(2023)
})
# Status after week 15
games$result[games$week > 15] <- NA
status <- nflseedR::nfl_clinching_status(games)
#> ℹ 19:49:16 | Compute clinching status
#> ℹ 19:49:16 | Compute standings
#> ℹ 3 statuses are "undecided" (teams with 3 games left). `search = TRUE` might
#>   decide them.
#>   The search helps most with 5 or fewer games left per team.
status
#>     season   team   conf  division games  wins true_wins losses  ties    pf
#>      <int> <char> <char>    <char> <int> <num>     <int>  <int> <int> <int>
#>  1:   2023    MIA    AFC  AFC East    14    10        10      4     0   441
#>  2:   2023    BUF    AFC  AFC East    14     8         8      6     0   379
#>  3:   2023    NYJ    AFC  AFC East    14     5         5      9     0   201
#>  4:   2023     NE    AFC  AFC East    14     3         3     11     0   186
#>  5:   2023    BAL    AFC AFC North    14    11        11      3     0   384
#>  6:   2023    CLE    AFC AFC North    14     9         9      5     0   309
#>  7:   2023    CIN    AFC AFC North    14     8         8      6     0   307
#>  8:   2023    PIT    AFC AFC North    14     7         7      7     0   223
#>  9:   2023    JAX    AFC AFC South    14     8         8      6     0   319
#> 10:   2023    IND    AFC AFC South    14     8         8      6     0   344
#> 11:   2023    HOU    AFC AFC South    14     8         8      6     0   306
#> 12:   2023    TEN    AFC AFC South    14     5         5      9     0   257
#> 13:   2023     KC    AFC  AFC West    14     9         9      5     0   319
#> 14:   2023    DEN    AFC  AFC West    14     7         7      7     0   304
#> 15:   2023     LV    AFC  AFC West    14     6         6      8     0   265
#> 16:   2023    LAC    AFC  AFC West    14     5         5      9     0   303
#> 17:   2023    DAL    NFC  NFC East    14    10        10      4     0   431
#> 18:   2023    PHI    NFC  NFC East    14    10        10      4     0   359
#> 19:   2023    NYG    NFC  NFC East    14     5         5      9     0   189
#> 20:   2023    WAS    NFC  NFC East    14     4         4     10     0   281
#> 21:   2023    DET    NFC NFC North    14    10        10      4     0   382
#> 22:   2023    MIN    NFC NFC North    14     7         7      7     0   290
#> 23:   2023     GB    NFC NFC North    14     6         6      8     0   300
#> 24:   2023    CHI    NFC NFC North    14     5         5      9     0   287
#> 25:   2023     TB    NFC NFC South    14     7         7      7     0   296
#> 26:   2023     NO    NFC NFC South    14     7         7      7     0   309
#> 27:   2023    ATL    NFC NFC South    14     6         6      8     0   258
#> 28:   2023    CAR    NFC NFC South    14     2         2     12     0   206
#> 29:   2023     SF    NFC  NFC West    14    11        11      3     0   425
#> 30:   2023     LA    NFC  NFC West    14     7         7      7     0   327
#> 31:   2023    SEA    NFC  NFC West    14     7         7      7     0   300
#> 32:   2023    ARI    NFC  NFC West    14     3         3     11     0   259
#>     season   team   conf  division games  wins true_wins losses  ties    pf
#>      <int> <char> <char>    <char> <int> <num>     <int>  <int> <int> <int>
#>        pa    pd   win_pct   div_pct  conf_pct       sov       sos div_rank
#>     <int> <int>     <num>     <num>     <num>     <num>     <num>    <int>
#>  1:   294   147 0.7142857 0.8000000 0.7000000 0.3214286 0.3928571        1
#>  2:   254   125 0.5714286 0.5000000 0.4444444 0.5000000 0.4948980        2
#>  3:   287   -86 0.3571429 0.2000000 0.3000000 0.5428571 0.5357143        3
#>  4:   299  -113 0.2142857 0.5000000 0.3333333 0.4761905 0.5306122        4
#>  5:   225   159 0.7857143 0.6000000 0.7000000 0.5064935 0.5204082        1
#>  6:   289    20 0.6428571 0.6000000 0.6666667 0.5238095 0.5357143        2
#>  7:   311    -4 0.5714286 0.0000000 0.3333333 0.5267857 0.5612245        3
#>  8:   280   -57 0.5000000 0.7500000 0.5000000 0.5306122 0.5204082        4
#>  9:   313     6 0.5714286 0.8000000 0.5454545 0.5089286 0.5765306        1
#> 10:   343     1 0.5714286 0.6000000 0.6000000 0.4285714 0.4846939        2
#> 11:   295    11 0.5714286 0.5000000 0.5555556 0.4642857 0.4693878        3
#> 12:   301   -44 0.3571429 0.0000000 0.3000000 0.4428571 0.5306122        4
#> 13:   245    74 0.6428571 0.7500000 0.7777778 0.4444444 0.4948980        1
#> 14:   351   -47 0.5000000 0.5000000 0.4444444 0.5000000 0.5153061        2
#> 15:   280   -15 0.4285714 0.5000000 0.4444444 0.3690476 0.4693878        3
#> 16:   345   -42 0.3571429 0.2500000 0.3333333 0.3714286 0.5102041        4
#> 17:   264   167 0.7142857 0.8000000 0.7000000 0.3785714 0.4336735        1
#> 18:   341    18 0.7142857 0.7500000 0.6666667 0.4928571 0.5204082        2
#> 19:   338  -149 0.3571429 0.5000000 0.4444444 0.2857143 0.4795918        3
#> 20:   423  -142 0.2857143 0.0000000 0.2000000 0.3392857 0.4897959        4
#> 21:   331    51 0.7142857 0.5000000 0.6666667 0.4285714 0.4540816        1
#> 22:   269    21 0.5000000 0.6666667 0.6666667 0.4387755 0.4795918        2
#> 23:   301    -1 0.4285714 0.5000000 0.4444444 0.5119048 0.5000000        3
#> 24:   329   -42 0.3571429 0.4000000 0.4444444 0.4142857 0.4897959        4
#> 25:   290     6 0.5000000 0.7500000 0.6000000 0.3877551 0.5051020        1
#> 26:   267    42 0.5000000 0.5000000 0.4444444 0.3061224 0.4183673        2
#> 27:   278   -20 0.4285714 0.6000000 0.4000000 0.4166667 0.4132653        3
#> 28:   348  -142 0.1428571 0.2000000 0.1000000 0.5000000 0.5255102        4
#> 29:   234   191 0.7857143 1.0000000 0.9000000 0.4805195 0.5000000        1
#> 30:   310    17 0.5000000 0.8000000 0.5555556 0.4183673 0.5306122        2
#> 31:   335   -35 0.5000000 0.2000000 0.5454545 0.4387755 0.5510204        3
#> 32:   376  -117 0.2142857 0.0000000 0.2222222 0.5476190 0.5663265        4
#>        pa    pd   win_pct   div_pct  conf_pct       sov       sos div_rank
#>     <int> <int>     <num>     <num>     <num>     <num>     <num>    <int>
#>            div_tie_broken_by conf_rank       conf_tie_broken_by games_left
#>                       <char>     <int>                   <char>      <int>
#>  1:                     <NA>         2                     <NA>          3
#>  2:                     <NA>         9                     <NA>          3
#>  3:                     <NA>        15                     <NA>          3
#>  4:                     <NA>        16                     <NA>          3
#>  5:                     <NA>         1                     <NA>          3
#>  6:                     <NA>         5                     <NA>          3
#>  7:                     <NA>         6   Head-To-Head Sweep (3)          3
#>  8:                     <NA>        10   Conference Win PCT (2)          3
#>  9: Head-To-Head Win PCT (3)         4                     <NA>          3
#> 10: Head-To-Head Win PCT (2)         7   Conference Win PCT (2)          3
#> 11: Head-To-Head Win PCT (2)         8   Conference Win PCT (2)          3
#> 12:                     <NA>        14 Common Games Win PCT (2)          3
#> 13:                     <NA>         3                     <NA>          3
#> 14:                     <NA>        11                     <NA>          3
#> 15:                     <NA>        12                     <NA>          3
#> 16:                     <NA>        13   Conference Win PCT (3)          3
#> 17:     Division Win PCT (2)         2   Conference Win PCT (2)          3
#> 18:     Division Win PCT (2)         5                     <NA>          3
#> 19:                     <NA>        12 Common Games Win PCT (2)          3
#> 20:                     <NA>        14                     <NA>          3
#> 21:                     <NA>         3                     <NA>          3
#> 22:                     <NA>         6   Conference Win PCT (3)          3
#> 23:                     <NA>        11                     <NA>          3
#> 24:                     <NA>        13                     <NA>          3
#> 25: Head-To-Head Win PCT (2)         4                     <NA>          3
#> 26: Head-To-Head Win PCT (2)         9                     <NA>          3
#> 27:                     <NA>        10   Head-To-Head Sweep (2)          3
#> 28:                     <NA>        16                     <NA>          3
#> 29:                     <NA>         1                     <NA>          3
#> 30: Head-To-Head Win PCT (2)         7   Conference Win PCT (2)          3
#> 31: Head-To-Head Win PCT (2)         8   Conference Win PCT (2)          3
#> 32:                     <NA>        15                     <NA>          3
#>            div_tie_broken_by conf_rank       conf_tie_broken_by games_left
#>                       <char>     <int>                   <char>      <int>
#>     div_status playoffs_status seed1_status
#>         <char>          <char>       <char>
#>  1:      alive           alive        alive
#>  2:      alive           alive    undecided
#>  3: eliminated      eliminated   eliminated
#>  4: eliminated      eliminated   eliminated
#>  5:      alive        clinched        alive
#>  6:      alive           alive        alive
#>  7: eliminated           alive   eliminated
#>  8: eliminated           alive   eliminated
#>  9:      alive           alive    undecided
#> 10:      alive           alive        alive
#> 11:      alive           alive        alive
#> 12: eliminated      eliminated   eliminated
#> 13:      alive           alive        alive
#> 14:      alive           alive   eliminated
#> 15:      alive           alive   eliminated
#> 16: eliminated       undecided   eliminated
#> 17:      alive        clinched        alive
#> 18:      alive        clinched        alive
#> 19: eliminated           alive   eliminated
#> 20: eliminated      eliminated   eliminated
#> 21:      alive           alive        alive
#> 22:      alive           alive   eliminated
#> 23: eliminated           alive   eliminated
#> 24: eliminated           alive   eliminated
#> 25:      alive           alive   eliminated
#> 26:      alive           alive   eliminated
#> 27:      alive           alive   eliminated
#> 28: eliminated      eliminated   eliminated
#> 29:   clinched        clinched        alive
#> 30: eliminated           alive   eliminated
#> 31: eliminated           alive   eliminated
#> 32: eliminated      eliminated   eliminated
#>     div_status playoffs_status seed1_status
#>         <char>          <char>       <char>
# }
```
