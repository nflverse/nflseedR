# Compute NFL Standings

Compute NFL Standings

## Usage

``` r
nfl_standings(
  games,
  ...,
  ranks = c("CONF", "DIV", "DRAFT", "NONE"),
  tiebreaker_depth = c("SOS", "PRE-SOV", "POINTS", "RANDOM"),
  playoff_seeds = NULL,
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

- ...:

  currently not used

- ranks:

  One of `"DIV"`, `"CONF"`, `"DRAFT"`, or `"NONE"` to specify which
  ranks - and thus the associated tiebreakers - are to be determined.

  - `"DIV"`: Adds the division ranking variable `div_rank`

  - `"CONF"` (default): `"DIV"` + the conference variable `conf_rank`.
    For better performance, it is possible to set `playoff_seeds` to a
    value \< 16 to make the function skip tiebreakers of irrelevant
    conference ranks.

  - `"DRAFT"`: `"CONF"` + the draft variable `draft_rank`. This is the
    actual pick in the draft based off game results. No trades of
    course.

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

- playoff_seeds:

  If `NULL` (the default), will compute all 16 conference ranks. This
  means, the function applies conference tiebreakers to all conference
  ranks. For better performance, it is possible to set this to a value
  \< 16 to make the function skip tiebreakers of those conference ranks.

- verbosity:

  One of `"MIN"`, `"MAX"`, or `"NONE"` allowing the user to set the
  grade of verbosity of status reports. They mean:

  - `"MIN"` (default): Prints main steps of the process.

  - `"MAX"`: Prints all steps of the complete tiebreaking process.

  - `"NONE"`: No status reports at all. Do this to maximize the
    performance.

## Value

A data.table of NFL standings including the ranks selected in the
argument `ranks`

## Details

nflseedR does not support all levels of tie-breakers at the moment. The
deepest tie-breaker currently is "best net points in all games". After
that, the decision is made at random. However, the need for the last
level ("best net touchdowns in all games") is extremely unlikely in
practice. Deeper levels than strength of schedule have never actually
been needed to resolve season-end standings since the NFL expanded to 32
teams.

## See also

For more information on the implemented tiebreakers, see
<https://nflseedr.com/articles/tiebreaker.html>

## Examples

``` r
# \donttest{
try({#to avoid CRAN test problems
  games <- nflreadr::load_schedules(2021:2022)
})
standings <- nflseedR::nfl_standings(games)
#> ℹ 12:40:52 | Initiate Standings & Tiebreaking Data
#> ℹ 12:40:52 | Compute Division Ranks
#> ℹ 12:40:52 | Compute Conference Ranks
print(standings, digits = 3)
#>     season   team   conf  division games  wins true_wins losses  ties    pf
#>      <int> <char> <char>    <char> <int> <num>     <int>  <int> <int> <int>
#>  1:   2021    BUF    AFC  AFC East    17  11.0        11      6     0   483
#>  2:   2021     NE    AFC  AFC East    17  10.0        10      7     0   462
#>  3:   2021    MIA    AFC  AFC East    17   9.0         9      8     0   341
#>  4:   2021    NYJ    AFC  AFC East    17   4.0         4     13     0   310
#>  5:   2021    CIN    AFC AFC North    17  10.0        10      7     0   460
#>  6:   2021    PIT    AFC AFC North    17   9.5         9      7     1   343
#>  7:   2021    CLE    AFC AFC North    17   8.0         8      9     0   349
#>  8:   2021    BAL    AFC AFC North    17   8.0         8      9     0   387
#>  9:   2021    TEN    AFC AFC South    17  12.0        12      5     0   419
#> 10:   2021    IND    AFC AFC South    17   9.0         9      8     0   451
#> 11:   2021    HOU    AFC AFC South    17   4.0         4     13     0   280
#> 12:   2021    JAX    AFC AFC South    17   3.0         3     14     0   253
#> 13:   2021     KC    AFC  AFC West    17  12.0        12      5     0   480
#> 14:   2021     LV    AFC  AFC West    17  10.0        10      7     0   374
#> 15:   2021    LAC    AFC  AFC West    17   9.0         9      8     0   474
#> 16:   2021    DEN    AFC  AFC West    17   7.0         7     10     0   335
#> 17:   2021    DAL    NFC  NFC East    17  12.0        12      5     0   530
#> 18:   2021    PHI    NFC  NFC East    17   9.0         9      8     0   444
#> 19:   2021    WAS    NFC  NFC East    17   7.0         7     10     0   335
#> 20:   2021    NYG    NFC  NFC East    17   4.0         4     13     0   258
#> 21:   2021     GB    NFC NFC North    17  13.0        13      4     0   450
#> 22:   2021    MIN    NFC NFC North    17   8.0         8      9     0   425
#> 23:   2021    CHI    NFC NFC North    17   6.0         6     11     0   311
#> 24:   2021    DET    NFC NFC North    17   3.5         3     13     1   325
#> 25:   2021     TB    NFC NFC South    17  13.0        13      4     0   511
#> 26:   2021     NO    NFC NFC South    17   9.0         9      8     0   364
#> 27:   2021    ATL    NFC NFC South    17   7.0         7     10     0   313
#> 28:   2021    CAR    NFC NFC South    17   5.0         5     12     0   304
#> 29:   2021     LA    NFC  NFC West    17  12.0        12      5     0   460
#> 30:   2021    ARI    NFC  NFC West    17  11.0        11      6     0   449
#> 31:   2021     SF    NFC  NFC West    17  10.0        10      7     0   427
#> 32:   2021    SEA    NFC  NFC West    17   7.0         7     10     0   395
#> 33:   2022    BUF    AFC  AFC East    16  13.0        13      3     0   455
#> 34:   2022    MIA    AFC  AFC East    17   9.0         9      8     0   397
#> 35:   2022     NE    AFC  AFC East    17   8.0         8      9     0   364
#> 36:   2022    NYJ    AFC  AFC East    17   7.0         7     10     0   296
#> 37:   2022    CIN    AFC AFC North    16  12.0        12      4     0   418
#> 38:   2022    BAL    AFC AFC North    17  10.0        10      7     0   350
#> 39:   2022    PIT    AFC AFC North    17   9.0         9      8     0   308
#> 40:   2022    CLE    AFC AFC North    17   7.0         7     10     0   361
#> 41:   2022    JAX    AFC AFC South    17   9.0         9      8     0   404
#> 42:   2022    TEN    AFC AFC South    17   7.0         7     10     0   298
#> 43:   2022    IND    AFC AFC South    17   4.5         4     12     1   289
#> 44:   2022    HOU    AFC AFC South    17   3.5         3     13     1   289
#> 45:   2022     KC    AFC  AFC West    17  14.0        14      3     0   496
#> 46:   2022    LAC    AFC  AFC West    17  10.0        10      7     0   391
#> 47:   2022     LV    AFC  AFC West    17   6.0         6     11     0   395
#> 48:   2022    DEN    AFC  AFC West    17   5.0         5     12     0   287
#> 49:   2022    PHI    NFC  NFC East    17  14.0        14      3     0   477
#> 50:   2022    DAL    NFC  NFC East    17  12.0        12      5     0   467
#> 51:   2022    NYG    NFC  NFC East    17   9.5         9      7     1   365
#> 52:   2022    WAS    NFC  NFC East    17   8.5         8      8     1   321
#> 53:   2022    MIN    NFC NFC North    17  13.0        13      4     0   424
#> 54:   2022    DET    NFC NFC North    17   9.0         9      8     0   453
#> 55:   2022     GB    NFC NFC North    17   8.0         8      9     0   370
#> 56:   2022    CHI    NFC NFC North    17   3.0         3     14     0   326
#> 57:   2022     TB    NFC NFC South    17   8.0         8      9     0   313
#> 58:   2022    CAR    NFC NFC South    17   7.0         7     10     0   347
#> 59:   2022     NO    NFC NFC South    17   7.0         7     10     0   330
#> 60:   2022    ATL    NFC NFC South    17   7.0         7     10     0   365
#> 61:   2022     SF    NFC  NFC West    17  13.0        13      4     0   450
#> 62:   2022    SEA    NFC  NFC West    17   9.0         9      8     0   407
#> 63:   2022     LA    NFC  NFC West    17   5.0         5     12     0   307
#> 64:   2022    ARI    NFC  NFC West    17   4.0         4     13     0   340
#>     season   team   conf  division games  wins true_wins losses  ties    pf
#>        pa    pd win_pct div_pct conf_pct   sov   sos div_rank
#>     <int> <int>   <num>   <num>    <num> <num> <num>    <int>
#>  1:   289   194   0.647   0.833   0.5833 0.428 0.472        1
#>  2:   303   159   0.588   0.500   0.6667 0.394 0.481        2
#>  3:   373   -32   0.529   0.667   0.5000 0.379 0.464        3
#>  4:   504  -194   0.235   0.000   0.3333 0.426 0.512        4
#>  5:   376    84   0.588   0.667   0.6667 0.462 0.472        1
#>  6:   398   -55   0.559   0.667   0.5833 0.490 0.521        2
#>  7:   371   -22   0.471   0.500   0.4167 0.415 0.514        3
#>  8:   392    -5   0.471   0.167   0.4167 0.460 0.531        4
#>  9:   354    65   0.706   0.833   0.6667 0.480 0.472        1
#> 10:   365    86   0.529   0.500   0.5833 0.431 0.495        2
#> 11:   452  -172   0.235   0.500   0.3333 0.397 0.498        3
#> 12:   457  -204   0.176   0.167   0.2500 0.569 0.512        4
#> 13:   364   116   0.706   0.833   0.5833 0.517 0.538        1
#> 14:   439   -65   0.588   0.500   0.6667 0.515 0.510        2
#> 15:   459    15   0.529   0.500   0.5000 0.500 0.510        3
#> 16:   322    13   0.412   0.167   0.2500 0.357 0.484        4
#> 17:   358   172   0.706   1.000   0.8333 0.431 0.488        1
#> 18:   385    59   0.529   0.500   0.5833 0.350 0.469        2
#> 19:   434   -99   0.412   0.333   0.5000 0.420 0.529        3
#> 20:   416  -158   0.235   0.167   0.2500 0.485 0.536        4
#> 21:   371    79   0.765   0.667   0.7500 0.480 0.479        1
#> 22:   426    -1   0.471   0.667   0.5000 0.434 0.507        2
#> 23:   407   -96   0.353   0.333   0.3333 0.373 0.524        3
#> 24:   467  -142   0.206   0.333   0.2500 0.627 0.528        4
#> 25:   353   158   0.765   0.667   0.6667 0.443 0.467        1
#> 26:   335    29   0.529   0.667   0.5833 0.516 0.512        2
#> 27:   459  -146   0.412   0.333   0.3333 0.315 0.472        3
#> 28:   404  -100   0.294   0.333   0.2500 0.412 0.509        4
#> 29:   372    88   0.706   0.500   0.6667 0.409 0.483        1
#> 30:   366    83   0.647   0.667   0.5833 0.492 0.490        2
#> 31:   365    62   0.588   0.333   0.5833 0.438 0.500        3
#> 32:   366    29   0.412   0.500   0.3333 0.424 0.519        4
#> 33:   286   169   0.812   0.667   0.8182 0.471 0.489        1
#> 34:   399    -2   0.529   0.500   0.5833 0.457 0.537        2
#> 35:   347    17   0.471   0.500   0.5000 0.415 0.502        3
#> 36:   316   -20   0.412   0.333   0.4167 0.458 0.538        4
#> 37:   322    96   0.750   0.500   0.7273 0.490 0.507        1
#> 38:   315    35   0.588   0.500   0.5000 0.456 0.509        2
#> 39:   346   -38   0.529   0.500   0.4167 0.451 0.519        3
#> 40:   381   -20   0.412   0.500   0.3333 0.492 0.524        4
#> 41:   350    54   0.529   0.667   0.6667 0.438 0.467        1
#> 42:   359   -61   0.412   0.500   0.4167 0.336 0.509        2
#> 43:   427  -138   0.265   0.250   0.3750 0.500 0.512        3
#> 44:   420  -131   0.206   0.583   0.2917 0.402 0.481        4
#> 45:   369   127   0.824   1.000   0.7500 0.422 0.453        1
#> 46:   384     7   0.588   0.333   0.5833 0.341 0.443        2
#> 47:   418   -23   0.353   0.500   0.4167 0.397 0.474        3
#> 48:   359   -72   0.294   0.167   0.2500 0.465 0.481        4
#> 49:   344   133   0.824   0.667   0.7500 0.460 0.474        1
#> 50:   342   125   0.706   0.667   0.6667 0.485 0.507        2
#> 51:   371    -6   0.559   0.250   0.3750 0.395 0.526        3
#> 52:   343   -22   0.500   0.417   0.4583 0.449 0.536        4
#> 53:   427    -3   0.765   0.667   0.6667 0.425 0.474        1
#> 54:   427    26   0.529   0.833   0.5833 0.451 0.535        2
#> 55:   371    -1   0.471   0.500   0.5000 0.449 0.524        3
#> 56:   463  -137   0.176   0.000   0.0833 0.480 0.571        4
#> 57:   358   -45   0.471   0.667   0.6667 0.426 0.503        1
#> 58:   374   -27   0.412   0.667   0.5000 0.437 0.474        2
#> 59:   345   -15   0.412   0.333   0.4167 0.462 0.507        3
#> 60:   386   -21   0.412   0.333   0.5000 0.429 0.467        4
#> 61:   277   173   0.765   1.000   0.8333 0.414 0.417        1
#> 62:   401     6   0.529   0.667   0.5000 0.382 0.462        2
#> 63:   384   -77   0.294   0.167   0.2500 0.341 0.517        3
#> 64:   449  -109   0.235   0.167   0.2500 0.368 0.529        4
#>        pa    pd win_pct div_pct conf_pct   sov   sos div_rank
#>            div_tie_broken_by conf_rank       conf_tie_broken_by
#>                       <char>     <int>                   <char>
#>  1:                     <NA>         3                     <NA>
#>  2:                     <NA>         6                     <NA>
#>  3:                     <NA>         9 Common Games Win PCT (2)
#>  4:                     <NA>        14   Head-To-Head Sweep (2)
#>  5:                     <NA>         4                     <NA>
#>  6:                     <NA>         7                     <NA>
#>  7:     Division Win PCT (2)        11      Division Tiebreaker
#>  8:     Division Win PCT (2)        12      Division Tiebreaker
#>  9:                     <NA>         1   Head-To-Head Sweep (2)
#> 10:                     <NA>         8   Conference Win PCT (3)
#> 11:                     <NA>        15                     <NA>
#> 12:                     <NA>        16                     <NA>
#> 13:                     <NA>         2                     <NA>
#> 14:                     <NA>         5 Common Games Win PCT (2)
#> 15:                     <NA>        10                     <NA>
#> 16:                     <NA>        13                     <NA>
#> 17:                     <NA>         3   Conference Win PCT (2)
#> 18:                     <NA>         7   Head-To-Head Sweep (2)
#> 19:                     <NA>        10   Head-To-Head Sweep (3)
#> 20:                     <NA>        15                     <NA>
#> 21:                     <NA>         1   Conference Win PCT (2)
#> 22:                     <NA>         9                     <NA>
#> 23:                     <NA>        13                     <NA>
#> 24:                     <NA>        16                     <NA>
#> 25:                     <NA>         2                     <NA>
#> 26:                     <NA>         8                     <NA>
#> 27:                     <NA>        12                     <NA>
#> 28:                     <NA>        14                     <NA>
#> 29:                     <NA>         4                     <NA>
#> 30:                     <NA>         5                     <NA>
#> 31:                     <NA>         6                     <NA>
#> 32:                     <NA>        11 Common Games Win PCT (2)
#> 33:                     <NA>         2                     <NA>
#> 34:                     <NA>         7   Head-To-Head Sweep (2)
#> 35:                     <NA>         9                     <NA>
#> 36:                     <NA>        10 Common Games Win PCT (2)
#> 37:                     <NA>         3                     <NA>
#> 38:                     <NA>         6                     <NA>
#> 39:                     <NA>         8                     <NA>
#> 40:                     <NA>        12                     <NA>
#> 41:                     <NA>         4                     <NA>
#> 42:                     <NA>        11   Conference Win PCT (2)
#> 43:                     <NA>        15                     <NA>
#> 44:                     <NA>        16                     <NA>
#> 45:                     <NA>         1                     <NA>
#> 46:                     <NA>         5   Conference Win PCT (2)
#> 47:                     <NA>        13                     <NA>
#> 48:                     <NA>        14                     <NA>
#> 49:                     <NA>         1                     <NA>
#> 50:                     <NA>         5                     <NA>
#> 51:                     <NA>         6                     <NA>
#> 52:                     <NA>         9                     <NA>
#> 53:                     <NA>         3                     <NA>
#> 54:                     <NA>         8                     <NA>
#> 55:                     <NA>        10                     <NA>
#> 56:                     <NA>        16                     <NA>
#> 57:                     <NA>         4                     <NA>
#> 58: Head-To-Head Win PCT (3)        11      Division Tiebreaker
#> 59: Head-To-Head Win PCT (3)        12      Division Tiebreaker
#> 60: Head-To-Head Win PCT (3)        13      Division Tiebreaker
#> 61:                     <NA>         2   Conference Win PCT (2)
#> 62:                     <NA>         7   Head-To-Head Sweep (2)
#> 63:                     <NA>        14                     <NA>
#> 64:                     <NA>        15                     <NA>
#>            div_tie_broken_by conf_rank       conf_tie_broken_by
# }
```
