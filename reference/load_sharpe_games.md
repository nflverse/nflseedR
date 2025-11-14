# Load Lee Sharpe's Games File

**\[deprecated\]**

Lee Sharpe maintains an important data set that contains broadly used
information on games in the National Football League. This function is a
convenient helper to download the file into memory without having to
remember the correct url.

## Usage

``` r
load_sharpe_games(...)
```

## Arguments

- ...:

  Arguments passed on to
  [`nflreadr::load_schedules`](https://nflreadr.nflverse.com/reference/load_schedules.html)

  `seasons`

  :   a numeric vector of seasons to return, default `TRUE` returns all
      available data.

## Value

A data frame containing the following variables for all NFL games since
1999:

- game_id:

  The ID of the game as assigned by the nflverse. Note that this value
  matches the `game_id` field in nflfastR if you wish to join the data.

- season:

  The year of the NFL season. This represents the whole season, so
  regular season games that happen in January as well as playoff games
  will occur in the year after this number.

- game_type:

  What type of game? One of the following values:

  `REG`

  :   a regular season game

  `WC`

  :   a wildcard playoff game

  `DIV`

  :   a divisional round playoff game

  `CON`

  :   a conference championship

  `SB`

  :   a Super Bowl

- week:

  The week of the NFL season the game occurs in. Please note that the
  `game_type` will differ for weeks \>= 18 because of the season
  expansion in 2021. Please use `game_type` to filter for regular season
  or postseason.

- gameday:

  The date on which the game occurred.

- weekday:

  The day of the week on which the game occurred.

- gametime:

  The kickoff time of the game. This is represented in 24-hour time and
  the Eastern time zone, regardless of what time zone the game was being
  played in.

- away_team:

  The away team.

- away_score:

  The number of points the away team scored. Is `NA` for games which
  haven't yet been played.

- home_team:

  The home team. Note that this contains the designated home team for
  games which no team is playing at home such as Super Bowls or NFL
  International games.

- home_score:

  The number of points the home team scored. Is `NA` for games which
  haven't yet been played.

- location:

  Either `Home` if the home team is playing in their home stadium, or
  `Neutral` if the game is being played at a neutral location. This
  still shows as `Home` for games between the Giants and Jets even
  though they share the same home stadium.

- result:

  Equals `home_score - away_score`. The number of points the home team
  scored minus the number of points the away team scored. Is `NA` for
  games which haven't yet been played. Convenient for evaluating against
  the spread bets.

- total:

  The sum of each team's score in the game. Equals
  `home_score + away_score`. Is `NA` for games which haven't yet been
  played. Convenient for evaluating over/under total bets.

- overtime:

  Whether the game went into overtime (= 1) or not (= 0).

- old_game_id:

  The id of the game issued by the NFL Game Statistics & Information
  System.

- away_rest:

  The number of days since that away team's previous game (7 is used for
  the team's first game of the season).

- home_rest:

  The number of days since that home team's previous game (7 is used for
  the team's first game of the season).

- away_moneyline:

  Odd of the away_team winning the game.

- home_moneyline:

  Odd of the home_team winning the game.

- spread_line:

  The spread line for the game. A positive number means the home team
  was favored by that many points, a negative number means the away team
  was favored by that many points. This lines up with the `result`
  column.

- away_spread_odds:

  Odd of the away_team covering the `spread_line`.

- home_spread_odds:

  Odd of the home_team covering the `spread_line`.

- total_line:

  The total line for the game.

- under_odds:

  Odd of the `total` being under the `total_line`.

- over_odds:

  Odd of the `total` being over the `total_line`.

- div_game:

  Whether the game was a divisional game (= 1) or not (= 0).

- roof:

  What was the status of the stadium's roof? Will be one of the
  following values:

  `closed`

  :   Stadium has a retractable roof which was closed

  `dome`

  :   An indoor stadium

  `open`

  :   Stadium has a retractable roof which was open

  `outdoors`

  :   An outdoor stadium

- surface:

  What type of ground the game was played on.

- temp:

  The temperature at the stadium (for `roof` types `outdoors` and `open`
  only).

- wind:

  The speed of the wind in miles/hour (for `roof` types `outdoors` and
  `open` only).

- away_qb_id:

  GSIS ID of the "starting quarterback" of the away team identified as
  the first quarterback (per roster data) listed as `passer` (in
  `nflfastR` play by play data) in 2+ plays that game. In the final
  regular season game it is the QB with the most plays as the `passer`.

- home_qb_id:

  GSIS ID of the "starting quarterback" of the home team identified as
  the first quarterback (per roster data) listed as `passer` (in
  `nflfastR` play by play data) in 2+ plays that game. In the final
  regular season game it is the QB with the most plays as the `passer`.

- away_qb_name:

  Full name of the "starting quarterback" of the away team identified as
  the first quarterback (per roster data) listed as `passer` (in
  `nflfastR` play by play data) in 2+ plays that game. In the final
  regular season game it is the QB with the most plays as the `passer`.

- home_qb_name:

  Full name of the "starting quarterback" of the home team identified as
  the first quarterback (per roster data) listed as `passer` (in
  `nflfastR` play by play data) in 2+ plays that game. In the final
  regular season game it is the QB with the most plays as the `passer`.

- away_coach:

  Name of the head coach of the away team.

- home_coach:

  Name of the head coach of the home team.

- referee:

  Name of the game's referee (head official).

- stadium_id:

  [Pro Football Reference](https://www.pro-football-reference.com/) ID
  of the stadium.

- stadium:

  Name of the stadium.

## See also

The internally called function
[`nflreadr::load_schedules()`](https://nflreadr.nflverse.com/reference/load_schedules.html)

## Examples

``` r
# \donttest{
try({#to avoid CRAN test problems
games <- load_sharpe_games()
dplyr::glimpse(games)
})
#> Rows: 7,263
#> Columns: 46
#> $ game_id          <chr> "1999_01_MIN_ATL", "1999_01_KC_CHI", "1999_01_PIT_CLE…
#> $ season           <int> 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999, 1999,…
#> $ game_type        <chr> "REG", "REG", "REG", "REG", "REG", "REG", "REG", "REG…
#> $ week             <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2,…
#> $ gameday          <chr> "1999-09-12", "1999-09-12", "1999-09-12", "1999-09-12…
#> $ weekday          <chr> "Sunday", "Sunday", "Sunday", "Sunday", "Sunday", "Su…
#> $ gametime         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ away_team        <chr> "MIN", "KC", "PIT", "OAK", "BUF", "SF", "CAR", "NE", …
#> $ away_score       <int> 17, 17, 43, 24, 14, 3, 10, 30, 25, 28, 10, 17, 35, 41…
#> $ home_team        <chr> "ATL", "CHI", "CLE", "GB", "IND", "JAX", "NO", "NYJ",…
#> $ home_score       <int> 14, 20, 0, 28, 31, 41, 19, 28, 24, 20, 27, 13, 36, 35…
#> $ location         <chr> "Home", "Home", "Home", "Home", "Home", "Home", "Home…
#> $ result           <int> -3, 3, -43, 4, 17, 38, 9, -2, -1, -8, 17, -4, 1, -6, …
#> $ total            <int> 31, 37, 43, 52, 45, 44, 29, 58, 49, 48, 37, 30, 71, 7…
#> $ overtime         <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,…
#> $ old_game_id      <chr> "1999091210", "1999091206", "1999091213", "1999091208…
#> $ gsis             <int> 598, 597, 604, 602, 591, 603, 592, 600, 588, 596, 589…
#> $ nfl_detail_id    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ pfr              <chr> "199909120atl", "199909120chi", "199909120cle", "1999…
#> $ pff              <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ espn             <chr> "190912001", "190912003", "190912005", "190912009", "…
#> $ ftn              <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ away_rest        <int> 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,…
#> $ home_rest        <int> 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,…
#> $ away_moneyline   <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ home_moneyline   <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ spread_line      <dbl> -4.0, -3.0, -6.0, 9.0, -3.0, 5.5, 3.5, 7.0, -3.0, 9.5…
#> $ away_spread_odds <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ home_spread_odds <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ total_line       <dbl> 49.0, 38.0, 37.0, 43.0, 45.5, 49.0, 38.0, 44.5, 37.0,…
#> $ under_odds       <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ over_odds        <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ div_game         <int> 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0,…
#> $ roof             <chr> "dome", "outdoors", "outdoors", "outdoors", "dome", "…
#> $ surface          <chr> "astroturf", "grass", "grass", "grass", "astroturf", …
#> $ temp             <int> NA, 80, 78, 67, NA, 76, NA, 73, 75, NA, NA, 88, 84, 7…
#> $ wind             <int> NA, 12, 12, 10, NA, 8, NA, 5, 3, NA, NA, 8, 10, 0, 5,…
#> $ away_qb_id       <chr> "00-0003761", "00-0006300", "00-0015700", "00-0005741…
#> $ home_qb_id       <chr> "00-0002876", "00-0010560", "00-0004230", "00-0005106…
#> $ away_qb_name     <chr> "Randall Cunningham", "Elvis Grbac", "Kordell Stewart…
#> $ home_qb_name     <chr> "Chris Chandler", "Shane Matthews", "Ty Detmer", "Bre…
#> $ away_coach       <chr> "Dennis Green", "Gunther Cunningham", "Bill Cowher", …
#> $ home_coach       <chr> "Dan Reeves", "Dick Jauron", "Chris Palmer", "Ray Rho…
#> $ referee          <chr> "Gerry Austin", "Phil Luckett", "Bob McElwee", "Tony …
#> $ stadium_id       <chr> "ATL00", "CHI98", "CLE00", "GNB00", "IND99", "JAX00",…
#> $ stadium          <chr> "Georgia Dome", "Soldier Field", "Cleveland Browns St…
# }
```
