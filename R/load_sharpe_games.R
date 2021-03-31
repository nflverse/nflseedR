#' Load Lee Sharpe's Games File
#'
#' @description Lee Sharpe maintains an important data set that contains
#' broadly used information on games in the National Football League. This
#' function is a convenient helper to download the file into memory without
#' having to remember the correct url.
#'
#' @examples
#' \donttest{
#' games <- load_sharpe_games()
#' dplyr::glimpse(games)
#' \dontshow{
#' # Close open connections for R CMD Check
#' future::plan("sequential")
#' }
#' }
#' @returns A data frame containing the following variables for all NFL games
#' since 1999:
#' \describe{
#' \item{game_id}{The ID of the game as assigned by the NFL. Note that this value matches the `game_id` field in nflscrapR if you wish to join the data.}
# \item{alt_game_id}{This is a more human-readable ID. It consists of: The season, an underscore, the two-digit week number, an underscore, the away team, an underscore, the home team.}
#' \item{season}{The year of the NFL season. This represents the whole season, so regular season games that happen in January as well as playoff games will occur in the year after this number.}
#' \item{game_type}{What type of game? One of the following values:
#' \itemize{
#' \item{`REG`}{: a regular season game}
#' \item{`WC`}{: a wildcard playoff game}
#' \item{`DIV`}{: a divisional round playoff game}
#' \item{`CON`}{: a conference championship}
#' \item{`SB`}{: a Super Bowl}
#' }
#' }
#' \item{week}{The week of the NFL season the game occurs in. This will be 1-17 for the regular season, 18 for wildcard playoff games, 19 for divisional playoff games, 20 for conference championships and 21 for Super Bowls.}
#' \item{gameday}{The date on which the game occurred.}
#' \item{weekday}{The day of the week on which the game occurred.}
#' \item{gametime}{The kickoff time of the game. This is represented in 24-hour time and the Eastern time zone, regardless of what time zone the game was being played in.}
#' \item{away_team}{The away team.}
#' \item{away_score}{The number of points the away team scored. Is `NA` for games which haven't yet been played.}
#' \item{home_team}{The home team. Note that this contains the designated home team for games which no team is playing at home such as Super Bowls or NFL International games.}
#' \item{home_score}{The number of points the home team scored. Is `NA` for games which haven't yet been played.}
#' \item{location}{Either `Home` if the home team is playing in their home stadium, or `Neutral` if the game is being played at a neutral location. This still shows as `Home` for games between the Giants and Jets even though they share the same home stadium.}
#' \item{result}{Equals `home_score - away_score`. The number of points the home team scored minus the number of points the away team scored. Is `NA` for games which haven't yet been played. Convenient for evaluating against the spread bets.}
#' \item{total}{The sum of each team's score in the game. Equals `home_score + away_score`. Is `NA` for games which haven't yet been played. Convenient for evaluating over/under total bets.}
#' \item{overtime}{Whether the game went into overtime (= 1) or not (= 0).}
#' \item{old_game_id}{The id of the game issued by the NFL Game Statistics & Information System.}
#' \item{away_rest}{The number of days since that away team's previous game (7 is used for the team's first game of the season).}
#' \item{home_rest}{The number of days since that home team's previous game (7 is used for the team's first game of the season).}
#' \item{away_moneyline}{Odd of the away_team winning the game.}
#' \item{home_moneyline}{Odd of the home_team winning the game.}
#' \item{spread_line}{The spread line for the game. A positive number means the home team was favored by that many points, a negative number means the away team was favored by that many points. This lines up with the `result` column.}
#' \item{away_spread_odds}{Odd of the away_team covering the `spread_line`.}
#' \item{home_spread_odds}{Odd of the home_team covering the `spread_line`.}
#' \item{total_line}{The total line for the game.}
#' \item{under_odds}{Odd of the `total` being under the `total_line`.}
#' \item{over_odds}{Odd of the `total` being over the `total_line`.}
#' \item{div_game}{Whether the game was a divisional game (= 1) or not (= 0).}
# \item{pfr}{The id of the game issued by [Pro Football Reference](https://www.pro-football-reference.com/)}
# \item{pff}{The id of the game issued by [Pro Football Focus](https://www.pff.com/)}
# \item{espn}{The id of the game issued by [ESPN](https://www.espn.com/)}
#' \item{roof}{What was the status of the stadium's roof? Will be one of the following values:
#' \itemize{
#' \item{`closed`}{: Stadium has a retractable roof which was closed}
#' \item{`dome`}{: An indoor stadium}
#' \item{`open`}{: Stadium has a retractable roof which was open}
#' \item{`outdoors`}{: An outdoor stadium}
#' }
#' }
#' \item{surface}{What type of ground the game was played on.}
#' \item{temp}{The temperature at the stadium (for `roof` types `outdoors` and `open` only).}
#' \item{wind}{The speed of the wind in miles/hour (for `roof` types `outdoors` and `open` only).}
#' \item{away_qb_id}{GSIS ID of the "starting quarterback" of the away team identified as the first
#' quarterback (per roster data) listed as `passer` (in `nflfastR` play by play data)
#' in 2+ plays that game. In the final regular season game it is the QB with the
#' most plays as the `passer`.}
#' \item{home_qb_id}{GSIS ID of the "starting quarterback" of the home team identified as the first
#' quarterback (per roster data) listed as `passer` (in `nflfastR` play by play data)
#' in 2+ plays that game. In the final regular season game it is the QB with the
#' most plays as the `passer`.}
#' \item{away_qb_name}{Full name of the "starting quarterback" of the away team identified as the first
#' quarterback (per roster data) listed as `passer` (in `nflfastR` play by play data)
#' in 2+ plays that game. In the final regular season game it is the QB with the
#' most plays as the `passer`.}
#' \item{home_qb_name}{Full name of the "starting quarterback" of the home team identified as the first
#' quarterback (per roster data) listed as `passer` (in `nflfastR` play by play data)
#' in 2+ plays that game. In the final regular season game it is the QB with the
#' most plays as the `passer`.}
#' \item{away_coach}{Name of the head coach of the away team.}
#' \item{home_coach}{Name of the head coach of the home team.}
#' \item{referee}{Name of the game's referee (head official).}
#' \item{stadium_id}{[Pro Football Reference](https://www.pro-football-reference.com/) ID of the stadium.}
#' \item{stadium}{Name of the stadium.}
#' }
#' @export
load_sharpe_games <- function(){
  fetched <- curl::curl_fetch_memory("https://github.com/leesharpe/nfldata/blob/master/data/games.rds?raw=true")
  if (fetched$status_code != 200) return(tibble::tibble())
  read_raw_rds(fetched$content)
}

read_raw_rds <- function(raw) {
  con <- gzcon(rawConnection(raw))
  ret <- readRDS(con)
  close(con)
  return(ret)
}
