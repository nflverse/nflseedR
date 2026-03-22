#' Compute NFL Regular Season Matchups
#'
#' @description
#' Use standings of a completed season to compute all matchups (including location)
#' of the following season based on NFL regular season schedule rules.
#'
#' @param previous_season_standings NFL standings of the previous season as
#'  computed by [nfl_standings()].
#'
#' @returns A data frame with one row per team/opponent matchup (this means
#'  544 rows in a 272 games season), including the following columns:
#'  \describe{
#'    \item{`season`}{The season following the season in `previous_season_standings`}
#'    \item{`team`}{The team abbreviation of the team we are looking at}
#'    \item{`opp`}{The team abbreviation of the team's opponent}
#'    \item{`opp_type`}{The opponent type. This will be on of the following.
#'      \describe{
#'        \item{`div_a`}{Divisional matchup on the road}
#'        \item{`div_h`}{Divisional matchup at home}
#'        \item{`intra_conf`}{Full division matchup intra conference}
#'        \item{`cross_conf`}{Full division matchup cross conference}
#'        \item{`div_rank`}{Same division rank matchup intra conference}
#'        \item{`cross_17th`}{17th game, same division rank matchup cross conference}
#'      }
#'    }
#'    \item{`location`}{Either `home` or `away`. It's impossible to predict neutral site games}
#'  }
#'
#' @export
#'
#' @details
#' Since the 2021 season, NFL teams play a 17 game schedule. NFL operations
#' explains the anatomy of a schedule here
#' <https://operations.nfl.com/gameday/nfl-schedule/creating-the-nfl-schedule/>.
#'
#' A team's opponents are determined based on the following rules:
#' * 6 games against divisional opponents
#' * 4 games against teams from a division within its conference
#' * 4 games against teams from a division in the other conference
#' * 2 games against teams from the remaining divisions in its conference that
#' finished the previous season on the same division rank
#' * 1 game ("the 17th game") against a non-conference opponent from a division
#' that the team is not scheduled to play and that finished the previous season
#' on the same division rank
#'
#' The host of matchups rotates regularly. Rotation has different cycles as
#' explained below. Here is how hosts are determined:
#' * Each team hosts 3 division matchups
#' * Intra conference division matchups rotate in a 6 year cycle
#' * Cross conference division matchups rotate in a 8 year cycle
#' * Intra conference divisional rank matchups rotate in a 6 year cylce
#' * AFC teams host the 17th game in odd years, NFC teams host in even years
#'
#' All rotation tables are stored inside nflseedR and used to compute the
#' compete regular season matchup table.
#'
#' @examples
#' # Compute all 2026 matchups using standings from the 2025 season
#'
#' # First load games of the 2025 season
#' games <- nflreadr::load_schedules(2025)
#'
#' # Second compute standings
#' standings <- nflseedR::nfl_standings(games, ranks = "DIV")
#'
#' # Finally compute matchups
#' matchups <- nflseedR::nfl_regular_season(standings)
#'
#' # Overview output
#' str(matchups)
nfl_regular_season <- function(previous_season_standings) {
  standings <- validate_standings(previous_season_standings)
  # pluck season from standings and add 1 to get next season
  next_season <- standings[, unique(season) + 1L]
  # now we can drop season from standings
  standings[, season := NULL]
  n_teams <- standings[, uniqueN(team)]

  # we build a template table using all team abbreviations from standings
  # and the default opponent types. Then we join standings to team abbreviations
  # to get last season's division rank and division abbreviation.
  opps <- data.table(
    team = standings[, unique(team)] |> sort(),
    opp_type = rep(
      c(
        rep("div_h", 3), # 3 x intra division home
        rep("div_a", 3), # 3 x intra division away
        rep("intra_conf", 4), # 4 x intra conference
        rep("div_rank", 2), # 2 x intra conference (same division rank)
        rep("cross_conf", 4), # 4 x cross conference
        "cross_17th" # 1 x cross conference 17th game
      ),
      n_teams
    )
  )[standings, on = "team"]

  # next we will use opp_type to set the opponent's previous season division rank
  # it'll be more efficient wehn we set opp_type as key
  setkey(opps, opp_type)

  # define division rank of opponent team based on opponent type
  # intra division is all ranks but the team's rank
  # intra conf and cross conf is all ranks
  # div rank and cross 17th is the team's rank
  opps["div_a", opp_div_rank := setdiff(1:4, div_rank), by = "team"]
  opps["div_h", opp_div_rank := setdiff(1:4, div_rank), by = "team"]
  opps["intra_conf", opp_div_rank := seq_len(4), by = "team"]
  opps["cross_conf", opp_div_rank := seq_len(4), by = "team"]
  opps["div_rank", opp_div_rank := div_rank]
  opps["cross_17th", opp_div_rank := div_rank]

  # define opponent division based on season and rotation tables
  # (rotation tables are stored in sysdata. See data-raw/internal_constants.R
  # for the code to compute them)
  # the result are named vectors where name is team division
  # and value is opponent division in that given season
  intra_conf_vctr <- data.table(intra_conf_rotation, key = "index")[
    list(next_season %% 3),
    setNames(intra_conf_opp, team_div)
  ]
  cross_conf_vctr <- data.table(cross_conf_rotation, key = "index")[
    list(next_season %% 4),
    setNames(cross_conf_opp, team_div)
  ]
  cross_17th_vctr <- data.table(cross_17th_rotation, key = "index")[
    list(next_season %% 4),
    setNames(cross_17th_opp, team_div)
  ]

  # intra division opponents obviously same as team division
  opps["div_a", opp_div := team_div]
  opps["div_h", opp_div := team_div]

  # intra conf, cross conf, and cross 17th calculated using the above
  # named vectors based on rotation tables
  opps["intra_conf", opp_div := intra_conf_vctr[team_div]]
  opps["cross_conf", opp_div := cross_conf_vctr[team_div]]
  opps["cross_17th", opp_div := cross_17th_vctr[team_div]]

  # for division of the div_rank opponents we need to compute the two
  # intra conf divisions the team's not playing to this point
  opps[
    "div_rank",
    opp_div := paste0(
      # This will be AFC or NFC
      substr(team_div, 1, 3),
      # This returns two cardinal directions
      setdiff(
        c("E", "N", "S", "W"),
        c(div_dir(team_div), div_dir(intra_conf_vctr[team_div]))
      )
    ),
    by = "team"
  ]

  # now join standings on opponent division and div_rank
  # add season column, and sort by team and opp
  opps <- opps[
    standings[, list(opp = team, opp_div = team_div, opp_div_rank = div_rank)],
    on = c("opp_div", "opp_div_rank")
  ]

  # add index used to join location rotation tables
  opps[,
    index := fcase(
      opp_type == "intra_conf" , next_season %% 6L ,
      opp_type == "cross_conf" , next_season %% 8L ,
      opp_type == "div_rank"   , next_season %% 6L ,
      opp_type == "cross_17th" , next_season %% 2L ,
      default = NA_integer_
    )
  ]

  # predefine location so we can update it in subsequent joins
  opps[, location := NA_character_]

  # make sure internal rotation table are data.tables for joins
  full_div_rotation <- data.table(full_div_rotation)
  div_rank_rotation <- data.table(div_rank_rotation)

  # this is an updating join by reference
  # see https://r-datatable.com/articles/datatable-joins.html "Updating by Reference"
  # insane data.table feature
  # join full_div_rotation by the given columns and update location in opps
  opps[
    full_div_rotation,
    on = c("team", "opp", "index", "opp_type"),
    location := i.location
  ]

  # do the same as above again by join div_rank_rotation on different columns
  # and update location in the affected rows
  opps[
    div_rank_rotation,
    on = c("team_div", "opp_div", "index"),
    location := i.location
  ]

  # div_a and div_h tell us location by design
  opps["div_a", location := "away", on = "opp_type"]
  opps["div_h", location := "home", on = "opp_type"]

  # we need conference for the next part
  opps[, team_conf := substr(team_div, 1, 3)]

  # cross 17th host: AFC hosts odd years. NFC hosts even years
  opps[
    "cross_17th",
    location := fcase(
      next_season %% 2 == 0 & team_conf == "NFC" , "home" ,
      next_season %% 2 == 0 & team_conf == "AFC" , "away" ,
      next_season %% 2 != 0 & team_conf == "NFC" , "away" ,
      next_season %% 2 != 0 & team_conf == "AFC" , "home" ,
      default = NA_character_
    ),
    on = "opp_type"
  ]

  out <- opps[, season := next_season][
    # remove cross_17th matchup from seasons where it didn't exist
    !(opp_type == "cross_17th" & season < 2021),
    list(
      season,
      team,
      opp,
      opp_type,
      location
    )
  ]
  setorder(out, team, opp)

  # return as data frame
  setDF(out)
}

validate_standings <- function(standings) {
  if (!is.data.table(standings)) {
    setDT(standings)
  }
  standings_names <- colnames(standings)
  required_vars <- c(
    "season",
    "team",
    "div_rank"
  )
  missing <- setdiff(required_vars, standings_names)
  if (length(missing) > 0) {
    cli::cli_abort(
      "The provided standings are missing the following column{?s}: {.val {missing}}",
      call = rlang::caller_env()
    )
  }
  n_seasons <- uniqueN(standings$season)
  if (n_seasons > 1) {
    cli::cli_abort(
      "The provided standings include more than 1 season!",
      call = rlang::caller_env()
    )
  }
  divs <- setNames(divisions$sdiv, divisions$team)
  s <- standings[, required_vars, with = FALSE]
  s[, team_div := unname(divs[team])]
  s[, team := nflreadr::clean_team_abbrs(team, keep_non_matches = FALSE)]
  s
}

# Extracts E, N, S, W from division names like AFCW
div_dir <- function(div) {
  substr(div, 4, 4)
}
