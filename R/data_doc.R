#' NFL team names and the conferences and divisions they belong to
#'
#' @docType data
#' @format A data frame with 36 rows and 4 variables containing NFL team level
#' information, including franchises in multiple cities:
#' \describe{
#'   \item{team}{Team abbreviation}
#'   \item{conf}{Conference abbreviation}
#'   \item{division}{Division name}
#'   \item{sdiv}{Division abbreviation}
#' }
#' This data frame is created using the `teams_colors_logos` data frame of the
#' `nflfastR` package. Please see `data-raw/divisions.R` for the code to create
#' this data.
#' @examples
#' str(divisions)
"divisions"

#' Example Games Data used in NFL Simulations
#'
#' @docType data
#' @format A data frame with 284 rows and 9 variables containing NFL schedule
#' information.
#' @details
#'  Please see `data-raw/sim_examples.R` for the code to create this data.
#' @examples
#' str(sims_games_example)
"sims_games_example"

#' Example Teams Data used in NFL Simulations
#'
#' @docType data
#' @format A data frame with 64 rows and 5 variables containing team name and
#' division information.
#' @details
#'  Please see `data-raw/sim_examples.R` for the code to create this data.
#' @examples
#' str(sims_teams_example)
"sims_teams_example"

#' Data Dictionary: Simulations | Standings
#'
#' A dataframe containing the data dictionary of the simulation output table "standings"
#'
#' @seealso <https://nflseedr.com/articles/articles/nflsim2.html#simulation-output>
"dictionary_standings"

#' Data Dictionary: Simulations | Games
#'
#' A dataframe containing the data dictionary of the simulation output table "games"
#'
#' @seealso <https://nflseedr.com/articles/articles/nflsim2.html#simulation-output>
"dictionary_games"

#' Data Dictionary: Simulations | Overall
#'
#' A dataframe containing the data dictionary of the simulation output table "overall"
#'
#' @seealso <https://nflseedr.com/articles/articles/nflsim2.html#simulation-output>
"dictionary_overall"

#' Data Dictionary: Simulations | Team Wins
#'
#' A dataframe containing the data dictionary of the simulation output table "team_wins"
#'
#' @seealso <https://nflseedr.com/articles/articles/nflsim2.html#simulation-output>
"dictionary_team_wins"

#' Data Dictionary: Simulations | Game Summary
#'
#' A dataframe containing the data dictionary of the simulation output table "game_summary"
#'
#' @seealso <https://nflseedr.com/articles/articles/nflsim2.html#simulation-output>
"dictionary_game_summary"
