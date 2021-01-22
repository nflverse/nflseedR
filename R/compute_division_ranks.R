#' Compute NFL Division Rankings using Game Results
#'
#' @param games A data frame containing real or simulated game scores. The
#' following variables are required:
#' \describe{
#'  \item{sim}{A simulation ID. Normally 1 - n simulated seasons.}
#'  \item{game_type}{A simulation ID. Normally 1 - n simulated seasons.}
#'  \item{week}{The week of the corresponding NFL season.}
#'  \item{away_team}{Team abbreviation of the away team (please see
#'    \code{\link{divisions}} for valid team abbreviations).}
#'  \item{home_team}{Team abbreviation of the home team (please see
#'    \code{\link{divisions}} for valid team abbreviations).}
#'  \item{result}{Equals home score - away score.}
#' }
#' @param teams This parameter is optional. If it is \code{NULL} the function
#'  will compute it internally, otherwise it has to be a data frame of all teams
#'  contained in the \code{games} data frame repeated for each simulation ID
#'  (\code{sim}). The following variables are required:
#' \describe{
#'  \item{sim}{A simulation ID. Normally 1 - n simulated seasons.}
#'  \item{team}{Team abbreviation of the team (please see
#'    \code{\link{divisions}} for valid team abbreviations).}
#'  \item{conf}{Conference abbreviation of the team (please see
#'    \code{\link{divisions}} for valid team abbreviations).}
#'  \item{division}{Division of the team (please see
#'    \code{\link{divisions}} for valid division names).}
#' }
#' @param tiebreaker_depth A single numeric value in the range of 1, 2, 3. The
#'  value controls the depth of tiebreakers that shall be applied. The deepest
#'  currently implemented tiebreaker is strength of schedule. The following
#'  values are valid:
#'  \describe{
#'  \item{tiebreaker_depth = 1}{Break all ties with a coinflip. Fastest variant.}
#'  \item{tiebreaker_depth = 2}{Apply head-to-head and division win percentage tiebreakers.}
#'  \item{tiebreaker_depth = 3}{Apply all tiebreakers through strength of schedule.}
#'  }
#' @param .debug Either \code{TRUE} or \code{FALSE}. Controls if additional
#' messages are printed to the console showing what the tiebreaking algorithms
#' are currently performing.
#' @param h2h A data frame that is used for head-to-head tiebreakers across the
#' tiebreaking functions. It is computed by the function
#' \code{\link{compute_division_ranks}}.
#' @returns A list of two data frames:
#'  \describe{
#'  \item{standings}{The division standings.}
#'  \item{h2h}{A data frame that is used for head-to-head tiebreakers across the
#'  tiebreaking functions.}
#'  }
#' @export
#' @examples
#' \donttest{
#' options(digits = 3)
#' options(tibble.print_min = 64)
#' library(dplyr)
#'
#' readRDS(url("https://github.com/leesharpe/nfldata/blob/master/data/games.rds?raw=true")) %>%
#'   dplyr::filter(season %in% 2019:2020) %>%
#'   dplyr::select(sim = season, game_type, week, away_team, home_team, result) %>%
#'   compute_division_ranks() %>%
#'   purrr::pluck("standings")
#' }
compute_division_ranks <- function(games,
                                   teams = NULL,
                                   tiebreaker_depth = 3,
                                   .debug = FALSE,
                                   h2h = NULL) {
  # catch invalid input
  if (!isTRUE(tiebreaker_depth %in% 1:3)) {
    stop(
      "The argument `tiebreaker_depth` has to be",
      "a single value in the range of 1-3!"
    )
  }

  required_vars <- c(
    "sim",
    "game_type",
    "week",
    "away_team",
    "home_team",
    "result"
  )

  if (!sum(names(games) %in% required_vars, na.rm = TRUE) >= 6 | !is.data.frame(games)) {
    stop(
      "The argument `games` has to be a data frame including ",
      "all of the following variables: ",
      glue_collapse(required_vars, sep = ", ", last = " and "),
      "!"
    )
  }

  if (is.null(teams)) { # compute teams df from games df
    pivot_games <- games %>%
      select(sim, home_team, away_team) %>%
      pivot_longer(cols = c("home_team", "away_team"), values_to = "team") %>%
      select(sim, team)

    teams <- bind_rows(
      data.frame(team = unique(games$away_team)),
      data.frame(team = unique(games$home_team))
    ) %>%
      distinct() %>%
      left_join(nflseedR::divisions %>% select(-"sdiv"), by = "team") %>%
      left_join(pivot_games, by = "team") %>%
      select(sim, everything()) %>%
      distinct() %>%
      arrange(division, team, sim)
  }

  # double games
  games_doubled <- double_games(games)

  # record of each team
  report("Calculating team data")
  teams <- teams %>%
    inner_join(games_doubled, by = c("sim", "team")) %>%
    filter(game_type == "REG") %>%
    group_by(sim, conf, division, team) %>%
    summarize(games = n(), wins = sum(outcome)) %>%
    ungroup()

  # add in tiebreaker info
  teams <- teams %>%
    inner_join(games_doubled, by = c("sim", "team")) %>%
    filter(game_type == "REG") %>%
    inner_join(teams,
      by = c("sim" = "sim", "opp" = "team"),
      suffix = c("", "_opp")
    ) %>%
    mutate(
      win_pct = wins / games,
      div_game = ifelse(division == division_opp, 1, 0),
      conf_game = ifelse(conf == conf_opp, 1, 0)
    ) %>%
    group_by(sim, conf, division, team, games, wins, win_pct) %>%
    summarize(
      div_pct = ifelse(sum(div_game) == 0, 0.5,
        sum(div_game * outcome) / sum(div_game)
      ),
      conf_pct = ifelse(sum(conf_game) == 0, 0.5,
        sum(conf_game * outcome) / sum(conf_game)
      ),
      sov = ifelse(sum(outcome == 1) == 0, 0,
        sum(wins_opp * (outcome == 1)) /
          sum(games_opp * (outcome == 1))
      ),
      sos = sum(wins_opp) / sum(games_opp)
    ) %>%
    ungroup()

  # below only if there are tiebreakers
  if (is.null(h2h) & tiebreaker_depth > TIEBREAKERS_NONE) {
    report("Calculating head to head")
    h2h <- teams %>%
      select(sim, team) %>%
      inner_join(teams %>% select(sim, team),
        by = "sim", suffix = c("", "_opp")
      ) %>%
      rename(opp = team_opp) %>%
      arrange(sim, team, opp) %>%
      left_join(games_doubled %>% filter(game_type == "REG"),
        by = c("sim", "team", "opp")
      ) %>%
      group_by(sim, team, opp) %>%
      summarize(
        h2h_games = sum(!is.na(outcome)),
        h2h_wins = sum(outcome, na.rm = TRUE),
        h2h_played = ifelse(h2h_games > 0, 1, 0)
      ) %>%
      ungroup()
  }

  #### FIND DIVISION RANKS ####

  # initialize division rank
  teams <- teams %>%
    mutate(div_rank = NA_real_)

  # determine division ranks
  dr <- 0
  while (any(is.na(teams$div_rank))) {
    # increment division rank
    dr <- dr + 1
    report(paste0("Calculating division rank #", dr))

    # update teams with this rank
    update <- teams %>%
      filter(is.na(div_rank)) %>%
      group_by(sim, division) %>%
      filter(win_pct == max(win_pct)) %>%
      mutate(div_rank = ifelse(n() == 1, dr, div_rank)) %>%
      ungroup() %>%
      break_division_ties(dr, h2h = h2h, tb_depth = tiebreaker_depth, .debug = .debug)

    # store updates
    teams <- teams %>%
      left_join(update, by = c("sim", "team")) %>%
      mutate(div_rank = ifelse(!is.na(new_rank), new_rank, div_rank)) %>%
      select(-new_rank)
  }

  max_reg_week <- max(games$week[games$game_type == "REG"], na.rm = TRUE)

  teams <- teams %>%
    mutate(max_reg_week = max_reg_week)

  return(list(standings = teams, h2h = h2h))
}
