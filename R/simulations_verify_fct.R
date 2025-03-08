#' Verify Custom NFL Result Simulation Function
#'
#' nflseedR supports custom functions to compute results in season simulations
#' through the argument `compute_results` in the season simulation function
#' [nfl_simulations]. To ensure that custom functions work as nflseedR expects
#' them to, it is recommended to verify their behavior.
#' This function first checks the structure of the output and then whether game
#' results are changed as expected. Whenever a problem is found, the function will
#' error with a hint to the problem (this means that you might be required to
#' iterate over all problems until the function stops erroring).
#' See below detail section for more information on expected behavior.
#'
#' @param compute_results A function to compute results of games. See below
#' detail section for more information on expected behavior.
#' @param ... Further arguments passed on to `compute_results`.
#' @param games An NFL schedule where some results are missing. `compute_results`
#' is supposed to compute those results on a weekly base. Defaults to
#' [nflseedR::sims_games_example]. Please see this example to understand
#' the required data structure.
#' @param teams A list of teams by simulation number. This is usually calculated
#' automatically and not user facing. It can be used to "transport" team information
#' like elo ratings from one simulated week to the next. Defaults to
#' [nflseedR::sims_teams_example]. Please see this example to understand
#' the required data structure.
#'
#' @details
#' The following sections detail the requirements for the `compute_results`
#' function. If anything is unclear, please see the source code of nflseedR's
#' default function `nflseedR_compute_results`.
#'
#' ## Required Function Arguments of `compute_results`
#' The function passed to `compute_results` is required to support the arguments
#' `"teams"`, `"games"`, and `"week_num"`. The two leading ones are already
#' described above. The latter is a factor with a length of 1, which identifies
#' the current week. Regular season weeks are labeled `"1"`, `"2"`, etc.
#' Playoff weeks are labeled `"WC"`, `"DIV"`, `"CON"`, and `"SB"`.
#'
#' ## Required Output Structure of `compute_results`
#' The function passed to `compute_results` is required to return a list of the
#' two objects `"teams"` and `"games"` as passed to it in the arguments of the
#' same name. The function must not remove rows or columns.
#' So the last line of `compute_results` usually looks like
#' ```
#' list("teams" = teams, "games" = games)
#' ```
#'
#' ## Required Behavior of `compute_results` when Computing Game Results
#' nflseedR calls `compute_results` for every week where a `result` is missing
#' in `games`. The variable `result` is defined as the point differential between
#' the home team and the away team. If the home team loses, the value is
#' therefore < 0, if it wins > 0 and if it ties == 0.
#' To support elo-based simulations, this is done in a loop so that elo ratings
#' can be updated based on the results and "transported" from week to week. You
#' can "transport" ratings or other information by joining them to the `"teams"`
#' table.
#' This behavior requires that `compute_results` only changes the results of
#' the current week - called `week_num`. And only if there is not already a
#' result.
#' So `compute_results` must only compute a result when
#' ```
#' week == week_num & is.na(result)
#' ```
#' For the playoffs, there is also the special case that matches cannot end in a
#' tie (`result == 0`). In most cases, ties are not simulated anyway because
#' they occur so rarely. But in the event that they are simulated, they must
#' not be in the playoffs.
#'
#' @return Returns `TRUE` invisibly if no problems are found.
#' @export
#'
#' @examples
#' simulations_verify_fct(nflseedR_compute_results)
simulations_verify_fct <- function(compute_results,
                                   ...,
                                   games = nflseedR::sims_games_example,
                                   teams = nflseedR::sims_teams_example){

  if (!is.function(compute_results)) {
    cli::cli_abort("The {.arg compute_results} argument must be a function!")
  }

  # check arguments
  function_args <- names(as.list(args(compute_results)))
  required_args <- c("teams", "games", "week_num")
  missing <- required_args[!required_args %chin% function_args]
  if (length(missing)){
    cli::cli_abort(c(
      "x" = "The function in argument {.arg compute_results} needs the following
      {cli::qty(length(missing))}argument{?s}: {.arg {missing}}"
    ))
  }

  games <- sims_validate_games(games)
  games[, sim := 1L]
  games[,.old_result := result]

  weeks_to_simulate <- games[is.na(result), unique(week)]

  # recall old data for comparison
  old_teams <- data.table::copy(teams)
  old_games <- data.table::copy(games)

  if (as.character(substitute(compute_results)) != "nflseedR_compute_results"){
    # convert games to data.frame to avoid data.table warnings regarding shallow copies
    games <- setDF(games)
  }

  # Simulate a couple of weeks, and check data structure and results each week
  # Errors, if anything problematic happens
  for (week_num in weeks_to_simulate) {

    return_value <- compute_results(
      teams = teams,
      games = games,
      week_num = week_num,
      ...
    )

    # currently, we will catch a maximum of 9 problems. Allocate the vector
    problems <- vector("character", length = 9L)
    i <- 0
    if (typeof(return_value) != "list") {
      i <- i + 1
      problems[i] <- "the returned value was not a list"
    } else {
      # Look for problems in teams output
      if (!("teams" %chin% names(return_value))) {
        i <- i + 1
        problems[i] <- "`teams` was not in the returned list"
      } else {
        teams <- return_value$teams
        if (nrow(teams) != nrow(old_teams)) {
          i <- i + 1
          problems[i] <- paste(
            "`teams` changed from", nrow(old_teams), "to",
            nrow(teams), "rows",
            collapse = " "
          )
        }
        if ( any( !colnames(old_teams) %chin% colnames(teams) ) ){
          i <- i + 1
          removed_names <- colnames(old_teams)[!colnames(old_teams) %chin% colnames(teams)]
          problems[i] <- paste0(
            "`teams` column(s) ", paste0("`", removed_names, "`", collapse = ", "), " removed"
          )
        }
      }
      # Look for problems in games output
      if (!("games" %chin% names(return_value))) {
        i <- i + 1
        problems[i] <- "`games` was not in the returned list"
      } else {
        out_games <- return_value$games
        if (nrow(out_games) != nrow(old_games)) {
          i <- i + 1
          problems[i] <- paste(
            "`games` changed from", nrow(old_games), "to",
            nrow(out_games), "rows",
            collapse = " "
          )
        }
        if ( any( !colnames(old_games) %chin% colnames(out_games) ) ){
          i <- i + 1
          removed_names <- colnames(old_games)[!colnames(old_games) %chin% colnames(out_games)]
          problems[i] <- paste0(
            "`games` column(s) ", paste0("`", removed_names, "`", collapse = ", "), " removed"
          )
        }
      }
    }

    # report data structure problems
    problems <- problems[problems != ""]
    names(problems) <- rep("x", length(problems))
    if (length(problems)) {
      cli::cli_abort(c(
        "i" = "During Week {.val {week_num}}, the {.code compute_results} function \\
       produced the following issues:",
        problems
      ))
    }

    # convert out_games to dt for further computations
    setDT(out_games)

    # If the above didn't fail, we need to make sure that results are OK
    # identify improper results values
    out_games[, problem := fcase(
      week == week_num & is.na(result),
      "a result from the current week is missing",
      week != week_num & !is.na(.old_result) & is.na(result),
      "a known result outside the current week was blanked out",
      !is.na(result) & result == 0 & game_type != "REG",
      "a playoff game resulted in a tie (had result == 0)",
      old_week > suppressWarnings(as.integer(week_num)) & is.na(.old_result) & !is.na(result),
      "a result outside the current week was entered",
      week != week_num & .old_result != result,
      "a known result outside the current week was updated",
      default = NA_character_
    )]

    problems <- out_games[!is.na(problem), unique(problem)]
    names(problems) <- rep("x", length(problems))
    # report result value problems
    if (length(problems)) {
      cli::cli_abort(c(
        "i" = "During Week {.val {week_num}}, your {.code compute_results} \\
              function had the following issues:",
        problems,
        "i" = "Make sure you only change results when {.code week == week_num} \\
              & {.code is.na(result)}"
      ))
    }
  }

  if (interactive() || identical(Sys.getenv("IN_PKGDOWN"), "true")) cli::cli_alert_success("No problems found!")

  invisible(TRUE)
}
