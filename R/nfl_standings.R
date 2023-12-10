#' Compute NFL Standings
#'
#' @param games
#' @param ranks
#' @param tiebreaker_depth
#' @param playoff_seeds
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
nfl_standings <- function(games,
                          ranks = c("DIV", "CONF", "DRAFT", "NONE"),
                          tiebreaker_depth = c("SOS", "PRE-SOV", "RANDOM"),
                          playoff_seeds = 7L,
                          verbose = getOption("nflseedR.verbose", default = TRUE)){

  # VALIDATE INPUT ----------------------------------------------------------
  games <- validate_games(games)
  ranks <- rlang::arg_match(ranks)
  tiebreaker_depth <- rlang::arg_match(tiebreaker_depth)
  if ( !any(isTRUE(verbose), isFALSE(verbose)) ){
    cli::cli_abort(
      "The {.arg verbose} argument can only be {.pkg TRUE} or {.pkg FALSE}"
    )
  }

  # INITIATE STANDINGS WITHOUT ANY RANKINGS ---------------------------------
  report("Initiate Standings")
  dg <- double_games(games, update = TRUE)
  standings <- init_standings(dg)
  h2h <- compute_h2h(dg)

  if (ranks == "NONE") return(finalize_standings(standings, games))

  # DIVISION RANKS ----------------------------------------------------------
  report("Compute Division Ranks")
  standings <- add_div_ranks(
    standings = standings,
    h2h = h2h,
    tiebreaker_depth = tiebreaker_depth,
    verbose = verbose
  )
  if (ranks == "DIV") return(finalize_standings(standings, games))

  # CONFERENCE RANKS --------------------------------------------------------
  report("Compute Conference Seeds")
  standings <- add_conf_ranks(
    standings = standings,
    h2h = h2h,
    tiebreaker_depth = tiebreaker_depth,
    verbose = verbose
  )
  if (ranks == "CONF") return(finalize_standings(standings, games))

  # DRAFT ORDER -------------------------------------------------------------
  report("Compute Draft Order")
  standings <- add_draft_ranks(
    standings = standings,
    h2h = h2h,
    tiebreaker_depth = tiebreaker_depth,
    verbose = verbose
  )

  return(finalize_standings(standings, games))
}

finalize_standings <- function(standings, games){
  if (attributes(games)[["uses_season"]]){
    colnames(standings)[colnames(standings) == "sim"] <- "season"
  }
  standings
}
