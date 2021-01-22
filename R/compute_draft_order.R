#' Compute NFL Draft Order using Game Results and Divisional Rankings
#'
#' @inheritParams compute_division_ranks
#'
#' @returns A data frame of standings including the final draft pick number
#'
#' @export
compute_draft_order <- function(teams,
                                h2h = NULL,
                                tiebreaker_depth = 3,
                                .debug = FALSE) {
  # catch invalid input
  if (!isTRUE(tiebreaker_depth %in% 1:3)) {
    stop(
      "The argument `tiebreaker_depth` has to be",
      "a single value in the range of 1-3!"
    )
  }

  if (!any((names(teams) %in% "exit")) | !is.data.frame(teams)) {
    stop(
      "The argument `teams` has to be a data frame including ",
      "the variable `exit` as computed in the playoff simulation!"
    )
  }

  if (is.null(h2h) & tiebreaker_depth > TIEBREAKERS_NONE) {
    stop(
      "You asked for tiebreakers but the argument `h2h` is NULL. ",
      "Did you forget to pass the `h2h` data frame? It is computed with the ",
      "function `compute_division_ranks()`."
    )
  }

  # set draft order variable
  teams <- teams %>%
    mutate(draft_order = NA_real_) %>%
    arrange(sim, division, team)

  # draft order loop
  for (do_num in length(unique(teams$team)):1)
  {
    # progress
    report(paste0("Calculating draft order #", do_num))

    # teams we can update
    update <- teams %>%
      filter(is.na(draft_order)) %>%
      group_by(sim) %>%
      filter(exit == max(exit)) %>%
      filter(win_pct == max(win_pct)) %>%
      filter(sos == max(sos)) %>%
      mutate(draft_order = ifelse(n() == 1, do_num, draft_order)) %>%
      ungroup() %>%
      break_draft_ties(do_num, h2h = h2h, tb_depth = tiebreaker_depth, .debug = .debug)

    # store updates
    teams <- teams %>%
      left_join(update, by = c("sim", "team")) %>%
      mutate(draft_order = ifelse(!is.na(new_do), new_do, draft_order)) %>%
      select(-new_do)
  } # end draft order loop

  return(teams)
}
