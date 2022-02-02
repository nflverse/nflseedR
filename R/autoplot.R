#' Automatically Plot nflseedR_simulation Object
#'
#' Creates automatic plots for wins, ranks, or points for an `nflseedR_simulation`
#'  object as created by `simulate_nfl()`.
#'
#' @param object An `nflseedR_simulation` object as created by `simulate_nfl()`.
#' @param type one of "wins", ...
#' @param ... unused, required by autoplot generic
#'
#' @details This function requires the following optional dependencies:
#' - "forcats",
#' - "ggplot2",
#' - "ggdist",
#' - "ggtext",
#' - "ggthemes",
#' - "gridtext"
#'
#' @examples
#' \donttest{
#'
#'   simulation <- .ffs_cache("foureight_sim.rds")
#'
#'   ggplot2::autoplot(simulation) # default is type = "wins"
#'   ggplot2::autoplot(simulation, type = "rank")
#'   ggplot2::autoplot(simulation, type = "points")
#'
#' }
#'
#' @seealso The examples [on the package website](https://nflseedr.com/articles/articles/nflsim.html)
#'
#' @return A ggplot2 object
#' @export
autoplot.nflseedR_simulation <- function(object,
                                         type = c("wins"),
                                         ...) {
  type <- match.arg(type)

  rlang::check_installed(c(
    "forcats",
    "ggplot2",
    "ggdist",
    "nflplotR",
    "ggthemes"
  ))

  switch(type,
    "wins" = p <- .plot_wins(object, ...)
  )
  p
}

#' @keywords internal
.plot_wins <- function(object, ...) {
  m <- object$teams %>%
    dplyr::group_by(conf, team) %>%
    dplyr::summarise(average_wins = mean(wins)) %>%
    dplyr::arrange(average_wins, .by_group = TRUE) %>%
    dplyr::ungroup()

  object$teams %>%
    ggplot2::ggplot(ggplot2::aes(
      x = wins,
      y = forcats::fct_reorder(.f = team, .x = wins, .fun = mean)
    )) +
    ggdist::stat_histinterval(
      ggplot2::aes(fill = team),
      breaks = 0:17,
      alpha = 0.5,
      .width = 0,
      point_colour = NA
    ) +
    ggdist::stat_histinterval(fill = NA, breaks = 0:17) +
    # ggplot2::geom_label(
    #   data = m,
    #   ggplot2::aes(x = 1, y = logo_html, label = sprintf("%2.1f", round(average_wins, 1))),
    #   nudge_y = 0.2,
    #   size = 3,
    #   label.size = 0.15,
    #   label.padding = ggplot2::unit(0.15, "lines"),
    #   hjust = 1
    # ) +
    ggplot2::scale_x_continuous(breaks = seq(0, 16, 2)) +
    nflplotR::scale_fill_nfl() +
    ggplot2::labs(
      title = sprintf("Season Win Totals - %s NFL Season", object$sim_params$nfl_season),
      subtitle = sprintf("Prediction Based on %s Simulated Regular Seasons | Sorted by Average Wins", object$sim_params$simulations),
      x = sprintf("%s Regular Season Wins", object$sim_params$nfl_season),
      caption = "https://github.com/nflverse/nflseedR/raw/autoplots/man/figures/caption.png"
    ) +
    ggthemes::theme_fivethirtyeight() +
    ggplot2::theme(
      # axis.text.y = nflplotR::element_nfl_logo(size = 0.8),
      axis.title.x = ggplot2::element_text(),
      axis.title.y = ggplot2::element_blank(),
      # strip.text = nflplotR::element_nfl_logo(size = 1),
      # plot.caption = nflplotR::element_path(size = 1),
      plot.caption = ggplot2::element_blank(),
      plot.title.position = "plot",
      panel.grid.major.y = ggplot2::element_blank()
    ) +
    ggplot2::facet_wrap(dplyr::vars(conf), nrow = 1, scales = "free_y") +
    NULL
}


#' @rdname autoplot.nflseedR_simulation
#' @param x An `nflseedR_simulation` object as created by `simulate_nfl()`.
#' @param y Ignored, required for compatibility with the `plot()` generic.
#' @export
plot.nflseedR_simulation <- function(x, ..., type = c("wins"), y) {
  type <- match.arg(type)
  ggplot2::autoplot(x, type = type, ...)
}

