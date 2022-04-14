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
                                         base_size = 11,
                                         caption.size = 0.5,
                                         conf.logo.size = 1,
                                         team.logo.size = 0.8,
                                         hist.breaks = -0.5:17.5,
                                         annotation.text.size = 2.5,
                                         annotation.line.size = 0.4,
                                         ...) {
  type <- match.arg(type)

  rlang::check_installed(c(
    "forcats",
    "ggplot2",
    "ggdist",
    "nflplotR",
    "ggthemes",
    "tidyr"
  ))

  switch(type,
    "wins" = p <- .plot_wins(object,
                             base_size = base_size,
                             caption.size = caption.size,
                             conf.logo.size = conf.logo.size,
                             team.logo.size = team.logo.size,
                             hist.breaks = hist.breaks,
                             annotation.text.size = annotation.text.size,
                             annotation.line.size = annotation.line.size,
                             ...)
  )
  p
}

#' @keywords internal
.plot_wins <- function(object,
                       base_size = 11,
                       caption.size = 0.5,
                       conf.logo.size = 1,
                       team.logo.size = 0.8,
                       hist.breaks = -0.5:17.5,
                       annotation.text.size = 2,
                       annotation.line.size = 0.4,
                       ...) {
  # compute top teams for percentile annotations
  top_teams <- object$teams %>%
    dplyr::group_by(conf, team) %>%
    dplyr::summarise(average_wins = mean(wins)) %>%
    dplyr::slice_max(average_wins, with_ties = FALSE) %>%
    dplyr::ungroup()

  # count number of teams for y-axis position of percentile annotations
  n_teams <- object$teams %>%
    dplyr::group_by(conf) %>%
    dplyr::summarise(teams = dplyr::n_distinct(team)) %>%
    dplyr::pull(teams) %>%
    max()

  # compute the percentile values that are printed by ggdist::stat_histinterval
  # for percentile annotations
  top_intervals <- object$teams %>%
    dplyr::filter(team %in% top_teams$team) %>%
    dplyr::group_by(team, conf) %>%
    dplyr::summarise(
      intervals = ggdist::median_qi(wins, .width = c(0.5, 0.9))
    ) %>%
    tidyr::unnest_wider(intervals) %>%
    tidyr::pivot_longer(starts_with("y")) %>%
    dplyr::mutate(
      string = dplyr::case_when(
        name == "ymin" & .width == 0.9 ~ "5th Percentile",
        name == "ymax" & .width == 0.9 ~ "95th",
        name == "y" ~ "50th",
        name == "ymin" ~ "25th",
        name == "ymax" ~ "75th",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::distinct(team, conf, name, value, string) %>%
    dplyr::mutate(
      hjust = dplyr::case_when(
        string %in% c("5th Percentile", "25th") ~ 1,
        string %in% c("75th", "95th") ~ 0,
        TRUE ~ 0.5
      )
    )

  # make actual plot
  object$teams %>%
    ggplot2::ggplot(ggplot2::aes(
      x = wins,
      y = forcats::fct_reorder(.f = team, .x = wins, .fun = mean)
    )) +
    ggplot2::geom_segment(
      data = top_intervals,
      ggplot2::aes(x = value, xend = value, y = n_teams + 1, yend = n_teams),
      alpha = 0.5, size = ggplot2::rel(annotation.line.size)
    ) +
    ggplot2::geom_text(
      data = top_intervals,
      ggplot2::aes(x = value, y = n_teams + 1, label = string, hjust = hjust),
      vjust = -0.25,
      size = ggplot2::rel(annotation.text.size)
    ) +
    ggdist::stat_histinterval(
      ggplot2::aes(fill = team),
      breaks = hist.breaks, # hard coded equivalent of breaks=seq(min(wins)-0.5, max(wins)+0.5, by=1)
      alpha = 0.5,
      slab_type = "histogram",
      # quantile translates to 1 - .width/2 because ggdist computes interval limits
      # this means we will get 5th, 25th, 50th, 75th, and 95% percentiles
      .width = c(0.5, 0.9),
      point_interval = "median_qi",
      interval_alpha = 1,
      point_alpha = 1
    ) +
    ggplot2::scale_x_continuous(breaks = seq(0, 16, 2), expand = ggplot2::expansion(add = c(0, 1))) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = c(0.2, 1.5))) +
    nflplotR::scale_fill_nfl() +
    ggplot2::labs(
      title = sprintf("Season Win Totals - %s NFL Season", object$sim_params$nfl_season),
      subtitle = sprintf("Prediction Based on %s Simulated Regular Seasons | Sorted by Average Wins", object$sim_params$simulations),
      x = sprintf("%s Regular Season Wins", object$sim_params$nfl_season),
      caption = "https://github.com/nflverse/nflseedR/raw/master/man/figures/caption.png"
    ) +
    ggthemes::theme_fivethirtyeight(base_size = base_size) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.caption = nflplotR::element_path(size = caption.size),
      strip.text = nflplotR::element_nfl_logo(size = conf.logo.size),
      axis.text.y = nflplotR::element_nfl_logo(size = team.logo.size),
      axis.title.x = ggplot2::element_text(),
      axis.title.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank()
    ) +
    ggplot2::facet_wrap(dplyr::vars(conf), nrow = 1, scales = "free_y") +
    NULL
}

#' @keywords internal
.plot_team_wins <- function(object,
                            base_size = 11,
                            caption.size = 0.5,
                            conf.logo.size = 1,
                            team.logo.size = 0.8,
                            hist.breaks = -0.5:17.5,
                            annotation.text.size = 2,
                            annotation.line.size = 0.4,
                            ...) {
  # compute top teams for percentile annotations
  top_teams <- object$teams %>%
    dplyr::group_by(conf, team) %>%
    dplyr::summarise(average_wins = mean(wins)) %>%
    dplyr::slice_max(average_wins, with_ties = FALSE) %>%
    dplyr::ungroup()

  # count number of teams for y-axis position of percentile annotations
  n_teams <- object$teams %>%
    dplyr::group_by(conf) %>%
    dplyr::summarise(teams = dplyr::n_distinct(team)) %>%
    dplyr::pull(teams) %>%
    max()

  # compute the percentile values that are printed by ggdist::stat_histinterval
  # for percentile annotations
  over_unders <- object$team_wins %>%
    dplyr::mutate(
      team = factor(
        team,
        levels = c(
          # AFC
          "CIN", "CLE", "BAL", "PIT",
          "BUF", "MIA", "NYJ", "NE",
          "IND", "JAX", "HOU", "TEN",
          "DEN", "LAC", "KC", "LV",
          # NFC
          "CHI", "DET", "GB", "MIN",
          "WAS", "DAL", "PHI", "NYG",
          "TB", "ATL", "CAR", "NO",
          "SF", "ARI", "LA", "SEA"
        ),
        ordered = TRUE
      )
    ) %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(
      intersect = compute_over_under(wins, over_prob, under_prob)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      string_pos = ifelse(intersect > 8, intersect - 5, intersect + 5)
    )

  # make actual plot
  object$team_wins %>%
    dplyr::mutate(
      team = factor(
        team,
        levels = c(
          # AFC
          "CIN", "CLE", "BAL", "PIT",
          "BUF", "MIA", "NYJ", "NE",
          "IND", "JAX", "HOU", "TEN",
          "DEN", "LAC", "KC", "LV",
          # NFC
          "CHI", "DET", "GB", "MIN",
          "WAS", "DAL", "PHI", "NYG",
          "TB", "ATL", "CAR", "NO",
          "SF", "ARI", "LA", "SEA"
        ),
        ordered = TRUE
      )
    ) %>%
    tidyr::pivot_longer(cols = c(over_prob, under_prob)) %>%
    ggplot2::ggplot(ggplot2::aes(x = wins, y = value, color = name)) +
    ggplot2::geom_line() +
    ggplot2::geom_vline(data = over_unders, ggplot2::aes(xintercept = intersect)) +
    ggplot2::geom_label(
      data = over_unders,
      ggplot2::aes(x = intersect, y = 0.5, label = round(intersect, 1)),
      inherit.aes = FALSE, fill = "#F0F0F0"
    ) +
    ggplot2::labs(
      title = sprintf("Season Win Totals - %s NFL Season", object$sim_params$nfl_season),
      subtitle = sprintf("Prediction Based on %s Simulated Regular Seasons | Sorted by Average Wins", object$sim_params$simulations),
      x = sprintf("%s Regular Season Wins", object$sim_params$nfl_season),
      caption = "https://github.com/nflverse/nflseedR/raw/master/man/figures/caption.png"
    ) +
    ggplot2::scale_x_continuous(breaks = seq(0, 16, 2), expand = ggplot2::expansion(add = c(0, 1))) +
    ggthemes::theme_fivethirtyeight(base_size = base_size) +
    ggplot2::theme(
      plot.title.position = "plot",
      # plot.caption = nflplotR::element_path(size = caption.size),
      # strip.text = nflplotR::element_nfl_wordmark(size = conf.logo.size),
      # axis.text.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(),
      axis.title.y = ggplot2::element_blank(),
      # panel.grid.major.y = ggplot2::element_blank()
    ) +
    ggplot2::facet_wrap(dplyr::vars(team), nrow = 8, scales = "free") +
    NULL
}

compute_over_under <- function(wins, over_probs, under_probs){
  over <-  approxfun(x = wins, y = over_probs, rule = 2)
  under <- approxfun(x = wins, y = under_probs, rule = 2)
  root <- uniroot(function(x){over(x) - under(x)}, interval = c(0,17))$root
}

#' @rdname autoplot.nflseedR_simulation
#' @param x An `nflseedR_simulation` object as created by `simulate_nfl()`.
#' @param y Ignored, required for compatibility with the `plot()` generic.
#' @export
plot.nflseedR_simulation <- function(x, ..., type = c("wins"), y) {
  type <- match.arg(type)
  ggplot2::autoplot(x, type = type, ...)
}
