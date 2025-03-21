#' Compute Pretty NFL Standings Table
#'
#' @description Uses the R package gt to create a pretty html table of NFL standings.
#'
#' @param standings A table of NFL standings. Usually computed by [`nfl_standings()`]
#' @param ... Currently unused. The function errors if objects are passed to
#'   the dots, i.e. when unnamed arguments are provided.
#' @param grp_by Group the output table by Division (`"div"`),
#'    Conference (`"conf"`), or complete league (`"nfl"`)
#' @param order_by Order teams by division rank, conference rank, or draft rank
#' @param reverse Teams are sorted by the argument `order_by` in ascending order
#'   by default. If `reverse` is set to `TRUE`, order will be reversed.
#'
#' @returns An object of class `gt_tbl`.
#' @export
#' @section Output of below examples:
#' \if{html}{\figure{standings_tbl1.png}{options: width=75\%}}
#' \if{html}{\figure{standings_tbl2.png}{options: width=75\%}}
#' @examples
#' \donttest{
#' # Calculate standings
#' s <- nflreadr::load_schedules(2024) |>
#'   nflseedR::nfl_standings(ranks = "DRAFT")
#'
#' # Create table
#' tbl1 <- nfl_standings_prettify(s, grp_by = "conf", order_by = "conf_rank")
#' tbl2 <- nfl_standings_prettify(s, grp_by = "nfl", order_by = "draft_rank")
#'
#' # The output of tbl1 and tbl2 is given in the above images.
#' }
nfl_standings_prettify <- function(
  standings,
  ...,
  grp_by = c("div", "conf", "nfl"),
  order_by = c("div_rank", "conf_rank", "draft_rank"),
  reverse = FALSE
) {
  # dots are supposed to force users to name arguments
  rlang::check_dots_empty()
  rlang::check_installed(
    c("gt (>= 0.9.0)", "scales (>= 1.2.0)", "nflplotR (>= 1.2.0)"),
    "to compute a summary table."
  )

  # Handle arguments
  grp_by <- rlang::arg_match(grp_by)
  order_by <- rlang::arg_match(order_by)

  if (!order_by %in% colnames(standings)) {
    cli::cli_abort(
      "The variable {.val {order_by}} is missing in your standings. \
      Do you need to change {.arg ranks} in {.fun nfl_standings}?"
    )
  }

  if (data.table::uniqueN(standings$season) > 1) {
    cli::cli_abort(
      "Detected more than 1 season in {.arg standings}. This function is \
      designed to handle one season only."
    )
  }

  grp_by <- switch(grp_by, "div" = "division", "conf" = "conf", "league" = NULL)

  # Reorder using data.table
  standings <- data.table::setDT(standings)
  standings <- data.table::setorderv(
    standings,
    order_by,
    order = if (isTRUE(reverse)) -1L else 1L
  )

  standings |>
    gt::gt(
      id = "nflseedR_standings",
      groupname_col = grp_by
    ) |>
    gt::cols_hide(gt::any_of(c(
      "division",
      "wins",
      "season",
      "conf",
      "exit"
    ))) |>
    gt::cols_move(
      columns = gt::everything(),
      after = order_by
    ) |>
    nflplotR::gt_nfl_logos(team) |>
    gt::fmt_number(c(win_pct, div_pct, conf_pct, sov, sos), decimals = 3) |>
    gt::sub_missing() |>
    table_theme() |>
    gt::cols_label(
      true_wins ~ "W",
      losses ~ "L",
      ties ~ "T",
      games ~ "G",
      div_rank ~ gt::html("Div<br>Rank"),
      gt::any_of("conf_rank") ~ gt::html("Conf<br>Rank"),
      gt::any_of("draft_rank") ~ gt::html("Draft<br>Rank"),
      win_pct ~ "pct",
      div_pct ~ gt::html("Div<br>PCT"),
      conf_pct ~ gt::html("Conf<br>PCT"),
      div_tie_broken_by ~ gt::html("Division Tie<br>broken by"),
      gt::any_of("conf_tie_broken_by") ~
        gt::html("Conference Tie<br>broken by"),
      gt::any_of("draft_tie_broken_by") ~ gt::html("Draft Tie<br>broken by")
    ) |>
    gt::cols_label_with(
      fn = ~ gt_add_tooltext(.x, tooltext = translate_label(.x))
    ) |>
    gt::cols_width(
      gt::any_of(c("true_wins", "losses", "ties")) ~ gt::px(30)
    ) |>
    gt::tab_style(
      style = gt::cell_borders(sides = "right", style = "dashed"),
      locations = gt::cells_body(columns = c(team, games, ties, pd, sos))
    ) |>
    gt::data_color(
      columns = "pd",
      palette = pd_colors,
      domain = c(-max(abs(standings$pd)), max(abs(standings$pd)))
    ) |>
    gt::tab_style(
      locations = gt::cells_row_groups(),
      style = list(
        gt::cell_text(align = "center", weight = "bold"),
        gt::cell_fill(color = "#F0F0F0")
      )
    ) |>
    gt::tab_header(
      paste(unique(standings$season), "NFL Standings")
    ) |>
    gt::opt_css(
      css = "
    .cell-output-display {
      overflow-x: unset !important;
    }
    div#nflseedR_standings {
      overflow-x: unset !important;
      overflow-y: unset !important;
    }
    #nflseedR_standings .gt_col_heading {
      position: sticky !important;
      top: 0 !important;
    }
    "
    ) |>
    gt::tab_source_note("nflseedR") |>
    gt::tab_style(
      locations = gt::cells_source_notes(),
      style = gt::cell_text(
        align = "right",
        size = "large",
        font = list(
          gt::google_font("Audiowide"),
          gt::default_fonts()
        )
      )
    ) |>
    gt::tab_style(
      locations = gt::cells_title(groups = "title"),
      style = gt::cell_text(
        weight = "bold",
        font = list(
          gt::google_font("Prosto One"),
          gt::default_fonts()
        )
      )
    )
}

gt_add_tooltext <- function(x, tooltext = "") {
  gt::html(paste0(
    "<div title=\"",
    tooltext,
    "\">",
    x,
    "</div>"
  ))
}

translate_label <- function(l) {
  switch(
    l,
    "G" = "Regular Season Games Played",
    "W" = "Wins",
    "L" = "Losses",
    "T" = "Ties",
    "pf" = "Points Scored",
    "pa" = "Points Allowed",
    "pd" = "Point Differential",
    "pct" = "Win Percentage (counting ties as half a win and half a loss)",
    "Div<br>PCT" = "Win Percentage Against Division Opponents",
    "Conf<br>PCT" = "Win Percentage Against Conference Opponents",
    "sov" = "Strength of Victory (combined win percentage of beaten opponents)",
    "sos" = "Strength of Schedule (combined win percentage of all opponents)",
    "Div<br>Rank" = "Division Rank after application of division tiebreakers",
    "Conf<br>Rank" = "Conference Rank (Seed) after application of conference tiebreakers",
    "Draft<br>Rank" = "Draft Pick in following draft after application of draft tiebreakers. This s before any trades, forfeits, or other modifications to draft picks.",
    "Division Tie<br>broken by" = "Tiebreaker used to break a tie with number of teams included",
    "Conference Tie<br>broken by" = "Tiebreaker used to break a tie with number of teams included",
    "Draft Tie<br>broken by" = "Tiebreaker used to break a tie with number of teams included"
  ) %||%
    l
}

pd_colors <- c(
  "#E55100FF",
  "#EE6C00FF",
  "#F47B00FF",
  "#FA8C00FF",
  "#FF9800FF",
  "#FFA626FF",
  "#FFB74CFF",
  "#FFCC7FFF",
  "#FFDFB2FF",
  "#FFF2DFFF",
  "#FFFFFFFF",
  "#E0F4FEFF",
  "#B2E5FCFF",
  "#80D3F9FF",
  "#4EC3F7FF",
  "#28B6F6FF",
  "#02A9F3FF",
  "#029AE5FF",
  "#0187D1FF",
  "#0177BDFF",
  "#00579AFF"
)
