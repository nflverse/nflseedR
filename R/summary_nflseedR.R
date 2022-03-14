#' @export
summary.nflseedR_simulation <- function(object, ...){
  rlang::check_installed(c("gt", "scales"), "to compute a summary table.")

  title <- paste("simulating the", object$sim_params$nfl_season, "NFL season")
  subtitle <- paste("summary of", object$sim_params$simulations, "simulations using nflseedR")

  data <- object$overall %>%
    mutate(division = gsub("AFC |NFC ", "", division))

  afc <- data %>%
    filter(conf == "AFC") %>%
    select(-conf) %>%
    arrange(division, desc(wins), desc(playoff))

  names(afc) <- paste0("afc_", names(afc))

  nfc <- data %>%
    filter(conf == "NFC") %>%
    select(-conf) %>%
    arrange(division, desc(wins), desc(playoff))

  names(nfc) <- paste0("nfc_", names(nfc))

  tbl <- bind_cols(afc, nfc)

  tbl %>%
    group_by(afc_division) %>%
    gt::gt() %>%
    table_theme() %>%
    gt::cols_label(
      afc_team = "",
      nfc_team = "",

      afc_wins = gt::html("AVG.<br>WINS"),
      nfc_wins = gt::html("AVG.<br>WINS"),

      afc_playoff = gt::html("Make<br>POs"),
      nfc_playoff = gt::html("Make<br>POs"),

      afc_div1 = gt::html("Win<br>DIV"),
      nfc_div1 = gt::html("Win<br>DIV"),

      afc_seed1 = gt::html("No.1<br>Seed"),
      nfc_seed1 = gt::html("No.1<br>Seed"),

      afc_won_conf = gt::html("Win<br>Conf"),
      nfc_won_conf = gt::html("Win<br>Conf"),

      afc_won_sb = gt::html("Win<br>SB"),
      nfc_won_sb = gt::html("Win<br>SB"),

      afc_draft1 = gt::html("No.1<br>Pick"),
      nfc_draft1 = gt::html("No.1<br>Pick"),

      afc_draft5 = gt::html("Top-5<br>Pick"),
      nfc_draft5 = gt::html("Top-5<br>Pick"),
    ) %>%
    gt::cols_hide(nfc_division) %>%
    gt::fmt_number(gt::ends_with("wins"), decimals = 1) %>%
    gt::fmt_percent(
      columns = c(
        gt::ends_with("playoff"),
        gt::ends_with("div1"),
        gt::ends_with("seed1"),
        gt::ends_with("won_conf"),
        gt::ends_with("won_sb"),
        gt::ends_with("draft1"),
        gt::ends_with("draft5")
      ),
      decimals = 0
    ) %>%
    gt::data_color(
      columns = c(
        gt::ends_with("playoff"),
        gt::ends_with("div1"),
        gt::ends_with("seed1"),
        gt::ends_with("won_conf"),
        gt::ends_with("won_sb"),
        gt::ends_with("draft1"),
        gt::ends_with("draft5")
      ),
      colors = scales::col_numeric(palette = table_colors, domain = c(0, 1))
    ) %>%
    gt::text_transform(
      locations = gt::cells_body(gt::ends_with("team")),
      fn = function(x){
        url <- data.frame(team_abbr = x) %>%
          left_join(
            nflreadr::load_teams() %>%
              filter(!team_abbr %in% c("LAR", "OAK", "SD", "STL")) %>%
              select(team_abbr, team_logo_espn),
            by = "team_abbr"
          ) %>%
          pull(team_logo_espn)
        gt::web_image(url = url, height = 30)
      }) %>%
    gt::tab_source_note(
      gt::html(gt::web_image(
        "https://github.com/nflverse/nflseedR/raw/master/man/figures/caption.png",
        height = 15
      ))
    ) %>%
    gt::tab_spanner(
      label = gt::html(gt::web_image(
        "https://github.com/nflverse/nflfastR-data/raw/master/AFC.png",
        height = 25
      )),
      columns = gt::starts_with("afc")
    ) %>%
    gt::tab_spanner(
      label = gt::html(gt::web_image(
        "https://github.com/nflverse/nflfastR-data/raw/master/NFC.png",
        height = 25
      )),
      columns = gt::starts_with("nfc")
    ) %>%
    gt::tab_style(
      locations = list(gt::cells_body(nfc_team), gt::cells_column_labels(nfc_team)),
      style = gt::cell_borders("left", weight = gt::px(2))
    ) %>%
    gt::tab_style(
      locations = gt::cells_body(columns = gt::ends_with("wins")),
      style = gt::cell_text(weight = "bold")
    ) %>%
    gt::tab_style(
      locations = gt::cells_source_notes(),
      style = gt::cell_text(align = "right")
    ) %>%
    gt::tab_header(
      tools::toTitleCase(title),
      tools::toTitleCase(subtitle)
    )
}

# Taken from Thomas Mock's package gtExtras to avoid the dependency
# on a non cran package.
# https://github.com/jthomasmock/gtExtras/blob/HEAD/R/gt_theme_538.R
table_theme <- function(gt_object,...) {

  gt_object %>%
    gt::opt_all_caps()  %>%
    gt::opt_table_font(
      font = list(
        gt::google_font("Chivo"),
        gt::default_fonts()
      ),
      weight = 300
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(
        sides = "top", color = "black", weight = gt::px(0)
      ),
      locations = gt::cells_column_labels(
        columns = gt::everything()
      )
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(
        sides = "bottom", color = "black", weight = gt::px(1)
      ),
      locations = gt::cells_row_groups()
    ) %>%
    gt::tab_options(
      column_labels.background.color = "white",
      heading.border.bottom.style = "none",
      table.border.top.width = gt::px(3),
      table.border.top.style = "none", #transparent
      table.border.bottom.style = "none",
      column_labels.font.weight = "normal",
      column_labels.border.top.style = "none",
      column_labels.border.bottom.width = gt::px(2),
      column_labels.border.bottom.color = "black",
      row_group.border.top.style = "none",
      row_group.border.top.color = "black",
      row_group.border.bottom.width = gt::px(1),
      row_group.border.bottom.color = "white",
      stub.border.color = "white",
      stub.border.width = gt::px(0),
      data_row.padding = gt::px(3),
      source_notes.font.size = 12,
      source_notes.border.lr.style = "none",
      table.font.size = 16,
      heading.align = "center",
      heading.background.color = "#F0F0F0",
      heading.title.font.weight = "700",
      ...
    ) %>%
    gt::opt_css(
      "tbody tr:last-child {
    border-bottom: 2px solid #ffffff00;
      }
    ",
    add = TRUE
    )
}

# output of ggsci::rgb_material("light-blue") + "white"
table_colors <- c("white",
  "#E0F4FEFF", "#B2E5FCFF", "#80D3F9FF", "#4EC3F7FF", "#28B6F6FF", "#02A9F3FF",
  "#029AE5FF", "#0187D1FF", "#0177BDFF", "#00579AFF"
)
