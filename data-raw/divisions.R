divisions <- nflfastR::teams_colors_logos %>%
  dplyr::left_join(
    teamcolors::teamcolors %>%
      dplyr::filter(league == "nfl") %>%
      dplyr::select(name, division),
    by = c("team_name" = "name")
  ) %>%
  dplyr::mutate(
    division = dplyr::case_when(
      team_abbr == "LV"  ~ "AFC West",
      team_abbr == "SD"  ~ "AFC West",
      team_abbr == "STL" ~ "NFC West",
      team_abbr == "WAS" ~ "NFC East",
      TRUE ~ division
    ),
    conf = stringr::str_sub(division, 1, 3),
    # div = stringr::str_replace_all(division, " ", "") %>% stringr::str_sub(4, -1L),
    sdiv = stringr::str_replace_all(division, " ", "") %>% stringr::str_sub(1, 4)
  ) %>%
  dplyr::select(team = team_abbr, conf, division, sdiv)

usethis::use_data(divisions, overwrite = TRUE)
rm(divisions)
