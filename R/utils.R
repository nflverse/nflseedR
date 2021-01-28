
# progress report using rlang to avoid usethis dependency
report <- function(msg) {
  rlang::inform(
    paste0(
      crayon::red(cli::symbol$bullet),
      " ",
      Sys.time(),
      ": ",
      msg
    )
  )
}

# this makes it so there's two rows per game (one/team)
double_games <- function(g) {
  g1 <- g %>%
    select(sim, game_type, week, away_team, home_team, result) %>%
    rename(team = away_team, opp = home_team) %>%
    mutate(result = -1 * result)
  g2 <- g %>%
    select(sim, game_type, week, away_team, home_team, result) %>%
    rename(team = home_team, opp = away_team)
  g <- bind_rows(g1, g2) %>%
    mutate(outcome = case_when(
      result > 0 ~ 1,
      result < 0 ~ 0,
      result == 0 ~ 0.5,
      TRUE ~ NA_real_
    ))
  return(g)
}

is_single_digit_numeric <- function(x) is.numeric(x) && length(x) == 1L && !is.na(x)
