# this makes it so there's two rows per game (one/team)
#' @import data.table
double_games <- function(g){
  setDT(g)
  away <- g[,list(sim, game_type, week, team = away_team, opp = home_team, result = -result)]
  home <- g[,list(sim, game_type, week, team = home_team, opp = away_team, result)]
  out <- rbind(away, home)
  out[, outcome := fcase(
    is.na(result), NA_real_,
    result > 0, 1,
    result < 0, 0,
    default = 0.5
  )]
  tibble::as_tibble(out)
}

#' @import data.table
compute_h2h <- function(gd, update = TRUE){
  if (!".h2h" %in% ls(envir = .nflseedR_env, all.names = !update)){
    report("Calculating head to head")
    setDT(gd, key = c("sim", "team", "opp"))
    out <- gd[game_type == "REG", list(
      h2h_games = .N,
      h2h_wins = sum(outcome),
      h2h_played = fifelse(.N > 0, 1, 0)
    ), by = c("sim", "team", "opp")]
    assign(".h2h", out, envir = .nflseedR_env)
  }
  get(".h2h", envir = .nflseedR_env)
}


# Unused data.frame variants ----------------------------------------------

# double_games.data.frame <- function(g) {
#   g1 <- g %>%
#     select(sim, game_type, week, away_team, home_team, result) %>%
#     rename(team = away_team, opp = home_team) %>%
#     mutate(result = -1 * result)
#   g2 <- g %>%
#     select(sim, game_type, week, away_team, home_team, result) %>%
#     rename(team = home_team, opp = away_team)
#   g <- bind_rows(g1, g2) %>%
#     mutate(outcome = case_when(
#       result > 0 ~ 1,
#       result < 0 ~ 0,
#       result == 0 ~ 0.5,
#       TRUE ~ NA_real_
#     ))
#   g
# }

# compute_h2h.data.frame <- function(gd) {
#   gd %>%
#     filter(game_type == "REG") %>%
#     group_by(sim, team, opp) %>%
#     summarize(
#       h2h_games = n(),
#       h2h_wins = sum(outcome, na.rm = TRUE),
#       h2h_played = ifelse(h2h_games > 0, 1, 0)
#     ) %>%
#     ungroup()
# }
