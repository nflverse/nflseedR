# this makes it so there's two rows per game (one/team)
#' @import data.table
double_games <- function(g, update = FALSE){
  if (!".dg" %in% ls(envir = .nflseedR_env, all.names = !update)){
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
    assign(".dg", out, envir = .nflseedR_env)
  }
  get(".dg", envir = .nflseedR_env)
}

#' @import data.table
compute_h2h <- function(gd, update = FALSE){
  if (!".h2h" %in% ls(envir = .nflseedR_env, all.names = !update)){
    report("Calculating Head-to-Head Data")
    setDT(gd, key = c("sim", "team", "opp"))
    out <- gd[game_type == "REG", list(
      h2h_games = .N,
      h2h_wins = sum(outcome)
    ), by = c("sim", "team", "opp")]
    assign(".h2h", out, envir = .nflseedR_env)
  }
  get(".h2h", envir = .nflseedR_env)
}
