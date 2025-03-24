
# progress report using rlang to avoid usethis dependency
report <- function(msg,
                   ...,
                   .cli_fct = cli::cli_alert_info,
                   .envir = parent.frame()) {
  .cli_fct(c(format(Sys.time(), '%H:%M:%S'), " | ", msg), ..., .envir = .envir)
}

# this makes it so there's two rows per game (one/team)
double_games <- function(g) {
  g1 <- g |>
    select(sim, game_type, week, away_team, home_team, result) |>
    rename(team = away_team, opp = home_team) |>
    mutate(result = -1 * result)
  g2 <- g |>
    select(sim, game_type, week, away_team, home_team, result) |>
    rename(team = home_team, opp = away_team)
  g <- bind_rows(g1, g2) |>
    mutate(outcome = case_when(
      result > 0 ~ 1,
      result < 0 ~ 0,
      result == 0 ~ 0.5,
      TRUE ~ NA_real_
    ))
  return(g)
}

is_single_digit_numeric <- function(x) is.numeric(x) && length(x) == 1L && !is.na(x)

# Identify sessions with sequential future resolving
is_sequential <- function() inherits(future::plan(), "sequential")

# strip nflverse attributes from games dataframe as they are misleading
# .internal.selfref is a data.table attribute that is not necessary in this case
strip_nflverse_attributes <- function(df){
  input_attrs <- names(attributes(df))
  input_remove <- input_attrs[grepl("nflverse|.internal.selfref", input_attrs)]
  attributes(df)[input_remove] <- NULL
  df
}

release_bullets <- function() {
  c(
    '`devtools::check_mac_release()`',
    '`rhub::rhub_check(platforms = nflseedR:::rhub_check_platforms())`',
    '`pkgdown::check_pkgdown()`',
    '`usethis::use_tidy_thanks()`',
    NULL
  )
}

rhub_check_platforms <- function(){
  # plts created with
  # out <- paste0('"', rhub::rhub_platforms()$name, '"', collapse = ",\n")
  # cli::cli_code(paste0(
  #   "plts <- c(\n", out, "\n)"
  # ))

  plts <- c(
    "linux",
    "m1-san",
    "macos",
    "macos-arm64",
    "windows",
    "atlas",
    "c23",
    "clang-asan",
    "clang-ubsan",
    "clang16",
    "clang17",
    "clang18",
    "clang19",
    "clang20",
    "donttest",
    "gcc-asan",
    "gcc13",
    "gcc14",
    "gcc15",
    "intel",
    "mkl",
    "nold",
    "noremap",
    "nosuggests",
    "rchk",
    "ubuntu-clang",
    "ubuntu-gcc12",
    "ubuntu-next",
    "ubuntu-release",
    "valgrind"
  )
  exclude <- c("rchk", "nosuggests", "valgrind")
  plts[!plts %in% exclude]
}
