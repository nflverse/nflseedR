s <- nflreadr::load_schedules()

games <- tidyr::crossing(i = 1000:1499, y = 2002:2021, w = 30) |>
  purrr::pmap(function(i, y, w, sched){
  sched |>
      dplyr::filter(season == y, week <= w, game_type == "REG") |>
      dplyr::mutate(
        sim = paste(i, y, w, sep = "_")
      )
}, sched = s
) |>
  purrr::list_rbind() |>
  dplyr::select(sim, game_type, week, away_team, home_team, result)


old <- microbenchmark::microbenchmark(
  {a <- nflseedR::compute_division_ranks(games, .debug = TRUE) |> purrr::pluck("standings")}
)

new <- microbenchmark::microbenchmark(
  {b <- compute_division_ranks(games, .debug = TRUE)},
  times = 1L
)

games$sim |> unique() |> length()

a <- nflseedR::compute_division_ranks(games, .debug = TRUE) |> purrr::pluck("standings")
b <- compute_division_ranks(games, .debug = FALSE)

data <- a |>
  dplyr::select("sim", "team", "division", "games":"div_rank") |>
  dplyr::left_join(
    b |> dplyr::select("sim", "team", "new_rank" = "div_rank", "div_tie_broken_by"),
    by = c("sim", "team")
  ) |>
  dplyr::filter(
    div_rank != new_rank
  )

games <- nflreadr::load_schedules(2002:2023) |>
  dplyr::filter(game_type == "REG", !is.na(result)) |>
  dplyr::select(sim = season, game_type, week, away_team, home_team, result)
