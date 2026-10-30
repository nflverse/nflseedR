test_that("standings_h2h() only counts regular season games", {
  g <- data.table::data.table(
    sim = 1L,
    game_type = c("REG", "REG", "WC"),
    week = c(1L, 10L, 19L),
    away_team = c("BUF", "MIA", "MIA"),
    home_team = c("MIA", "BUF", "BUF"),
    result = c(3L, -7L, 10L)
  )
  g <- standings_validate_games(g)
  h2h <- standings_h2h(
    standings_double_games(g, verbosity = 0L),
    verbosity = 0L
  )
  data.table::setkey(h2h, team)

  expect_identical(h2h$team, c("BUF", "MIA"))
  expect_identical(h2h$h2h_games, c(2L, 2L))
  expect_identical(h2h$h2h_wins, c(0, 2))
  expect_identical(h2h$h2h_pd, c(-10L, 10L))
})

test_that("frank_by() matches frankv() by group", {
  set.seed(1)
  dt <- data.table::data.table(
    g1 = sample(1:5, 300, replace = TRUE),
    g2 = sample(c("a", "b"), 300, replace = TRUE),
    x = sample(c(1:4, NA), 300, replace = TRUE),
    y = round(stats::runif(300), 1)
  )
  expected <- dt[,
    r := frankv(list(x, -y), ties.method = "min"),
    by = c("g1", "g2")
  ]$r

  expect_identical(frank_by(list(dt$g1, dt$g2), list(dt$x, -dt$y)), expected)
  expect_identical(frank_by(list(integer()), list(double())), integer())
})

test_that("min_by() matches min() by group", {
  set.seed(2)
  dt <- data.table::data.table(
    g = sample(letters[1:6], 300, replace = TRUE),
    x = sample(c(1:10, NA), 300, replace = TRUE)
  )
  expected <- dt[, m := min(x), by = "g"]$m

  expect_identical(min_by(dt$x, list(dt$g)), expected)
})

test_that("tie_winner_loser() matches frankv() by group", {
  set.seed(3)
  dt <- data.table::data.table(
    group = sample(c(letters[1:20], NA), 400, replace = TRUE),
    value = sample(c(0, 0.25, 0.5, 1, NA), 400, replace = TRUE)
  )
  dt[,
    `:=`(
      winner = frankv(-value, ties.method = "max") == 1,
      loser = frankv(-value, ties.method = "dense") != 1
    ),
    by = "group"
  ]
  out <- tie_winner_loser(dt$group, dt$value)

  expect_identical(out$tie_winner, dt$winner)
  expect_identical(out$tie_loser, dt$loser)
})

test_that("tie_winner_loser() handles empty and missing values", {
  expect_no_warning(out <- tie_winner_loser(character(), double()))
  expect_identical(out, list(tie_winner = logical(), tie_loser = logical()))

  out <- tie_winner_loser(c("a", "a", "b"), c(NA, 0.5, NA))
  expect_identical(
    out,
    list(tie_winner = c(FALSE, TRUE, TRUE), tie_loser = c(TRUE, FALSE, FALSE))
  )
})

test_that("join_cols() matches merge(all.x = TRUE) without copying", {
  x <- data.table::data.table(
    sim = rep(1:3, each = 4),
    team = rep(c("A", "B", "C", "D"), 3),
    val = 1:12
  )
  i <- data.table::data.table(
    sim = c(3L, 1L, 2L),
    team = c("B", "D", "A"),
    new1 = c(0.5, 1, 0),
    new2 = c("x", "y", "z")
  )
  expected <- merge(x, i, by = c("sim", "team"), all.x = TRUE)
  address_before <- data.table::address(x)
  out <- join_cols(x, i)

  expect_identical(data.table::address(out), address_before)
  expect_equal(out, expected, ignore_attr = TRUE)
})
