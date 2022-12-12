test_that("fmt_pct_special works", {
  x <-   c(0, 0.004, 0.009, 0.011, 0.9,   0.98, 0.994,    .995,   .9989,    .999,    .9991, .99999999)
  exp <- c("0%", "<1%", "<1%", "1%", "90%", "98%", "99%", "99.5%", "99.9%", "99.9%", ">99.9%", "100%")
  expect_identical(fmt_pct_special(x), exp)
  expect_error(fmt_pct_special(as.character(x)))
})
