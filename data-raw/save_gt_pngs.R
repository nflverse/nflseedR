library(nflseedR)
set.seed(20220315)
sim <- nflseedR::simulate_nfl(
    nfl_season = 2021,
    fresh_season = TRUE,
    simulations = 20
  )
tbl <- summary(sim)
gt::gtsave(tbl, "man/figures/summary_tbl.png")
