# simulations check teams and playoff seeds before simulating

    Code
      nflseedR::nfl_simulations(games, simulations = 2, chunks = 1)
    Condition
      Error in `nflseedR::nfl_simulations()`:
      ! The `games` table includes invalid team abbreviations: "OAK".
      i Valid abbreviations are the teams in `nflseedR::divisions` without "LAR", "OAK", "SD", and "STL".
      i `nflreadr::clean_team_abbrs()` converts old abbreviations.

---

    Code
      nflseedR::nfl_simulations(nflseedR::sims_games_example, playoff_seeds = 17,
      simulations = 2, chunks = 1)
    Condition
      Error in `nflseedR::nfl_simulations()`:
      ! The `playoff_seeds` argument must be in range 1 - 16

