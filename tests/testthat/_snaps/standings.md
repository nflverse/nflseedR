# nfl_standings() checks its input

    Code
      nfl_standings(games, verbosity = "NONE")
    Condition
      Error in `nfl_standings()`:
      ! The `games` table includes invalid team abbreviations: "OAK".
      i Valid abbreviations are the teams in `nflseedR::divisions` without "LAR", "OAK", "SD", and "STL".
      i `nflreadr::clean_team_abbrs()` converts old abbreviations.

---

    Code
      nfl_standings(games, tiebreaker_depth = "POINTS", verbosity = "NONE")
    Condition
      Error in `nfl_standings()`:
      ! `tiebreaker_depth = "POINTS"` needs the variables "home_score" and "away_score" in `games`.

