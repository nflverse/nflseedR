# nfl_clinching_status() hints at the search

    Code
      clinching_status_search_hint(s)
    Message
      i 3 statuses are "undecided" (teams with 3 games left). `search = TRUE` might decide them.
        The search helps most with 5 or fewer games left per team.

# nfl_clinching_status() checks its arguments

    Code
      nfl_clinching_status(games, status = "WC", verbosity = "NONE")
    Condition
      Error in `nfl_clinching_status()`:
      ! `status` must be one of "DIV", "PLAYOFFS", or "1SEED", not "WC".
    Code
      nfl_clinching_status(games, scenarios = NA, verbosity = "NONE")
    Condition
      Error in `nfl_clinching_status()`:
      ! `scenarios` must be `TRUE` or `FALSE`.
    Code
      nfl_clinching_status(games, search = "no", verbosity = "NONE")
    Condition
      Error in `nfl_clinching_status()`:
      ! `search` must be `TRUE` or `FALSE`.
    Code
      nfl_clinching_status(games, "DIV", verbosity = "NONE")
    Condition
      Error in `nfl_clinching_status()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = "DIV"
      i Did you forget to name an argument?
    Code
      nfl_clinching_status(games, verbosity = "ALL")
    Condition
      Error in `nfl_clinching_status()`:
      ! `verbosity` must be one of "MIN", "MAX", or "NONE", not "ALL".

