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

