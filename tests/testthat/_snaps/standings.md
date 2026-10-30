# large draft ties abort the draft tiebreaker loop

    Code
      with_reference_seed(1L, nfl_standings(games, ranks = "DRAFT", verbosity = "NONE"))
    Condition
      Error in `add_draft_ranks()`:
      ! Entered infinite loop in draft tiebreaking procedure

