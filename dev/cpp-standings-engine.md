# C++ standings engine

Living plan for replacing the data.table implementation of `nfl_standings()`
with a C++ engine. Branch: `cpp-standings-engine`. Started 2026-10-30.

**Agents: read this whole file before touching any standings code.** Keep the
[status](#status) and [decision log](#decision-log) up to date at the end of
every session. Never resolve an open decision on your own. Ask the maintainer
and record the answer here.

## Goal

Make `nfl_standings()` (and the draft ranking step in `nfl_simulations()`)
30x or more faster without changing its interface or output schema.

Baseline on master `6673c0e`, 10,000 simulated regular seasons (2.7M game
rows), Apple Silicon, `verbosity = "NONE"`:

| `ranks` | time |
|---|---|
| `"DIV"` | 4.0s |
| `"CONF"` | 8.2s |
| `"DRAFT"` | 9.2s |
| `"CONF"`, one season | 82ms per call |

Profiling showed about 93% of the time inside `[.data.table` (35% in
`forderv` alone). Each step filters, sorts or joins the full table, even
though each season has only 32 teams and every tie involves at most 4 teams
per step. 76% of teams are in a conference win percentage tie at the start,
so the tiebreaking loops run over a large part of the table every time.

## Approach

This is **not** a line-by-line translation of the data.table code. The engine
is designed from the rules, one season at a time:

1. The rules are the spec:
   [`vignettes/articles/tiebreaker.Rmd`](../vignettes/articles/tiebreaker.Rmd)
   plus the [rule spec](#rule-spec) below, which makes the article precise
   and records how nflseedR interprets it.
2. The current R implementation (moved to `R/standings_r_*.R`) is the
   **reference** in tests. Wherever the rules don't settle a behavior, the
   reference does, unless the decision log says otherwise.
3. Every known difference between the article and the reference is a
   decision in the [decision log](#decision-log). The engine follows the
   recorded decision. Comparison tests allow exactly those differences and
   nothing else.

## Scope

In scope:

- `nfl_standings()` for all `ranks`, `tiebreaker_depth`, `playoff_seeds` and
  `verbosity` values.
- The draft ranking step in `simulations_simulate_chunks()` (currently
  `add_draft_ranks(dg = NULL)` with the `h2h` attribute workaround).

Out of scope:

- The legacy `compute_division_ranks()`, `compute_conference_seeds()` and
  `compute_draft_order()` functions and `simulate_nfl()`.
- Game simulation (`nflseedR_compute_results()`) and playoff simulation.
- Threads inside C++. Parallelism stays at the `furrr` chunk level (see D8).

## Rule spec

Definitions for one season. "Games" means regular season games
(`game_type == "REG"`) unless stated otherwise. A tie game counts as half a
win and half a loss.

### League structure

Every season has **exactly 32 teams** in 2 conferences, 8 divisions and 4
teams per division. The valid teams are

```r
nflseedR::divisions |>
  dplyr::filter_out(team %in% c("LAR", "OAK", "STL", "SD"))
```

(`ARI ATL BAL BUF CAR CHI CIN CLE DAL DEN DET GB HOU IND JAX KC LA LAC LV MIA
MIN NE NO NYG NYJ PHI PIT SEA SF TB TEN WAS`). The old abbreviations `LAR`,
`OAK`, `STL` and `SD` are invalid. `standings_validate_games()` errors on any
other abbreviation in `away_team` or `home_team` (D9). The engine relies on
these constants: fixed-size arrays (`std::array<..., 32>`, a 32×32
head-to-head matrix, 4 teams per division) and no code paths for other
league sizes.

**Teams without games** are possible in exactly one situation: week 1 has
started but not every team has played yet. The reference leaves such teams
out of the output (only teams with at least one regular season game get a
row). The engine keeps that behavior: fixed 32-team arrays plus a "has
played" mask that controls which rows are written. Ranks are computed among
the teams that have played. Tested by the "partial week 1" snapshot.

### Records

- `wins` = wins + 0.5 × ties. `win_pct = wins / games`.
- `div_pct`, `conf_pct`: the same restricted to division or conference games.
  0 if no such games.
- **Head-to-head** between teams in set S: games of a team against the other
  members of S. Win percentage = wins / games. A team with no games against S
  gets 0 (division procedure).
- **Head-to-head sweep** (conference and draft procedures): 1 if the team won
  every game against every other member of S *and* played each of them, 0 if
  it lost all of them, otherwise not applicable (treated as 0.5, i.e. no
  winner). For 2 teams from different divisions or conferences this is the
  same as head-to-head win percentage, because such teams meet at most once.
- **Common games** of set S: games against opponents that *every* member of
  S played. Members of S are never common opponents. Common win percentage =
  wins / games in those games, 0 if none. Common point differential = sum of
  `result` in those games.
- **Strength of victory (SOV)**: sum of `wins` of beaten opponents (counted
  once per win) / sum of `games` of those opponents. 0 if no wins.
- **Strength of schedule (SOS)**: sum of `wins` of all opponents (once per
  game) / sum of `games` of those opponents.
- **Combined points ranking** among a set of teams (conference or league):
  rank of points scored (descending) + rank of points allowed (ascending),
  `ties.method = "min"`. Lower is better.
- **Net points**: point differential in conference games (`conf_pd`, NA if
  no conference games) or all games (`pd`).
- Net touchdowns are not implemented (no data). The step after net points is
  the coin toss.

All comparisons of percentages must be exact. Count wins in half-wins so
every percentage is a ratio of integers and compare with cross-multiplication.
Output columns use the same formulas as today (`wins / games` in double), so
they are bit-identical to the reference.

### Tiebreaker depth

`tiebreaker_depth` cuts the step lists off. Remaining ties go to a coin toss.

| depth | division and conference steps used |
|---|---|
| `"RANDOM"` (0) | none: rank by win percentage, break every tie at random |
| `"PRE-SOV"` (1) | everything before SOV |
| `"SOS"` (2, default) | everything through SOS |
| `"POINTS"` (3) | everything through net points in all games |

Draft: see D3.

### Division ranks (`div_rank`)

Rank teams within each division by `win_pct`. Each block of teams with equal
`win_pct` is broken with these steps (the same list for 2 and 3+ clubs):

1. Head-to-head win percentage.
2. Division win percentage.
3. Common games win percentage.
4. Conference win percentage.
5. SOV. 6. SOS.
7. Combined points ranking (conference). 8. Combined points ranking (league).
9. Common games point differential. 10. Point differential.
11. Coin toss.

After any step that splits the block, each remaining group of tied teams
restarts at step 1 with only its own members (head-to-head and common games
are recomputed for the smaller set). How many ranks one step assigns is D1.

### Conference ranks (`conf_rank`)

Division winners (`div_rank == 1`) take ranks 1 to 4, everybody else ranks
5 to 16. Within each group, rank by `win_pct`. Each block of teams with equal
`win_pct` is resolved **one seed at a time**:

1. Candidates = all unseeded teams in the block.
2. If all candidates are from the same division, the best `div_rank` takes
   the seed.
3. Otherwise apply division reduction: keep only the best `div_rank` of each
   division. The other teams sit out this seed only.
4. Run the steps on the candidates until one team is best:
   1. Head-to-head sweep.
   2. Conference win percentage.
   3. Common games win percentage, minimum of four (D2).
   4. SOV. 5. SOS.
   6. Combined points ranking (conference). 7. Combined points ranking
      (league).
   8. Net points in conference games. 9. Net points in all games.
   10. Coin toss.

   A step with one best team: that team takes the seed. A step that
   eliminates some teams but leaves 2 or more tied: restart step 4 with the
   remaining teams. A step that changes nothing: go to the next step.
5. Remove the seeded team from the block and go back to 1. Teams that were
   eliminated or reduced come back for the next seed. Division ranks
   ("original seeding within a division") never change.

For division winners the reduction never applies because every candidate is
from a different division. With `playoff_seeds = k`, stop once seed k is
assigned and set lower ranks to `NA` in the output (D7).

### Draft ranks (`draft_rank`)

The draft procedure works differently from the division and conference
procedures:

- **Reversed:** the worse team gets the earlier pick. Every comparison is
  inverted ("worst SOV", "loser of head-to-head").
- **Playoff exit comes first:** non-playoff teams pick 1–18, then Wild Card
  losers, Divisional losers, Conference losers, the Super Bowl loser, and the
  Super Bowl winner last (`exit`).
- **SOS is the first tiebreaker** and is part of the initial order, not a
  late step.
- **Existing ranks are reused:** division and conference tiebreakers are not
  re-run. `div_rank` and `conf_rank` decide ties within a division or
  conference.
- **At most two teams reach the real tiebreakers:** after reduction, only one
  team per conference is left. There is no 3+ club format.

Procedure:

1. Sort by `exit`, `win_pct`, `sos` ascending. Each block with equal values is
   resolved **one pick at a time**:
2. If all candidates are from the same division, the worst `div_rank` takes
   the pick. Else, if all are from the same conference, the worst `conf_rank`
   takes it.
3. Otherwise apply reduction: keep the worst `div_rank` of each division,
   then the worst `conf_rank` of each conference. Exactly two teams remain,
   one from each conference.
4. Two-club steps, the worse team takes the pick: head-to-head (loser),
   common games win percentage (minimum of four, worse), SOV (worse), and
   then see D3, then coin toss.
5. Remove the team that took the pick and go back to 2.

Draft ranks need all 16 conference ranks, no matter what `playoff_seeds` is
(D7).

In `nfl_standings()`, `exit` is the `game_type` of each team's last game
(`"SB"` becomes `"SB_WIN"` for the winner), with `"REG"` for non-playoff
teams. In simulations, `exit` comes from the simulated playoffs.

### Tie labels (`*_tie_broken_by`)

These labels are kept unchanged:

- Division: `"Head-To-Head Win PCT (n)"`, `"Division Win PCT (n)"`,
  `"Common Games Win PCT (n)"`, `"Conference Win PCT (n)"`, `"SOV (n)"`,
  `"SOS (n)"`, `"Conference Points Rank (n)"`, `"League Points Rank (n)"`,
  `"Common Games Point Differential (n)"`, `"Point Differential (n)"`,
  `"Coin Toss"`.
- Conference: `"Division Tiebreaker"`, `"Head-To-Head Sweep (n)"`,
  `"Conference Win PCT (n)"`, `"Common Games Win PCT (n)"`, `"SOV (n)"`,
  `"SOS (n)"`, `"Conference Points Rank (n)"`, `"League Points Rank (n)"`,
  `"Conference Point Differential (n)"`, `"League Point Differential (n)"`,
  `"Coin Toss"`.
- Draft: `"Division Tiebreaker"`, `"Conference Tiebreaker"`,
  `"Head-To-Head (n)"`, `"Common Games Win PCT (n)"`, `"SOV (n)"`,
  `"Coin Toss"`.

`n` is the number of candidates in the step that decided it. Teams never in a
tie get `NA`.

Behavior of the reference:

- Division: every team in a tie gets the label of the step that fixed its
  rank.
- Conference and draft: the team that takes the seed or pick gets the label.
  The last remaining team of a block gets `NA` (D4).
- With `"RANDOM"`, every team sharing a win percentage gets `"Coin Toss"`. In
  conference ranks this groups by `(sim, conf, win_pct)` across division
  winners and wild cards.

## Output schema

Columns and types must stay as they are (the output is consumed by
`nfl_simulations()`, `summary()`, `standings_prettify()` and users):

`sim|season <int>, team <chr>, conf <chr>, division <chr>, games <int>,
wins <dbl>, true_wins <int>, losses <int>, ties <int>, [pf, pa, pd],
win_pct <dbl>, div_pct <dbl>, conf_pct <dbl>, sov <dbl>, sos <dbl>,
div_rank <int>, div_tie_broken_by <chr>, conf_rank <int>,
conf_tie_broken_by <chr>, exit <chr>, draft_rank <int>,
draft_tie_broken_by <chr>`

- Columns after `sos` exist only for the requested `ranks`.
- `pf`, `pa`, `pd` exist only if `home_score`/`away_score` were supplied.
- Sorted by sim, division, div_rank (or sim, division if no ranks).
- `season` instead of `sim` if the input used `season`.
- Today a `*_tie_broken_by` column is missing if no tie happened (D6).

## Architecture

```
R/standings.R          nfl_standings(): validate, encode, call C++, decode
R/standings_engine.R   encoding (teams/sims to integers), label lookup,
                       MAX-verbosity reporting, engine switch
R/standings_r_*.R      current R implementation, renamed, otherwise untouched
src/
  Makevars             CXX_STD = CXX20 (no other flags)
  Makevars.win         same
  cpp11.cpp            generated by cpp11::cpp_register(), never edit
  bindings.cpp         the only file including cpp11 or R headers
  engine/
    types.hpp          TeamId, Tiebreaker (enum class), Ratio, constants
    season.hpp/.cpp    SeasonData: records, h2h matrix, SOV, SOS, points ranks
    division.cpp       division procedure
    conference.cpp     conference procedure
    draft.cpp          draft procedure
    rng.hpp            Rng concept, RRng (R's RNG), test RNG
    engine.hpp/.cpp    rank_seasons(): loop over seasons, write outputs
tests/testthat/
  test-engine.cpp            C++ unit tests (testthat's Catch)
  test-standings_engine.R    comparison tests against the reference
  helper-standings_engine.R  random season generator, comparison helpers
data-raw/bench_standings.R   benchmark (baseline numbers above)
```

R only compiles `.cpp` files at the top level of `src/`. Either keep `.cpp`
files flat in `src/` (e.g. `src/engine_division.cpp`) with headers in
`src/engine/`, or list the objects in `Makevars`. Prefer the first.

### Boundary

- R validates and encodes. Team abbreviations become ids 0..31 from the 32
  valid teams (see [league structure](#league-structure)), with ids for
  teams, divisions and conferences in alphabetical order, so sorting by id is
  the same as sorting by name. Keep the lookup tables in `R/sysdata.rda`
  (built in `data-raw/internal_constants.R`). Sims become dense ids
  1..S. `game_type` becomes an integer code.
- One C++ entry point per job:
  - `standings_cpp(games..., ranks, depth, playoff_seeds)` returns a list of
    columns, rows already in output order.
  - `draft_ranks_cpp(games..., sim_team, div_rank, conf_rank, exit, depth)`
    for simulations. It rebuilds `SeasonData` from the games (microseconds)
    instead of passing `h2h` around. This removes the `in_sim` / `h2h`
    attribute workaround.
- C++ returns labels as integer codes (tiebreaker × n). R turns them into
  text with one vector lookup.
- `verbosity = "MAX"`: C++ returns how often each step was used, and R
  prints the step messages afterwards in canonical order.

### Engine

- Pure C++20, no R headers. Inputs `std::span<const int32_t>` /
  `std::span<const double>`, outputs written to preallocated spans.
- Group games by sim in O(n) (counting sort on dense sim ids), then process
  one season at a time. Scratch memory (32×32 head-to-head matrix, records)
  is reused across seasons: no allocation in the loop, memory independent of
  the number of seasons.
- Procedures are step tables (`constexpr std::array` of
  `{Tiebreaker, min_depth, metric}`), not copied code. Two strategies:
  *rank the block* (division) and *pick one winner* (conference, draft).
- `struct Ratio { int32_t num, den; }` with `operator<=>` by
  cross-multiplication (64-bit).
- Random numbers only through an `Rng` concept. `RRng` wraps
  `GetRNGstate()`/`PutRNGstate()` (RAII) and `R_unif_index()`. Coin toss =
  uniform choice among the remaining candidates (one at a time) or a
  Fisher–Yates shuffle (rank a block).
- Errors: R validates before calling. Inside C++, a broken assumption throws
  `nflseedr::EngineError`. cpp11 turns it into an R error. Check for user
  interrupts every ~1,000 seasons with `cpp11::check_user_interrupt()`.

## C++ conventions

Also summarized in `AGENTS.md`.

- cpp11 (no Rcpp), C++20, `SystemRequirements: C++20`. If an old CRAN macOS
  build can't handle it, fall back to C++17 (replace `std::span` and `<=>`).
- Use `std::span`, `std::array`, `constexpr`, `enum class`, concepts,
  `[[nodiscard]]`, `noexcept` in hot paths, `int32_t` for R integers. Avoid
  `<format>`, modules, coroutines, exceptions in the hot path.
- C++ Core Guidelines: no raw `new`/`delete`, no global mutable state, const
  by default, RAII for every resource.
- Never call the R API (`Rf_error`, `R_CheckUserInterrupt`, allocation)
  while C++ objects with destructors are alive. Use `cpp11::safe[]`,
  `cpp11::check_user_interrupt()` or throw.
- Not allowed on CRAN: printing (`std::cout`, `printf`), `exit`, `abort`,
  `rand()`, non-portable flags in `Makevars` (`-O3`, `-march=native`,
  `-Werror`, `-Wall`). Strict warnings belong in `~/.R/Makevars` and CI.
- Tooling (in `.Rbuildignore`): `.clang-format` (Google based, 80 columns),
  `.clang-tidy` (`bugprone-*`, `performance-*`, `modernize-*`,
  `cppcoreguidelines-*`, `readability-*`, minus noisy checks), `.clangd`
  with R and cpp11 include paths. Install with `brew install llvm`. The
  `clangd-lsp` Claude Code plugin gives live diagnostics.
- `devtools::document()` regenerates `src/cpp11.cpp` and `R/cpp11.R`.

## Tests

1. **Comparison harness** (written before any C++):
   - Generator for random seasons rich in ties: results in -3..3 including
     tie games, small scores so point steps are reached, partial seasons
     (weeks 1–4, and a partly played week 1 with teams without games), and
     played playoffs for draft ranks. Always the 32 valid teams.
   - Every combination of `ranks` × `tiebreaker_depth` × `playoff_seeds`.
   - Compare C++ and reference: identical output, except
     - groups decided by `"Coin Toss"`: same teams, same set of ranks;
     - the differences that the decision log allows.
   - About 2,000 seasons in `devtools::test()`; a larger run on demand
     (`NFLSEEDR_LONG_TESTS=true`).
   - Prove the harness can fail: break one step on purpose, see it fail,
     revert.
2. **C++ unit tests** (`usethis::use_catch()`): `Ratio` ordering, each
   strategy on hand-built 2-, 3- and 4-team ties, restart behavior, coin
   toss with a fixed RNG sequence.
3. **Rule scenarios** in R: one small hand-built season per tiebreaker step,
   each named after the step it tests. Use known historical cases where
   possible.
4. **Snapshots**: standings snapshots of real seasons stay identical (they
   rarely hit deep tiebreakers and contain few coin tosses; investigate any
   change). Simulation snapshots change once because the random number
   stream changes (D5); review them before accepting.
5. **Deep tiebreaker reference** (`test-standings.R`, created before M0 from
   the R implementation):
   - Fixture `tests/testthat/deep_tiebreaker_games.rds`: synthetic
     tie-heavy seasons on the 2022 schedule, with scores, picked so that
     together they reach every tiebreaker label the reference can produce at
     depths `"POINTS"`, `"SOS"` and `"PRE-SOV"`. Built by
     `data-raw/build_deep_tiebreaker_games.R`. Don't regenerate it unless
     needed. It is frozen so the snapshots stay comparable.
   - Snapshots `_snaps/standings/deep_tiebreakers_{points,sos,pre-sov}.csv`:
     full `ranks = "DRAFT"` output of the reference, seeded with
     `with_reference_seed()` from `helpers.R` (pins R's default RNG kinds
     because `test-simulations.R` leaves L'Ecuyer-CMRG behind), sorted by sim
     and team so diffs are per team.
   - A test checks that the fixture really reaches the deep steps.
   - Partial week 1 snapshot (teams without games).
   - These snapshots are the yardstick for the C++ engine. A C++ result may
     differ from them only in (a) teams in ties decided by `"Coin Toss"`
     (D5) and everything that follows from them, and (b) differences
     required by decided entries in the decision log. Don't accept a
     snapshot change blindly. For every changed row, name the coin toss or
     decision that explains it. The comparison test (item 1) should read
     these CSV files directly, so it can mask coin toss groups instead of
     failing on them.
   - `draft_tiebreaker_loop_games.rds` reproduces D10. Its snapshot records
     the current error and must change to valid draft ranks with the engine.
6. **Sanitizers and coverage**: rhub `clang-asan`, `clang-ubsan`, `valgrind`
   (`.github/workflows/rhub.yaml`), `covr` including C++.

## Milestones

Each milestone is one or more commits that pass `devtools::test()`. Until M5,
the R engine stays the default and C++ is behind
`options(nflseedR.standings_engine = "cpp")`.

- **M0 Setup.** `usethis::use_cpp11()`, Makevars, tooling files, `dev/` and
  tooling in `.Rbuildignore`, move the R implementation to
  `R/standings_r_*.R`, engine switch, comparison harness (reference vs
  itself), benchmark script.
- **M1 Records.** Team validation (D9), `SeasonData` and `ranks = "NONE"`. All columns
  bit-identical.
- **M2 Division ranks.** Needs D1.
- **M3 Conference ranks** including `playoff_seeds`. Needs D2, D4, D7.
- **M4 Draft ranks** and the simulations entry point. Remove the `h2h`
  attribute workaround. Needs D3, D6.
- **M5 Switch.** C++ by default, MAX verbosity, NEWS bullets (one per
  behavior change in the decision log), docs, `devtools::check()` clean on
  all OS, rhub sanitizers clean, benchmark at least 30x faster than the
  baseline.
- **M6 Cleanup** (next release). Remove the R engine from `R/`, keep it in
  `tests/` only if still needed as reference.

## Status

| Milestone | State | Notes |
|---|---|---|
| M0 | not started | |
| M1 | not started | |
| M2 | not started | |
| M3 | not started | |
| M4 | not started | |
| M5 | not started | |
| M6 | not started | |

## Decision log

Status values: **open** (ask the maintainer), **decided** (follow it).

**D1 Division: how many ranks one step decides. Open, needed for M2.**
The reference ranks the whole block by the deciding step: a 3-way
head-to-head that gives A > B > C sets all three ranks. The article's "other
procedures" say only one club advances per step and the rest restart at step
1, so B vs C would get a fresh 2-club head-to-head and could flip. This
changes `div_rank` 2–4 and with it the wild card division reduction.
Recommendation: one club per step (rule-faithful). Alternative: keep the
reference behavior.

**D2 Conference/draft common games minimum. Open, needed for M3.**
The reference checks "minimum of four" per team: teams with fewer than 4
common games sit out the step while the others are compared. Recommendation:
apply the step only if every candidate has at least 4 common games,
otherwise skip it. Alternative: keep the reference behavior.

**D3 Draft steps after SOV and tiebreaker depth. Open, needed for M4.**
The article lists combined points ranking (league) and net points (all games)
after SOV. The reference goes straight from SOV to the coin toss and ignores
`tiebreaker_depth` for draft steps (only `"RANDOM"` changes anything). SOS is
always used because it is part of the initial order. Recommendation:
implement the article's steps and use the same depth cut-offs as the other
procedures (SOV at depth 2 or higher, point steps at 3). Alternative: keep
the reference.

**D4 Label of the last team in a conference/draft block. Open, needed for
M3.** The reference leaves it `NA` although its rank was decided by the last
step. Recommendation: keep `NA` (no output change). Alternative: give it the
label of the step that decided the last duel.

**D5 Random number stream. Decided.** The C++ coin toss uses R's RNG
differently than `frankv(ties.method = "random")`. Results stay reproducible
with `set.seed()` but differ from the R engine. Accept, update simulation
snapshots, add a NEWS bullet.

**D6 Stable output columns. Open, needed for M4.** Today
`*_tie_broken_by` columns are missing when no tie of that kind happened.
Recommendation: always include them (`NA_character_`) when the matching
`ranks` is requested.

**D7 `playoff_seeds` with `ranks = "DRAFT"`. Open, needed for M3.** The
reference sets conference ranks below `playoff_seeds` to `NA` before the
draft step, which then uses them. Recommendation: always compute all 16
conference ranks internally when draft ranks are requested and set the
lower ones to `NA` only in the output. Alternative: ignore `playoff_seeds`
when `ranks = "DRAFT"`.

**D8 Threads. Decided.** No threads inside C++ for now. R's RNG is not
thread-safe, and chunks already run in parallel with `furrr`. If needed
later: seed a counter-based RNG per season from R's stream.

**D9 Team abbreviations and league size. Decided (2026-10-30).** Always
exactly 32 teams. Only the 32 current abbreviations are valid (`LAR`, `OAK`,
`STL`, `SD` are not; see [league structure](#league-structure)).
`standings_validate_games()` errors with a cli message listing any invalid
abbreviations. Today they quietly get `NA` division and conference. Add a
NEWS bullet.

**D10 Draft loop counter bug. Decided (fix in the engine).** The reference
aborts with "Entered infinite loop in draft tiebreaking procedure" when many
teams are tied for draft picks (same `exit`, `win_pct` and `sos`). Each
`while` round places at most one team per tie, and the counter stops after
18 rounds. Found with synthetic tie-heavy seasons (draft ties of 13+ teams,
about 1 in 5 such seasons fails). Real seasons are very unlikely to hit it.
The conference loop has the same kind of cap (12 rounds) but can't exceed it
because a block has at most 12 wild card teams. The engine places one team
per iteration of a bounded loop over the block (at most 32 iterations), so
no arbitrary cap is needed. Regression fixture:
`tests/testthat/draft_tiebreaker_loop_games.rds`. Add a NEWS bullet.

## Session protocol

1. Read this file, `AGENTS.md` and the tiebreaker article.
2. Work on the next unfinished milestone. If it needs an open decision, ask
   the maintainer first.
3. Before committing: `air format .`, `clang-format`, `devtools::test()`;
   for C++ changes also `devtools::check()` before closing a milestone.
4. Update [status](#status) and the [decision log](#decision-log), and add
   new findings to the rule spec. Rules found in the reference that the spec
   doesn't cover go into the spec, and into the decision log if they differ
   from the article.
