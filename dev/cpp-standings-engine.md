# C++ standings engine

`nfl_standings()` computes standings with a C++ engine (`src/`). The old
data.table implementation is kept as the internal `nfl_standings2()`
(`R/standings_r*.R`) so both can be compared (e.g.
`data-raw/bench_standings.R`). Branch:
`cpp-standings-engine`, started 2026-10-30.

**Agents: read this file before touching standings, tiebreaker or `src/`
code.** Record every new behavior difference between `nfl_standings()` and
`nfl_standings2()` in the [decision log](#decision-log). Never resolve an open
decision on your own; ask the maintainer and record the answer here.

## Goal

Make `nfl_standings()` (and the draft ranking step in `nfl_simulations()`)
30x or more faster without changing its interface.

`data-raw/bench_standings.R`: 10,000 simulated regular seasons (2.7M game
rows), Apple Silicon, `verbosity = "NONE"`, optimized build:

| `ranks` | `nfl_standings2()` | `nfl_standings()` | speedup |
|---|---|---|---|
| `"DIV"` | 3.29s | 186ms | 18x |
| `"CONF"` | 6.39s | 207ms | 31x |
| `"DRAFT"` | 6.99s | 229ms | 31x |
| `"CONF"`, one season | 41ms | 0.89ms | 46x |

Both columns include copying the 2.7M-row input (about 70ms). Of the rest,
the C++ engine itself takes about 0.1s; validating and encoding the input in
R takes most of the remaining time.

## Rule spec

The rules are
[`vignettes/articles/tiebreaker.Rmd`](../vignettes/articles/tiebreaker.Rmd).
This section makes them precise. Where the rules don't settle a behavior,
`nfl_standings2()` does, unless the decision log says otherwise.

"Games" means regular season games (`game_type == "REG"`) unless stated
otherwise. A tie game counts as half a win and half a loss.

### League structure

Every season has **exactly 32 teams** in 2 conferences, 8 divisions and 4
teams per division: `nflseedR::divisions` without `LAR`, `OAK`, `STL` and
`SD` (D9). The engine relies on these constants (fixed-size arrays, a 32×32
head-to-head matrix).

Only teams with at least one regular season game get an output row.
**Division, conference and draft ranks (and their labels) are `NA` until
every team has played a game** in that season.

### Records

- `wins` = wins + 0.5 × ties. `win_pct = wins / games`.
- `div_pct`, `conf_pct`: the same restricted to division or conference games.
  0 if no such games.
- **Head-to-head** (division): win percentage in games against the other
  tied teams, 0 without such games.
- **Head-to-head sweep** (conference and draft): best if the team won every
  game against every other tied team *and* played each of them, worst if it
  lost all of them, otherwise in between.
- **Common games** of the tied teams: games against opponents that *every*
  tied team played. Tied teams are never common opponents. Common win
  percentage (0 if none), common point differential (sum of `result`).
- **SOV**: sum of `wins` of beaten opponents (once per win) / sum of their
  `games`. 0 without wins. **SOS**: the same for all opponents (once per
  game).
- **Combined points ranking** among the conference or league teams: rank of
  points scored (descending) + rank of points allowed (ascending), ties get
  the lowest rank. Lower is better.
- **Net points**: point differential in conference games (worst possible if
  no conference games) or all games.
- Net touchdowns are not implemented (no data).

**Comparing records** (overall, division, conference): by win percentage.
During the regular season, equal percentages are broken by net wins (wins −
losses), so 2-0 ranks ahead of 1-0 and 0-0 ahead of 0-1 (D13). Once the
regular season is complete (its final week, 17 through 2020 and 18 since
2021, or a playoff game was played), only percentages count, even if a
cancelled game left teams with fewer games. With `sim` instead of `season`,
18 weeks are assumed; `nfl_simulations()` always ranks complete seasons.
Head-to-head and common games compare percentages only.

Win counts are half-wins, so every percentage is a ratio of integers and
comparisons are exact (`Ratio`, cross-multiplication). Output columns use the
same double formulas as `nfl_standings2()` and are bit-identical.

### Tiebreaker depth

| depth | steps used (division, conference, draft) |
|---|---|
| `"RANDOM"` (0) | none: every tie is broken at random |
| `"PRE-SOV"` (1) | everything before SOV |
| `"SOS"` (2, default) | everything through SOS |
| `"POINTS"` (3) | everything through net points in all games. Needs scores. |

### One team per step

All procedures pick **one team at a time** from a block of tied teams (same
`win_pct`; draft: same `exit`, `win_pct`, `sos`). Run the steps on the
candidates: a step with one best team gives it the next rank. A step that
eliminates some teams restarts the steps with the remaining teams. A step
that changes nothing goes on to the next step. After the coin toss, or when
a team took the rank, all teams that are left come back for the next rank.

### Division ranks (`div_rank`)

Steps: head-to-head win percentage, division win percentage, common games
win percentage, conference win percentage, SOV, SOS, combined points ranking
(conference), combined points ranking (league), common games point
differential, point differential, coin toss.

The step that decides the last two teams labels both of them.

### Conference ranks (`conf_rank`)

Division winners take ranks 1 to 4, all other teams 5 to 16. For every rank:

1. If all candidates are from the same division, rank them all by `div_rank`
   ("Division Tiebreaker").
2. Otherwise keep only the best `div_rank` of each division (reduction).
3. Steps: head-to-head sweep, conference win percentage, common games win
   percentage (only with at least 4 common opponents, D14), SOV,
   SOS, combined points ranking (conference), combined points ranking
   (league), net points in conference games, net points, coin toss.

The last team of a block gets no label (D4). With `playoff_seeds = k`, blocks
that start below seed k are skipped and ranks below k are `NA`. Ignored for
`ranks = "DRAFT"` (D7).

### Draft ranks (`draft_rank`)

Worse teams pick first, so every comparison is reversed.

1. Sort by `exit` (non-playoff teams, then Wild Card, Divisional, Conference
   and Super Bowl losers, the Super Bowl winner last), `win_pct`, `sos`.
2. If all candidates are from the same division, the worse `div_rank` picks
   first ("Division Tiebreaker"). Else if all are from the same conference,
   the worse `conf_rank` ("Conference Tiebreaker").
3. Otherwise keep the worst `conf_rank` of each conference (two teams).
4. Steps: head-to-head, common games win percentage (minimum of four common
   opponents, D14), SOV, combined points ranking (league), net points, coin toss (D3).

`exit` is the last playoff round of a team (`"SB_WIN"` for the winner,
`"REG"` without playoff games). In simulations, it comes from the simulated
playoffs and existing division and conference ranks are reused
(`standings_add_draft_ranks()`).

### Labels (`*_tie_broken_by`)

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
  `"League Points Rank (n)"`, `"Point Differential (n)"`, `"Coin Toss"`.

`n` is the number of candidates in the deciding step. Teams never in a tie
get `NA`. With `"RANDOM"`, every team of a tied block gets `"Coin Toss"`.

## Output schema

`sim|season, team, conf, division, games <int>, wins <dbl>,
true_wins <int>, losses <int>, ties <int>, [pf, pa, pd], win_pct, div_pct,
conf_pct, sov, sos <dbl>, div_rank <int>, div_tie_broken_by <chr>,
conf_rank <int>, conf_tie_broken_by <chr>, exit <chr>, draft_rank <int>,
draft_tie_broken_by <chr>`

- Rank and label columns exist for the requested `ranks` (labels always,
  D6). `exit` only for `"DRAFT"`.
- `pf`, `pa`, `pd` only with `home_score`/`away_score` (integer if both score
  columns are integer).
- Sorted by season, division, division rank, team.

## Architecture

```
R/standings.R          nfl_standings(): match arguments, call standings_compute()
R/standings_cpp.R      standings_compute(): validate, encode, call C++, decode;
                       standings_add_draft_ranks() for simulations; labels
R/standings_utils.R    standings_validate_games() (shared)
R/standings_r.R        nfl_standings2(): the old implementation (internal)
R/standings_r_*.R      its helpers, unchanged apart from finalize_standings()
                       moving here
src/
  bindings.cpp         standings_cpp(), draft_ranks_cpp(), elo_week_cpp(),
                       RRandom (R's RNG).
                       The standings functions take the games and league as
                       one list from standings_encode_games(). The only
                       engine file that includes R or cpp11 headers
  engine/types.hpp     constants, Ratio, Step, label codes, RandomSource
  engine/season.hpp    SeasonData: records, h2h, SOV, SOS, points ranks
  season.cpp
  engine/procedures.hpp  division, conference and draft procedures
  procedures.cpp
  engine/engine.hpp    compute_standings(), compute_draft_ranks(): season loop
  engine.cpp
  engine/elo.hpp       simulate_elo_week(): the elo model of
  elo.cpp              cpp_compute_results(), see its section below
  cpp11.cpp            generated by devtools::document(), never edit
```

- R encodes teams as ids 0..31 (alphabetical, so sorting by id equals sorting
  by name), seasons as dense ids, `game_type` as 0..4. Label codes are
  `step * 10 + n` (`Step` in `types.hpp`), decoded by `standings_labels()`.
- Games are grouped by season with a counting sort. Each season is processed
  on its own with fixed-size arrays.
- Coin tosses use `R_unif_index()`, so `set.seed()` makes results
  reproducible (but different from `nfl_standings2()`, D5).

## C++ conventions

Also summarized in `AGENTS.md`.

- cpp11 (no Rcpp), C++20, `SystemRequirements: C++20`.
- Never call the R API while C++ objects with destructors are alive. Throw
  `nflseedr::EngineError`, use `cpp11::safe[]` and
  `cpp11::check_user_interrupt()`.
- Not allowed on CRAN: printing, `exit`, `abort`, `rand()`, non-portable
  flags in `Makevars`.
- Run `clang-format -i` on changed C++ files (`.clang-format`: Google
  style, 80 columns).

## Tests

Top-level snapshot tests only. The weekly comparison with the NFL is the
strongest one. Snapshots are CSV files sorted by season and
team, so diffs show changed teams. Coin tosses are seeded with
`with_reference_seed()` (`tests/testthat/helpers.R`).

- `test-standings.R` (`nfl_standings()`): 2006–2025
  (`games_2006_2025.rds`, built by `data-raw/build_games_2006_2025.R`), the
  example simulation games, and the deep tiebreaker games at every depth. A
  test checks that the deep tiebreaker games reach every tiebreaker step.
  The standings of every week of 2006–2025 are compared with the official NFL
  standings (`nfl_standings_2006_2025.rds`, built with the internal `nflapi`
  package by `data-raw/build_nfl_standings_2006_2025.R`). The snapshot
  `nfl_weekly_differences.csv` lists every team whose rank differs (18 of
  11,038 team-weeks after D14): coin tosses in week 1 (the NFL seems to put
  the alphabetically first team ahead) and two unexplained pairs (2008 week 2
  TEN/PIT: the NFL seems to skip the conference points ranking; 2019 week 7
  MIN/SEA: the NFL doesn't use net wins there, against D13).
  Short tests for `ranks`/`playoff_seeds`, missing ranks before every team
  played, large draft ties (D10) and input errors.
- `deep_tiebreaker_games.rds` (built by
  `data-raw/build_deep_tiebreaker_games.R`): synthetic tie-heavy seasons on
  the 2022 schedule that reach every tiebreaker step.

The C++ engine was verified against `nfl_standings2()` before these snapshots
were taken (2026-10-30): identical output on 1,000 random tie-heavy seasons,
real seasons 2002–2025 and the deep tiebreaker games, apart from coin tosses
and the decisions below. Review every snapshot change: name the coin toss or
decision that explains it.

## `cpp_compute_results()`

Exported C++ port of `nflseedR_compute_results()` (`R/simulations_utils.R`,
`src/elo.cpp`, `src/engine/elo.hpp`, `elo_week_cpp()` in `bindings.cpp`).
It is not part of the standings engine but shares `src/`, `RandomSource` and
the conventions above. Added 2026-10-30 (D15).

**Contract: same output.** For the same seed it returns what
`nflseedR_compute_results()` returns, within floating-point tolerance. The
tests compare with `expect_equal()`. Any change to the elo model must go
into both functions.

- Normal draws use `RandomSource::normal()` (R's `norm_rand()`), one per
  missing result in game order. `rnorm(n, mu, 13)` draws the same way, so the
  random stream matches. A missing mean draws nothing, like `rnorm()`.
- Elo values can differ from R in the last bits (about 1e-13 after many
  weeks): compilers may turn `pow(10.0, x)` into `exp10(x)` or fuse
  multiply-add. Both could be prevented (a `volatile` base,
  `#pragma clang fp contract(off)`), but we don't use compiler-specific
  workarounds, to keep the code portable for CRAN.

**R side.** `cpp_compute_results()` keeps the elo start values in R (same
`rnorm()` call as the original), finds the week's rows with `sims_week_rows()` (like `nfl_simulations()`;
`positions_of_cpp()` on factor codes is about 4x faster than `which()`), and passes
only those rows with teams matched to integer ids. Use `%chin%`, not `%in%`,
for the string checks: `%in%` cost about 28ms per week at 50,000 sims.
`nfl_simulations()` doesn't show its "own function" message for
`cpp_compute_results`, and sends both built-in functions to the workers by
name (`sims_builtin_results()`), so furrr doesn't scan them for globals.

**Behavior difference.** If a game's elo difference is `NA` (a team missing
from `teams`, missing rest), the result stays `NA`.
`nflseedR_compute_results()` errors there (`round_out()` drops the `NA` and
the lengths no longer match), so no working input changes.

**Benchmark** (`data-raw/bench_compute_results.R`, Apple Silicon, optimized
build):

| | `nflseedR_compute_results()` | `cpp_compute_results()` | |
|---|---|---|---|
| function alone, all weeks, 50,000 sims | 6.07s | 2.87s | 2.1x |
| `nfl_simulations()`, 50,000 sims, 1 chunk | 9.68s | 6.47s | −33% |

Benchmark only optimized builds. `load_all()` compiles with `-O0`, and
`R CMD INSTALL` reuses stale `-O0` objects in `src/` unless run with
`--preclean` (that made both functions look twice as slow and C++ slower
than R).

**Where the time goes** (50,000 sims, one week, 800,000 games): the C++ call
takes about 40ms, mostly `norm_rand()`, `pow()` and `log()`. Preparing its
inputs in R takes about 50ms (subsetting the week's rows, `chmatch()` of
team names, `unique(teams$team)`). A scratch prototype that read the strings
directly in C++ (CHARSXP pointer comparison) was about 3.9x faster than R
for the function alone and 37% faster end to end. Not done because it
puts string handling into `bindings.cpp` (`STRING_ELT()` while C++ objects
are alive). That's the next step if more speed is needed.

## Decision log

**D1 Division: one team per step. Decided (2026-10-30).** `nfl_standings2()`
ranks the whole block by the deciding step (a 3-way head-to-head A > B > C
sets all three ranks). The engine follows the rules: one team per step, the
others restart at step 1. Real seasons 2002–2025: only labels change
(NFC South 2012, 2022, 2025). The new labels match the official tiebreaker
notes (Wikipedia season pages), e.g. 2025: Carolina by 3-team head-to-head,
then Tampa Bay over Atlanta by common games (2 teams).

**D2 Common games minimum of four. Decided (2026-10-30), replaced by D14.**
The conference and draft step applies only if every candidate has at least 4
common games. `nfl_standings2()` let teams with fewer sit out the step.

**D3 Draft steps after SOV. Decided (2026-10-30).** Combined points ranking
(league) and net points follow SOV, and draft steps use the depth cut-offs
of the other procedures. `nfl_standings2()` went from SOV to the coin toss
and ignored the depth.

**D4 Label of the last team in a conference or draft block. Decided
(2026-10-30).** Stays `NA`.

**D5 Random number stream. Decided.** Coin tosses differ from
`nfl_standings2()`, reproducible with `set.seed()`.

**D6 Stable output columns. Decided (2026-10-30).** `*_tie_broken_by`
columns are always there for the requested `ranks`.

**D7 `playoff_seeds` with `ranks = "DRAFT"`. Decided (2026-10-30):
ignored.** Draft ranks need all conference ranks.

**D8 Threads. Decided.** No threads in C++.

**D9 Team abbreviations. Decided (2026-10-30).** Only the 32 current
abbreviations. `nfl_standings()` errors on others and points to
`nflreadr::clean_team_abbrs()`. Historical schedules from
`nflreadr::load_schedules()` (e.g. 2008–2019 with `STL`, `SD`, `OAK`) need
that cleaning now.

**D10 Draft loop counter bug. Decided: fixed.** `nfl_standings2()` aborts
with "Entered infinite loop in draft tiebreaking procedure" for large draft
ties. The engine has no such limit.

**D11 `tied_for` column. Decided (2026-10-30): dropped.** `nfl_standings2()`
leaks it with `"RANDOM"` and `ranks = "CONF"`/`"DRAFT"`.

**D12 Division points rank bug. Decided (2026-10-30): fixed.** When two tie blocks of the same division (e.g. two
2-team blocks after a 4-team tie split at SOS) both reach the combined points
ranking, `nfl_standings2()` ranks them together. Teams of the worse block can
end up ahead of the better block, e.g. against win percentage. Found with the
deep tiebreaker fixture (sim 27) and random seasons. The engine ranks each
block separately.

**D13 Records with different numbers of games. Decided (2026-10-30).**
Equal percentages are broken by net wins (wins − losses) for the overall,
division and conference records, as in the official NFL standings during the
season (e.g. conference 2-0 ahead of 1-0, 0-2 overall ahead of 0-3).
Compared with the NFL's weekly standings 2006–2025, this reduced the
differing division-weeks from 161 to 1 and conference-weeks from 177 to 7
(of 345 weekly standings). It applies only during the regular season, not
after its final week, e.g. 2022 when the cancelled BUF-CIN game left both
teams with 16 games. The rule isn't in the official tiebreaking procedures,
which only mention percentages.

**D14 Minimum of four common opponents. Decided (2026-10-30).** The
conference and draft common games step needs at least four common opponents,
not four common games (a team can play a common opponent twice if it's a
division rival). Then every candidate also has at least four common games, so
this replaces D2. Found in the NFL's weekly standings: 2009 week 16, HOU and
PIT had 4 common games each but only 3 common opponents (LV, TEN, CIN). The
NFL skipped the step and ranked PIT ahead by SOV. No other weekly standings
2006–2025 changed. D2 and D14 only differ during the season: in the final
standings 2006–2025, they agree for all 8,960 pairs of teams from different
divisions (in 2009 week 16 they disagree for 24 pairs).

**D15 C++ port of `nflseedR_compute_results()`. Decided by the maintainer
(2026-10-30).** Exported as a new function `cpp_compute_results()`.
`nflseedR_compute_results()` stays unchanged and remains the default of
`nfl_simulations()`. `RandomSource` gets a `normal()` method for the normal
draws. See the section [`cpp_compute_results()`](#cpp_compute_results).
