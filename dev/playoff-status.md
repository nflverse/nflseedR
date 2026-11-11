# Playoff status (clinching and elimination)

Plan for a new exported function that says, for every team, whether it has
clinched or been eliminated from a division title, a playoff berth and the
No. 1 seed, given the games played so far. Status: **milestones 1–5
done** (2026-11-11): exported as `nfl_clinching_status()`, experimental. Built on the C++ standings engine, so read
`dev/cpp-standings-engine.md` first. Open decisions are at the end; never
resolve them without the maintainer.

## Goals

For a team X, a **goal** is one of:

| goal | X reaches it if |
|---|---|
| division | `div_rank == 1` |
| playoffs | `conf_rank <= seeds`, by any route (P1) |
| No. 1 seed | `conf_rank == 1` |

A **completion** is any result (win, loss or tie) for every unplayed game.
For every goal the function answers two questions:

- **Can X reach it?** Is there a completion where X reaches the goal?
- **Can X miss it?** Is there a completion where X misses the goal?

Outcomes we can't know count as both possible: coin tosses, and tiebreaking
steps that use points (P2). So X has clinched a goal only if it reaches it
in every completion, however those steps go. Likewise for eliminated.

| can reach | can miss | status |
|---|---|---|
| no | yes | eliminated |
| yes | no | clinched |
| yes | yes | alive |
| one of the two not proven | | undecided |

Every clinched, eliminated and alive is proven: by an infeasible flow (no
completion exists) or by a concrete completion run through the standings
engine (one exists). Undecided means the answer depends on tiebreakers the
search could not settle. It should be rare and only happen late in the
season.

## League formats

The function takes the remaining games from the schedule, so the number of
games and weeks needs no special handling. The number of seeds does.

| seasons | games | weeks | seeds per conference | wild cards | bye |
|---|---|---|---|---|---|
| 2002–2019 | 16 | 17 | 6 | 2 | seeds 1–2 |
| 2020 | 16 | 17 | 7 | 3 | seed 1 |
| 2021– | 17 | 18 | 7 | 3 | seed 1 |

## Interface

Decided (P4, P6, P7):

```r
nfl_clinching_status(
  games,
  ...,
  status = c("DIV", "PLAYOFFS", "1SEED"),
  scenarios = FALSE,
  verbosity = c("MIN", "MAX", "NONE")
)
```

- `games`: like `nfl_standings()` (a schedule from
  `nflreadr::load_schedules()`), one or more seasons. Rows with a `result`
  are played, regular season rows with `result = NA` are remaining.
  Cancelled games must already be removed (P3). "Status after week 12"
  means: set `result` to `NA` after week 12. Validation reuses
  `standings_validate_games()`.
- `status`: one or more goals (`rlang::arg_match(multiple = TRUE)`). Only
  the requested goals are computed, because the playoff goal is by far the
  most expensive.
- `scenarios`: also return the completions that prove each answer (P6).
- No `tiebreaker_depth`: P2 fixes it (all steps through SOS; anything after
  that is unknown).

Output: a data.table with one row per season and team:

`season, team, conf, division, wins, losses, ties, games_left <int>,
div_status, playoffs_status, seed1_status <chr>`

Column names can't start with a digit, so `"1SEED"` becomes `seed1_status`.

Only the status columns for the requested goals are included. Sorted like
`nfl_standings()` (season, division, team).

## Algorithm

### Reduction to win percentage

At the end of the season, "ahead in the standings" means a higher win
percentage. Before tiebreakers:

- Compare final win percentages exactly (`Ratio`), as in the end-of-season
  standings. Game counts can differ when a cancelled game was removed (P3).
  With equal game counts that's the same as comparing
  half-wins (win = 2, tie = 1). With unequal counts, for a given percentage
  of X, the largest number of half-wins team Y can have and still be behind
  or tied is an integer, so the flows below work in both cases.
- **X wins out** is the best case and **X loses out** the worst case for the
  win totals: each of X's wins also adds a loss to a competitor. So the
  "can reach" checks fix X's games as wins and the "can miss" checks fix them
  as losses.
- Only games within the conference matter for the win totals of conference
  teams relative to each other. A game against the other conference can
  always go to the other team (when capping) or to the conference team (when
  pushing it up), so it never constrains a flow.

Each check below runs twice: **strict** (competitors must stay strictly
behind X, or must get strictly ahead) and **weak** (a tie in win percentage
is allowed).

- strict feasible → yes, whatever the tiebreakers say.
- weak infeasible → no, whatever the tiebreakers say.
- weak feasible, strict infeasible → it depends on tiebreakers; see below.

### Two flow problems

Remaining games are nodes with 2 units of supply each (split 2/0 = win,
1/1 = tie, 0/2 = loss), with edges to both teams of the game.

- **Cap** (can a set of teams all stay at or below their limits?): team
  `i -> sink` with capacity `limit_i - current_i`. Feasible iff the max flow
  uses every unit. Teams outside the set have unlimited capacity.
- **Push** (can a set of teams all reach their targets?): team `i -> sink`
  with capacity `target_i - current_i` (≤ 0 means already there). Feasible iff
  the max flow equals the sum of these capacities. Units not needed can go
  anywhere.

The engine needs one small max-flow (Dinic) on at most about 120 game nodes
and 16 team nodes. A team that can never reach X's level (or is always above
it) is handled before any flow: it's either irrelevant or forced.

### Division

- Can reach: cap the 3 rivals at X's best total. One flow.
- Can miss: some rival gets to X's worst total. Each rival on its own, which
  is just arithmetic (`current + remaining`, X-rival games go to the rival).

### No. 1 seed

If any team Y is ahead of X, then Y's division winner is also ahead of X (or
Y is X's rival). So X is the No. 1 seed iff X is ahead of all 15 other
conference teams.

- Can reach: cap all 15 at X's best total. One flow.
- Can miss: some single team reaches X's worst total. Arithmetic, 15 checks.

### Playoffs

With `w` wild cards (2 or 3, see league formats) and `k_d` = number of teams
of division `d` ahead of X, X makes the playoffs iff

    k_X == 0  or  sum_d max(k_d - 1, 0) <= w - 1

(every division puts its winner ahead of X for free; at most `w - 1`
non-winners may be ahead of X).

- Can reach: X wins out. Enumerate the largest **allowed sets** `A` of teams
  that may be ahead of X: `1 + e_d` teams per division with
  `sum e_d = w - 1`. Cap everyone outside `A`. Reachable iff one cap flow is
  feasible. Also try the division path (cap the 3 rivals only) first.
  Pruning: teams already above X's best total must be in `A`, teams that can
  never reach it are never in `A`. Stop at the first feasible set.
- Can miss: X loses out. Enumerate the smallest **forced sets** `B` of teams
  that get ahead of X: at least 1 rival and `sum max(|B ∩ d| - 1, 0) = w`.
  Push everyone in `B`. Missable iff one push flow is feasible.

Worst case without pruning is about 10k small flows per team. Early in the
season a quick scenario (below) settles almost every team before any
enumeration; late in the season pruning leaves few candidates. Benchmark
before optimizing (e.g. the polynomial top-k elimination algorithms of
Gusfield and Martel).

### Tiebreakers

**Scenarios.** A feasible flow is a concrete completion. Run it through the
existing procedures (`SeasonData::build()` on played + completed games,
`rank_divisions()`, `rank_conferences()`, depth SOS) to see where X really
ends up. Whatever the engine produces is a possible outcome, so a scenario
proves "can reach" or "can miss", including tiebreakers. If a coin toss
affects X's goal, both outcomes are possible (P2): the scenario proves both
questions. This includes coin tosses between other teams, e.g. two teams
tied for a division title, where the loser takes a wild card from X. To
find these, the engine reports every coin toss and the scenario is rerun
with each toss flipped.

**Tiebreakers in clinching and elimination proofs.** Scenarios apply the
full NFL tiebreakers, but a scenario only shows that something *is*
possible. Clinching means proving that X can *never* miss, in any
completion, and the flows only count wins. So when a competitor can at best
tie X, the flows alone can't tell who wins the tie, and the answer is
undecided.

Example: one game left, X is 10-6, rival Y is 9-7, and X swept Y. Y can at
most tie X at 10-7, and X then wins the division on head-to-head. The NFL
says X clinched. The wins-only flows see that Y can tie and say undecided.

A tie can only be counted for X if X wins it in *every* completion. Two
things make that harder than "X beat Y":

- **Who else is in the tie.** The proof can't choose a two-team tie; it has
  to cover completions where a third team Z ties too. Then the division
  head-to-head is computed among all three (X 2-0 vs Y but 0-2 vs Z can
  lose), the conference head-to-head sweep doesn't apply if X never played
  Z, and the wild card reduction can replace Y with a tied division rival of
  Y that X never played.
- **Whether the step is already decided.** Most steps (division and
  conference record, common games, SOV, SOS) depend on unplayed games.
  Points and coin tosses never count (P2).

**Sweep rule** (milestone 2). Call team Y **swept by X** if all their games
are played and X won every one. If X swept *every* team that can tie X at
the level of the check, X wins any tie among them, whatever its size:

- Division: X's head-to-head percentage among the tied teams is 1, every
  other tied team lost to X, so X is first.
- Conference: X's own rivals in the tie lose the division tiebreaker to X
  (as above), so X survives the reduction. Then X played and beat every
  other tied team, so X wins the head-to-head sweep step.

Then the flows can count ties as X ahead (strict push in the "can miss"
check). The mirror for elimination: every team that can tie X swept X, and
ties count as X behind (strict cap in the "can reach" check). If even one
team that can tie X isn't swept, all ties stay unsettled.

"Can tie X" is arithmetic: the team's possible range of final win
percentages contains X's level in that check (best total for "can reach",
worst for "can miss"). X losing out stays the worst case under this rule:
games still to play are never between X and a swept team, so flipping one
of X's wins only lowers X or lifts an unswept team.

The rule covers most head-to-head clinches in practice: division rivals
play twice (2-0 is a sweep), wild card teams at most once (1-0 is a sweep).
It misses ties where X wins the head-to-head without a sweep (e.g. a tie
game in the series) and ties decided by later steps, e.g. a finished
division record (milestone 4).

**Order of work** for one team and goal:

1. **Quick scenarios.** X wins out and every other game goes a simple way
   (e.g. home team wins); same with X losing out. Settles most teams early
   in the season with no flow at all.
2. **Strict and weak flows** as above.
3. **Gray zone** (weak feasible, strict infeasible): run the weak flow's
   completion through the engine. If it doesn't prove the answer, try
   more completions within a fixed budget: other allowed or forced sets,
   other flow solutions (randomized augmenting order), and flipping
   unconstrained games in ways that help X's tiebreakers (e.g. teams X beat
   win their other games for a higher SOV). No exhaustive search (P5).
4. Not settled within the budget → undecided.

As built (milestone 3): no quick scenarios, because the flows are fast
enough (all 345 weekly states of 2006–2025 in about 2.5s in a debug build).
Every yes from a strict flow (or from the sweep rule) keeps its flow's
completion as the scenario, without running the engine. The gray zone
search, per allowed or forced set that is weak feasible:

- tightening: starting from the weak caps (or pushes), make the teams that
  can tie X strictly behind (or ahead) one at a time if the flow stays
  feasible, in up to 4 orders (rotations of the team list). Deterministic,
  no randomness, so the result doesn't depend on the seed.
- the games no check constrains go to the home team, then to the away
  team.
- each completion runs through `rank_divisions()` and `rank_conferences()`
  (depth SOS, conference ranks only through the playoff seeds) for every
  coin toss sequence, at most 64 runs.
- at most 32 completions per question.

Flipping games to help X's tiebreakers (e.g. higher SOV) isn't done.

### Tie proofs (milestone 4)

Replace the sweep rule (which they include). `TieProof` works per check,
with X's remaining games fixed (all won for "can reach", all lost for "can
miss").

**Ranges.** For every step through SOS (division: head-to-head, division,
common games and conference percentage, SOV, SOS; conference: head-to-head
sweep, conference percentage, common games with at least four common
opponents, SOV, SOS) the lowest and highest value a team of a tie group can
end with. Every team of a tie group ends with exactly X's win percentage, so
its remaining games bring a known number of half-wins; a record against
some opponents gets at least what the other remaining games can't bring.
Opponents in the group count with exactly that level in SOV and SOS. SOV of
a team with games left is unknown (0 to 1).

**One pick.** X is picked first from a group if a step makes X best for
sure. If X is at least as good as everyone for sure, the step can also
leave X with the teams that can be level with X, and X must be picked first
from each of these smaller groups (the procedure restarts with them;
induction, cached per group). Not picked: some team is better for sure, or
the teams at least as good as X for sure stay with X in every smaller group.
A coin toss (all steps passed) proves nothing.

**Sides.** Of the teams that can tie X in the check:

- a rival is *behind X* if X is picked first from every division group with
  it (X ranks ahead in the division), *ahead of X* if X is never picked
  from such a group.
- the teams of another division are all *behind X* if X wins every division
  tie (X survives every reduction) and X is picked first in the conference
  procedure from every set of one team per other division that includes one
  of them; *ahead of X* if X is never picked from such a set.

The division goal uses the rival sides, the playoffs and the No. 1 seed the
conference sides.

**Flows with sides.** Every cap and push takes the teams that must be
strictly behind (ahead), the others may tie:

- can reach yes: cap with only the teams behind X allowed to tie; no: cap
  infeasible even with only the teams ahead of X strict.
- can miss yes: push with only the teams ahead of X allowed to tie; no:
  push infeasible even with only the teams behind X strict.

Fixing X's games is sound: changing one of X's results to a win only turns
ties with X into X being strictly ahead, and the conditions are monotone in
the set of teams ahead of X.

**Self-check.** With `verify = TRUE` (tests only), every no that needed the
tie proofs (weak flow feasible) runs the weak flow's completion, the one
with the most ties at X's level, through the procedures for every coin toss
sequence and throws if X reaches (misses) the goal. It runs in the weekly
tests; in 103 of the 160 states of weeks 10–17 it checks at least one
answer.

### Season over

With no games left, the status comes straight from the final standings:
clinched or eliminated, except when a coin toss affects X's goal. Then the
goal is undecided until the NFL actually tosses the coin (P2). Points steps
are known at that point but still not used (depth SOS, P2).

## Architecture

```
R/clinching_status.R      nfl_clinching_status(): argument checks, then
                          clinching_status_compute(): validate
                          (standings_validate_games(allow_missing_results =
                          TRUE)), encode played and remaining games with
                          standings_encode_games(), call C++, decode
src/engine/flow.hpp       small Dinic max-flow (vectors reused across flows)
src/flow.cpp
src/engine/status.hpp     compute_clinching_status(): season loop, checks per
src/status.cpp            goal, gray zone search
src/engine/scenario.hpp   ScenarioRunner: a completion through the procedures
src/scenario.cpp          for every coin toss sequence
src/engine/tie_proof.hpp  TieProof: which tied teams X always ranks ahead of
src/tie_proof.cpp         or behind (milestone 4)
src/bindings.cpp          clinching_status_cpp()
```

- Same conventions as the engine: pure C++ outside `bindings.cpp`,
  `EngineError`, interrupt checks between seasons, `RandomSource` only if
  the scenario search uses randomness (seeded, reproducible with
  `set.seed()`).
- Coin tosses: the procedures are unchanged. `ScenarioRunner`
  (`src/engine/scenario.hpp`) passes a scripted `RandomSource` that records
  every toss and forces given results, and tries the other results of every
  toss depth-first. No R RNG is used.
- Seeds per season from the league formats table
  (`clinching_status_seeds()`).
- `check_league()`, `group_games()` and `GamesBySeason` moved from
  `engine.cpp`'s anonymous namespace to `engine.hpp`, shared with the
  standings engine.
- The C++ side always keeps the proving scenario per team and goal; R only
  decodes it with `scenarios = TRUE` (P6) and in tests.

## Testing

No brute force (it only works with a couple of games left). Five checks
instead, all on every week of 2006–2025 (`games_2006_2025.rds`) with the
results after that week set to `NA`.

### 1. Official NFL flags

The NFL API (internal `nflapi` package, maintainer only) returns, per team
and week, `clinched_division`, `clinched_playoff`, `clinched_eliminated`,
`clinched_home_field` (No. 1 seed), `clinched_bye` and `clinched_wild_card`.
Checked for 2008 and 2023: flags start around week 12–14 and grow each week;
2023 week 18 has 14 playoff teams and 18 eliminated.

Mapping (all formats):

- `clinched_division` ↔ division clinched
- `clinched_playoff` ↔ playoffs clinched
- `clinched_eliminated` ↔ playoffs eliminated
- `clinched_home_field` ↔ No. 1 seed clinched
- `clinched_bye` means seeds 1–2 through 2019 and seed 1 since 2020. Not used.
- The NFL has no flags for division or No. 1 seed elimination; checks 2–5
  cover those.

The NFL's week isn't necessarily the first week a team clinched or was
eliminated: the flags may lag, or be set at a particular time in the week.
So compare the **first week** per team, season and goal, not every week:

- **Same week:** expected for most.
- **We're earlier:** possible and allowed. Every case goes into a snapshot
  (`nfl_status_differences.csv`) and is reviewed with its proof (which
  completions were impossible). A missed tiebreaker or a broken flow would
  show up here too, so each one needs an explanation.
- **We're later:** only allowed if we're undecided in the NFL's week, or
  if the NFL used a tiebreaker the sweep rule doesn't cover. Otherwise
  it's a bug in our code or a wrong NFL flag. Also in the snapshot.
- **Contradiction:** we say alive while the NFL says clinched or eliminated.
  Our alive comes with a concrete scenario, so it can be checked by hand.

Known special case: the 2022 BUF-CIN game (week 17) is not in
`games_2006_2025.rds` (P3). So in the week 16 state it's missing, although
at that time it was still a remaining game. Expect differences for AFC teams
around 2022 weeks 16–17. (Milestone 2: BUF's division and CIN's playoff
berth are clinched in week 15, a week before the NFL.)

Fixture: `tests/testthat/nfl_clinch_2006_2025.rds` (built by
`data-raw/build_nfl_clinch_2006_2025.R`): season, week, team and the four
flags above, every team and week of 2006–2025, no missing values.

What the flags mean (checked 2026-11-11 on the whole fixture):

- Week n's flags include all of week n's games, like the standings of week
  n in `nfl_standings_2006_2025.rds`. Every division clinch that wins alone
  prove (current wins above every rival's maximum) is flagged in that very
  week (326 of 326 team-weeks), so the flags don't lag behind.
- No flag ever turns off again. None of the implications of the invariants
  (check 5) is violated, nor the per-conference and per-division counts.
- Final week: all four flags match the final standings exactly (640
  team-seasons).
- First week with a flag: weeks 12–18 for division, playoffs and No. 1 seed,
  weeks 11–18 for eliminated. Of the 141 teams that clinched their division
  by wins alone at some point, the NFL flagged 32 one or two weeks earlier
  (through multi-team flows or tiebreakers), which is roughly what our
  version has to match.

### 2. The real season as a scenario

The actual rest of the season is one completion. If X clinched a goal in
week n, X really reached it at the end of the season. If X was eliminated,
X really missed it. Cheap, needs no new fixture, covers every format.

### 3. Scenario self-check

Every alive answer is backed by scenarios (`scenarios = TRUE`). Run each
through `nfl_standings()` and check that X really reaches (and misses) the goal.
Uses the same engine, so this only catches bugs in the scenario search,
not in the engine.

### 4. Simulations

`nfl_simulations()` from the same state: if X reached a goal in any
simulation, X must not be eliminated from it; if X missed it in any
simulation, X must not have clinched it. Catches wrong proofs in early
weeks.

### 5. Invariants

- Once clinched or eliminated, it stays that way for the rest of the season.
- Division clinched ⇒ playoffs clinched; No. 1 seed clinched ⇒ division
  clinched; playoffs eliminated ⇒ division and No. 1 seed eliminated.
- Per conference and week: at most `seeds` teams clinched the playoffs, at
  most 1 per division clinched it, at most 1 the No. 1 seed.
- Final week: every status follows from the final standings (except coin
  tosses).

Plus short synthetic tests: tie games, the cancelled 2022 BUF-CIN game,
2019 (6 seeds) vs 2020 (7 seeds), input errors.

### Results of milestone 2

`tests/testthat/test-clinching_status.R` runs checks 1, 2 and 5 on every
week of 2006–2025 (345 states, about 1s in a debug build). Snapshot:
`_snaps/clinching_status/nfl_status_differences.csv`.

- **Check 2 (real season)** and **check 5 (invariants)**: no violations.
- **Alive** already comes out of milestone 2: a feasible strict flow is a
  completion where X reaches (or misses) the goal whatever the tiebreakers
  say, so it proves the answer without the engine.
- **Nested goals.** No. 1 seed ⇒ division ⇒ playoffs, so an answer left
  open by one goal's checks is taken from another goal where that is sound
  (e.g. can't miss the division ⇒ can't miss the playoffs). Without it,
  20 team-weeks had the division clinched but the playoffs undecided,
  because the playoff sweep rule needs every conference team that can tie
  X, not only the rivals. Unrequested goals are computed only when needed
  for this.
- **Check 1 (NFL flags)**, first week per team, season and goal (840 NFL
  first weeks):

  | goal | same | earlier | later | NFL only |
  |---|---|---|---|---|
  | division | 136 | 1 | 11 | 12 |
  | playoffs | 185 | 1 | 43 | 23 |
  | eliminated | 289 | 0 | 72 | 27 |
  | No. 1 seed | 33 | 0 | 1 | 6 |

  Never a contradiction: in every later or NFL-only case we're undecided
  in the NFL's week. The two earlier cases are 2022 BUF and CIN (the
  missing BUF-CIN game).
- **Gray zone.** Of the 195 later or NFL-only cases, 56 are in the final
  state with no games left (division 9, playoffs 21, eliminated 23, No. 1
  seed 3); milestone 3 settles those from the final standings. The other
  139 have games left. Scenarios (milestone 3) only prove "can reach" and
  "can miss", so they can't produce these clinches and eliminations; only
  more tiebreakers in the proofs (milestone 4) can. Milestone 3 will mostly
  turn undecided into alive (about 400 undecided team-weeks per goal with
  games left).

### Results of milestone 3

- **Season over** is exact: every coin toss sequence of the final standings
  (at most 4,096 runs). The final week of every season 2006–2025 matches the
  final standings, without an undecided status (no coin toss decided a
  goal). When a coin toss does decide a goal with no games left, the status
  is undecided, not alive (see [season over](#season-over)); also when the
  coin tosses exceed the budget and the normal search finds both answers.
  Tested with a season of only week 1.
- **Undecided** team-weeks dropped from 416 / 445 / 422 (division /
  playoffs / No. 1 seed) to 141 / 181 / 184.
- **Check 3 (scenarios)**: all 56,983 scenarios of 2006–2025 reach or miss
  the goal in `nfl_standings()` as they claim (one needs a coin toss and
  isn't checked). The test runs 2012, 2019 and 2023. Checks 2 and 5 still
  hold.
- **Check 1 (NFL flags)**: all 56 NFL-only cases (final week) now match.
  141 differences remain (snapshot): the 2 BUF/CIN cases and 139 where we're
  later and undecided in the NFL's week, all with games left. Scenarios
  can't settle these (they need proofs of no), so only milestone 4 can.

  | goal | same | earlier | later |
  |---|---|---|---|
  | division | 145 | 1 | 14 |
  | playoffs | 206 | 1 | 45 |
  | eliminated | 312 | 0 | 76 |
  | No. 1 seed | 36 | 0 | 4 |
- **Scenario output** (internal, `scenarios = TRUE`): a list of the status
  table and a scenario table with one row per scenario and remaining game:
  `scenario` (id), `season`, `team`, `goal`, `proves`, `coin_toss`, then the
  columns of `games` with the scenario's results (1, 0, -1). `scenario` and
  `coin_toss` go beyond P6 (see P6). A season with no games left has no
  rows.

### Results of milestone 4

- **Check 1 (NFL flags)**: 94 of the 139 later cases now match the NFL's
  week. 45 differences remain later (snapshot, 47 rows with BUF/CIN), all
  undecided in the NFL's week:

  | goal | same | earlier | later |
  |---|---|---|---|
  | division | 157 | 1 | 2 |
  | playoffs | 223 | 1 | 28 |
  | eliminated | 373 | 0 | 15 |
  | No. 1 seed | 40 | 0 | 0 |
- **Undecided** team-weeks: 27 / 86 / 117 (division / playoffs / No. 1
  seed), from 141 / 181 / 184.
- **Soundness**: checks 2, 3 and 5 hold, the self-check passes on every
  week of 2006–2025, and about 62,000 random completions of weeks 9–17
  (half of them the worst case of a decided team, 1% and 10% tie games) never
  contradict a clinch or elimination. The test runs a smaller version
  (check 4 without `nfl_simulations()`: random results instead of the elo
  model).
- Remaining gaps are mostly SOV of teams with games left (unknown), ties
  among teams that X never played, and the per-team sides being
  conservative (e.g. a team of another division counts only if X wins or
  loses against every set of representatives with it).

### Results of milestone 5

- `nfl_clinching_status(games, ..., status, scenarios, verbosity)`, marked
  experimental with `lifecycle::badge("experimental")`. Arguments are
  checked up front (`rlang::check_dots_empty()`, `rlang::arg_match()`, and
  `rlang::is_bool()` for `scenarios`, since the package has no standalone
  `check_*` functions). `verbosity = "MAX"` reports how many teams got each
  status.
- Input: a plain `data.table` copy of `games`, so tibbles work (their class
  survives `setDT()`) and the user's table isn't changed. Playoff games
  without a result are dropped before validation (their teams may be
  missing).
- Scenarios are the attribute `"scenarios"` (P6).
- `data-raw/bench_clinching_status.R`, Apple Silicon, optimized build:

  | case | median |
  |---|---|
  | every week of 2006–2025 (345 calls) | 0.56s |
  | 2023 after week 15, all goals | 2.1ms |
  | the same, `status = "PLAYOFFS"` | 1.7ms |
  | the same, `scenarios = TRUE` | 3.5ms |
  | 1,000 simulated seasons after week 12, one call | 0.95s |

## Milestones

1. Fixture with the official NFL flags, and a look at what they mean week
   to week. **Done** (2026-11-11), see check 1.
2. Max-flow + win-percentage checks for all three goals, with the sweep
   rule (clinched, eliminated, undecided only). Compare with the NFL flags
   to see how big the gray zone is. **Done** (2026-11-11), see
   [results of milestone 2](#results-of-milestone-2).
3. Scenarios through the engine (alive, and possible outcomes through
   tiebreakers and coin tosses). **Done** (2026-11-11), see
   [results of milestone 3](#results-of-milestone-3).
4. Only if the NFL comparison shows it's worth it: later tiebreaking steps
   that played games have already decided (e.g. a finished division record),
   which need a careful look at who else can be in the tie. **Done**
   (2026-11-11), see [tie proofs](#tie-proofs-milestone-4).
5. R interface, docs, `_pkgdown.yml`, NEWS, benchmark. **Done**
   (2026-11-11), see [results of milestone 5](#results-of-milestone-5).

## Decisions

**P1 What the playoff goal means. Decided by the maintainer (2026-11-11).** A
playoff berth by any route, including a division title:
`conf_rank <= seeds`, like the NFL's `clinched_playoff`. Column
`playoffs_status`.

**P2 Coin tosses and points. Decided by the maintainer (2026-11-11).**
Coin tosses and points tiebreakers can't be used to clinch or eliminate,
because their outcomes are unknown. Both outcomes count as possible.
Tiebreakers run through SOS; anything after that is unknown. So there's no
`tiebreaker_depth` argument.

**P3 Cancelled games. Decided by the maintainer (2026-11-11).** Cancelled
games are removed from `games` before it reaches the function, so every
regular season game without a result is a remaining game. Teams are compared
by win percentage, as in the end-of-season standings, so different game
counts are fine.

**P4 Function name, goals and states. Decided by the maintainer
(2026-11-11).** `nfl_clinching_status(games, ...)` with
`status = c("DIV", "PLAYOFFS", "1SEED")` (one or more goals) and the states
clinched, alive, eliminated, undecided.

**P5 Exhaustive search. Decided by the maintainer (2026-11-11): no.** The
function never enumerates all completions. What the flows, settled
tiebreakers and the scenario budget can't decide stays undecided.

**P6 Scenario output. Decided by the maintainer (2026-11-11):** an argument
`scenarios = FALSE`. With `TRUE`, the result also includes a table with
season, team, goal, what the scenario proves (`"reach"` or `"miss"`) and the
results of the remaining games, in the format of `games` so it can go
straight into `nfl_standings()`. Decided by the maintainer
(2026-11-11): the table is the attribute `"scenarios"` of the result, so the
return type doesn't depend on the argument. It keeps the two columns added
in milestone 3: `scenario` (an id to tell scenarios apart) and `coin_toss`
(the scenario needs a particular coin toss, so `nfl_standings()` reproduces
it only with that toss).

**P7 Output column names. Decided by the maintainer (2026-11-11):**
`div_status`, `playoffs_status`, `seed1_status`.
