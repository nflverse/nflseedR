# Playoff status (clinching and elimination)

Plan for a new exported function that says, for every team, whether it has
clinched or been eliminated from a division title, a playoff berth and the
No. 1 seed, given the games played so far. Status: **milestones 1–5
done** (2026-11-11), milestone 6 (tighter tie proofs) partly done, milestone 7
(pairwise strength and wild card sides) done (2026-11-12), milestone 8 (gap 3,
per-team part) done (2026-11-12), milestone 9 (budgeted search, off by
default) done (2026-11-12), set constraints open (P11): exported as `nfl_clinching_status()`, experimental, returning the current standings with the status added (P9). Built on the C++ standings engine, so read
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
  search = FALSE,
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
- `search`: run the budgeted search for open answers (P12, milestone 9).
  Off by default (decided by the maintainer, 2026-11-12), because it makes
  calls with many open answers several times slower. With `FALSE` and
  `verbosity` not `"NONE"`, a hint says how many statuses
  of teams with games left are undecided, that `search = TRUE` might decide
  them, and that it helps most with 5 or fewer games left per team.
- No `tiebreaker_depth`: P2 fixes it (all steps through SOS; anything after
  that is unknown).

Output (P9): the current standings, i.e. `nfl_standings()` of the played
games (`ranks = "CONF"`, `tiebreaker_depth = "SOS"`, all 16 conference
ranks), with these columns added at the end:

`games_left <int>, div_status, playoffs_status, seed1_status <chr>`

Column names can't start with a digit, so `"1SEED"` becomes `seed1_status`.

Only the status columns for the requested goals are included. Rows and
order are those of `nfl_standings()`: only teams that have played a game,
sorted by season, division and division rank. `verbosity` is passed on to
`nfl_standings()`. The standings get `season_complete` from the remaining
games (complete iff none are left), not from the last week with a played
game, so a partly played final week is ranked as in-season (net wins break
ties of equal percentages).

The internal `clinching_status_compute()` keeps its own table (one row per
season and team, all 32 teams: `season, team, conf, division, wins, losses,
ties, games_left` and the status columns, sorted by season, division and
team); the tests use it. `clinching_status_standings()` joins it onto the
standings.

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

**Pairwise comparisons** (milestone 7). Every use of the ranges compares two
teams (`lead(a, b)`: a ahead for sure, never behind, or neither). For SOV and
SOS it also bounds the difference of the two values directly
(`lowest_strength_difference()`), because separate ranges count a shared
opponent low for one team and high for the other at the same time. With the
weights fixed (games per opponent for SOS, wins per opponent for SOV), the
difference is linear in the opponents' half-wins: tied teams end at their
tie level, every open game goes the way that lowers the difference most.
For SOV the results of the two teams' own open games are enumerated (depth
first, only results that leave both at their tie level, at most 4 open
games); with more, SOV stays unknown. All of this is a relaxation of the
real completions, so it's sound, and never weaker than the ranges.

**One pick.** X is picked first from a group if a step makes X best for
sure. If X is at least as good as everyone for sure, the step can also
leave X with the teams that can be level with X, and X must be picked first
from each of these smaller groups (the procedure restarts with them;
induction, cached per group). Not picked: some team is better for sure, or
the teams at least as good as X for sure stay with X in every smaller group.
A coin toss (all steps passed) proves nothing.

**Sides.** Of the teams that can tie X in the check:

- a rival is *behind X* if X ranks ahead of it in the division ranking of
  every division group with it, *ahead of X* if X always ranks behind it.
  "Ranks ahead" follows the picks (milestone 6): from the teams one pick can
  pick, X must be one, or every other one must leave a group where X still
  ranks ahead. So in a three-way tie X can lose the first pick to a third
  team and still be ahead of the rival.
- the teams of another division are all *behind X* if X wins every division
  tie (X survives every reduction) and X is picked first in the conference
  procedure from every set of one team per other division that includes one
  of them; *ahead of X* if X is never picked from such a set. For the
  playoffs (milestone 6), teams that win their division whenever they are
  level with X (no other team of their division can reach X's win
  percentage) are left out of these sets: division winners are ranked apart
  from the wild cards, and one team of a division ahead of X is free in the
  playoff count, so their own side doesn't matter either.
- milestone 7 refines the sides of teams of another division, per team
  instead of per division, and for the wild card in four ways (each one is
  tried after the existing rule, so sides only grow):
  - *behind X* if X survives every reduction and is picked first from every
    set of representatives in which this team represents its division. It
    can only be picked while it does.
  - wild card, *behind X*: the conference procedure reduces again after
    every pick, and a rival that ranks ahead of X in the division is ahead
    of X anyway (the flows count it). So team y is behind X if y can't be
    picked (`possible_picks()`) from any set of representatives in which y
    represents its division and X's division is represented by X or by a
    tied rival that X doesn't always rank ahead of and that doesn't always
    win the division when level.
  - wild card, *ahead of X* (the mirror): X can't be picked from any set of
    representatives in which y's division is represented by y or by a tied
    team of y's division that may rank ahead of y.
  - "survives every reduction" for the wild card: X wins every division tie
    with its tied rivals, or no other rival can end above X and X wins the
    tie that is left after any first pick (the division winner isn't in the
    pool).
  - "wins its division whenever level with X" also counts teams that win
    every division tie with the teams of their division that can be level
    (before: only teams no division rival could reach).

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
src/status.cpp            goal, gray zone search, GameSearch (budgeted
                          search over remaining games, `search = TRUE`)
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

### Results of milestone 6

Why we're later than the NFL, from the three cases that were two weeks late
(2013 SEA playoffs, 2015 SEA playoffs, 2023 NYJ eliminated). Method: biased
random completions in the NFL's week (`nfl_standings()`, the threatening
teams win or lose with probability 0.8) to find the tie that decides, then
counterfactuals: take one competitor out of the tie (lose out for a clinch,
win out for an elimination, so the true answer stays) and see whether the
proof goes through. No completion contradicted the NFL (40,000 per case).

Three gaps, all in `TieProof` (`src/tie_proof.cpp`):

1. **Division winners in wild card tie groups. Fixed.** 2015 after week 16:
   SEA (9-7 losing out) beats ATL on common games, but WAS can also end 9-7,
   and then wins the NFC East. The set `{SEA, ATL, WAS}` went through the
   conference procedure, WAS has the best conference record (.667 vs .500),
   so SEA wasn't "picked first" and ATL's side stayed unknown. Now
   `conference_sides(tied, wild_card = true)` (playoffs goal only) drops
   teams that win their division whenever level with X
   (`wins_division_when_level()`). The No. 1 seed keeps the old sets.
2. **"Ranks ahead" meant "picked first". Fixed.** 2013 after week 13: if SEA
   (11-1) loses out, SF and ARI can tie it at 11-5. SF wins the three-way
   division tie, then SEA always beats ARI, but the proof needed SEA picked
   first from `{SEA, SF, ARI}`. Now `division_sides()` uses
   `ranks_ahead()` / `ranks_behind()`: recursive over `possible_picks()`, a
   superset of the teams one pick of a procedure can pick (same step ranges
   as `wins()`; survivors of a step lie between the teams nobody can beat
   and the teams nobody beats for sure). `wins()` / `loses()` stay as
   shortcuts, so the new sides are never weaker than the old ones. Only the
   division ranking uses it; the conference procedure re-reduces divisions
   after each pick, which `possible_picks()` doesn't model.
3. **Position in a tie, not pairwise sides. Open.** The flows only take
   per-team sides (X always ahead of y, or always behind). Some ties need
   "X is never last of these teams":
   - 2015 after week 15: SEA, ATL and MIN can all end 9-7 for two wild
     cards. SEA always gets in (2,441 of 2,441 sampled three-way ties), but
     beats ATL in some completions and MIN in others, so neither side is
     known. Proven only with both WAS (gap 1) and MIN out of the tie. Since
     milestone 6 we're one week late (16 vs 15) instead of two.
   - 2023 after week 15, NYJ eliminated (NFL 15, ours 17 at the time): NYJ
     (8-9 winning out) can be level with up to ten AFC teams and beats HOU
     and DEN head-to-head, so they're not "always ahead". Any one of CIN,
     PIT, HOU, JAX, DEN or LV winning out proved it. **Solved in milestone
     7** by per-team sides, not a set constraint.

   Ideas, not decided: let the flows count "at least k of this set of tied
   teams are ahead of X" (a set constraint instead of per-team sides), or
   enumerate the tie groups at X's level that the flow's weak completion
   allows and prove X's position in each. Both need the maintainer.

Results (check 1, NFL flags): 6 of the 45 later cases now match the NFL's
week, 2015 SEA moved from two weeks to one. 39 later cases remain, all
undecided in the NFL's week (snapshot, 41 rows with BUF/CIN).

| fix | cases now matching the NFL |
|---|---|
| ranks ahead (gap 2) | 2009 PHI, 2011 PIT, 2012 BAL, 2013 SEA, 2023 BAL (playoffs) |
| wild card sets (gap 1) | 2012 SEA (playoffs); 2015 SEA 17 → 16 |

Checks 2–5 and the self-check (`verify = TRUE`) pass on every week of
2006–2025; full test suite passes. Benchmark unchanged (all weeks 0.58s).
`dev/check-cpp.sh` reports nothing in the changed code (its findings in
`types.hpp` and `procedures.hpp` predate this work).

Scripts used for the analysis (not in the repo): biased completions with
`nfl_standings(sims, playoff_seeds = seeds)` (teams out of the playoffs have
`conf_rank = NA`), and counterfactuals that fix a team's remaining results
and rerun `clinching_status_compute(g, status = "PLAYOFFS")`. Take a team out
of a tie in the direction that moves it *away* from X's level: making a 9-5
team lose out can put it exactly at X's 9-7.

### Results of milestone 7

From the analysis of the 39 remaining later cases (P10): fixing all of
one competitor's remaining games proves every one of them in the NFL's week,
so each case was one tie short. Biased random completions in the NFL's week
(X at its worst case) never contradicted the NFL. They sorted the cases into
SOV with games left (4 cases), X's position in the tie (gap 3, 30), and ties
where every team was always on one side but the proof didn't see it (5).

Fixes (all in `src/tie_proof.cpp`, no changes to `procedures.cpp`):

| fix | example |
|---|---|
| pairwise SOV/SOS | 2024 LA and SEA after week 17: both can end 10-7, LA's SOV is 0.009 to 0.062 ahead, but LA's lowest SOV (0.424) was below SEA's highest (0.441) |
| SOV with games left | 2015 NE after week 14 (division and playoffs): NYJ's own open games enumerated |
| per-team sides of another division | 2013 CAR after week 16: ARI beat CAR, but CAR beats SF and only one of them can represent the NFC West |
| re-reduction after every pick | 2006 DAL after week 15: NYG ranks ahead of DAL, takes a wild card, then DAL beats ATL |
| division winners when level | 2022 DAL after week 15: SF wins every NFC West tie with SEA |
| mirror for elimination | 2012 NYJ after week 15: CIN only ends 8-8 by losing to PIT, which then represents the AFC North and beats NYJ |

Many cases of gap 3 went away too: they needed exactly these per-team sides,
not a set constraint.

Results (check 1, NFL flags): 30 of the 39 later cases now match the NFL's
week, including 2023 NYJ eliminated (two weeks late before). 9 remain, all
one week late and undecided in the NFL's week (snapshot, 11 rows with
BUF/CIN): playoffs 2011 NO, 2015 SEA, 2023 PHI; eliminated 2008 DET, 2009
KC, 2012 TB, 2019 NYJ, 2021 CHI, 2023 CAR.

| goal | same | earlier | later |
|---|---|---|---|
| division | 159 | 1 | 0 |
| playoffs | 248 | 1 | 3 |
| eliminated | 382 | 0 | 6 |
| No. 1 seed | 40 | 0 | 0 |

Soundness: compared with milestone 6 on every week of 2006–2025, only
undecided statuses changed (37 to clinched or eliminated, 4 to alive). Each
of the 37 held in 5,000 completions with X at its worst case, random team
strengths and 6% tie games (185,000 in all, no violation; the same check
finds violations for each of 20 alive teams treated as clinched). Checks
2–5 and the self-check pass. One test per fix (`verify = TRUE`), all failing
before.

Benchmark (`data-raw/bench_clinching_status.R`, same machine and session;
the machine was slower than for milestone 5):

| case | milestone 6 | milestone 7 |
|---|---|---|
| every week of 2006–2025 | 1.11s | 1.41s |
| 2023 after week 15, all goals | 3.6ms | 4.1ms |
| 1,000 simulated seasons after week 12 | 1.05s | 1.93s |

Most of the extra time is `ranges()` and `possible_picks()` for the new
groups. Enumerating SOV results over all 3^6 combinations made the
simulations 15x slower; depth first with pruning and at most 4 games fixed
that. Caching the ranges or the pairwise bounds per group was slower than
recomputing them.

### Results of milestone 8

In [gap 3](#gap-3-xs-position-in-a-tie-handoff) under "Per-team part": the
new rules, the soundness bug (2021 DAL) and its fix, the open cases and
what each needs.

### Results of milestone 9

`GameSearch` (`src/status.cpp`, P12) runs for every team and goal the checks
leave open while games are left, once per open question ("can reach", "can
miss"). It fixes one remaining game at a time (home win, away win, tie) and
reruns the checks of that team on each part (`Season`, `ScenarioRunner` and
`TeamStatus` rebuilt with the fixed games played); a part answering yes
gives a completion of the whole season, the answer is no if every part says
no. The next game is the one with the most teams that can end level with X
(at X's best or worst case), then conference teams; X's own games last.
Parts run the gray zone with 2 completions instead of 32 (the search splits
further anyway): that cut the simulation benchmark by a quarter and settles
the same cases. Budget: 200 parts per question (`kSearchBudget`); interrupt
checks every 64 parts.

Budgets on the 10 open cases of milestone 8 (debug build): 200 and 1,000
settle 5, 20,000 settles 7 (2023 SF needs about 5,000), 100,000 still 7.
The other 3 (2012 TB, 2015 SEA, 2019 NYJ) and 2025 LA are the "X is never
last" kind (P11): splitting on games pins down who ends level too slowly.
A prototype in R (each part about 3 ms) found the same.

Results, compared with milestone 8 on every week of 2006–2025: 101
undecided statuses settled, 92 to alive (each with a completion) and 9 to
clinched or eliminated: 2008 DET, 2009 KC and 2023 CAR eliminated, 2023 and
2024 PHI clinched (all five now in the NFL's week), plus 2006 NYG, 2008 DET
and 2017 DEN division and 2023 NO No. 1 seed eliminated. Each of the 9 held
in 20,000 completions with X at its worst case, random team strengths (sd 3)
and 6% tie games. 2021 DAL after week 15 is now alive: the search finds the
completion of the soundness bug of milestone 8 (PHI takes the NFC East on
SOV, NO the NFC South, SF, LA and TB the wild cards). Check 1: 5 later cases
left (2012 TB, 2015 SEA, 2019 NYJ, 2023 SF, 2025 LA) plus BUF/CIN. Checks
2–5, the self-check and the full test suite pass.

The search is off by default (`search = FALSE`, decided by the maintainer,
2026-11-12); the function then hints at it if teams with games left are
undecided. The tests run the NFL comparison, the scenario check and the
random completions with `search = TRUE`, the tests of single proofs without
it (so the search can't hide a proof that got weaker). The "5 or fewer
games left" in the hint comes from every week of 2006–2025: share of
undecided statuses the search settled, by the team's games left:

| games left | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
|---|---|---|---|---|---|---|---|---|
| undecided without search | 8 | 20 | 34 | 41 | 28 | 28 | 17 | 2 |
| settled by the search | 100% | 70% | 56% | 63% | 61% | 43% | 29% | 0% |

The search only combines the checks, so it's exactly as sound as they are:
a wrong proof (like the one in milestone 8) gives a wrong answer at the
first part, before any split.

Benchmark (`data-raw/bench_clinching_status.R`, optimized build):

| case | milestone 8 | milestone 9 |
|---|---|---|
| every week of 2006–2025 | 1.53s | 2.28s |
| 2023 after week 15, all goals | 5.6ms | 19.2ms |
| 1,000 simulated seasons after week 12 | 2.26s | 11.1s |

Almost all of the time goes to questions the search can't settle within
the budget (weeks 10–13 with many open answers): every part rebuilds the
season and reruns the flows and tie proofs, about 15–50 µs. With a budget of
5,000, every week took 13.6s and 1,000 simulated seasons about 160s.

### Still to do

#### Gap 3: X's position in a tie (handoff)

**Problem.** The max-flow checks take tie results only as per-team sides
(`TieSides`: teams X always ranks ahead of, teams X always ranks behind).
Every other team that can end level with X counts as either way: tied but
ahead of X in the "can miss" check, tied but behind X in the "can reach"
check. Some proofs need a statement about the group instead: "of these tied
teams, X is never last" or "at least k of them are ahead of X". No
per-team side can express that.

**Per-team part (milestone 8, done 2026-11-12).** Some of the 9 cases left
by milestone 7 had teams that were always on one side in the samples, so
they fall under P10. Changes, all in `src/tie_proof.cpp`:

| rule | case |
|---|---|
| a step only counts where the earlier steps are equal | 2021 CHI eliminated: CAR is never behind CHI on common games and has the better SOV whenever they're equal there; the SOV comparison used to include the completions where common games already decided |
| opponents' tie levels bound records | a team in the tie group ends at its tie level too, which bounds what it can win or lose against another team of the group (`pct_range()`, and the enumeration below checks every group member). A group that can't all end level has no possible picks |
| a rival only represents X's division in the wild card pool if another rival can win the division | 2011 NO clinched: ATL (which NO can't always beat) only takes a wild card if NO wins the NFC South, so CHI only meets NO. Wild card sides may assume X isn't its division winner: the division path is a separate cap with the rival sides, and every forced set has a rival ahead of X. Also needed: DAL only ends 10-6 by beating NYG, so `{DAL, NYG}` can't be level and DAL wins its division whenever level |
| a team that can win its division while level with X isn't behind X in the wild card count, unless no other team of its division can be ahead of X at the same time (`level_division_winners()`) | soundness fix, see below |

**Soundness bug found and fixed.** The first version of these rules
clinched 2021 DAL after week 15, a week before the NFL. It was wrong
(pointed out by the maintainer with
[nflplayoffscenarios.com](https://nflplayoffscenarios.com/2021/week/16/DAL-has-not-clinched-a-playoff-berth/),
written after all week 15 games): DAL loses out to 10-7, PHI wins out and
takes the NFC East on SOV, NO wins out and the NFC South over TB on
head-to-head, SF wins out. Then ARI, LA and SF are above 10-7, TB beat DAL,
and DAL is 8th (302 of 20,000 completions with these teams fixed). The
proof had NO behind DAL (DAL beats NO, and the wild card rules of milestone
7 only look at the pool), so NO had to end strictly above 10-7. But NO wins
its division at 10-7 and is ahead of DAL as a division winner; one team per
division ahead of X is free in the count, but TB is ahead too. The flaw is
in the wild card rules of milestones 4 and 7 (`x_first`,
`never_picked_before_x()`); the representative rule above only exposed it
(before, PHI in `x_reps` blocked NO's side by accident). Fix: a team y of
another division keeps "behind X" only if a team of its division always
ends above X, or every other team z of its division that may be level is
behind X in the pool too, wins its division whenever level, or always ranks
ahead of y in their division (then y isn't the winner while z is level).

The fix costs three clinches of milestone 7 that the NFL confirms (now one
week later than the NFL, undecided in the NFL's week): 2023 SF after week
14, 2024 PHI after week 14, 2025 LA after week 15. In 2023, GB can win the
NFC North at 10-7 while MIN, which beat SF, is 10-7 in the pool, so GB's
side is mixed; SF still clinched, but only because GB counts ahead only as
the division winner. That is a statement about a group again (P11).

Random team strengths didn't find the DAL counterexample: it needs about
ten teams to win or lose out together. The self-check and the stress test
only find violations in completions that are likely enough to sample.

`lead()` enumerates the results of both teams' open games (at most 4, as
for SOV in milestone 7) for every step after the first, keeps those that
leave the group at its tie levels and the two teams equal at every earlier
record step (the procedure only reaches a step with the whole group if no
earlier step separated them), and compares the step's values (records
exactly, SOV and SOS with the linear bound). If no result is left, the step
is never reached with both, and either answer holds.

Results (check 1): 2011 NO and 2021 CHI match the NFL's week; 2023 SF,
2024 PHI and 2025 LA are now a week later (see the soundness fix). Compared
with milestone 7 on every week of 2006–2025: 1 undecided to clinched (2011
NO playoffs), 4 to eliminated (2021 CHI playoffs; 2023 NYJ division in weeks
13 and 14, 2023 CHI division in week 14), 3 clinched to undecided (above).
Each new answer held in 20,000 completions with X at its worst case, random
team strengths (sd 3) and 6% tie games (100,000 in all, no violation; the
same check finds violations for each of 8 alive teams treated as
eliminated). Checks 2–5, the self-check and the full test suite pass. One
test per rule (`verify = TRUE`), all failing before (2021 DAL after week 15
is undecided).

Benchmark (`data-raw/bench_clinching_status.R`, same session):

| case | milestone 7 | milestone 8 |
|---|---|---|
| every week of 2006–2025 | 1.41s | 1.53s |
| 2023 after week 15, all goals | 4.1ms | 5.6ms |
| 1,000 simulated seasons after week 12 | 1.94s | 2.26s |

The time goes to the enumeration in `lead()`. The first version was twice
as slow on the late state; skipping the first step (nothing to condition
on), fixed-size arrays and precomputed division and conference masks
brought it down.

**The open cases.** The NFL snapshot (`nfl_status_differences.csv`) is
computed with `search = TRUE`. After milestone 9 these are left, all one
week late and undecided in the NFL's week: 2012 TB eliminated, 2015 SEA
playoffs, 2019 NYJ eliminated, 2023 SF playoffs and 2025 LA playoffs. The
search settles 2008 DET, 2009 KC, 2023 CAR, 2023 PHI and 2024 PHI; with the
default `search = FALSE` they are still late. The table below is the
analysis of milestone 8 (before the search), still useful for what the
proofs alone lack; 2023 SF, 2024 PHI and 2025 LA came from the soundness
fix and need "counts ahead only as the division winner". To see what each needs, the sides
the samples show (`tie_groups()`, 4,000 completions with X at its worst
case) were forced into the flows with a temporary debug hook (never
committed): first only teams that were always on one side, then also the
mixed teams by their majority side (unsound, only to see whether set
reasoning would be enough).

| case | NFL week | one-sided teams enough | what it needs |
|---|---|---|---|
| 2008 DET eliminated | 11 | no (not even with the mixed ones) | the division path stays feasible: CHI and GB below 6-10, MIN level. DET only wins that tie on division record if MIN loses to CHI, which puts CHI at 6-10. Tie proofs would need the flow's caps (teams strictly behind X have few wins left) |
| 2009 KC eliminated | 13 | yes (JAX) | JAX ahead of KC in `{BAL, JAX, KC, NYJ}` at 7-9. KC could be picked there on SOV if NYJ's SOV were below KC's; never in 197 sampled groups. Needs SOV bounds in a four-team group |
| 2012 TB eliminated | 15 | no | set constraint (MIN, DAL, CHI, NYG mixed) |
| 2015 SEA playoffs | 15 | no | set constraint: SEA is never last of `{SEA, ATL, MIN}`, and ATL and MIN are both mixed |
| 2019 NYJ eliminated | 14 | no | set constraint (IND, LV, PIT, HOU, LAC, TEN mixed) |
| 2023 CAR eliminated | 13 | yes (MIN or SEA) | MIN ahead of CAR at 6-11: CHI only represents the NFC North ahead of MIN if it beats DET and GB, and then its conference record beats CAR's. Needs ranges conditioned on the division ranking. SEA: the same with LA |
| 2023 PHI playoffs | 15 | no | set constraint (LA, MIN, SEA mixed) |

So per-team rules can't settle more than 2009 KC and 2023 CAR, and each of
those needs a rule that fits one case. Four cases need set reasoning, and
2008 DET needs tie proofs that know the flow's caps.

**Where it lives in the code.**

- `src/status.cpp`: `GameSearch` (milestone 9) wraps everything below:
  it reruns `TeamStatus` with remaining games fixed, so every improvement
  of the proofs also makes the search settle more, and every unsound proof
  makes it wrong too.
- `src/status.cpp`: `decide()` takes `yes_strict` / `no_strict` masks;
  `decide_reach()` / `decide_miss()` build them from `tie_sides()`, which
  calls `TieProof::division_sides()` or `conference_sides()`.
  `reach_answer()` / `miss_answer()` run the cap and push flows; for the
  playoffs they go through `wild_card_sets()` (allowed sets A) and the
  forced sets B. A set constraint would have to enter here: per allowed or
  forced set, or as an extra condition on the flow's completion.
- `src/tie_proof.cpp`: `wins()`, `loses()`, `possible_picks()` (one pick of
  a procedure, proven by induction over subgroups), `ranks_ahead()` /
  `ranks_behind()` (division), `conference_sides()` and the wild card rules
  of milestone 7 (`never_picked_before_x()`, `x_never_picked_before()`,
  `survives_wild_card_reduction()`, `wins_division_when_level()`).
  Everything compares two teams through `lead()`, which enumerates the two
  teams' open games (`lowest_difference()`).

**Ideas for the set constraints (need a decision by the maintainer, P11).**

1. *Set constraints in the flows.* For a group G of teams that can tie X,
   prove "X is never behind more than m of G whenever they are level" and
   let the flow count G's members ahead of X as at most m instead of
   per team. The playoff count (`sum_d max(k_d - 1, 0) <= w - 1`) already
   works with counts, so this fits the allowed/forced set enumeration. The
   proof of "at most m" is an induction over the picks like `wins()`.
2. *Enumerate the tie groups at X's level.* For every set T of teams that
   can tie X, take T as exactly the teams level with X and prove which
   subsets of T can rank ahead of X (picks of the conference procedure
   until X is picked, as `possible_picks()` does). Then the flows don't
   need exact levels: "can miss" no needs, for every T, no forced set B
   with B ∩ T a possible ahead-set and `push(T ∪ B)` feasible (T weak, B
   outside T strict); "can reach" no likewise with `cap`. Teams outside T
   are free, which only relaxes, so it's sound. Only in the gray zone (weak
   flow feasible, no proof yet), with at most 2^|tied| sets (up to 11 tied
   teams in these cases). Partial enumeration, so check it against P5.
3. 2008 DET is different from both: it needs tie proofs that use the
   caps of the flow (a team that must stay strictly behind X has a bound on
   its remaining wins, which bounds its results against tied teams).

**Tools** (`dev/clinching-diagnostics.R`, run from the package root):

- `later_cases()`: the open cases of the snapshot.
- `single_team_fixes(case)`: fixes each competitor to win or lose out and
  lists the fixes that prove the case. Every one of the 39 cases of
  milestone 7 had at least one; it shows which teams the proof is short of.
- `tie_sides(case)`: who ends level with X and on which side (`$teams`,
  `$steps`, `$wrong` must be 0).
- `tie_groups(case)`: the tie groups at X's level, who in them is ranked
  ahead of X, and X's rank. Shows the groups behind a mixed side.
- `level_with(...)`: completions where given teams all end level with X
  (others fixed to win or lose out to reach rare groups), and how often X
  ranks ahead of team y. Use it to check a suspected per-team side.
- `show_completion(season, week, winners)`: one completion through the
  engine, to check a suspected counterexample. Both suspected
  counterexamples of milestone 7 (2006 DAL, 2022 DAL) turned out to be
  wrong, and they showed which rule was missing.
- To see what the proof derives, temporarily print from `TieProof` (e.g.
  `tied`, `x_ahead`, `x_behind` at the end of `conference_sides()`) to
  `stderr`, guarded by an environment variable with X's team id (ids are
  `standings_league()$team` order, 0-based). Never commit it (CRAN).
  Milestone 8 printed, in `decide()`, the masks and the first allowed or
  forced set whose flow stayed feasible, and in `x_never_picked_before()` /
  `never_picked_before_x()` the representative sets that failed. An
  environment variable with a team mask OR'd into the sides in
  `tie_sides()` (the oracle above) shows whether a side would be enough.

**Soundness checks before committing** (what milestones 7–9 did):

1. `devtools::test(filter = "clinching_status")`: the weekly tests run the
   self-check (`verify = TRUE`), scenarios and random completions.
2. Compare every week of 2006–2025 with the previous code (a `git
   worktree` of the last commit, `clinching_status_compute()` per week):
   only undecided statuses may change.
3. For every newly decided team-week, run thousands of completions with X
   at its worst case, random team strengths (so that varied groups end
   level) and about 6% tie games, through `nfl_standings(playoff_seeds =
   seeds)`, and count contradictions. Also run it on some alive teams
   treated as decided, to see that it finds violations.
4. `data-raw/bench_clinching_status.R` against the previous commit in the
   same session (the machine's speed varies a lot between sessions).
5. Don't trust checks 1–3 alone. In milestone 8 they all passed on a wrong
   clinch (2021 DAL after week 15): its counterexample needs about ten
   teams to win or lose out together, which random completions never hit.
   For every newly decided status, and every status earlier than the NFL,
   look for an independent answer: nflplayoffscenarios.com has a page per
   team and week since 2021 (e.g. `/2021/week/16/DAL-has-not-clinched-a-playoff-berth/`,
   written after the previous week's games), and the NFL flags in check 1.
   An "earlier than the NFL" case is a bug until shown otherwise; don't
   explain it away with timing.
6. If a status looks wrong, rebuild the scenario from the external
   explanation with `fix_team()` and `random_completions()` (fix the named
   teams, randomize the rest) and trace which side the proof used: print
   the masks in `decide()` and the failing sets (see the tools above).
   `search = TRUE` can also find the counterexample once the proof is
   fixed.

**Other gaps, not seen in the NFL comparison:**

- SOV of a tied team with more than 4 open games stays unknown
  (`kMaxEnumeratedGames`). Only matters early in the season.
- The pairwise SOV/SOS bound treats opponents' open games independently
  of the tie (a team of the group needing wins doesn't make its opponents
  lose). Sound, but loose.
- `possible_picks()` is a superset per step; the new wild card rules use
  it for groups without X, so a loose superset there costs proofs.

**Speed.** Milestone 7 made 1,000 simulated seasons 1.8x slower (1.05s →
1.93s). If that matters: skip the new wild card rules for teams whose side
the flows don't need, or compute sides lazily from the flows' failing sets.

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
6. Tighter tie proofs from the cases where we're later than the NFL (P8).
   Gaps 1 and 2 **done** (2026-11-11), gap 3 open, see
   [results of milestone 6](#results-of-milestone-6).
7. Pairwise SOV/SOS bounds and per-team wild card sides (P10). **Done**
   (2026-11-12), see [results of milestone 7](#results-of-milestone-7).
8. Gap 3, the per-team part (P10): conditioned steps, opponents' tie
   levels, wild card representatives. **Done** (2026-11-12), see
   [gap 3](#gap-3-xs-position-in-a-tie-handoff). The set constraints are
   open (P11).
9. Budgeted search for open answers (P12). **Done** (2026-11-12), see
   [results of milestone 9](#results-of-milestone-9).

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

**P5 Exhaustive search. Decided by the maintainer (2026-11-11): no.
Relaxed by P12 (a budgeted search, off by default).** The
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

**P8 Tie proofs closer to the NFL. Decided by the maintainer (2026-11-11):**
fix gaps 1 (division winners out of wild card tie groups) and 2 (rank ahead
through the picks, not only picked first) of
[milestone 6](#results-of-milestone-6), in the clinching code only (no
changes to the tiebreakers in `procedures.cpp`). Gap 3 is open and needs a
new decision.

**P9 Output as current standings. Decided by the maintainer
(2026-11-11):** `nfl_clinching_status()` returns `nfl_standings()` of the
played games with `games_left` and the status columns added at the end
(see [interface](#interface)). The standings arguments aren't exposed:
always `ranks = "CONF"`, `tiebreaker_depth = "SOS"` (as P2) and all 16
conference ranks. Sorted like `nfl_standings()`. Only teams that have
played a game get a row, as in `nfl_standings()` (zero rows before
kickoff). `verbosity` is passed on to `nfl_standings()`.

**P10 Pairwise strength and per-team sides. Decided by the maintainer
(2026-11-12):** from the analysis of the 39 later cases, do the SOV cases
(bound SOV and SOS beyond "unknown" with games left) and the cases where
every team was always on one side, in the clinching code only. Done as
[milestone 7](#results-of-milestone-7). Gap 3 (set constraints in the flows)
is still open.

**P11 Gap 3 set constraints. Open, needs the maintainer.** After
milestone 8, four of the open cases (2012 TB, 2015 SEA, 2019 NYJ, 2023 PHI)
need statements about a group of tied teams, not per-team sides, and so do
the three clinches the soundness fix of milestone 8 gave up (a team counts
ahead of X only as its division's winner). Two
designs in [gap 3](#gap-3-xs-position-in-a-tie-handoff): set constraints
in the flows (idea 1) or enumerating the tie groups at X's level (idea 2,
partial enumeration, see P5). Also open: whether to go after 2009 KC and
2023 CAR with rules that fit one case each, and 2008 DET (tie proofs that
use the flow's caps).

**P12 Budgeted search for undecided answers. Decided by the maintainer
(2026-11-12): yes.** Relaxes P5. When the proofs leave "can reach" or "can
miss" open and games are left, split on one remaining game at a time (home
win, away win, tie) and run the existing checks on each part, games of teams
that can end level with X first. A part answering yes gives a completion; the
answer is no if every part answers no. Within a fixed node budget per
question, then undecided as before. Implemented in C++ (`GameSearch` in
`src/status.cpp`). P11 (set constraints) stays open.
