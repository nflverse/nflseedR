# Playoff status (clinching and elimination)

`nfl_clinching_status()` says, for every team, whether it has clinched or
been eliminated from a division title, a playoff berth and the No. 1 seed,
given the games played so far. Exported, experimental. Built on the C++
standings engine, so read `dev/cpp-standings-engine.md` first.

This file describes the code as it is. The history of how it got there
(milestones 1–15, handoffs, benchmarks per milestone) was dropped on
2026-11-16; it is in git (`dev/playoff-status.md` before that date).

## Summary for agents

- **Done and safe, don't re-derive:** the flows, the tie proofs, the tie
  groups, the search and the tests below. Every clinched and eliminated of
  2006–2025 matches the NFL's first week, and every elimination of the 2026
  week 10 comparison with nflplayoffscenarios.com is found.
- **Never decide alone:** proof rules, budgets and anything in
  [decisions](#decisions). Ask the maintainer and add a P entry.
- **A change is safe if** every status is identical (or only undecided
  ones change) in `all_statuses()` of `dev/clinching-week10-2026.R`
  (every week of 2006–2025 with and without the search, and the 2,187 week
  10 results: 339 eliminated, 1,688 alive, 160 undecided),
  `devtools::test(filter = "clinching_status")` passes, and
  `dev/check-cpp.sh` reports nothing new. Use an optimized build
  (`pkgbuild::compile_dll(debug = FALSE)`); also compare a call with
  several seasons, which uses the small budgets.
- **Traps:**
  - Random completions don't find rare counterexamples (2021 DAL, see
    [lessons](#lessons)). A new no needs `verify = TRUE` and the group
    check.
  - Alive answers depend on which completion a flow returns. Changing the
    games or edges of a flow that returns a completion changes statuses
    (P20).
  - A team with an empty range at a step is ahead of and behind every other
    team (`leads()`): the step is never reached with it.
- **Open:** see [still to do](#still-to-do).

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
- `search`: run the budgeted search for open answers (P12).
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

Pruning keeps this small: about one flow in five of a run is one of these.

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

**Order of work** for one team, goal and question ("can reach", "can
miss"), in `TeamStatus` (`src/status.cpp`):

1. **Flows with sides.** The [tie proofs](#tie-proofs) say which teams X
   always ranks ahead of or behind when tied. Yes if the flow is feasible
   with only the teams X ranks ahead of allowed to tie; no if it is
   infeasible even with only the teams X ranks behind held strictly. A yes
   keeps its flow's completion as the scenario, without running the engine.
2. **Gray zone** (neither): per allowed or forced set that is weak
   feasible, make the teams that can tie X strict one at a time while the
   flow stays feasible, in up to 4 orders (rotations of the team list, no
   randomness). The games no check constrains go to the home team, then to
   the away team. Each completion runs through the procedures for every
   coin toss sequence (at most 64 runs), at most 32 completions per
   question. A completion that shows the answer proves yes.
3. **[Tie groups](#tie-groups)** at X's level: prove no, or find a yes.
4. **[Search](#search)**, only with `search = TRUE`.
5. Still open: undecided.

The goals are nested (No. 1 seed ⇒ division ⇒ playoffs), so an answer one
goal's checks leave open is taken from another goal where that is sound
(`final_proof()`), e.g. can't miss the division ⇒ can't miss the playoffs.
Unrequested goals are only computed for this.

### Tie proofs

`TieProof` (`src/tie_proof.cpp`) works per check,
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

**Pairwise comparisons.** Every use of the ranges compares two teams
(`leads(a, b)`: each ahead for sure, never behind, or neither). For SOV and
SOS it also bounds the difference of the two values directly
(`lowest_differences()`, `linear_strength_difference()`), because separate ranges count a shared
opponent low for one team and high for the other at the same time. With the
weights fixed (games per opponent for SOS, wins per opponent for SOV), the
difference is linear in the opponents' half-wins: tied teams end at their
tie level, every open game goes the way that lowers the difference most.
For SOV the results of the two teams' own open games are enumerated (depth
first, only results that leave both at their tie level, at most 4 open
games); with more, SOV stays unknown. All of this is a relaxation of the
real completions, so it's sound, and never weaker than the ranges.

**One pick.** `possible_picks()` gives the teams one pick can pick (a
superset). A step drops every team some other team beats for sure and keeps
every team no team can beat; the procedure restarts with every set of
survivors in between (induction, cached per group). A team that is never
behind a survivor has the best value too, so it survives with it (P19). If
every step can leave the whole group level, a coin toss can pick any of
them. X is picked first (`wins()`) if it is the only possible pick, and
never (`loses()`) if it isn't one. Until P19, `wins()` and `loses()` were
inductions of their own.

**Sides.** Of the teams that can tie X in the check:

- a rival is *behind X* if X ranks ahead of it in the division ranking of
  every division group with it, *ahead of X* if X always ranks behind it.
  "Ranks ahead" follows the picks: from the teams one pick can
  pick, X must be one, or every other one must leave a group where X still
  ranks ahead. So in a three-way tie X can lose the first pick to a third
  team and still be ahead of the rival.
- the teams of another division are all *behind X* if X wins every division
  tie (X survives every reduction) and X is picked first in the conference
  procedure from every set of one team per other division that includes one
  of them; *ahead of X* if X is never picked from such a set. For the
  playoffs, teams that win their division whenever they are
  level with X (no other team of their division can reach X's win
  percentage) are left out of these sets: division winners are ranked apart
  from the wild cards, and one team of a division ahead of X is free in the
  playoff count, so their own side doesn't matter either.
- The sides of teams of another division are refined per team instead of
  per division, and for the wild card in four ways (each one is
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

### Tie groups

For the questions the flows and the gray zone leave open, all three goals
(`TeamStatus::tie_groups()`, P11).

- **Enumeration.** Every conference team (the rivals for the division
  title) that can end on more than one side of X is set to level, above or
  below, depth first. `Check::exact()` (a flow with lower and upper bounds
  on every team's half-wins) drops what can't happen. A branch ends early
  where the tiebreakers don't matter (`keeps_x_from()`): X reaches the goal
  even if all teams that can still end level or above are ahead, or misses
  it by the teams above alone.
- **Outcome of a group** (`TieProof::playoff_outcome()`,
  `division_outcome()`, `first_seed_outcome()`): with the teams above and
  level known, follow the procedures with `possible_picks()` at every pick.
  Playoffs: each division without a team above X gives its title to a
  possible first pick of its level teams, the others go through the wild
  card picks (the possible best team of each division, then one pick of the
  conference procedure) until X is picked or the wild cards are gone. Every
  pick is a superset, so "X reaches it in every branch" and "in none" are
  proven. This covers "X is never last of these teams" and "a team counts
  ahead only as its division's winner".
- **Answer.** No if no group can give the answer. For a group that may give
  it, the completion of its flow runs through the procedures: yes if it
  shows the answer, otherwise the group stays open.
- **Three proofs per group**, the cheap one first:
  1. Without a context: only the group's teams are known to be level.
     Cached across the groups of a question.
  2. With a **context** (`TieProof::set_context()`, P13): every team's side
     bounds what it takes from its open games, as **pair bounds** (P15):
     for every pair with open games, the half-wins one takes from the
     other, tightened to a fixed point (`propagate()`, `tighten()`). A
     second `TieProof` per check; its caches are lost with every group.
  3. With **facts** (`carry_facts()`, P15) for at most
     `Budgets::fact_groups` groups, and with the **joint check** (P17, P18)
     if at most `Budgets::joint_groups` groups are still open. See below.
- The flow that only has to answer leaves out games of a bounded team
  against a team without bounds (they only lower what the other games must
  bring). The completion comes from the full flow, only when a group needs
  one. Don't change which games are in a flow that returns a completion:
  some alive answers depend on the completion (P20).

**What the proofs with a context use** (all in `src/tie_proof.cpp`):

- **Pooled head-to-head** (`beaten_head_to_head()`): the games among the
  teams of a group hand out a fixed number of half-wins, so a team is
  beaten for sure if the others can't hold what it leaves at its best.
- **Steps a group passed** (`Passage`, `settle()`): the procedure only
  reaches a step with the whole group if no earlier step separated its
  teams, so the group is level at every earlier record step
  (`level_records()`: head-to-head at .500 each, the sweep with nobody
  winning or losing all games, other records within the percentages all can
  reach). Each record tightens the pair bounds again. One `Passage` per
  group and pick carries the bounds from step to step. Nothing left: the
  step is never reached with the whole group, and no team survives it.
- **Facts** (`Fact`, `reduction_facts()`): a step that drops teams restarts
  with the survivors, which take along what the larger group said: level at
  the earlier steps, the survivors level at this step, the dropped teams
  below. Not cached, so it is budgeted.
- **How a division was won** (`PickPath`, `first_seed_paths()`): the pick
  among the division winners takes along the facts of every way to every
  winner, at most `kMaxWinnerPaths` (256) combinations. Only for the No. 1
  seed; the wild card picks don't carry facts.
- **Joint check** (`jointly_feasible()`, P17): the pair bounds look at one
  team at a time. A flow checks whether all records on the way to a pick
  can hold at once. Costly, so only for the last open groups; there the
  ways to the winners may be up to `kMaxJointWinnerPaths` (65,536), and a
  record step that drops teams gives one case per record the survivors can
  share instead of a range per team (`reduction_cases()`, P18).

### Search

`GameSearch` (`src/status.cpp`, P12, `search = TRUE`): for a question that
is still open while games are left, fix one remaining game at a time (home
win, away win, tie) and rerun the checks on each part, with small budgets.
A part answering yes gives a completion of the whole season; the answer is
no if every part says no. The next game is the one with the most teams that
can end level with X, then conference teams, X's own games last. 200 parts
per question (`kSearchBudget`), then undecided. It only combines the checks,
so it is exactly as sound as they are. Every part rebuilds `SeasonData` and
its checks (P21).

### Budgets

`Budgets` in `src/status.cpp`, per team and question.

| | single season (P14) | more seasons | parts of the search |
|---|---|---|---|
| completions in the gray zone | 32 | 32 | 2 |
| flows for the tie groups | 20,000 | 200 (2,000 with 4 or fewer games left) | 30 |
| groups that may stay open | no cap | 16 | 2 |
| groups with facts (P15) | 128 | 4 | 2 |
| groups with the joint check (P17, P18) | 512 | 0 | 0 |

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
src/engine/team_mask.hpp  sets of teams as bits, and the half-wins that leave
                          a team level with X
src/engine/checks.hpp     Season and Check: the flows on the win totals when
                          X wins out or loses out (internal, shared by
                          status.cpp and group_check.cpp)
src/engine/status.hpp     compute_clinching_status(): season loop, checks per
src/status.cpp            goal, gray zone search, tie groups at X's level,
                          GameSearch (budgeted search over remaining games,
                          `search = TRUE`)
src/engine/group_check.hpp  group check (`group_samples`, tests and
src/group_check.cpp       diagnostics)
src/engine/scenario.hpp   ScenarioRunner: a completion through the procedures
src/scenario.cpp          for every coin toss sequence
src/engine/tie_proof.hpp  TieProof: which tied teams X always ranks ahead of
src/tie_proof.cpp         or behind, what X gets in a given tie group, the
                          propagation of what the steps say, and a flow that
                          checks the records on the way to a pick jointly
src/bindings.cpp          clinching_status_cpp()
```

- Same conventions as the engine: pure C++ outside `bindings.cpp`,
  `EngineError`, interrupt checks between seasons. Only the group check
  draws random numbers (`RandomSource`, reproducible with `set.seed()`);
  without it R's RNG isn't touched.
- Coin tosses: the procedures are unchanged. `ScenarioRunner`
  (`src/engine/scenario.hpp`) passes a scripted `RandomSource` that records
  every toss and forces given results, and tries the other results of every
  toss depth-first. No R RNG is used.
- Seeds per season from the league formats table
  (`clinching_status_seeds()`).
- `check_league()`, `group_games()` and `GamesBySeason` (`engine.hpp`) are
  shared with the standings engine.
- The C++ side always keeps the proving scenario per team and goal; R only
  decodes it with `scenarios = TRUE` (P6) and in tests.

## Testing

No brute force (it only works with a couple of games left). Five checks
instead, all on every week of 2006–2025 (`games_2006_2025.rds`) with the
results after that week set to `NA`, in
`tests/testthat/test-clinching_status.R`. The NFL comparison, the scenario
check and the random completions run with `search = TRUE`; the tests of
single proofs without it, so the search can't hide a proof that got weaker.

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
- **We're later:** only allowed if we're undecided in the NFL's week.
  Otherwise it's a bug in our code or a wrong NFL flag. Also in the snapshot.
- **Contradiction:** we say alive while the NFL says clinched or eliminated.
  Our alive comes with a concrete scenario, so it can be checked by hand.

Known special case: the 2022 BUF-CIN game (week 17) is not in
`games_2006_2025.rds` (P3). So in the week 16 state it's missing, although
at that time it was still a remaining game. Expect differences for AFC teams
around 2022 weeks 16–17. (BUF's division and CIN's playoff berth are clinched
in week 15, a week before the NFL.)

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

### Self-check and group check

Both only run in tests and diagnostics (`clinching_status_compute()`).

- **Self-check** (`verify = TRUE`): every no that needed tie proofs runs
  the completion of its flow or tie group through the procedures for every
  coin toss sequence and throws if X gets the other outcome.
- **Group check** (`group_samples = n`, `src/group_check.cpp`): an
  independent check of every no without the tie proofs. It walks the tie
  groups, and the flow's completion and `n` random completions of every
  group go through the procedures. Every other completion is steered
  against the answer through SOV, then SOS, of one level team after the
  other. It checks X at its worst case only, and passing proves nothing,
  but a group that many teams must win or lose out for is no longer out of
  reach. The attribute `"group_check"` counts the checked answers and those
  where its 20,000 flows ran out. `dev/clinching-diagnostics.R` has
  `group_check(season, week, samples)`.

## Results

Every week of 2006–2025 (345 states, 11,040 team-weeks), 2026-11-16:

- **NFL flags** (check 1, first week per team, season and goal), with and
  without the search: division 159 the same week, playoffs 251, eliminated
  388, No. 1 seed 40. Never later than the NFL. Earlier only 2022 BUF and
  CIN (the missing BUF-CIN game, P3); these are the two rows of the
  snapshot `nfl_status_differences.csv`.
- **Undecided team-weeks** (division / playoffs / No. 1 seed): 10 / 13 /
  26 without the search, 0 / 2 / 9 with it. On top of `search = FALSE`, the
  search decides one team-week (2015 PIT, No. 1 seed, after week 14); the
  rest it settles are alive. It settles most open answers of teams with 5
  or fewer games left, which is what the hint of `search = FALSE` says.
- **Checks 2–5, the self-check and the group check** (5,261 answers with 12
  completions per group, 3 incomplete, no error) hold. The three incomplete
  ones are early eliminations from the No. 1 seed race.
- **Week 10 of 2026** (LAC, No. 1 seed, all 3^7 results of the 7 games the
  50 scenarios of nflplayoffscenarios.com cover; `week10_compare()` in
  `dev/clinching-week10-2026.R`): all 339 results the site has eliminated
  are eliminated and nothing else is. Of the 1,848 others, 1,688 are alive
  and 160 undecided.
- **Time** (optimized build, Apple Silicon, one call per week): every week
  of 2006–2025 about 3.5s, 3.8s with the search. All 20 seasons after week
  12 in one call: 0.2s with the search. `data-raw/bench_clinching_status.R`
  has the benchmark.
- `devtools::check()` and `dev/sanitize-cpp.sh` are clean (2026-11-16).

### Lessons

- **2021 DAL.** A first version of the wild card rules clinched DAL after
  week 15, wrongly: PHI can win the NFC East on SOV while NO wins the NFC
  South at DAL's level, and a division winner level with X is ahead of X.
  Fix: `level_division_winners()`. Random completions with any team
  strengths never showed it (it needs about ten teams to win or lose out
  together), which is why the group check steers its completions.
- **Diagnosing an open result.** Fix one competitor's remaining games
  (`single_team_fixes()` in `dev/clinching-diagnostics.R`) to see which tie
  the proof is short of. For a pick that should be impossible: dump the
  facts of that pick and put them into an integer program (one variable per
  pair with open games, the totals and the facts as rows). Infeasible: the
  pair bounds miss something. Feasible: turn a solution into results, run
  them through `nfl_standings()` and see which step the proof gets wrong.
- **Tried and dropped:** caching ranges or pairwise bounds per group
  (slower than recomputing); the context on every group (10 times slower);
  enumerating more than 4 open games for SOV (P16); the smaller flow for
  completions and one flow builder (P20); a local search for
  counterexamples by flipping one game at a time (doesn't find 2021 DAL).

## Still to do

1. **Independent verification.** The division and No. 1 seed eliminations
   have no outside source. Comparing with nflplayoffscenarios.com (2021 on)
   is manual; a fixture built from it needs the maintainer (external site).
2. **What's still undecided.** A group stays open when `possible_picks()`
   can't order the level teams (SOV or SOS with games left) and the flow's
   one completion doesn't show the answer. Ideas, all needing the
   maintainer: more completions per group (`sample()` of the group check
   builds them), which is what the 160 week 10 results need; a prune for
   teams X always ranks behind in the No. 1 seed groups; facts in the wild
   card picks (no known case needs it).
3. **Known loose ends in the proofs.** SOV of a tied team with more than 4
   open games is unknown (`kMaxEnumeratedGames`). The pairwise SOV/SOS
   bound treats opponents' open games independently of the tie. Both are
   sound, only loose.
4. **`dev/clinching-week10-2026.R`** holds scenarios copied from an
   external site and is temporary. The maintainer wants to keep closing
   gaps to the site, so ask before removing it.
5. **Before a release.** The R-hub sanitizer builds
   (`rhub::rhub_check(platforms = c("clang-asan", "clang-ubsan"))`, see
   `dev/cpp-standings-engine.md`) need a push, so the maintainer starts
   them. They last passed on commit `42c659d` (2026-11-15).

## Decisions

All decided by the maintainer. P16, P20 and P21 are decisions not to do
something.

- **P1 Playoff goal** (2026-11-11): a berth by any route, including a
  division title (`conf_rank <= seeds`).
- **P2 Coin tosses and points** (2026-11-11): never used to clinch or
  eliminate; both outcomes count as possible. Tiebreakers run through SOS,
  so there is no `tiebreaker_depth` argument.
- **P3 Cancelled games** (2026-11-11): removed from `games` before the
  call. Teams are compared by win percentage, so game counts may differ.
- **P4 Name, goals and states** (2026-11-11): `nfl_clinching_status()`,
  `status = c("DIV", "PLAYOFFS", "1SEED")`, clinched, alive, eliminated,
  undecided.
- **P5 No exhaustive search** (2026-11-11), relaxed by P11 and P12.
- **P6 Scenarios** (2026-11-11): `scenarios = FALSE`; with `TRUE` the
  attribute `"scenarios"` holds one row per scenario and remaining game, in
  the format of `games`, with `scenario`, `season`, `team`, `goal`,
  `proves` and `coin_toss`.
- **P7 Column names** (2026-11-11): `div_status`, `playoffs_status`,
  `seed1_status`.
- **P8, P10 Tie proofs closer to the NFL** (2026-11-11, 2026-11-12):
  division winners out of wild card ties, rank ahead through the picks,
  pairwise SOV/SOS bounds, per-team wild card sides. In the clinching code
  only, never in `procedures.cpp`.
- **P9 Output** (2026-11-11): `nfl_standings()` of the played games
  (`ranks = "CONF"`, `tiebreaker_depth = "SOS"`) with `games_left` and the
  status columns added. Only teams that have played a game.
- **P11 Tie groups** (2026-11-12): enumerate the tie groups at X's level.
  No rules that fit one case each.
- **P12 Search** (2026-11-12): a budgeted search over remaining games, off
  by default because it makes calls with many open answers slower.
- **P13 Context and pooled head-to-head** (2026-11-13).
- **P14 Budgets for a single season** (2026-11-13): 20,000 flows and no cap
  on open groups only with one season in the input; the maintainer only
  runs single seasons, and many seasons would take too long. No argument.
- **P15 Propagation** (2026-11-15): pair bounds, steps passed, facts, and
  128 / 4 / 2 groups with facts.
- **P16 SOV with many open games** (2026-11-15): tried and reverted. Three
  more statuses of 2006–2025 decided, a third slower.
- **P17 Joint check and more winner paths** (2026-11-15).
- **P18 Split by the survivors' record** (2026-11-16): kept, to close as
  many gaps to nflplayoffscenarios.com as possible, with 512 groups and
  65,536 ways.
- **P19 One induction for a pick** (2026-11-16): `possible_picks()` only
  follows survivor sets that hold every team never behind one of them;
  `wins()` and `loses()` ask it. No status changes. Without the rule,
  5 statuses are lost.
- **P20 One flow builder** (2026-11-16): no. `cap()`, `push()` and
  `exact()` differ in which games are in the flow and where spare wins go,
  which decides the completion. Unifying them turns about 4 statuses of
  2006–2025 from alive to undecided.
- **P21 Two larger changes** (2026-11-16): no to always building pair
  bounds (changes what the proofs show, costs time where they are cheap)
  and to incremental updates in `GameSearch` (7% of the time, would make
  `SeasonData` mutable).
