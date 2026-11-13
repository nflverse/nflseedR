# WebAssembly build of the clinching engine

`compute_clinching_status()` (`src/engine/status.hpp`) built with Emscripten,
together with `compute_standings()` (`src/engine/engine.hpp`) for the ranks
`nfl_clinching_status()` adds, for static web apps such as the "What if?" app (nflseedR-what-if), which
used to run nflseedR through webR/shinylive. Consumer docs, download URLs
and the JS API are in `wasm/README.md`.

## Layout

Everything lives in `wasm/`, which is in `.Rbuildignore`; R never compiles
or ships any of it.

| path | what |
|---|---|
| `wasm/build.sh` | `emcc` on `src/*.cpp` except `bindings.cpp` and `cpp11.cpp`, plus the glue |
| `wasm/src/clinching.cpp` | C interface around the engine, the counterpart of `clinching_status_cpp()` and `standings_cpp()` in `src/bindings.cpp` |
| `wasm/js/nflseedr-clinching.js` | JS wrapper: `createClinchingEngine()`, `clinchingStatus()` |
| `wasm/fixtures/make-fixtures.R` | writes input games and expected output of `clinching_status_compute()` plus the ranks of `nfl_clinching_status()` as JSON |
| `wasm/test/clinching.test.mjs` | Node test: the wasm build must match the fixtures exactly |
| `.github/workflows/wasm.yaml` | build, test, publish |

## Rules

- The engine isn't changed for the wasm build. If something doesn't compile
  with Emscripten, ask the maintainer.
- Build flags: `-std=c++20 -O3 -fexceptions`, `MODULARIZE`, `EXPORT_ES6`,
  `ALLOW_MEMORY_GROWTH`, `STACK_SIZE=8MB`. The emsdk version is pinned in
  `wasm.yaml` (`EMSDK_VERSION`).
- No interrupts (the `InterruptCheck` is a no-op), no random numbers in the
  status (`rng = nullptr`, `group_samples = 0`) and `verify = false`.
- Standings: `compute_standings()` on the played games with the options of
  `clinching_status_standings()`: `Ranks::kConference`, depth 2 (`"SOS"`),
  16 playoff seeds (`playoff_seeds = NULL`), and a season is complete when
  no status row of it has games left. Coin tosses come from
  `SeededRandom` (`std::mt19937`, a `RandomSource`) with the `seed` the
  wrapper passes (random by default), so they never match R's.
- The glue catches every exception and returns its message; the wrapper
  throws it as a JS `Error`.
- The wrapper ports the R code around the engine. Every ported piece names
  the R function it mirrors: `standings_league_build()`,
  `standings_validate_games()`, `standings_game_type()`,
  `standings_encode_games()`, `standings_labels()`,
  `clinching_status_seeds()`, `clinching_status_labels()`,
  `clinching_status_compute()`, `clinching_status_standings()` and
  `clinching_status_scenarios()`. When one of them changes, change the
  wrapper too; the fixtures catch most differences.
- If the interface of `compute_clinching_status()` or `compute_standings()`
  changes (options, output columns), or the step ids of the labels
  (`src/engine/types.hpp`), update `wasm/src/clinching.cpp` and `COLUMNS` or
  `LABEL_STEPS` in the wrapper.

## Output columns

`clinchingStatus()` returns the columns of `clinching_status_compute()`
(`season`/`sim`, `team`, `conf`, `division`, `wins`, `losses`, `ties`,
`games_left`, the requested status columns) followed by four columns of the
standings, joined by season and team as in `clinching_status_standings()`:

| column | engine field | R column of `nfl_clinching_status()` |
|---|---|---|
| `div_rank` | `StandingsRows::div_rank` | `div_rank` |
| `div_tie_broken_by` | `div_label`, decoded by `standingsLabel(code, "div")` | `div_tie_broken_by` |
| `conf_rank` | `conf_rank` | `conf_rank` |
| `conf_tie_broken_by` | `conf_label`, decoded by `standingsLabel(code, "conf")` | `conf_tie_broken_by` |

`kMissing` becomes `null`. Rows are sorted by season, division, `div_rank`
and team like `nfl_clinching_status()`. Differences to R: `wins` counts only
wins (R's has half wins for ties), and teams without a played game keep
their row with `null` ranks (R drops them), last in their division.

## Stack size

`compute_clinching_status()` keeps large arrays on the stack
(`TeamArray<Proof>` and similar in `src/status.cpp`). All fixtures overflow
a 128 KB stack and pass with 256 KB. Emscripten's default of 64 KB aborts
with "memory access out of bounds", so `build.sh` sets 8 MB, the native
main-thread stack R runs on.

## Fixtures

`make-fixtures.R` uses `tests/testthat/games_2006_2025.rds` (no network) and
writes to `wasm/fixtures/json` (git-ignored, always written fresh from the
current R code in CI). The expected status rows are those of
`clinching_status_compute()` with the rank columns of
`nfl_clinching_status()`, in its row order. It stops if a coin toss decides
a rank, because coin tosses are random in both. The Node test also checks
`standingsLabel()` for the steps the fixtures miss, `null` ranks without
played games and the `seed`.

| fixture | covers |
|---|---|
| `2024-week-15` | current format |
| `2022-week-14-ties` | ties (weeks 1 and 13) |
| `2016-week-14-6-seeds` | 6 seeds, ties |
| `2023-week-15` | `"undecided"` statuses (3) |
| `2023-week-15-search` | `search = TRUE` (decides 2 of the 3) |
| `2021-week-17-scenarios` | `scenarios = TRUE` |
| `2018-2019-two-seasons` | two seasons, a complete one with playoff games, two goals |

## Publishing

On pushes to `master`, `wasm.yaml` uploads `clinching.wasm`,
`clinching.js`, `nflseedr-clinching.js` and `manifest.json` (nflseedR
version, git SHA, emsdk version) to the prerelease `wasm-latest`, replacing
the assets and moving the tag to the commit. Stable URLs:
`https://github.com/nflverse/nflseedR/releases/download/wasm-latest/<file>`.
Pull requests and manual runs only attach the files to the run as the
artifact `nflseedr-clinching-wasm`.

## Status

First build 2026-11-13 with emsdk 4.0.10: `clinching.wasm` 158 KiB,
`clinching.js` 16 KiB. Runtime in Node 24 on an Apple silicon Mac: 2.4 ms for
2024 after week 15 (68 ms for 2023 after week 15 with `search = TRUE`). All
fixtures identical to R. No engine changes were needed; only the stack size.

2026-11-13: ranks and tiebreakers of the standings added. `clinching.wasm`
195 KiB; runtime for 2024 after week 15 unchanged (2.5 ms). All fixtures
identical to `nfl_clinching_status()`.
