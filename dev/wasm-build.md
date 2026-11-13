# WebAssembly build of the clinching engine

`compute_clinching_status()` (`src/engine/status.hpp`) built with Emscripten
for static web apps such as the "What if?" app (nflseedR-what-if), which
used to run nflseedR through webR/shinylive. Consumer docs, download URLs
and the JS API are in `wasm/README.md`.

## Layout

Everything lives in `wasm/`, which is in `.Rbuildignore`; R never compiles
or ships any of it.

| path | what |
|---|---|
| `wasm/build.sh` | `emcc` on `src/*.cpp` except `bindings.cpp` and `cpp11.cpp`, plus the glue |
| `wasm/src/clinching.cpp` | C interface around the engine, the counterpart of `clinching_status_cpp()` in `src/bindings.cpp` |
| `wasm/js/nflseedr-clinching.js` | JS wrapper: `createClinchingEngine()`, `clinchingStatus()` |
| `wasm/fixtures/make-fixtures.R` | writes input games and expected output of `clinching_status_compute()` as JSON |
| `wasm/test/clinching.test.mjs` | Node test: the wasm build must match the fixtures exactly |
| `.github/workflows/wasm.yaml` | build, test, publish |

## Rules

- The engine isn't changed for the wasm build. If something doesn't compile
  with Emscripten, ask the maintainer.
- Build flags: `-std=c++20 -O3 -fexceptions`, `MODULARIZE`, `EXPORT_ES6`,
  `ALLOW_MEMORY_GROWTH`, `STACK_SIZE=8MB`. The emsdk version is pinned in
  `wasm.yaml` (`EMSDK_VERSION`).
- No interrupts (the `InterruptCheck` is a no-op), no random numbers
  (`rng = nullptr`, `group_samples = 0`) and `verify = false`.
- The glue catches every exception and returns its message; the wrapper
  throws it as a JS `Error`.
- The wrapper ports the R code around the engine. Every ported piece names
  the R function it mirrors: `standings_league_build()`,
  `standings_validate_games()`, `standings_game_type()`,
  `standings_encode_games()`, `clinching_status_seeds()`,
  `clinching_status_labels()`, `clinching_status_compute()` and
  `clinching_status_scenarios()`. When one of them changes, change the
  wrapper too; the fixtures catch most differences.
- If the interface of `compute_clinching_status()` changes (options, output
  columns), update `wasm/src/clinching.cpp` and `COLUMNS` in the wrapper.

## Stack size

`compute_clinching_status()` keeps large arrays on the stack
(`TeamArray<Proof>` and similar in `src/status.cpp`). All fixtures overflow
a 128 KB stack and pass with 256 KB. Emscripten's default of 64 KB aborts
with "memory access out of bounds", so `build.sh` sets 8 MB, the native
main-thread stack R runs on.

## Fixtures

`make-fixtures.R` uses `tests/testthat/games_2006_2025.rds` (no network) and
writes to `wasm/fixtures/json` (git-ignored, always written fresh from the
current R code in CI):

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
