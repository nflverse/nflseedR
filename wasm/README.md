# Clinching engine as WebAssembly

The C++ engine behind `nfl_clinching_status()` built with Emscripten, so a
static web app can compute clinching and elimination status in the browser
without R. Not part of the R package (`.Rbuildignore`).

## Download

Every push to `master` that touches the engine rebuilds it and replaces the
assets of the prerelease
[`wasm-latest`](https://github.com/nflverse/nflseedR/releases/tag/wasm-latest).
The URLs stay the same:

| file | URL |
|---|---|
| `clinching.wasm` (engine) | <https://github.com/nflverse/nflseedR/releases/download/wasm-latest/clinching.wasm> |
| `clinching.js` (Emscripten glue) | <https://github.com/nflverse/nflseedR/releases/download/wasm-latest/clinching.js> |
| `nflseedr-clinching.js` (wrapper) | <https://github.com/nflverse/nflseedR/releases/download/wasm-latest/nflseedr-clinching.js> |
| `manifest.json` | <https://github.com/nflverse/nflseedR/releases/download/wasm-latest/manifest.json> |

`manifest.json` has the nflseedR version (from `DESCRIPTION`), the git SHA
and the emsdk version of the build. Every workflow run (including pull
requests) also attaches the files as the artifact `nflseedr-clinching-wasm`.

Download the files when you build or deploy your app and serve them from
your own site, all three in the same folder:

```sh
base=https://github.com/nflverse/nflseedR/releases/download/wasm-latest
for f in clinching.wasm clinching.js nflseedr-clinching.js manifest.json; do
  curl -fsSL -o "public/nflseedr/$f" "$base/$f"
done
```

Don't load them from github.com at runtime: release downloads are
redirects that aren't meant for browsers fetching them cross-origin, and the
rolling assets change without notice. Keep `manifest.json` to know which
version you shipped.

## Use

```js
import { createClinchingEngine } from "./nflseedr/nflseedr-clinching.js";

const engine = await createClinchingEngine();
const { status, scenarios } = engine.clinchingStatus({
  // Like nflreadr::load_schedules(): played games have a `result`
  // (home - away), regular season games with `result: null` are remaining
  games: [{ season: 2024, game_type: "REG", week: 16, away_team: "KC",
            home_team: "HOU", result: null }, /* ... */],
  status: ["DIV", "PLAYOFFS", "1SEED"], // default: all three
  scenarios: false,                     // default
  search: false,                        // default
  // teams: [{ team, conf, division }]  // default: the 32 current teams
});
```

`status` has one row per season and team, sorted by season, division and
team, like the internal `clinching_status_compute()` (the table
`nfl_clinching_status()` adds to the standings):

```js
{ season: 2024, team: "BUF", conf: "AFC", division: "AFC East",
  wins: 11, losses: 3, ties: 0, games_left: 3,
  div_status: "clinched", playoffs_status: "clinched", seed1_status: "alive" }
```

Each status is `"clinched"`, `"alive"`, `"eliminated"` or `"undecided"`;
only requested goals get a column. With `scenarios: true`, `scenarios` holds
the rows of the `"scenarios"` attribute in R (one row per scenario and
remaining game, with the results that prove the answer), otherwise it is
`null`. Use `sim` instead of `season` for simulated seasons (always 7 seeds).

The rules for `games` are those of `nfl_clinching_status()`: remove
cancelled games, set `result` to `null` for games after the week of
interest, and use the current team abbreviations. Invalid input and errors
inside the engine throw a JS `Error` with the message. The call is
synchronous; for long calls (`search: true` early in a season), run it in a
Web Worker.

## Build and test locally

```sh
source /path/to/emsdk/emsdk_env.sh       # emsdk 4.0.10, see wasm.yaml
wasm/build.sh                            # writes wasm/dist
Rscript wasm/fixtures/make-fixtures.R    # writes wasm/fixtures/json
node --test wasm/test/clinching.test.mjs
```

Details (layout, what the wrapper ports from R, the stack size) are in
`dev/wasm-build.md`.
