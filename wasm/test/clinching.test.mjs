// The wasm build must give the same results as R (wasm/README.md).
//
// Usage: node --test wasm/test/
// Env: NFLSEEDR_WASM_DIST (default wasm/dist), NFLSEEDR_WASM_FIXTURES
// (default wasm/fixtures/json, written by wasm/fixtures/make-fixtures.R)
import assert from "node:assert/strict";
import { readdirSync, readFileSync, statSync } from "node:fs";
import { join, resolve } from "node:path";
import { after, before, test } from "node:test";
import { pathToFileURL } from "node:url";

const root = resolve(import.meta.dirname, "../..");
const dist = resolve(process.env.NFLSEEDR_WASM_DIST ?? join(root, "wasm/dist"));
const fixtureDir = resolve(
  process.env.NFLSEEDR_WASM_FIXTURES ?? join(root, "wasm/fixtures/json"),
);
const fixtures = readdirSync(fixtureDir)
  .filter((f) => f.endsWith(".json"))
  .sort()
  .map((f) => JSON.parse(readFileSync(join(fixtureDir, f), "utf8")));

const { createClinchingEngine, standingsLabel, TEAMS } = await import(
  pathToFileURL(join(dist, "nflseedr-clinching.js")).href
);
let engine;
const timings = [];

before(async () => {
  engine = await createClinchingEngine();
});

after(() => {
  const kb = (statSync(join(dist, "clinching.wasm")).size / 1024).toFixed(1);
  console.log(`clinching.wasm: ${kb} KiB`);
  for (const [name, ms] of timings) {
    console.log(`${name}: ${ms.toFixed(1)} ms`);
  }
});

test("fixtures exist", () => {
  assert.ok(fixtures.length >= 7, `only ${fixtures.length} fixtures`);
});

for (const fixture of fixtures) {
  test(`same results as R: ${fixture.name}`, () => {
    const start = performance.now();
    const out = engine.clinchingStatus({ games: fixture.games, ...fixture.options });
    timings.push([fixture.name, performance.now() - start]);
    assert.deepEqual(out.status, fixture.expected.status);
    assert.deepEqual(out.scenarios, fixture.expected.scenarios);
  });
}

test("built-in teams match the teams in the fixtures", () => {
  const teams = new Map(TEAMS.map((t) => [t.team, t]));
  for (const row of fixtures.flatMap((f) => f.expected.status)) {
    const t = teams.get(row.team);
    assert.deepEqual([t.conf, t.division], [row.conf, row.division]);
  }
});

// standings_labels() in R/standings_cpp.R; the fixtures don't cover every step
test("tiebreaker labels decode like standings_labels()", () => {
  assert.equal(standingsLabel(-1, "div"), null);
  assert.equal(standingsLabel(12, "div"), "Head-To-Head Win PCT (2)");
  assert.equal(standingsLabel(23, "div"), "Head-To-Head (3)");
  assert.equal(standingsLabel(23, "conf"), "Head-To-Head Sweep (3)");
  assert.equal(standingsLabel(72, "conf"), "SOS (2)");
  assert.equal(standingsLabel(122, "div"), "Point Differential (2)");
  assert.equal(standingsLabel(122, "conf"), "League Point Differential (2)");
  assert.equal(standingsLabel(130, "conf"), "Coin Toss");
  assert.equal(standingsLabel(140, "conf"), "Division Tiebreaker");
  assert.equal(standingsLabel(150, "draft"), "Conference Tiebreaker");
});

test("teams without a played game have no ranks", () => {
  const games = fixtures[0].games.map((g) => ({ ...g, result: null }));
  const { status } = engine.clinchingStatus({ games });
  assert.equal(status.length, 32);
  for (const row of status) {
    assert.deepEqual(
      [row.div_rank, row.conf_rank, row.div_tie_broken_by],
      [null, null, null],
    );
  }
  const order = status.map((r) => `${r.division} ${r.team}`);
  assert.deepEqual(order, [...order].sort());
});

test("the same seed gives the same coin tosses", () => {
  // Week 1 of 2024: many ties that only a coin toss breaks
  const games = fixtures
    .find((f) => f.name === "2024-week-15")
    .games.map((g) => ({ ...g, result: g.week === 1 ? g.result : null }));
  const ranks = (seed) =>
    engine
      .clinchingStatus({ games, status: ["DIV"], seed })
      .status.map((r) => [r.team, r.conf_rank, r.conf_tie_broken_by]);
  assert.deepEqual(ranks(1), ranks(1));
  assert.ok(ranks(1).some(([, , label]) => label === "Coin Toss"));
});

test("errors become JS errors", () => {
  const { games } = fixtures[0];
  assert.throws(
    () => engine.clinchingStatus({ games, status: ["WIN"] }),
    /`status` must be/,
  );
  assert.throws(
    () =>
      engine.clinchingStatus({
        games: [{ ...games[0], away_team: "OAK" }],
      }),
    /invalid team abbreviations: OAK/,
  );
  // A league the engine rejects: the EngineError message comes through
  const teams = TEAMS.map((t) => ({ ...t, division: t.conf }));
  assert.throws(
    () => engine.clinchingStatus({ games, teams }),
    (e) => e instanceof Error && e.message.length > 0,
  );
  // The engine still works after an error
  const out = engine.clinchingStatus({ games });
  assert.equal(out.status.length, 32);
});
