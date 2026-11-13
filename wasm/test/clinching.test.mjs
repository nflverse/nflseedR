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

const { createClinchingEngine, TEAMS } = await import(
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
