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

// Draft ranks aren't in nfl_clinching_status(); tested below
const withoutDraft = (rows) =>
  rows.map(({ draft_rank, draft_tie_broken_by, ...row }) => row);

for (const fixture of fixtures) {
  test(`same results as R: ${fixture.name}`, () => {
    const start = performance.now();
    const out = engine.clinchingStatus({ games: fixture.games, ...fixture.options });
    timings.push([fixture.name, performance.now() - start]);
    assert.deepEqual(withoutDraft(out.status), fixture.expected.status);
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

const fixtureGames = (name) => fixtures.find((f) => f.name === name).games;
// Teams in draft order and the playoff teams (draft_rank null)
function draftOrder(status) {
  const picks = status
    .filter((r) => r.draft_rank !== null)
    .sort((a, b) => a.draft_rank - b.draft_rank);
  return {
    ranks: picks.map((r) => r.draft_rank),
    teams: picks.map((r) => r.team),
    labels: Object.fromEntries(
      picks
        .filter((r) => r.draft_tie_broken_by !== null)
        .map((r) => [r.team, r.draft_tie_broken_by]),
    ),
    playoffs: status
      .filter((r) => r.draft_rank === null)
      .map((r) => r.team)
      .sort(),
  };
}
const range = (n) => Array.from({ length: n }, (_, i) => i + 1);

// draft_rank 1-18 of nflseedR::nfl_standings(games, ranks = "DRAFT") with
// all games of 2024 including the playoffs, none of them with a label
const DRAFT_2024 = [
  "TEN", "CLE", "NYG", "NE", "JAX", "LV", "NYJ", "CAR", "NO",
  "CHI", "SF", "DAL", "MIA", "IND", "ATL", "ARI", "CIN", "SEA",
];
const PLAYOFFS_2024 = [
  "BAL", "BUF", "DEN", "DET", "GB", "HOU", "KC", "LA", "LAC", "MIN", "PHI",
  "PIT", "TB", "WAS",
];

test("draft ranks of a complete season match nfl_standings()", () => {
  const all = fixtureGames("2024-complete");
  const reg = all.filter((g) => g.game_type === "REG");
  for (const games of [all, reg]) {
    const start = performance.now();
    const { status } = engine.clinchingStatus({ games });
    if (games === reg) timings.push(["2024 REG", performance.now() - start]);
    const order = draftOrder(status);
    assert.deepEqual(order.ranks, range(18));
    assert.deepEqual(order.teams, DRAFT_2024);
    assert.deepEqual(order.labels, {});
    assert.deepEqual(order.playoffs, PLAYOFFS_2024);
    for (const r of status.filter((r) => r.draft_rank === null)) {
      assert.ok(r.conf_rank <= 7, r.team);
      assert.equal(r.draft_tie_broken_by, null);
    }
  }
});

test("draft ranks mid-season", () => {
  // 2024 through week 10, expected values from nfl_standings() of the
  // played games with ranks = "CONF" and playoff_seeds = NULL, then
  // standings_add_draft_ranks() with exit 1 for conf_rank 1-7, 0 otherwise
  const { status } = engine.clinchingStatus({
    games: fixtureGames("2024-week-10"),
  });
  const order = draftOrder(status);
  assert.deepEqual(order.ranks, range(18));
  assert.deepEqual(
    order.teams,
    status
      .filter((r) => r.conf_rank > 7)
      .map((r) => r.team)
      .sort((a, b) => order.teams.indexOf(a) - order.teams.indexOf(b)),
  );
  assert.deepEqual(order.teams, [
    "JAX", "NYG", "CLE", "TEN", "LV", "NE", "CAR", "NYJ", "NO",
    "MIA", "DAL", "CIN", "IND", "TB", "CHI", "SEA", "LA", "SF",
  ]);
  // NYJ and NO (3-7) have the same SOS
  assert.deepEqual(order.labels, { NYJ: "SOV (2)" });
});

test("draft ranks of 6-seed seasons leave 12 teams out", () => {
  const { status } = engine.clinchingStatus({
    games: fixtureGames("2016-week-14-6-seeds"),
  });
  const order = draftOrder(status);
  assert.deepEqual(order.ranks, range(20));
  assert.equal(order.playoffs.length, 12);
});

test("the same seed gives the same draft ranks", () => {
  // Week 1 of 2024: many ties that only a coin toss breaks
  const games = fixtureGames("2024-week-15").map((g) => ({
    ...g,
    result: g.week === 1 ? g.result : null,
  }));
  const draft = (seed) =>
    engine
      .clinchingStatus({ games, status: ["DIV"], seed })
      .status.map((r) => [r.team, r.draft_rank, r.draft_tie_broken_by]);
  assert.deepEqual(draft(1), draft(1));
  assert.ok(draft(1).some(([, , label]) => label === "Coin Toss"));
  assert.notDeepEqual(draft(1), draft(2));
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
    assert.deepEqual([row.draft_rank, row.draft_tie_broken_by], [null, null]);
  }
  const order = status.map((r) => `${r.division} ${r.team}`);
  assert.deepEqual(order, [...order].sort());
});

test("no draft ranks while some teams haven't played", () => {
  // Week 1 of 2024 without its last game: ranks need every team (like
  // div_rank and conf_rank)
  const games = fixtureGames("2024-week-15").map((g) => ({
    ...g,
    result: g.week === 1 ? g.result : null,
  }));
  const last = games.findLastIndex((g) => g.week === 1);
  games[last] = { ...games[last], result: null };
  const { status } = engine.clinchingStatus({ games });
  const idle = [games[last].away_team, games[last].home_team];
  assert.deepEqual(
    status.filter((r) => r.wins + r.losses + r.ties === 0).map((r) => r.team).sort(),
    idle.sort(),
  );
  for (const row of status) {
    assert.deepEqual(
      [row.draft_rank, row.draft_tie_broken_by, row.conf_rank],
      [null, null, null],
    );
  }
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
