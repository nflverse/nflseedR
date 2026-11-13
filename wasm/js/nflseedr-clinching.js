// nflseedR clinching engine in the browser: a thin wrapper around the
// WebAssembly build of compute_clinching_status() (src/engine/status.hpp).
// It ports the R code around the engine in R/clinching_status.R and
// R/standings_*.R. Each ported piece names the R function it mirrors; keep
// them in sync. See wasm/README.md.
//
//   import { createClinchingEngine } from "./nflseedr-clinching.js";
//   const engine = await createClinchingEngine();
//   const { status, scenarios } = engine.clinchingStatus({ games });

import createClinchingModule from "./clinching.js";

const GOALS = ["DIV", "PLAYOFFS", "1SEED"];
const STATUS_COLUMNS = ["div_status", "playoffs_status", "seed1_status"];

// Column ids of column() in wasm/src/clinching.cpp
const COLUMNS = {
  sim: 0,
  team: 1,
  wins: 2,
  losses: 3,
  ties: 4,
  games_left: 5,
  div_status: 6,
  playoffs_status: 7,
  seed1_status: 8,
  scenario_sim: 9,
  scenario_team: 10,
  scenario_goal: 11,
  scenario_proves: 12,
  scenario_coin_toss: 13,
  scenario_result: 14,
};

// The 32 teams of nflseedR::divisions without LAR, OAK, SD and STL
// (standings_league_build())
export const TEAMS = [
  ["ARI", "NFC", "NFC West"], ["ATL", "NFC", "NFC South"],
  ["BAL", "AFC", "AFC North"], ["BUF", "AFC", "AFC East"],
  ["CAR", "NFC", "NFC South"], ["CHI", "NFC", "NFC North"],
  ["CIN", "AFC", "AFC North"], ["CLE", "AFC", "AFC North"],
  ["DAL", "NFC", "NFC East"], ["DEN", "AFC", "AFC West"],
  ["DET", "NFC", "NFC North"], ["GB", "NFC", "NFC North"],
  ["HOU", "AFC", "AFC South"], ["IND", "AFC", "AFC South"],
  ["JAX", "AFC", "AFC South"], ["KC", "AFC", "AFC West"],
  ["LA", "NFC", "NFC West"], ["LAC", "AFC", "AFC West"],
  ["LV", "AFC", "AFC West"], ["MIA", "AFC", "AFC East"],
  ["MIN", "NFC", "NFC North"], ["NE", "AFC", "AFC East"],
  ["NO", "NFC", "NFC South"], ["NYG", "NFC", "NFC East"],
  ["NYJ", "AFC", "AFC East"], ["PHI", "NFC", "NFC East"],
  ["PIT", "AFC", "AFC North"], ["SEA", "NFC", "NFC West"],
  ["SF", "NFC", "NFC West"], ["TB", "NFC", "NFC South"],
  ["TEN", "AFC", "AFC South"], ["WAS", "NFC", "NFC East"],
].map(([team, conf, division]) => ({ team, conf, division }));

const sortedUnique = (x, compare) => [...new Set(x)].sort(compare);
const byNumber = (a, b) => a - b;
const byString = (a, b) => (a < b ? -1 : a > b ? 1 : 0);

// standings_league_build(): teams in alphabetical order with 0-based
// division and conference ids, also alphabetical
function buildLeague(teams) {
  const sorted = [...teams].sort((a, b) => byString(a.team, b.team));
  const divisions = sortedUnique(sorted.map((t) => t.division), byString);
  const confs = sortedUnique(sorted.map((t) => t.conf), byString);
  return sorted.map((t) => ({
    team: t.team,
    conf: t.conf,
    division: t.division,
    division_id: divisions.indexOf(t.division),
    conf_id: confs.indexOf(t.conf),
  }));
}

// standings_validate_games() and standings_check_teams(): returns the name
// of the season id ("sim" or "season")
function validateGames(games, league) {
  const required = ["game_type", "week", "away_team", "home_team", "result"];
  const has = (key) => games.every((g) => key in g);
  const id = has("sim") ? "sim" : "season";
  if (!has(id) || !required.every(has)) {
    throw new Error(
      "`games` has to include one of the identifiers `sim` or `season` as " +
        `well as all of the following variables: ${required.join(", ")}`,
    );
  }
  const valid = new Set(league.map((t) => t.team));
  const teams = games.flatMap((g) => [g.away_team, g.home_team]);
  const invalid = sortedUnique(teams.filter((t) => !valid.has(t)), byString);
  if (invalid.length > 0) {
    throw new Error(
      `\`games\` includes invalid team abbreviations: ${invalid.join(", ")}`,
    );
  }
  return id;
}

// standings_game_type()
const GAME_TYPES = ["REG", "WC", "DIV", "CON", "SB"];
function gameType(type) {
  const id = GAME_TYPES.indexOf(type);
  if (id < 0) {
    throw new Error(`\`games\` includes invalid \`game_type\` values: ${type}`);
  }
  return id;
}

// clinching_status_seeds(): 6 seeds through 2019, 7 since 2020. With `sim`
// instead of `season`, the current 7 seeds.
export function clinchingStatusSeeds(sims, usesSeason) {
  return sims.map((s) => (usesSeason && s <= 2019 ? 6 : 7));
}

// clinching_status_labels(): status codes of src/engine/status.hpp
function statusLabel(code) {
  return ["clinched", "alive", "eliminated", "undecided"][code - 1] ?? null;
}

// standings_encode_games(): the season id, game type, away and home team of
// `n` games as four blocks in one array, plus the results
function encodeGames(rows, id, sims, teamIds, placeholder) {
  const n = rows.length;
  const ints = new Int32Array(4 * n);
  const result = new Float64Array(n);
  rows.forEach((g, i) => {
    ints[i] = sims.indexOf(g[id]) + 1;
    ints[n + i] = gameType(g.game_type);
    ints[2 * n + i] = teamIds.get(g.away_team);
    ints[3 * n + i] = teamIds.get(g.home_team);
    result[i] = placeholder ? 0 : g.result;
  });
  return { ints, result, n };
}

export async function createClinchingEngine(moduleOptions = {}) {
  const mod = await createClinchingModule(moduleOptions);

  // Copy a typed array into the wasm heap (at least one element, so empty
  // inputs still get a valid pointer)
  const pointers = [];
  function toHeap(x) {
    const ptr = mod._malloc(Math.max(x.byteLength, 8));
    pointers.push(ptr);
    const heap = x instanceof Float64Array ? mod.HEAPF64 : mod.HEAP32;
    heap.set(x, ptr / x.BYTES_PER_ELEMENT);
    return ptr;
  }

  function readColumn(handle, name) {
    const size = mod._nflseedr_column_size(handle, COLUMNS[name]);
    const ptr = mod._nflseedr_column_data(handle, COLUMNS[name]) / 4;
    return Array.from(mod.HEAP32.subarray(ptr, ptr + size));
  }

  // clinching_status_cpp() in src/bindings.cpp
  function run(played, remaining, nSims, league, seeds, goals, opts) {
    try {
      const leagueIds = Int32Array.from([
        ...league.map((t) => t.division_id),
        ...league.map((t) => t.conf_id),
      ]);
      const handle = mod._nflseedr_clinching_status(
        toHeap(played.ints),
        toHeap(played.result),
        played.n,
        toHeap(remaining.ints),
        toHeap(remaining.result),
        remaining.n,
        nSims,
        toHeap(leagueIds),
        toHeap(Int32Array.from(seeds)),
        goals.reduce((bits, on, g) => bits | (on ? 1 << g : 0), 0),
        opts.scenarios ? 1 : 0,
        opts.search ? 1 : 0,
      );
      try {
        const error = mod.UTF8ToString(mod._nflseedr_error(handle));
        if (error !== "") throw new Error(error);
        return Object.fromEntries(
          Object.keys(COLUMNS).map((name) => [name, readColumn(handle, name)]),
        );
      } finally {
        mod._nflseedr_free(handle);
      }
    } finally {
      while (pointers.length > 0) mod._free(pointers.pop());
    }
  }

  // clinching_status_scenarios(): one row per scenario and remaining game of
  // its season, with the results of the scenario
  function decodeScenarios(out, remaining, id, sims, league) {
    // Rows of each season's remaining games, in the order the engine got them
    const rows = sims.map((s) => remaining.filter((g) => g[id] === s));
    const scenarios = [];
    let offset = 0;
    out.scenario_sim.forEach((simId, i) => {
      const common = {
        scenario: i + 1,
        [id]: sims[simId - 1],
        team: league[out.scenario_team[i]].team,
        goal: GOALS[out.scenario_goal[i]],
        proves: ["reach", "miss"][out.scenario_proves[i] - 1],
        coin_toss: out.scenario_coin_toss[i] === 1,
      };
      for (const game of rows[simId - 1]) {
        const { sim, season, ...cols } = game;
        cols.result = out.scenario_result[offset++];
        scenarios.push({ ...common, ...cols });
      }
    });
    return scenarios;
  }

  // nfl_clinching_status() / clinching_status_compute(): one row per season
  // and team (all teams with a played or remaining game), sorted by season,
  // division and team
  function clinchingStatus({
    games,
    teams = TEAMS,
    status = GOALS,
    scenarios = false,
    search = false,
  }) {
    const unknown = status.filter((s) => !GOALS.includes(s));
    if (status.length === 0 || unknown.length > 0) {
      throw new Error(`\`status\` must be one or more of ${GOALS.join(", ")}`);
    }
    const league = buildLeague(teams);
    const teamIds = new Map(league.map((t, i) => [t.team, i]));
    const isNA = (x) => x === null || x === undefined || Number.isNaN(x);
    // Playoff games that aren't played yet don't matter
    const rows = games.filter((g) => !(g.game_type !== "REG" && isNA(g.result)));
    const id = validateGames(rows, league);
    const sims = sortedUnique(rows.map((g) => g[id]), byNumber);
    const played = rows.filter((g) => !isNA(g.result));
    // Cancelled games must be removed, so every regular season game without
    // a result is still to be played
    const remaining = rows.filter(
      (g) => isNA(g.result) && g.game_type === "REG",
    );

    const out = run(
      encodeGames(played, id, sims, teamIds, false),
      // Results of remaining games are placeholders
      encodeGames(remaining, id, sims, teamIds, true),
      sims.length,
      league,
      clinchingStatusSeeds(sims, id === "season"),
      GOALS.map((g) => status.includes(g)),
      { scenarios, search },
    );

    const table = out.sim.map((simId, i) => {
      const team = league[out.team[i]];
      const row = {
        [id]: sims[simId - 1],
        team: team.team,
        conf: team.conf,
        division: team.division,
        wins: out.wins[i],
        losses: out.losses[i],
        ties: out.ties[i],
        games_left: out.games_left[i],
      };
      GOALS.forEach((goal, g) => {
        if (status.includes(goal)) {
          row[STATUS_COLUMNS[g]] = statusLabel(out[STATUS_COLUMNS[g]][i]);
        }
      });
      return row;
    });
    table.sort(
      (a, b) =>
        a[id] - b[id] ||
        byString(a.division, b.division) ||
        byString(a.team, b.team),
    );
    return {
      status: table,
      scenarios: scenarios
        ? decodeScenarios(out, remaining, id, sims, league)
        : null,
    };
  }

  return { clinchingStatus, module: mod };
}

export default createClinchingEngine;
