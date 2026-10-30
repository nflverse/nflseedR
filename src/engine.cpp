#include "engine/engine.hpp"

#include <algorithm>
#include <cmath>
#include <tuple>

#include "engine/season.hpp"

namespace nflseedr {

namespace {

constexpr int32_t kInterruptEvery = 1000;

void check_league(const League& league) {
  std::array<int32_t, kDivisions> div_size{};
  std::array<int32_t, kConferences> conf_size{};
  for (TeamId t = 0; t < kTeams; ++t) {
    const int32_t d = league.division[t];
    const int32_t c = league.conference[t];
    if (d < 0 || d >= kDivisions || c < 0 || c >= kConferences) {
      throw EngineError("Invalid division or conference id");
    }
    ++div_size[d];
    ++conf_size[c];
  }
  for (int32_t n : div_size) {
    if (n != kTeamsPerDivision) throw EngineError("Divisions need 4 teams");
  }
  for (int32_t n : conf_size) {
    if (n != kTeamsPerConference) {
      throw EngineError("Conferences need 16 teams");
    }
  }
}

// Games grouped by season: games of season s (1-based) are
// sorted[offset[s - 1] .. offset[s]).
struct GamesBySeason {
  std::vector<Game> sorted;
  std::vector<int32_t> offset;

  [[nodiscard]] std::span<const Game> season(int32_t s) const {
    return std::span(sorted).subspan(offset[s - 1], offset[s] - offset[s - 1]);
  }
};

GamesBySeason group_games(const GamesInput& in) {
  const size_t n = in.sim.size();
  if (in.type.size() != n || in.away.size() != n || in.home.size() != n ||
      in.result.size() != n ||
      (in.has_scores &&
       (in.away_score.size() != n || in.home_score.size() != n))) {
    throw EngineError("All game columns need the same length");
  }
  GamesBySeason out;
  out.offset.assign(in.n_sims + 1, 0);
  for (size_t i = 0; i < n; ++i) {
    const int32_t s = in.sim[i];
    if (s < 1 || s > in.n_sims) throw EngineError("Invalid season id");
    if (in.away[i] < 0 || in.away[i] >= kTeams || in.home[i] < 0 ||
        in.home[i] >= kTeams || in.away[i] == in.home[i]) {
      throw EngineError("Invalid team id");
    }
    if (in.type[i] < kRegular || in.type[i] > kSuperBowl) {
      throw EngineError("Invalid game type");
    }
    if (std::isnan(in.result[i])) throw EngineError("Missing game result");
    ++out.offset[s];
  }
  for (int32_t s = 1; s <= in.n_sims; ++s) out.offset[s] += out.offset[s - 1];
  out.sorted.resize(n);
  std::vector<int32_t> next(out.offset.begin(), out.offset.end() - 1);
  for (size_t i = 0; i < n; ++i) {
    Game& g = out.sorted[next[in.sim[i] - 1]++];
    g.away = in.away[i];
    g.home = in.home[i];
    g.type = in.type[i];
    g.result = in.result[i];
    if (in.has_scores) {
      g.away_score = in.away_score[i];
      g.home_score = in.home_score[i];
    }
  }
  return out;
}

double pct(int32_t half_wins, int32_t games) {
  return games == 0 ? 0.0 : (half_wins * 0.5) / games;
}

}  // namespace

StandingsRows compute_standings(const GamesInput& games, const League& league,
                                const StandingsOptions& options,
                                RandomSource& rng,
                                const InterruptCheck& interrupt) {
  check_league(league);
  const GamesBySeason by_season = group_games(games);
  if (options.season_complete.size() != static_cast<size_t>(games.n_sims)) {
    throw EngineError("Need one regular season status per season");
  }
  // D7: draft ranks need all conference ranks, so playoff_seeds is ignored
  const int32_t playoff_seeds = options.ranks == Ranks::kDraft
                                    ? kTeamsPerConference
                                    : options.playoff_seeds;

  StandingsRows out;
  SeasonData data;
  for (int32_t s = 1; s <= games.n_sims; ++s) {
    if (s % kInterruptEvery == 0) interrupt();
    data.build(by_season.season(s), league, games.has_scores,
               options.season_complete[s - 1] != 0);

    RankResult div;
    RankResult conf;
    RankResult draft;
    // Ranks only make sense once every team has played
    if (options.ranks != Ranks::kNone && data.all_played()) {
      div = rank_divisions(data, league, options.depth, rng);
      if (options.ranks >= Ranks::kConference) {
        conf = rank_conferences(data, league, div.rank, playoff_seeds,
                                options.depth, rng);
      }
      if (options.ranks == Ranks::kDraft) {
        draft = rank_draft(data, league, div.rank, conf.rank, data.exit,
                           options.depth, rng);
      }
    }

    TeamSet teams;
    for (TeamId t = 0; t < kTeams; ++t) {
      if (data.played(t)) teams.push(t);
    }
    std::stable_sort(teams.begin(), teams.end(), [&](TeamId a, TeamId b) {
      return std::tuple(league.division[a], div.rank[a], a) <
             std::tuple(league.division[b], div.rank[b], b);
    });

    for (TeamId t : teams) {
      out.sim.push_back(s);
      out.team.push_back(t);
      out.games.push_back(data.games[t]);
      out.wins.push_back(data.half_wins[t] * 0.5);
      out.true_wins.push_back(data.true_wins[t]);
      out.losses.push_back(data.losses[t]);
      out.ties.push_back(data.ties[t]);
      out.pf.push_back(data.pf[t]);
      out.pa.push_back(data.pa[t]);
      out.win_pct.push_back(pct(data.half_wins[t], data.games[t]));
      out.div_pct.push_back(pct(data.div_half_wins[t], data.div_games[t]));
      out.conf_pct.push_back(pct(data.conf_half_wins[t], data.conf_games[t]));
      out.sov.push_back(pct(data.sov_half_wins[t], data.sov_games[t]));
      out.sos.push_back(pct(data.sos_half_wins[t], data.sos_games[t]));
      out.div_rank.push_back(div.rank[t]);
      out.div_label.push_back(div.label[t]);
      // Blocks that cross the last playoff seed are ranked completely
      out.conf_rank.push_back(conf.rank[t] <= playoff_seeds ? conf.rank[t]
                                                            : kMissing);
      out.conf_label.push_back(conf.label[t]);
      out.exit.push_back(data.exit[t]);
      out.draft_rank.push_back(draft.rank[t]);
      out.draft_label.push_back(draft.label[t]);
    }
  }
  return out;
}

DraftRanks compute_draft_ranks(const GamesInput& games, const League& league,
                               const TeamRanksInput& teams, int32_t depth,
                               RandomSource& rng,
                               const InterruptCheck& interrupt) {
  check_league(league);
  const GamesBySeason by_season = group_games(games);
  const size_t n = teams.sim.size();
  if (teams.team.size() != n || teams.div_rank.size() != n ||
      teams.conf_rank.size() != n || teams.exit.size() != n) {
    throw EngineError("All team columns need the same length");
  }

  // Rows of season s: rows[row_offset[s - 1] .. row_offset[s])
  std::vector<int32_t> row_offset(games.n_sims + 1, 0);
  for (size_t i = 0; i < n; ++i) {
    if (teams.sim[i] < 1 || teams.sim[i] > games.n_sims) {
      throw EngineError("Invalid season id");
    }
    if (teams.team[i] < 0 || teams.team[i] >= kTeams) {
      throw EngineError("Invalid team id");
    }
    ++row_offset[teams.sim[i]];
  }
  for (int32_t s = 1; s <= games.n_sims; ++s) {
    row_offset[s] += row_offset[s - 1];
  }
  std::vector<int32_t> rows(n);
  std::vector<int32_t> next(row_offset.begin(), row_offset.end() - 1);
  for (size_t i = 0; i < n; ++i) {
    rows[next[teams.sim[i] - 1]++] = static_cast<int32_t>(i);
  }

  DraftRanks out;
  out.rank.assign(n, kMissing);
  out.label.assign(n, kMissing);
  SeasonData data;
  for (int32_t s = 1; s <= games.n_sims; ++s) {
    if (s % kInterruptEvery == 0) interrupt();
    // Draft ranks are only computed after the regular season
    data.build(by_season.season(s), league, games.has_scores, true);
    if (!data.all_played()) continue;
    TeamArray<int32_t> div_rank{};
    TeamArray<int32_t> conf_rank{};
    TeamArray<int32_t> exit{};
    div_rank.fill(kMissing);
    conf_rank.fill(kMissing);
    for (int32_t k = row_offset[s - 1]; k < row_offset[s]; ++k) {
      const int32_t i = rows[k];
      const TeamId t = teams.team[i];
      div_rank[t] = teams.div_rank[i];
      conf_rank[t] = teams.conf_rank[i];
      exit[t] = teams.exit[i];
    }
    const RankResult draft =
        rank_draft(data, league, div_rank, conf_rank, exit, depth, rng);
    for (int32_t k = row_offset[s - 1]; k < row_offset[s]; ++k) {
      const int32_t i = rows[k];
      out.rank[i] = draft.rank[teams.team[i]];
      out.label[i] = draft.label[teams.team[i]];
    }
  }
  return out;
}

}  // namespace nflseedr
