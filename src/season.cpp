#include "engine/season.hpp"

#include <algorithm>

namespace nflseedr {

namespace {

// Rank of every team that played among the teams in `members` (ties get the
// lowest rank). `better(a, b)` is true if a ranks ahead of b.
template <typename Better>
void min_rank(std::span<const TeamId> members, TeamArray<int32_t>& rank,
              Better better) {
  for (TeamId t : members) {
    int32_t r = 1;
    for (TeamId u : members) {
      if (better(u, t)) ++r;
    }
    rank[t] = r;
  }
}

void points_ranks(const SeasonData& s, std::span<const TeamId> members,
                  TeamArray<int32_t>& out) {
  TeamArray<int32_t> pf_rank{};
  TeamArray<int32_t> pa_rank{};
  min_rank(members, pf_rank,
           [&](TeamId a, TeamId b) { return s.pf[a] > s.pf[b]; });
  min_rank(members, pa_rank,
           [&](TeamId a, TeamId b) { return s.pa[a] < s.pa[b]; });
  for (TeamId t : members) out[t] = pf_rank[t] + pa_rank[t];
}

}  // namespace

bool SeasonData::all_played() const noexcept {
  return std::ranges::all_of(games, [](int32_t g) { return g > 0; });
}

void SeasonData::build(std::span<const Game> season_games, const League& league,
                       bool has_scores, bool complete) {
  *this = SeasonData{};
  regular_season_complete = complete;

  for (const Game& g : season_games) {
    if (g.type != kRegular) {
      // Exit = last playoff round, SB_WIN for the Super Bowl winner
      exit[g.away] = std::max(exit[g.away], g.type);
      exit[g.home] = std::max(exit[g.home], g.type);
      if (g.type == kSuperBowl) {
        exit[g.result > 0 ? g.home : g.away] = kSuperBowlWin;
      }
      continue;
    }
    const std::array<TeamId, 2> team{g.home, g.away};
    const std::array<double, 2> result{g.result, -g.result};
    const std::array<double, 2> score{has_scores ? g.home_score : 3.0,
                                      has_scores ? g.away_score : 3.0};
    const bool div_game = league.division[g.home] == league.division[g.away];
    const bool conf_game =
        league.conference[g.home] == league.conference[g.away];
    for (int i = 0; i < 2; ++i) {
      const TeamId t = team[i];
      const TeamId o = team[1 - i];
      const int32_t half = result[i] > 0 ? 2 : (result[i] < 0 ? 0 : 1);
      games[t] += 1;
      half_wins[t] += half;
      true_wins[t] += half == 2;
      losses[t] += half == 0;
      ties[t] += half == 1;
      if (div_game) {
        div_games[t] += 1;
        div_half_wins[t] += half;
      }
      if (conf_game) {
        conf_games[t] += 1;
        conf_half_wins[t] += half;
        conf_pd[t] += result[i];
      }
      pf[t] += score[i];
      pa[t] += score[i] - result[i];
      h2h_games[t][o] += 1;
      h2h_half_wins[t][o] += half;
      h2h_pd[t][o] += result[i];
    }
  }

  // SOV and SOS need the records of all opponents
  for (const Game& g : season_games) {
    if (g.type != kRegular) continue;
    const std::array<TeamId, 2> team{g.home, g.away};
    const std::array<double, 2> result{g.result, -g.result};
    for (int i = 0; i < 2; ++i) {
      const TeamId t = team[i];
      const TeamId o = team[1 - i];
      sos_half_wins[t] += half_wins[o];
      sos_games[t] += games[o];
      if (result[i] > 0) {
        sov_half_wins[t] += half_wins[o];
        sov_games[t] += games[o];
      }
    }
  }

  // Combined points rankings among the teams that played
  std::array<TeamId, kTeams> league_members{};
  std::array<std::array<TeamId, kTeams>, kConferences> conf_members{};
  int32_t n_league = 0;
  std::array<int32_t, kConferences> n_conf{};
  for (TeamId t = 0; t < kTeams; ++t) {
    if (!played(t)) continue;
    league_members[n_league++] = t;
    const int32_t c = league.conference[t];
    conf_members[c][n_conf[c]++] = t;
  }
  points_ranks(*this, std::span(league_members).first(n_league),
               league_points_rank);
  for (int32_t c = 0; c < kConferences; ++c) {
    points_ranks(*this, std::span(conf_members[c]).first(n_conf[c]),
                 conf_points_rank);
  }
}

}  // namespace nflseedr
