#include "engine/elo.hpp"

#include <algorithm>
#include <cmath>
#include <unordered_map>

namespace nflseedr {

namespace {

constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();

// Row of a sim and team in the team table. Uses a dense table if the sims
// cover a small range (as in simulations), a hash map otherwise.
class TeamRows {
 public:
  explicit TeamRows(const EloTeams& teams) {
    const size_t n = teams.sim.size();
    int32_t max_team = -1;
    bool first = true;
    for (size_t i = 0; i < n; ++i) {
      if (!valid(teams.sim[i], teams.team[i])) continue;
      max_team = std::max(max_team, teams.team[i]);
      if (first || teams.sim[i] < min_sim_) min_sim_ = teams.sim[i];
      if (first || teams.sim[i] > max_sim_) max_sim_ = teams.sim[i];
      first = false;
    }
    if (first) return;
    n_teams_ = static_cast<int64_t>(max_team) + 1;
    const int64_t size =
        (static_cast<int64_t>(max_sim_) - min_sim_ + 1) * n_teams_;
    dense_ = size <= 4 * static_cast<int64_t>(n) + 1024;
    if (dense_) table_.assign(static_cast<size_t>(size), kMissing);
    // Backwards, so the first row of a sim and team wins
    for (size_t i = n; i-- > 0;) {
      if (!valid(teams.sim[i], teams.team[i])) continue;
      const int32_t row = static_cast<int32_t>(i);
      if (dense_) {
        table_[static_cast<size_t>(dense_key(teams.sim[i], teams.team[i]))] =
            row;
      } else {
        map_[key(teams.sim[i], teams.team[i])] = row;
      }
    }
  }

  [[nodiscard]] int32_t find(int32_t sim, int32_t team) const {
    if (n_teams_ == 0 || !valid(sim, team) || team >= n_teams_) {
      return kMissing;
    }
    if (dense_) {
      if (sim < min_sim_ || sim > max_sim_) return kMissing;
      return table_[static_cast<size_t>(dense_key(sim, team))];
    }
    const auto it = map_.find(key(sim, team));
    return it == map_.end() ? kMissing : it->second;
  }

 private:
  // Sims are R integers, so INT_MIN is NA
  static bool valid(int32_t sim, int32_t team) {
    return sim != std::numeric_limits<int32_t>::min() && team >= 0;
  }
  [[nodiscard]] int64_t dense_key(int32_t sim, int32_t team) const {
    return (static_cast<int64_t>(sim) - min_sim_) * n_teams_ + team;
  }
  [[nodiscard]] static int64_t key(int32_t sim, int32_t team) {
    return static_cast<int64_t>(sim) * (int64_t{1} << 32) + team;
  }

  int32_t min_sim_ = 0;
  int32_t max_sim_ = 0;
  int64_t n_teams_ = 0;
  bool dense_ = true;
  std::vector<int32_t> table_;
  std::unordered_map<int64_t, int32_t> map_;
};

// Rounds away from zero
int32_t round_out(double x) {
  return static_cast<int32_t>(x < 0 ? std::floor(x) : std::ceil(x));
}

}  // namespace

EloWeek simulate_elo_week(const EloGames& games, const EloTeams& teams,
                          RandomSource& rng) {
  const size_t n = games.sim.size();
  const TeamRows rows(teams);
  EloWeek out;
  out.result.assign(games.result.begin(), games.result.end());
  std::vector<int32_t> away_row(n), home_row(n);
  std::vector<double> shift(n);

  for (size_t i = 0; i < n; ++i) {
    away_row[i] = rows.find(games.sim[i], games.away[i]);
    home_row[i] = rows.find(games.sim[i], games.home[i]);
    const double home_elo =
        home_row[i] == kMissing ? kNaN : teams.elo[home_row[i]];
    const double away_elo =
        away_row[i] == kMissing ? kNaN : teams.elo[away_row[i]];
    // 25 elo points per point. Home field is worth about 1.8 points.
    double elo_diff = home_elo - away_elo + games.rest_diff[i] / 7 * 25;
    if (games.home_field[i]) elo_diff += 45;
    if (games.postseason[i]) elo_diff *= 1.2;
    const double wp = 1 / (std::pow(10.0, -elo_diff / 400) + 1);

    // Like rnorm(), a missing mean doesn't draw and gives no result
    if (out.result[i] == kNoResult && !std::isnan(elo_diff)) {
      out.result[i] = round_out(elo_diff / 25 + 13 * rng.normal());
    }
    const int32_t result = out.result[i];
    if (result == kNoResult) {
      shift[i] = kNaN;
      continue;
    }
    double outcome = 0.5;
    double elo_input = 1.0;
    if (result > 0) {
      outcome = 1;
      elo_input = elo_diff * 0.001 + 2.2;
    } else if (result < 0) {
      outcome = 0;
      elo_input = -elo_diff * 0.001 + 2.2;
    }
    const double elo_mult =
        std::log(std::max(std::abs(static_cast<double>(result)), 1.0) + 1.0) *
        2.2 / elo_input;
    shift[i] = 20 * elo_mult * (outcome - wp);
  }

  // Away teams first, then home teams (a later game of a team wins). Teams
  // without a game or a result keep their elo.
  std::vector<double> team_shift(teams.elo.size(), 0.0);
  for (size_t i = 0; i < n; ++i) {
    if (away_row[i] != kMissing) team_shift[away_row[i]] = -shift[i];
  }
  for (size_t i = 0; i < n; ++i) {
    if (home_row[i] != kMissing) team_shift[home_row[i]] = shift[i];
  }
  out.elo.resize(teams.elo.size());
  for (size_t i = 0; i < teams.elo.size(); ++i) {
    out.elo[i] =
        teams.elo[i] + (std::isnan(team_shift[i]) ? 0.0 : team_shift[i]);
  }
  return out;
}

}  // namespace nflseedr
