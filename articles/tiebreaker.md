# NFL Tiebreaking Procedures

NFL tiebreaking procedures for division ranks and conference seeds are
documented [on the NFL.com
website](https://www.nfl.com/standings/tie-breaking-procedures) and [on
the NFL Football Operations
website](https://operations.nfl.com/the-rules/nfl-tie-breaking-procedures/).
They are mostly identical, but not exactly[¹](#fn1).

Draft pick assignment is also documented [on the NFL.com
website](https://www.nfl.com/standings/tie-breaking-procedures) and [on
the NFL Football Operations
website](https://operations.nfl.com/journey-to-the-nfl/the-nfl-draft/the-rules-of-the-draft/).
However, the author is of the opinion that both texts are so cumbersome
and misleadingly worded that he has written his own summary instead.

This website documents the currently in nflseedR implemented process in
case the above linked websites change in the future.

## Break a Tie Within a Division

*This is used to calculate the variable `div_rank`*

If, at the end of the regular season, two or more clubs in the same
division finish with identical won-lost-tied percentages, the following
steps will be taken until a champion is determined.

### Two Clubs

1.  Head-to-head (best won-lost-tied percentage in games between the
    clubs).
2.  Best won-lost-tied percentage in games played within the division.
3.  Best won-lost-tied percentage in common games.
4.  Best won-lost-tied percentage in games played within the conference.
5.  Strength of victory in all games.
6.  Strength of schedule in all games
7.  Best combined ranking (see [Other Tie-Breaking
    Procedures](#other-tie-breaking-procedures)) among conference teams
    in points scored and points allowed in all games.
8.  Best combined ranking (see [Other Tie-Breaking
    Procedures](#other-tie-breaking-procedures)) among all teams in
    points scored and points allowed in all games.
9.  Best net points in common games.
10. Best net points in all games.
11. Best net touchdowns in all games.
12. Coin toss.

### Three or More Clubs

If two clubs remain tied after one or more clubs are eliminated during
any step, tiebreaker restarts at Step 1 of two-club format. If three
clubs remain tied after a fourth club is eliminated during any step,
tiebreaker restarts at Step 1 of three-club format.

1.  Head-to-head (best won-lost-tied percentage in games among the
    clubs).
2.  Best won-lost-tied percentage in games played within the division.
3.  Best won-lost-tied percentage in common games.
4.  Best won-lost-tied percentage in games played within the conference.
5.  Strength of victory in all games.
6.  Strength of schedule in all games.
7.  Best combined ranking (see [Other Tie-Breaking
    Procedures](#other-tie-breaking-procedures)) among conference teams
    in points scored and points allowed in all games.
8.  Best combined ranking (see [Other Tie-Breaking
    Procedures](#other-tie-breaking-procedures)) among all teams in
    points scored and points allowed in all games.
9.  Best net points in common games.
10. Best net points in all games.
11. Best net touchdowns in all games.
12. Coin toss.

## Break a Tie for Conference Seeds

*This is used to calculate the variable `conf_rank`*

The seven postseason participants from each conference are seeded as
follows

1.  The division champion with the best record.
2.  The division champion with the second-best record.
3.  The division champion with the third-best record.
4.  The division champion with the fourth-best record.
5.  The wild card club with the best record.
6.  The wild card club with the second-best record.
7.  The wild card club with the third-best record.

The following procedures will be used to break standings ties for
postseason playoffs and to determine regular-season schedules. NOTE: Tie
games count as one-half win and one-half loss for both clubs.

If it is necessary to break ties to determine the three Wild Card clubs
from each conference, the following steps will be taken.

1.  If the tied clubs are from the same division, apply division
    tiebreaker.
2.  If the tied clubs are from different divisions, apply the following
    steps.

### Two Clubs

1.  Head-to-head, if applicable.
2.  Best won-lost-tied percentage in games played within the conference.
3.  Best won-lost-tied percentage in common games, minimum of four.
4.  Strength of victory in all games.
5.  Strength of schedule in all games.
6.  Best combined ranking (see [Other Tie-Breaking
    Procedures](#other-tie-breaking-procedures)) among conference teams
    in points scored and points allowed in all games.
7.  Best combined ranking (see [Other Tie-Breaking
    Procedures](#other-tie-breaking-procedures)) among all teams in
    points scored and points allowed in all games.
8.  Best net points in conference games.
9.  Best net points in all games.
10. Best net touchdowns in all games.
11. Coin toss.

### Three or More Clubs

If two clubs remain tied after one or more clubs are eliminated during
any step, tiebreaker restarts at Step 1 of two-club format. If three
clubs remain tied after a fourth club is eliminated during any step,
tiebreaker restarts at Step 2 of three-club format.

When the first Wild Card team has been identified, the procedure is
repeated to name the second and third Wild Card (i.e., eliminate all but
the highest-ranked club in each division prior to proceeding to Step 2).
In situations in which three teams from the same division are involved
in the procedure, the original seeding of the teams remains the same for
subsequent applications of the tiebreaker if the top-ranked team in that
division qualifies for a Wild Card berth.

1.  Apply division tiebreaker to eliminate all but the highest ranked
    club in each division prior to proceeding to step 2. The original
    seeding within a division upon application of the division
    tiebreaker remains the same for all subsequent applications of the
    procedure that are necessary to identify the ~~two~~\[sic!\] three
    Wild-Card participants.
2.  Head-to-head sweep. (Applicable only if one club has defeated each
    of the others or if one club has lost to each of the others.)
3.  Best won-lost-tied percentage in games played within the conference.
4.  Best won-lost-tied percentage in common games, minimum of four.
5.  Strength of victory in all games.
6.  Strength of schedule in all games.
7.  Best combined ranking (see [Other Tie-Breaking
    Procedures](#other-tie-breaking-procedures)) among conference teams
    in points scored and points allowed in all games.
8.  Best combined ranking (see [Other Tie-Breaking
    Procedures](#other-tie-breaking-procedures)) among all teams in
    points scored and points allowed in all games.
9.  Best net points in conference games.
10. Best net points in all games.
11. Best net touchdowns in all games.
12. Coin toss.

## Other Tie-Breaking Procedures

1.  Only one club advances to the playoffs in any tie-breaking step.
    Remaining tied clubs revert to the first step of the applicable
    division or Wild Card tie-breakers. As an example, if two clubs
    remain tied in any tie-breaker step after all other clubs have been
    eliminated, the procedure reverts to Step 1 of the two-club format
    to determine the winner. When one club wins the tiebreaker, all
    other clubs revert to Step 1 of the applicable two-club or
    three-club format.
2.  In comparing records against common opponents among tied teams, the
    best won-lost-tied percentage is the deciding factor, since teams
    may have played an unequal number of games.
3.  To determine home field priority among division winners, apply Wild
    Card tiebreakers.
4.  To determine home field priority for Wild Card qualifiers, apply
    division tiebreakers (if teams are from the same division) or Wild
    Card tiebreakers (if teams are from different divisions).
5.  To determine the *best combined ranking* among conference team’s in
    points scored and points allowed, add a team’s position in the two
    categories, and the lowest score wins. For example, if Team A is
    first in points scored and second in points allowed, its combined
    ranking is “3.” If Team B is third in points scored and first in
    points allowed, its combined ranking is “4.” Team A then wins the
    tiebreaker. If two teams are tied for a position, both teams are
    awarded the ranking as if they held it solely. For example, if Team
    A and Team B are tied for first in points scored, each team is
    assigned a ranking of “1” in that category, and if Team C is third,
    its ranking will still be “3.”

## Break a Tie for the Draft (“Selection Meeting”)

*This is used to calculate the variable `draft_rank`*

The order of selection is determined by the reverse order of finish in
the previous season. Barring any trades between clubs, each round starts
with the team that finished with the worst record and ends with the
Super Bowl champions.

Picks are assigned by win percentage in ascending order as follows:

1.  Clubs not participating in the playoffs shall select in the first
    through 18th positions.
2.  The losers of the Wild Card games shall select in the 19th through
    24th positions.
3.  The losers of the Divisional playoff games shall select in the 25th
    through 28th positions.
4.  The losers of the Conference Championship games shall select 29th
    and 30th.
5.  The winner of the Super Bowl game shall select last and the Super
    Bowl loser will select next-to-last.

In situations where teams finished the previous season with identical
win percentage, the determination of draft position is decided by
strength of schedule — the aggregate winning percentage of a team’s
opponents. The team that played the schedule with the lowest winning
percentage will be awarded the earlier pick.

If the teams have the same strength of schedule, division or conference
tiebreakers are applied first.

### Divisional Draft Pick Tie

If all teams tied for a pick are from the same division, then the
division rank is used. The lower division rank gets the earlier pick.

### Conference Draft Pick Tie

If all teams tied for a pick are from the same conference, then the
conference rank is used. The lower conference rank gets the earlier
pick.

### Inter-Conference Draft Pick Tie

If the divisional or conference tiebreakers are not applicable, or ties
still exist between teams of different conferences, ties will be broken
by the following procedure:

Ties involving THREE OR MORE clubs from different conferences will be
broken by applying

1.  divisional tiebreakers to determine the lowest-ranked team in a
    division,
2.  conference tiebreakers to determine the lowest-ranked team within a
    conference.

After this process, there can only be 2 inter-conference teams
participating in the following steps (draft pick assignment is reversed
to division or conference ties. Worse teams get earlier picks):

1.  Head-to-head, if applicable. The loser gets the earlier pick.
2.  Worst won-lost-tied percentage in common games (minimum of four).
3.  Worst Strength of victory in all games.
4.  Worst combined ranking among all teams in points scored and points
    allowed in all games.
5.  Worst net points in all games.
6.  Worst net touchdowns in all games.
7.  Coin toss.

------------------------------------------------------------------------

1.  Yes, you are reading right. And the difference is huge when you try
    to efficiently code the process, smh. NFL Football Ops prefaces the
    wildcard tiebreaking process for 3 or more clubs with *Note: If two
    clubs remain tied after a third club or other clubs are eliminated,
    the tiebreaker reverts to step 1 of the applicable two-club format.*
    NFL.com adds a second - very important - sentence to this, which is
    *If three clubs remain tied after a fourth club is eliminated during
    any step, tiebreaker restarts at Step 2 of three-club format.*
    That’s a big difference because the first part of that section
    sounds like we are only allowed to restart the process if two clubs
    remain tied. But we actually have to restart every time a team is
    eliminated. That’s good news because it is easier to code.
