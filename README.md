home_ice_advantage
==================

How much of a difference does home-ice advantage make in a playoff series?  Teams with home-ice advantage tend to win their series, but they also tend to get home-ice advantage by being the better team.  This script estimates and plots the probability that the team with home-ice advantage wins the series given three user inputs:

 1.  `p.teamA` is the probability that team A
 2.  `p.home.ice` is the increased probability that team A beats team B on team A's ice and the probability that team B beats team A on team B's ice.  The default is 0.04, which was the single-game home-ice advantage in 2013 ([citation](http://www.sportingcharts.com/nhl/stats/team-home-and-away-winning-percentages/2013/)).
 3.  `n.simulations` is the number of series we would like to simulate for each p.teamA.  The more simulations we use, the more precise our estimates will be but the longer the estimate takes to finish.

Here are the results:

![Home-ice advantage](https://raw.githubusercontent.com/jtwalsh0/home_ice_advantage/master/home-ice%20advantage.png)
