home_ice_advantage
==================

A script that uses simulation to estimate the effect home-ice advantage has in a seven-game playoff series.

This script uses simulation to estimate the probability of one team beating another in a seven-game series.  Many observers argue that playing the majority of games on one's own ice significantly increases the probability of winning the series; for example, see Nicholas Goss's article in [Bleacher Report](http://bleacherreport.com/articles/1630882-how-much-does-home-ice-matter-in-the-stanley-cup-playoffs).  It is true that teams with home-ice advantage tend to win their series, but they also tend to get there by being the better team.  How important is home-ice advantage once we control for the relative quality of the teams?

The script's user chooses three inputs: 
 1.  p.teamA is the probability that team A
 2.  p.home.ice is the increased probability that team A beats team B on team A's ice and the probability that team B beats team A on team B's ice.  The default is 0.04, which was the single-game home-ice advantage in 2013 ([citation](http://www.sportingcharts.com/nhl/stats/team-home-and-away-winning-percentages/2013/)).
 3.  n.simulations is the number of series we would like to simulate for each p.teamA.  The more simulations we use, the more precise our estimates will be but the longer the estimate takes to finish.

The script plots the results.

