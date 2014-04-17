#################################
##                             ##
##      HOME-ICE SIMULATOR     ##
##                             ##
##          Joe Walsh          ##
## https://github.com/jtwalsh0 ##
##        April 17, 2014       ##
##                             ##
#################################


## This script uses simulation to estimate the probability of one team beating
## another in a seven-game series.  Many observers argue that playing the majority
## of games on one's own ice significantly increases the probability of winning 
## the series; for example, see Nicholas Goss's article in Bleacher Report
## (http://bleacherreport.com/articles/1630882-how-much-does-home-ice-matter-in-the-stanley-cup-playoffs).
## It is true that teams with home-ice advantage tend to win their series, but they 
## also tend to get there by being the better team.  How important is home-ice
## advantage once we control for the relative quality of the teams?
##
## The script's user chooses three inputs: 
##   - p.teamA is the probability that team A
##   - p.home.ice is the increased probability that team A beats team B on team A's
##     ice and the probability that team B beats team A on team B's ice.  The 
##     default is 0.04, which was the single-game home-ice advantage in 2013 (see
##     http://www.sportingcharts.com/nhl/stats/team-home-and-away-winning-percentages/2013/).
##   - n.simulations is the number of series we would like to simulate for each p.teamA.
##     The more simulations we use, the more precise our estimates will be but the
##     longer the estimate takes to finish.
##
## The script plots the results.


# Set the seed for replicability
set.seed(12345)


# Create a function that takes the probability of team A winning a game on neutral ice, how much 
# more likely home teams are to winning than if they were playing on neutral ice, and the number
# of simulations we want for each set of p.teamA as inputs.
function.probability.of.winning.series <- function(p.teamA=.5, p.home.ice=.04, n.simulations=1000){

  # Matrix to store how many games each team won in a simulated series.
  # Each team starts with zero wins.
  series.results <- matrix(0, nrow=n.simulations, ncol=2)
    colnames( series.results ) <- c("TeamA", "TeamB")

  # At the extremes (e.g. probability of sharks winning is .01 and home-ice
  # advantage is .03), the probabilities can top 1 or drop below 0.  Fix that.
  with.home.ice <- p.teamA + p.home.ice
    if( with.home.ice < 0 )  with.home.ice <- 0
    if( with.home.ice > 1 )  with.home.ice <- 1
  without.home.ice <- p.teamA - p.home.ice
    if( without.home.ice < 0 )  without.home.ice <- 0
    if( without.home.ice > 1 )  without.home.ice <- 1
    
  for(i in 1:n.simulations){
  
    # Cycle until a team wins four games
    while( (series.results[i,1] < 4)  &  (series.results[i,2] < 4) ){
            
      if( sum(series.results[i,]) < 2 ){  # First two games played at teamA
        teamA.wins.the.game <- rbinom(n=1, size=1, prob=with.home.ice)
        if( teamA.wins.the.game == 1 ) series.results[i,1] <- series.results[i,1] + 1  else  series.results[i,2] <- series.results[i,2] + 1
      } else if( sum(series.results[i,]) >= 2  &  sum(series.results[i,]) < 4){  # Next two games at teamB
        teamA.wins.the.game <- rbinom(n=1, size=1, prob=without.home.ice)
        if( teamA.wins.the.game == 1 ) series.results[i,1] <- series.results[i,1] + 1  else  series.results[i,2] <- series.results[i,2] + 1
      } else if( sum(series.results[i,]) == 4){  # Fifth game at teamA
        teamA.wins.the.game <- rbinom(n=1, size=1, prob=with.home.ice)
        if( teamA.wins.the.game == 1 ) series.results[i,1] <- series.results[i,1] + 1  else  series.results[i,2] <- series.results[i,2] + 1
      } else if( sum(series.results[i,]) == 5){  # Sixth game at teamB
        teamA.wins.the.game <- rbinom(n=1, size=1, prob=without.home.ice)
        if( teamA.wins.the.game == 1 ) series.results[i,1] <- series.results[i,1] + 1  else  series.results[i,2] <- series.results[i,2] + 1
      } else if( sum(series.results[i,]) == 6){  # Seventh game at teamA
        teamA.wins.the.game <- rbinom(n=1, size=1, prob=with.home.ice)
        if( teamA.wins.the.game == 1 ) series.results[i,1] <- series.results[i,1] + 1  else  series.results[i,2] <- series.results[i,2] + 1
      }
        
    }
  
  }

  # series.results==4 creates a table of TRUE values where the cell equals 4
  # and FALSE values where it does not.  R treats TRUE as 1 and FALSE as 0 in 
  # arithmetic operations -- go ahead, type TRUE + TRUE and see what you get --
  # so I use the mean function and apply to get the proportion of series wins
  # for each team.
  return( apply(series.results==4, 2, mean) )

}



# Matrix to store results for home-ice advantage of .00 (no advantage)
matrix.probability.of.winning.series.00 <- matrix(NA, nrow=101, ncol=2)
  rownames(matrix.probability.of.winning.series.00) <- paste("p = ", 0:100/100, sep="")
  colnames(matrix.probability.of.winning.series.00) <- c("teamA", "teamB")

# Matrix to store results for home-ice advantage of .04
matrix.probability.of.winning.series.04 <- matrix(NA, nrow=101, ncol=2)
  rownames(matrix.probability.of.winning.series.04) <- paste("p = ", 0:100/100, sep="")
  colnames(matrix.probability.of.winning.series.04) <- c("teamA", "teamB")

for(i in 1:101){
  
  # Print the value of p.teamA so the user knows much progress the computer has made
  print((i-1)/100)
  
  # Store the results of no home-ice advantage in the appropriate matrix
  matrix.probability.of.winning.series.00[i,] <- function.probability.of.winning.series( p.teamA=(i-1)/100, p.home.ice=.00, n.simulations <- 1000 )
  
  # Store the results of home-ice advantage (4% is the default)
  matrix.probability.of.winning.series.04[i,] <- function.probability.of.winning.series( p.teamA=(i-1)/100, p.home.ice=.04, n.simulations <- 1000 )

}


# Plot the results
png("home-ice advantage.png")
  
  plot(x=c(0,1), y=c(0,1), type='n', axes=FALSE, 
       main="Home-Ice Advantage",
       xlab="Pr(Team A beats Team B on neutral ice)",
       ylab="Pr(Team A beats Team B in a seven-game series)")
  
  axis(1, at=0:10/10, labels=0:10/10)
  axis(2)
  
  points(0:100/100, matrix.probability.of.winning.series.00[,1], type='l', col='black')
  points(0:100/100, matrix.probability.of.winning.series.04[,1], type='l', col='red')
  
  legend('topleft', lwd=2, col=c("black","red"), legend=c("no advantage", "home ice advantage 4%"))

dev.off()
