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
## another in a seven-game series and to plot the results.  The user can choose the
## following inputs: 
##   - teamA.name is the name of the team with home-ice advantage (7th game played there)
##   - teamB.name is the name of the team without home-ice advantage
##   - p.teamA is the probability that team A beats team B on neutral ice
##   - p.home.ice is the increased probability that team A beats team B on team A's
##     ice and the probability that team B beats team A on team B's ice.  The 
##     default is 0.04, which was the single-game home-ice advantage in 2013 (see
##     http://www.sportingcharts.com/nhl/stats/team-home-and-away-winning-percentages/2013/).
##   - how.many.wins.teamA.has enables the user to simulate from the middle of a series, 
##     e.g. team A starts with 0, 1, 2, 3, or 4 wins.
##   - how.many.wins.teamB.has enables the user to simulate from the middle of a series, 
##     e.g. team B starts with 0, 1, 2, 3, or 4 wins.
##   - n.simulations is the number of series we would like to simulate for each p.teamA.
##     The more simulations we use, the more precise our estimates will be but the
##     longer the estimate takes to finish.
##
## The script creates a barplot of the results.


# Set the seed for replicability
set.seed(12345)


# Create a function that takes the probability of team A winning a game on neutral ice, how much 
# more likely home teams are to winning than if they were playing on neutral ice, and the number
# of simulations we want for each set of p.teamA as inputs.
function.probability.of.winning.series <- function(p.teamA=.5, 
                                                   p.home.ice=.04, 
                                                   how.many.wins.teamA.has=0,
                                                   how.many.wins.teamB.has=0,
                                                   n.simulations=1000){

  # Matrix to store how many games each team won in a simulated series.
  # Each team starts with zero wins.
  series.results <- matrix(0, nrow=n.simulations, ncol=2)
    colnames( series.results ) <- c("TeamA", "TeamB")
  
    # We can simulate from the current state of the series, e.g. 0 games to 0, 1 game to 0,
    # 3 games to 1, 1 game to 3.
    series.results[,1] <- how.many.wins.teamA.has
    series.results[,2] <- how.many.wins.teamB.has
    
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
  #return( apply(series.results==4, 2, mean) )
  
  # Return a list with series outcomes
    list( teamA.4games_teamB.0games = mean( series.results[,1]==4 & series.results[,2]==0 ),
          teamA.4games_teamB.1games = mean( series.results[,1]==4 & series.results[,2]==1 ),
          teamA.4games_teamB.2games = mean( series.results[,1]==4 & series.results[,2]==2 ),
          teamA.4games_teamB.3games = mean( series.results[,1]==4 & series.results[,2]==3 ),
          teamA.3games_teamB.4games = mean( series.results[,1]==3 & series.results[,2]==4 ),
          teamA.2games_teamB.4games = mean( series.results[,1]==2 & series.results[,2]==4 ),
          teamA.1games_teamB.4games = mean( series.results[,1]==1 & series.results[,2]==4 ),
          teamA.0games_teamB.4games = mean( series.results[,1]==0 & series.results[,2]==4 )
          )

}






## Plot relative frequency of series outcomes (sweeps, 4 games to 1, 3 games to 4, etc.)
plot_series_results <- function(teamA.name="teamA", 
                                teamB.name="teamB",
                                plot_function_p.teamA=.5, 
                                plot_function_p.home.ice=.04, 
                                plot_function_how.many.wins.teamA.has=0, 
                                plot_function_how.many.wins.teamB.has=0, 
                                plot_function_n.simulations=1000){
  
  # Call the simulation function
  simulated_results <- function.probability.of.winning.series(p.teamA=plot_function_p.teamA,
                                                              p.home.ice=plot_function_p.home.ice, 
                                                              how.many.wins.teamA.has=plot_function_how.many.wins.teamA.has, 
                                                              how.many.wins.teamB.has=plot_function_how.many.wins.teamB.has, 
                                                              n.simulations=plot_function_n.simulations)
  
  pr.teamA.wins.series <- round( sum(unlist(simulated_results)[1:4]), 2)
  # Barplot the simulated results
  barplot(unlist(simulated_results),
          names.arg=c("4-0", "4-1", "4-2", "4-3", "3-4", "2-4", "1-4", "0-4"),
          main=paste(teamA.name, "-", teamB.name, "Simulated Series"),
          sub=paste("Pr(" , teamA.name, " wins the series) = ", pr.teamA.wins.series, sep=""),
          xlab=paste(teamA.name, "wins -", teamB.name, "wins", sep=" "),
          ylab="estimated probability")

}



# Example: Boston-Detroit series before game 1
plot_series_results(teamA.name="Boston", 
                    teamB.name="Detroit", 
                    plot_function_p.teamA=.62, 
                    plot_function_p.home.ice=.04, 
                    plot_function_how.many.wins.teamA.has=0, 
                    plot_function_how.many.wins.teamB.has=0, 
                    plot_function_n.simulations=10000)
