nsi <- 4 # states
q <- 1 #years back
r <- 1 #years ahead
m <- 1 #running mean length
nsims <- 250

plotdir <- 'plots'
dataset <- 'meko' # 'meko' or 'woodhouse' or 'nino3'
  # dont regenerate the transition information if it exits, 
  # just load it from a data file
cache <- TRUE
hist.cdf <- 'gamma' # 'ecdf' or 'gamma'
transition <- 'knn' # knn or locfit or gamma
back.weight <- 'equal' # equal or flow
seasonal.total <- TRUE
running.mean <- TRUE


#Paramters to implement
mode <- 'drop-one' # or 'retroactive'
conditional.pools <- FALSE
scaled <- FALSE



for(ns in nsi){
  source('second-year.R')
}
