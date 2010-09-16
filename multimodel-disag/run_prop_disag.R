source("functions/skew.R")
source("functions/sim_knn.R")
source('functions/wapply.R')
require(lubridate)
#######get data and format

year.s <- 1906
nsim <- 1000 # number of simulations	
	# monthly observed inteveneing natural flows at all 
	# natural flow nodes in upper basin
nat.mon <- read.csv('data/natural-flows-intervening-ac-ft.csv',skip=3)

#	#get the days in each month including leap years
#years <- year.s:(year.s+nrow(nat.mon)/12-1)
#days.in.month <- c(31,28,31,30,31,30,31,31,30,31,30,31)
#days.in.month <- rep(days.in.month,length(years))
#days.in.month[(years-year.s)[leap.year(years)]*12 + 2] <- 29
#
#	# convert acre-feet/month to cfs
#nat.mon <- nat.mon/days.in.month*24*60*60/43560
#
	# Create a time series object
nat.mon.ts <- ts(nat.mon,start=c(year.s,1),frequency=12)
lf <- stack.ts(nat.mon.ts[,"Colorado.R.At.Lees.Ferry..AZ"])

	# number of sites
n.sites <- ncol(nat.mon.ts)
	# number of times (months)
n.months <- frequency(nat.mon.ts)
	# Aggregated annual series at each site
nat.ann <- wapply(nat.mon.ts,sum,12)
	# aggregated interveneing flows, summing the ac-ft/month gives ac-ft/year
	# basically equivalent to total flow at lees ferry in
	#  not sure why its slightly off from the natural flow (rounding error?)
nat.ann.tot <- ts(apply(nat.ann,1,sum),start=year.s,frequency=1) 
n.years <- length(time(nat.ann.tot))

	#array for obs common era data
	# 3d array: years in rows, months in columns, sites in layers
historical <- array(data=NA, dim=c(n.years, n.months, n.sites))

for(j in 1:n.sites)
	historical[,,j] <- matrix(nat.mon.ts[,j], nrow=n.years, ncol=n.months, byrow=T)
	
# Use this to check if the values accutally sum to the totals
# all.equal(apply(historical,1,sum),as.vector(nat.ann.tot))

# do the disag 
pd <- pdisag(historical)
d <- disag(pd,plot=F)

