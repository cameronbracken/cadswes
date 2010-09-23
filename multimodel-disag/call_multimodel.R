    
suppressMessages(require(e1071)) # for skewness function
source("functions/libmm.R")
source("functions/selectpredictors.R")
source("functions/multimodel.R")
source("functions/wapply.R")
source("functions/proportion_disag.R")

nsims <- 1000
predmonth <- "apr"
outputfile = 'pred.out'

    #file of predictors 1949:2005
predictorfile <- "data/allpredictors.txt"
datafile <- "data/natural-flows-intervening-ac-ft.csv"

predictors <- get.predictors(predictorfile,predmonth)#input file
year.s <- start(predictors)[1]
year.e <- end(predictors)[1]

nat.mon <- read.csv(datafile,skip=3)
year.s.flow <- 1906

	# Create a time series object
nat.mon.ts <- ts(nat.mon,start=c(year.s.flow,1),frequency=12)
nat.mon.ts <- window( nat.mon.ts, start=c(year.s,1) , end = c(year.e,12))
n.sites <- ncol(nat.mon.ts)
n.years <- length(time(predictors))
n.months <- frequency(nat.mon.ts)

	# Aggregated annual series at each site
nat.ann <- wapply(nat.mon.ts,sum,12)
	# aggregated interveneing flows, summing the ac-ft/month gives ac-ft/year
	# basically equivalent to total flow at lees ferry in
	#  not sure why its slightly off from the natural flow (rounding error?)
nat.ann.tot <- ts(apply(nat.ann,1,sum),start=year.s.flow,frequency=1)
#nat.ann.tot <- window(nat.ann.tot,start=year.s,end=year.e)

	#array for obs common era data
	# 3d array: years in rows, months in columns, sites in layers
historical <- array(data=NA, dim=c(n.years, n.months, n.sites))

for(j in 1:n.sites)
	historical[,,j] <- matrix(nat.mon.ts[,j], nrow=n.years, 
	    ncol=n.months, byrow=T)/10^6

    # flow in MAF/year
response <- nat.ann.tot/10^6

calibration <- selectpredictors(predictors,response)
pred <- multimodel(calibration, predictors, response, predictors, nsims )

pd <- pdisag(historical, sims=pred, nsim=nsims)
d <- disag(pd,plot=T)
plot(d,)