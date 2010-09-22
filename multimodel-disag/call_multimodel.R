    # for skewness function
require(e1071)
require(leaps)
source("functions/libmm.R")
source("functions/selectpredictors.R")
source("functions/multimodel.R")
source("functions/wapply.R")

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

	# Aggregated annual series at each site
nat.ann <- wapply(nat.mon.ts,sum,12)
	# aggregated interveneing flows, summing the ac-ft/month gives ac-ft/year
	# basically equivalent to total flow at lees ferry in
	#  not sure why its slightly off from the natural flow (rounding error?)
nat.ann.tot <- ts(apply(nat.ann,1,sum),start=year.s.flow,frequency=1)
nat.ann.tot <- window(nat.ann.tot,start=year.s,end=year.e)

    # flow in MAF/year
response <- nat.ann.tot/10^6

calibration <- selectpredictors(predictors,response,verbose=1)


pred <- multimodel(calibration, predictors, response, predictors, 
            nrow(predictors), nsims, outputfile)
#
#write(t(pred),file=outputfile,ncolumns=nsims)
#