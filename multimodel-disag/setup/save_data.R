    #file of predictors 1949:2005
predictorfile <- "data/allpredictors.txt"
datafile <- "data/natural-flows-intervening-ac-ft.csv"
year.s.flow <- 1906

site.names <- c("Colorado River At Glenwood Springs, CO", 
"Colorado River Near Cameo, CO", 
"Taylor River Below Taylor Park Reservoir, CO", 
"Gunnision River Above Blue Mesa Reservoir,CO", 
"Gunnison River At Crystal Reservoir,CO", 
"Gunnison River Near Grand Junction, CO", 
"Dolores River Near Cisco, UT", "Colorado River Near Cisco UT", 
"Green R Bel Fontenelle Res WY", "Green R. Nr Green River, WyY", 
"Green River Near Greendale, UT", "Yampa River Near Maybell, CO", 
"Little Snake River Near Lily, CO", "Duchesne River Near Randlett, UT", 
"White River Near Watson, UT", "Green River At Green River, UT", 
"San Rafael River Near Green River, UT", "San Juan River Near Archuleta,NM", 
"San Juan River Near Bluff, UT", "Colorado R At Lees Ferry, AZ")

predictors <- get.predictors(predictorfile, predmonth)#input file
year.s <- start(predictors)[1]
year.e <- end(predictors)[1]

nat.mon <- read.csv(datafile,skip=3)

i2t <- as.matrix(read.table('data/i2t.txt'))
nat.mon.tot <- intervening2total(nat.mon,i2t)

	# Create a time series object
nat.mon.intv.ts <- ts(nat.mon,start=c(year.s.flow,1),frequency=12)
nat.mon.intv.ts <- window( nat.mon.intv.ts, start=c(year.s,1) , end = c(year.e,12))
n.sites <- 20 #ncol(nat.mon.intv.ts)
n.years <- 57# length(time(predictors))
n.months <- 12# frequency(nat.mon.intv.ts)

	# Aggregated annual series at each site
nat.ann.intv <- wapply(nat.mon.intv.ts,sum,12)
	# aggregated interveneing flows, summing the ac-ft/month gives ac-ft/year
	# basically equivalent to total flow at lees ferry in
	#  not sure why its slightly off from the natural flow (rounding error?)
nat.ann.tot <- ts(apply(nat.ann.intv,1,sum),start=year.s,frequency=1)
#nat.ann.tot <- window(nat.ann.tot,start=year.s,end=year.e)

	#array for obs common era data
	# 3d array: years in rows, months in columns, sites in layers
historical.intv <- historical.tot <- array(data=NA, dim=c(n.years, n.months, n.sites))

for(j in 1:n.sites){
	historical.intv[,,j] <- matrix(nat.mon[,j], nrow=n.years, 
	    ncol=n.months, byrow=T)/10^6
    historical.tot[,,j] <- matrix(nat.mon.tot[,j], nrow=n.years, 
	    ncol=n.months, byrow=T)/10^6
}

if(seasonal){
    historical.tot <- historical.tot[,4:7,]
    historical.intv <- historical.intv[,4:7,]
}

historical <- if(intervening) historical.intv else historical.tot

    # flow in MAF/year, sum of all months and sites compose each annual value
response <- rowSums(historical)

save(predictors, response, historical, n.sites, n.years, n.months, year.s, 
    year.e, site.names, file='data/multimodel.RData')