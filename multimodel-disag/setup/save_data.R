# This file reads in all the necessary data for the multimodel framework and 
# saves it in an RData file 

    #file of predictors 1949:2005
predictorfile <- "data/climate_predictors_crb.txt"
swefile <- "data/swe_crb.txt"
pdsifile <- "data/pdsi_crb.txt"
datafile <- "data/natural-flows-intervening-ac-ft.csv"
year.s.flow <- 1906
year.e.flow <- 2007


site.names <- c("Colorado River At Glenwood Springs, CO", 
"Colorado River Near Cameo, CO", 
"Taylor River Below Taylor Park Reservoir, CO", 
"Gunnision River Above Blue Mesa Reservoir, CO", 
"Gunnison River At Crystal Reservoir, CO", 
"Gunnison River Near Grand Junction, CO", 
"Dolores River Near Cisco, UT", "Colorado River Near Cisco, UT", 
"Green R Bel Fontenelle Res, WY", "Green R. Nr Green River, WY", 
"Green River Near Greendale, UT", "Yampa River Near Maybell, CO", 
"Little Snake River Near Lily, CO", "Duchesne River Near Randlett, UT", 
"White River Near Watson, UT", "Green River At Green River, UT", 
"San Rafael River Near Green River, UT", "San Juan River Near Archuleta, NM", 
"San Juan River Near Bluff, UT", "Colorado R At Lees Ferry, AZ")

usgs.num <- structure(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", 
"11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "09072500", 
"09095500", "09109000", "09124700", "09127800", "09152500", "09180000", 
"09180500", "09211200", "09217000", "09234500", "09251000", "09260000", 
"09302000", "09306500", "09315000", "09328500", "09355500", "09379500", 
"09380000"), .Dim = c(20L, 2L), .Dimnames = list(NULL, c("Node Number", 
"USGS Gauge Number")))
rownames(usgs.num) <- site.names

time.names <- month.name[if(seasonal) 4:7 else 1:12]

predictors <- get.predictors(predictorfile, swefile, pdsifile, predmonth)#input file
year.s <- start(predictors)[1]
year.e <- end(predictors)[1]

nat.mon.intv <- read.csv(datafile,skip=3)

i2t <- as.matrix(read.table('data/i2t.txt'))
t2i <- as.matrix(read.table('data/t2i.txt'))
nat.mon.tot <- intervening2total(nat.mon.intv,i2t)

n.sites <- ncol(nat.mon.intv)
n.years <-  min(year.e,year.e.flow) - year.s + 1
n.months <- 12


	# Create a time series object to easly crop data
nat.mon.intv.ts <- ts(nat.mon.intv,start=c(year.s.flow,1),frequency=n.months)
nat.mon.intv.ts <- window( nat.mon.intv.ts, start=c(year.s,1) , 
    end = c(min(year.e,year.e.flow), n.months))
nat.mon.tot.ts <- ts(nat.mon.tot,start=c(year.s.flow,1),frequency=n.months)
nat.mon.tot.ts <- window( nat.mon.tot.ts, start=c(year.s,1) , 
    end = c(min(year.e,year.e.flow), n.months))

	# Aggregated annual series at each site, sum because data is flow volume
nat.ann.intv <- wapply(nat.mon.intv.ts,sum,12)
nat.ann.tot <- intervening2total(nat.ann.intv,i2t)

	# aggregated interveneing flows, summing the ac-ft/month gives ac-ft/year
	# basically equivalent to total flow at lees ferry in
	#  not sure why its slightly off from the natural flow (rounding error?)
nat.ann.tot.agg <- ts(apply(nat.ann.intv,1,sum),start=year.s,frequency=1)
#nat.ann.tot <- window(nat.ann.tot,start=year.s,end=year.e)

	#array for obs common era data
	# 3d array: years in rows, months in columns, sites in layers
historical.intv <- historical.tot <- array(data=NA, dim=c(n.years, n.months, n.sites))

for(j in 1:n.sites){
	historical.intv[,,j] <- matrix(nat.mon.intv.ts[,j], nrow=n.years, 
	    ncol=n.months, byrow=T)/10^6
    historical.tot[,,j] <- matrix(nat.mon.tot.ts[,j], nrow=n.years, 
	    ncol=n.months, byrow=T)/10^6
}

historical.tot.full <- historical.tot
historical.intv.full <- historical.intv

if(seasonal){
    historical.tot <- historical.tot[,4:7,]
    historical.intv <- historical.intv[,4:7,]
}

historical <- if(intervening) historical.intv else historical.tot

    # flow in MAF/year, sum of all months and sites compose each annual value
response <- ts(rowSums(historical),start = year.s, frequency=1)

save(predictors, response, historical, historical.intv, historical.tot, 
    historical.intv.full, historical.tot.full, 
    n.sites, n.years, n.months, year.s, t2i, i2t,
    year.e, site.names, time.names, file='data/multimodel.RData')
