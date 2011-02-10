# ftp://ftp.ncdc.noaa.gov/pub/data/paleo/drought/NAmericanDroughtAtlas.v2/NADAv2a.txt
test <- matrix(scan("data/NADAv2a.txt",skip=1), ncol=287,byrow=T)

years <- test[,1]
test <- test[,-1]
gridpoints <- c(102,103,117,118,131,132)

#210 - 2006 complete data at all the six grid points
pdsi.ts <- ts(test[211:length(years),gridpoints],start=210,frequency=1)