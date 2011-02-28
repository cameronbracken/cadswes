#!/usr/bin/env Rscript

dl <- T
na.strings = "-999.999"

library(XML)

base <- "http://www.esrl.noaa.gov/psd/cgi-bin/data/timeseries/timeseries.pl?"
args <- list()
args[["ntype"]] <- "1" # dont change
args[["Submit"]] <- "Create+Timeseries"
args[["typeout"]] <- "1"
args[["iseas"]] <- "1"
args[["iarea"]] <- "0"
args[["level"]] <- "500"
#args[["var"]] <- "Geopotential+Height"
path <- file.path('.')
datapath <- file.path('..','..')
raw <- 'raw'
processed <- 'processed'

if(!file.exists(raw))
    dir.create(raw)

if(!file.exists(processed))
    dir.create(processed)

vars <- c("Geopotential+Height", "Zonal+Wind", "Meridonal+Wind", "SST")
var_short <- c("gph", "znw", "mdw", "sst")
regions <- read.table(file.path(datapath,'box_coords.dat'),header=T)
lf.ts <- structure(c(16933581, 13140416, 12505894, 20805422, 11165419, 
8496102, 9413908, 11426874, 21500963, 15862511, 9598169, 11524160, 
10010259, 17377609, 8840900, 10863586, 19875027, 10679844, 11670830, 
13739932, 15272159, 15344136, 15461094, 13164269, 18689788, 13326340, 
17050056, 11237398, 5442752, 15349440, 17949858, 17860022, 8945976, 
17530325, 24426521, 25425745, 20949407, 23187151, 15579800, 11441587, 
10014874, 9552890, 12244383, 10882629, 18418487, 10600233, 20425637, 
14580049, 21758137, 16802075, 16216071, 10859813, 10780741, 6222118, 
10548586, 9836735, 16951350, 13012891, 11232533), .Tsp = c(1949, 
2007, 1), class = "ts")


for(i in 1:nrow(regions)){
    
        # file names for this predictor
    this.var <- vars[regions$Variable[i]]
    this.region <- c('pos','neg')[regions$Region[i]+1]
    prefix <- paste(month.abb[regions$SeasonStart[i]],
        month.abb[regions$SeasonEnd[i]],this.var,this.region,sep='_')
    rawfile <- file.path(path,raw,paste(prefix,'html',sep='.'))
    processedfile <- file.path(path,processed,paste(prefix,'dat',sep='.'))
        
    cat(prefix,'\n')
        # creat the components of the GET url
    url <- base
    for(j in 1:length(args)){
        url <- 
        if(names(args)[j]=='level' & this.var=="SST")
            paste(url,'&',names(args)[j],'=',"2000",sep='')
        else
            paste(url,'&',names(args)[j],'=',args[j],sep='')
    }

    url <- paste(url,'&','lat1=',regions$lat2[i],sep='')
    url <- paste(url,'&','lat2=',regions$lat1[i],sep='')    
    url <- paste(url,'&','lon1=',regions$lon1[i],sep='')
    url <- paste(url,'&','lon2=',regions$lon2[i],sep='')
    url <- paste(url,'&','mon1=',regions$SeasonStart[i]-1,sep='')
    url <- paste(url,'&','mon2=',regions$SeasonEnd[i]-1,sep='')
    url <- paste(url,'&','var=',this.var,sep='')
    
        # (possibly) download and read the data from the html file
    if(dl) download.file(url,rawfile,quiet=F)
    o <- htmlTreeParse(rawfile,useInternalNodes = TRUE)
        # then spit is back out in a form R can read easily
    cat(xpathApply(o,'//pre',xmlValue)[[1]],file=processedfile)
    years <- scan(processedfile,n=2,quiet=T)
    
        # initialize the matrix to store the data
    if(i ==1 ){
        predictors <- matrix(NA,ncol=nrow(regions),nrow=diff(years)+1)
        colnames(predictors) <- 1:nrow(regions)
    }
        
        #read back in the data 
    this.data <- read.table(processedfile,header=T,nrows=diff(years)+1,
        na.strings = "-999.999")[,2]
    leadtime <- tolower(ifelse(regions$SeasonEnd[i] == 12, 
        month.abb[1],month.abb[regions$SeasonEnd[i]+1]))
        
        #Need to lag some predictors by 1 relative to the flow since 
        # for example the 1948 gph predicts the 1949 flow volume for the 
        # january lead time.  This means we cant use the 1948 data for all 
        # predictors. 
    if(leadtime =='jan' ||leadtime =='nov')
        predictors[2:(diff(years)+1),i] <- this.data[1:diff(years)]
    else
        predictors[,i] <- this.data
        
        # dont use the first year if the lead time is november or january
    if(!any(regions$SeasonEnd[i] != c(9,12))) predictors[1,i] <- NA
        # set the column name of this predictor
    colnames(predictors)[i] <- paste(var_short[regions$Variable[i]],
        leadtime,this.region,sep='_')
    
}
    # set the row names
rownames(predictors) <- 0:diff(years) + 1948

    # dont use the forst year of data since some timeseries are lagged
raw_predictors <- predictors[2:nrow(predictors),]
    # initilize the matric to hold differenced predictors
raw_predictors_diff <- matrix(NA,nrow=nrow(raw_predictors),
    ncol=ncol(raw_predictors)/2)
colnames(raw_predictors_diff) <- 1:(ncol(raw_predictors)/2)
b <- 0

    # difference between the positive and negative regions
for(i in seq(2,ncol(raw_predictors),by=2)){
    b <- b + 1
    raw_predictors_diff[,b] <- raw_predictors[,i] - raw_predictors[,i-1]
    c1 <- cor(raw_predictors[1:length(lf.ts),i],lf.ts)
    c2 <- cor(raw_predictors[1:length(lf.ts),i-1],lf.ts)
    c3 <- cor(raw_predictors_diff[1:length(lf.ts),b],lf.ts)
    #if(abs(c1) > abs(c3))
    #    raw_predictors_diff[,b] <- raw_predictors[,i]
    #if(abs(c2) > abs(c1))
    #    raw_predictors_diff[,b] <- raw_predictors[,i-1]
        
    colnames(raw_predictors_diff)[b] <- substr(colnames(raw_predictors)[i],1,7)
}
rownames(raw_predictors_diff) <- 0:(diff(years)-1) + 1949

w <- options()$width
options('width'=2000)
sink(paste(file.path(datapath,'climate_predictors_crb.txt'),sep=''))
print(raw_predictors_diff)
sink()
options('width'=w)

#lat1=42&lat2=40&lon1=137&lon2=140&iseas=1&mon1=9&mon2=9
