#!/usr/bin/env Rscript
#http://www.esrl.noaa.gov/psd/data/timeseries/

library(XML)
dl <- T
raw <- 'raw'
processed <- 'processed'

if(!file.exists(raw))
    dir.create(raw)

if(!file.exists(processed))
    dir.create(processed)

# colorado 5 - 2
# utah 42 - 4, 5, 6, 7
# wyoming 48 - 3
divs <- list(
    colorado=list(state="++5",div=2),
    utah=list(state="+42",div=c(4,5,6,7)),
    wyoming=list(state="+48",div=3))
ndiv <- 6
#http://www.esrl.noaa.gov/psd/cgi-bin/data/timeseries/timeseries.pl?ntype=2&typediv=3&state=++5&averaged=11&division=2&year1=1948&year2=2011&anom=0&iseas=0&mon1=0&mon2=0&typeout=1&y1=&y2=&plotstyle=0&Submit=Create+Timeseries
#http://www.esrl.noaa.gov/psd/cgi-bin/data/timeseries/timeseries.pl?ntype=2&typediv=3&state=+42&averaged=11&division=4&year1=1948&year2=2011&anom=0&iseas=0&mon1=0&mon2=0&typeout=1&y1=&y2=&plotstyle=0&Submit=Create+Timeseries
#http://www.esrl.noaa.gov/psd/cgi-bin/data/timeseries/timeseries.pl?ntype=2&typediv=3&state=+48&averaged=11&division=3&year1=1948&year2=2011&anom=0&iseas=0&mon1=0&mon2=0&typeout=1&y1=&y2=&plotstyle=0&Submit=Create+Timeseries
#files <- list.files(,'*.dat')
ave <- c(1,9)
y1 <- 1948
y2 <- 2011

i <- 0
for(s in 1:length(divs)){
    for(j in 1:length(divs[[s]]$div)){
        i <- i + 1
        this.div <- divs[[s]]$div[j]
        url <- "http://www.esrl.noaa.gov/psd/cgi-bin/data/timeseries/timeseries.pl?ntype=2"
        url <- paste(url,"typediv=3",paste("state=",divs[[s]]$state,sep=''),"averaged=11",
            paste("division=",this.div,sep=''),paste("year1=",y1,sep=''),
            paste("year2=",y2,sep=''),"anom=0","iseas=0","mon1=0","mon2=0",
            "typeout=1","y1=","y2=","plotstyle=0","Submit=Create+Timeseries",sep='&')
        
        rawfile <- file.path(raw,paste(names(divs)[s],this.div,'raw.txt',sep='_'))
        processedfile <- file.path(processed,paste(names(divs)[s],'_',this.div,'.txt',sep=''))
        
        if(dl) download.file(url,rawfile,quiet=F)
        o <- htmlTreeParse(rawfile,useInternalNodes = TRUE)
            # then spit is back out in a form R can read easily
        cat(xpathApply(o,'//pre',xmlValue)[[1]],file=processedfile)
        years <- scan(processedfile,n=2,quiet=T)
    
            # initialize the matrix to store the data
        if(i ==1 ){
            #predictors <- matrix(NA,ncol=nrow(regions),nrow=diff(years)+1)
            #colnames(predictors) <- 1:nrow(regions)
            pdsi.mat.nov <- pdsi.mat.jan <- pdsi.mat.feb <- pdsi.mat.apr <- 
                matrix(NA,nrow=diff(years),ncol=ndiv)
        }
        
            #read back in the data 
        #this.data <- read.table(processedfile,header=T,nrows=diff(years)+1,
        #    na.strings = "-999.999")[,2]
        data <- as.matrix(read.table(processedfile,skip=2,
                nrows=diff(years)+1,na.strings = "-999.999")[,-1])
        for(k in 2:(diff(years)+1)){
            pdsi.mat.nov[k-1,i] <- mean(data[k-1,10])
            pdsi.mat.jan[k-1,i] <- mean(data[k-1,12])
            pdsi.mat.feb[k-1,i] <- mean(data[k,1])
            pdsi.mat.apr[k-1,i] <- mean(data[k,3])
        }
    }
}

pdsi <- cbind(apply(pdsi.mat.nov,1,mean), apply(pdsi.mat.jan,1,mean), 
    apply(pdsi.mat.feb,1,mean), apply(pdsi.mat.apr,1,mean))
    
colnames(pdsi) <- c("pdi_nov","pdi_jan","pdi_feb","pdi_apr")
rownames(pdsi) <- 1:nrow(pdsi)+1948
pdsi.ts <- ts(pdsi,1948)
sink('../../pdsi_crb.txt')
print(pdsi)
sink()