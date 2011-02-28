#!/usr/bin/env Rscript
    # the directory with ALL the raw snow course datafiles from:
    # http://www.wcc.nrcs.usda.gov/ftpref/data/snow/snow_course/table/history/{colorado,utah,wyoming}
url <- "http://www.wcc.nrcs.usda.gov/ftpref/data/snow/snow_course/table/history"
dl <- T
    #
    # obtained by:
    #
    # $ wget -r -l1 --no-parent -A.txt http://www.wcc.nrcs.usda.gov/ftpref/data/snow/snow_course/table/history/wyoming/
    # $ wget -r -l1 --no-parent -A.txt http://www.wcc.nrcs.usda.gov/ftpref/data/snow/snow_course/table/history/colorado/
    # $ wget -r -l1 --no-parent -A.txt http://www.wcc.nrcs.usda.gov/ftpref/data/snow/snow_course/table/history/utah/
    # $ mkdir raw_snow_course
    # $ cp www.wcc.nrcs.usda.gov/ftpref/data/snow/snow_course/table/history/wyoming/* raw_snow_course
    # $ cp www.wcc.nrcs.usda.gov/ftpref/data/snow/snow_course/table/history/colorado/* raw_snow_course
    # $ cp www.wcc.nrcs.usda.gov/ftpref/data/snow/snow_course/table/history/utah/* raw_snow_course
    
    # the dir with only datafiles from the uc basin, will be created if it 
    # does not exist and the files will be downloaded
dir <- 'uc_only'
    # minimum and maximum years in any files to size the data array
yrange <- 1930:2009
    # the beginning year to actually use
use <- 1949
    # list of all the snow course sites in colorado, utah and wyoming
xs <- read.table('../list.csv',header=T,sep=',',quote='')
    # list of all the snow course sites in the upper basin 
uc.sites <- scan('../uc_sites.txt',what=character(),sep=',',strip.white=T)

    # get the file names for all the sites in the upper basin available
used.sites <- as.vector(xs$Station[xs$Sitename %in% uc.sites])
fnames <- paste(tolower(used.sites),'.txt',sep='')

if(!file.exists(dir))
    dir.create(dir)
    
    
tf <- tempfile()

swemat <- array(NA,c(length(yrange),6,length(fnames)))
for( i in 1:length(fnames) ){
    
    this.file <- file.path(dir,fnames[i])
    
    if(dl)
        download.file(paste(url,xs$State[xs$Station == used.sites[i]],fnames[i],sep='/'),this.file)
        
        # write a temporary file with just the relevant data
    xx <- readLines(this.file)
    xx <- xx[xx!='']
    writeLines(xx[9:max(grep('[0-9]-1',xx))],tf)
    
        # read in the data as fixed width
    data <- read.fwf(tf,widths=c(4,rep(5,18)),strip.white=T)
        # get the starting year of the data 
    sy <- as.numeric(strsplit(as.character(data[1,1]),'-')[[1]][1])
    sy <- if(sy < 25) sy + 2000 else sy + 1900
        # extract just jan-apr
    data <- data[,seq(4,ncol(data),3)]
    names(data) <- month.abb[1:6]
    rownames(data) <- sy:(sy+nrow(data)-1)
    spos <- which(yrange == sy)
        
        # put the data in the correct position in the swe matrix
    swemat[spos:(spos+nrow(data)-1),,i] <- data.matrix(data)
}
    
snow_course <- matrix(NA,ncol=4,nrow=length(yrange))
for(i in 1:4)
    snow_course[,i] <- apply(swemat[,i,],1,mean,na.rm=T)
rownames(snow_course) <- yrange
colnames(snow_course) <- month.abb[1:4]

snow_course.ts <- window(ts(snow_course,start=yrange[1]),use)
snow_course <- snow_course[which(yrange==use):nrow(snow_course),]

write.table(snow_course,quote=F,file='snow_course.txt')

