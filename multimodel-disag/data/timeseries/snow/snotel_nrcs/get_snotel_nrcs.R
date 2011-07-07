#!/usr/bin/env Rscript
require(stringr)
# the directory with ALL the raw snow course datafiles from:
# http://www.wcc.nrcs.usda.gov/ftpref/data/snow/snow_course/table/history/{colorado,utah,wyoming}
#
url <- "http://www.wcc.nrcs.usda.gov/ftpref/data/snow/snotel/table/history"
dl <- T
# to get ALL the data, simply run the following commands
#
# $ wget -r -l1 --no-parent -A_s.txt http://www.wcc.nrcs.usda.gov/ftpref/data/snow/snotel/table/history/wyoming/
# $ wget -r -l1 --no-parent -A_s.txt http://www.wcc.nrcs.usda.gov/ftpref/data/snow/snotel/table/history/colorado/
# $ wget -r -l1 --no-parent -A_s.txt http://www.wcc.nrcs.usda.gov/ftpref/data/snow/snotel/table/history/utah/
# $ mkdir raw_snotel
# $ cp www.wcc.nrcs.usda.gov/ftpref/data/snow/snotel/table/history/wyoming/* raw_snotel
# $ cp www.wcc.nrcs.usda.gov/ftpref/data/snow/snotel/table/history/colorado/* raw_snotel
# $ cp www.wcc.nrcs.usda.gov/ftpref/data/snow/snotel/table/history/utah/* raw_snotel
rawdir <- 'raw_snotel'
    # the dir with only datafiles from the uc basin, will be created if it 
    # does not exist and the files will be copied from the rawdir
dir <- 'uc_only'
    # minimum and maximum years in any files to size the data array
yrange <- 1979:2010
    # list of all the snow course sites in colorado, utah and wyoming
xs <- read.table('../list.csv',header=T,sep=',',quote='')
    # list of all the snow course sites in the upper basin 
uc.sites <- scan('../uc_sites.txt',what=character(),sep=',',strip.white=T)

    # get the file names for all the sites in the upper basin available
used.sites <- as.vector(xs$Station[xs$Sitename %in% uc.sites])
fnames <- paste(tolower(used.sites),'_s.txt',sep='')

datamat <- array(NA,c(length(yrange),12,length(fnames)))
rownames(datamat) <- yrange

if(!file.exists(dir))
    dir.create(dir)

for(i in 1:length(fnames)){
    
    this.file <- file.path(dir,fnames[i])
    if(dl)
          download.file(paste(url,xs$State[xs$Station == used.sites[i]],fnames[i],sep='/'),this.file)
        
    df <- readLines(this.file)
    
    sy <- as.numeric(strsplit(df[1],'  ')[[1]][2]) 
    sy <- if(sy < 50) sy + 2000 else sy + 1900
    name <- str_replace_all(tolower(strsplit(df[3],', ')[[1]][2]),' ','_')
        
    means <- grep('min',df)
    datalines <- df[means]
    datalines <- substr(df[means],5,nchar(datalines[1]))
    con <- textConnection(datalines)
    datavals <- as.matrix(read.fwf(con,rep(6,12)))
    close(con)
    
    rownames(datavals) <- seq(sy,len=nrow(datavals))
    colnames(datavals) <- c(month.abb[10:12],month.abb[1:9])
    
    spos <- which(yrange == sy)
    datamat[spos:(nrow(datavals)+spos-1),,i] <- datavals
    
    #fnames[i] <- paste(name,'.txt',sep='')
    #cat('\n',fnames[i],'\n')
    #print(datavals[rownames(datavals)==2010,5])
    #write.table(datavals,quote=F,col.names=F,file=fnames[i])
}

snotel <- matrix(NA,ncol=4,nrow=length(yrange))
for(i in 4:7)
    snotel[,i-3] <- apply(datamat[,i,],1,mean,na.rm=T)
rownames(snotel) <- yrange
colnames(snotel) <- month.abb[1:4]

snotel.ts <- ts(snotel,start=yrange[1])

write.table(snotel,quote=F,file='snotel.txt')

