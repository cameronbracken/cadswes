#!C:/R/bin/Rscript

f <- 'scratch.txt'
out <- 'FlamingGorgeData.PercentExceedance.dat'
args <- commandArgs(trailingOnly=T)

# Arg key
# 1 - Full path to master control file
# 2 - dmi directory
# 3 - Start Date
# 4 - Start Time 
# 5 - End Date
# 6 - End Time 
# 7 - Timestep

setwd(file.path(Sys.getenv('MIDTERM_MODEL_HOME'),'dmi'))
source('lib.R')



start <- strsplit(args[3],'-')[[1]]
start <- as.integer(c(start[1],start[2]))


x <- scan('FlamingGorgeData.GreenAboveFlamingGorge.dat',skip=6,quiet=T)
x <- ts(x,start=start,frequency=12)
x.stack <- ts.stack(x)
# Calculate the Apr-July volume
x.vol <- (x.stack[,'Apr']*31+(x.stack[,'May']+x.stack[,'Jun']+x.stack[,'Jul'])*30)*3600*24
x.rep <- x


in.month <- function(x,mon,i)
	round(time(x)[i]-floor(time(x)[i]),3) %in% round((mon-1)/12,3)


y <- 1
for(i in 1:length(x)){
	
	if(in.month(x,3,i))
		y <- y + 1
		
	if(in.month(x,c(8:12,1:2),i)) {
		x.rep[i] <- x.vol[y]
	} else {
		if( floor(time(x)[i])==max(floor(time(x))) )
			x.rep[i] <- x.vol[y]
		else
			x.rep[i] <- x.vol[y+1]
	}
}
#print(as.matrix(x))

quant <- numeric(length(x))

fixedPeriod <- 10*12
#the first 10 years use the same data
quant[1:fixedPeriod] <- ecdf(x.vol[1:fixedPeriod])(x.rep[1:fixedPeriod])

#remaining data, use only data available up to given date
for(i in (fixedPeriod+1):length(x))
	quant[i] <- ecdf(x.vol[1:i])(x.rep[i])

quant[is.na(quant)] <- .5

#convert to percent exceedance
quant <- 1- quant
	
cat('start_date:',args[3],'24:00\n',file=out)
cat('end_date:',args[5],'24:00\n',file=out,append=T)
cat('timestep: 1 MONTH\n',file=out,append=T)
#cat('units: cfs\n',file=out,append=T)
cat('scale: 1.000000\n',file=out,append=T)
cat(sprintf('%9.7f',round(quant,7)),sep='\n',file=out,append=T)


#./quantile.R /cygdrive/z/public_html/pmm/rules-dev/dmi/Flaming_Gorge_Quantile_Output/metaControl /cygdrive/z/public_html/pmm/rules-dev/dmi/Flaming_Gorge_Quantile_Output/ 1905-10-31 24:00 2007-12-31 24:00 1MONTH
