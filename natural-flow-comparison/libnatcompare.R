##########################
#
# Functions (put these in a package)
#
##########################
get.cbrfc.unregulated.data.url <- function(site.names, start = "1900-01-01", end = Sys.Date())
{
		
	url <- paste("http://www.nwrfc.noaa.gov/westernwater/inc/getcsv.php?id=", site.names,
		"&tablelist=obs&dataobs=", start, "&dataobsend=", end, "&datacharcs=&datacharcsend=&datafcst=&datafcstend=&dataens_memb=&dataens_membend=&dataobs_mean=&dataobs_meanend=&datacli_index=&datacli_indexend=&dataobsind=&ver=table&page=data", 
		sep='')
		
}

download.and.read.cbrfc.unregulated.data <- function(site.names, 
	download.dir = 'downloads', from.cache = TRUE, missing.val = -9999, ...)
{
	# This function takes a a character vector of river forcast center site names,
	# and downloads the unregulated flow data for that site from the RFC. The data 
	# is read in and returned in a list
	
	urls <- get.cbrfc.unregulated.data.url( site.names[!is.na(site.names)], ...)
	
	#Create dir of downloaded data
	if(!file.exists(download.dir)) dir.create( download.dir )

	#Create list of files
	files <- character()
	for(i in 1:length(site.names)){
		if(!is.na(site.names[i]))
			files <- c( files, file.path( download.dir, paste( site.names[i], 'csv',sep='.' )))
	}
	
	#Download and read in files
	data.cbrfc <- list()
	for(i in 1:length(urls)) {
		if( !from.cache || !file.exists(files[i]) )
			download.file( urls[i], destfile=files[i], method = "wget", quiet = T)
		data.cbrfc[[i]] <- unique( read.csv( files[i] ) )
	}
	
	names(data.cbrfc) <- site.names[!is.na(site.names)]
	
	for(i in 1:length(data.cbrfc)){
			#data is listed with most recent first so get the last entry
		n <- length(as.character(data.cbrfc[[i]]$obsdate))
			#Get the start year and month
		s <- strsplit(as.character(data.cbrfc[[i]]$obsdate[n]),'-')[[1]]
		s <- as.integer(s)
			#must reverse the data for the same reason
		data.cbrfc[[i]] <- ts(rev(data.cbrfc[[i]]$val), start=c(s[1], s[2]), frequency=12)
		data.cbrfc[[i]][ data.cbrfc[[i]] == missing.val ] = NA
		data.cbrfc[[i]][ data.cbrfc[[i]] < 0 ] = 0
	}
	
	# coerce time series list into multi dimensional time series
	
	# first find the time series with the largest range 
	widest <- 1
	r.widest <- range(time(data.cbrfc[[1]]))
	r.widest <- r.widest[2] - r.widest[1]
	for(i in 2:length(data.cbrfc)){
		r <- range(time(data.cbrfc[[i]]))
		r <- r[2] - r[1]
		if(r > r.widest){
			widest <- i
			s.widest <- start(data.cbrfc[[widest]])
		}
	
	}
	
	#initilize twice so range is range of longest ts
	mts <- cbind(data.cbrfc[[1]],data.cbrfc[[widest]])
	mts <- cbind(data.cbrfc[[1]],data.cbrfc[[2]])
	for(i in 2:(length(data.cbrfc)-1))		
		mts <- cbind(mts,data.cbrfc[[i+1]])
		
	mts <- ts(mts,start = s.widest, frequency = 12)
	attr(mts,'dimnames') <- list(NULL,site.names)	
	
	return(mts)
	
}

cumsum.ts <- function(x){
	
	# does cumsum on a time series, converts NA to zero
	# because NA will corrupt the remaining values in the sum 
	
	x[is.na(x)] = 0
	if(is.null(dim(x)))
			# Single dimension time series
		x <- ts(cumsum(as.vector(x)),start=start(x),frequency=frequency(x))
	else
			# Multi dimension time series
		for(i in 1:dim(x)[2])
			x[,i] <- cumsum(as.vector(x[,i]))
	return(x)
	
	
}

diff.ts <- function(x, y, cumsum = FALSE, percent = FALSE){
	
	# takes either single or multidimensional ts and performs 
	# differences, can return the strait difference, cumulative 
	# difference or percent difference 
	if( cumsum )
		z <- cumsum.ts(x-y)
	else if( percent ) 
		z <- 200*abs(x - y)/(x + y)
	else 
		z <- x - y
		
	attr(z,"dimnames") <- attributes(x)$dimnames
	return(z)
	
}

	# calculate a statistic for each period in a time series
	# default period id frequency but any window will work
seasonal.stat <- function(x, fun, win = frequency(x), na.rm = T){
	
	#figure out if ts is single or multi dimensional
	if(is.null(dim(x))){
			#Single ts
		return(seasonal.stat.work(x,fun=fun,win=win,na.rm=na.rm))
	}else{
		
			# Multidimensional ts
			# get the seasonal stat for each ts and bind the result together 
			# into a matrix
		mts <- cbind(
				seasonal.stat.work(x[,1],fun=fun,win=win,na.rm=na.rm),
				seasonal.stat.work(x[,2],fun=fun,win=win,na.rm=na.rm)
				)
		for(i in 2:(dim(x)[2]-1)){
			mts <- cbind(mts, seasonal.stat.work(x[,i+1],fun=fun,win=win,na.rm=na.rm))
		}
		mts <- as.data.frame(mts)
		names(mts) <- attributes(x)$dimnames[[2]]
		return(mts)
	}
	
}

seasonal.stat.work <- function(x,fun,win=frequency(x),na.rm=T){
	
	#calcualte a statistic for each period in a time series
	
	nyears <- length(unique(floor(time(x))))
    syear <- start(x)[1]

	times <- seq(syear,by=1/win,length.out=length(x))
	years <- floor(times)

	stacked <- matrix(NA,nrow=nyears,ncol=win)

	#Must use a loop because ts may not start at begining of year
	thisYear <- syear
	for(i in 1:nyears){

	    thisRow <- x[years==thisYear]
	    stacked[thisYear - syear + 1, 1:length(thisRow)] <- thisRow
	    thisYear = thisYear + 1

	}

	return(as.vector(apply(stacked,2,fun,na.rm=na.rm)))
	
}

ts.annual.mean <- function(x){

    options(warn=-1)
    nyears <- length(unique(floor(time(x))))
    syear <- start(x)[1]
    nst = vector('numeric',nyears)

    for(i in 1:nyears)  nst[i] <- mean(window(x,syear - 1 + i,
                                        c(syear - 1 + i, frequency(x))))
   
    nst <- ts(nst, start = c(syear,1), frequency = 1) 
    return(nst)

}

wapply <- function(x, fun, win.len = length(x), ...){
    
    r <- (length(x) %% win.len)
    if(r > 0) x <- x[1:(length(x)-r)]
    stack <- matrix(x,nrow = win.len)
    return(apply(stack,2,fun,...))
    
}


ts.next.time <- function(x){
    en <- end(x)
    en <- if(en[2] == frequency(x))
            c(en[1] + 1, 1)
        else
            c(en[1], en[2] + 1)
    return(en)
}

ts.order.start <- function(x,start.per){

        f = frequency(x)
        ord <- if(start.per == 1) 
                start.per:f
           else if(start.per == f) 
                c(f, 1:(f - 1))
           else
                c(start.per:f, 1:(start.per - 1))
        return(ord)
}

##########################
#
# End Functions
#
##########################