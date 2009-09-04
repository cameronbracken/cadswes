#!/usr/bin/env Rscript

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
	# This function takes a a character vector of tiver forcast center site names,
	# and downloads the unregulated flow data for that site
	
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
		data.cbrfc[[i]] <- data.cbrfc[[i]] * 1000 # convert to ac-ft/mon
	}
	return(data.cbrfc)
	
}

cumsum.ts <- function(x){
	
	x[is.na(x)] = 0
	ts(cumsum(as.vector(x)),start=start(x),frequency=frequency(x))
	
}

diff.ts <- function(x, y, cumsum = FALSE, percent = FALSE){
	
	#takes either two lists of ts or two single ts
	if( is.list(x) ){
		z <- list()
		for(i in 1:length(x)){
			if( cumsum )
				z[[i]] <- cumsum.ts(x[[i]]-y[[i]])
			else if( percent ) 
				z[[i]] <- 200*abs(x[[i]] - y[[i]])/(x[[i]] + y[[i]])
			else
				z[[i]] <- x[[i]]-y[[i]]
		}
		
	}else{
		if( cumsum )
			z <- cumsum.ts(x-y)
		else if( percent ) 
			z <- 200*abs(x - y)/(x + y)
		else 
			z <- x - y
	}
	return(z)
	
}

##########################
#
# End Functions
#
##########################

##########################
#
# Main Program
#
##########################

site <- list(
	# Paired lists of ubrfc and usbr names
	codes = c(
		#NA,      #Taylor Park (not online)
		"BMDC2", #Blue Mesa
		"CLSC2", #Crystal
		#"MPSC2", #Morrow Point
		"NVRN5", #Navajo
		#NA,      #Valecito (neither has data)
		"GLDA3", #Lees Ferry
		"GBRW4", #Fontenelle
		"GRNU1" #Flaming Gorge
	),
	#  The usbr names are imposed by me, the real names from
	#  the natural flow data set are too long!
	names = c(
		#"TaylorPark",
		"BlueMesa",
		"Crystal",
		#NA,             #Morrow Point
		"Navajo",
		#NA,             #Valecito (neither has data)
		"LeesFerry",
		"Fontenelle",
		"FlamingGorge"
	)
)

# get the cbrfc data
data.cbrfc <- download.and.read.cbrfc.unregulated.data( site$codes)
# enforce the time limits
for(i in 1:length(site$names))
	data.cbrfc[[i]] <- window(data.cbrfc[[i]], c(1971,1), c(2000,12))

names(data.cbrfc) <- site$names[!is.na(site$names)]

#read and transform the usbr data to a list of timeseries starting at 1971, ending at 2000
data.usbr <- read.csv( 'USBRLocations-acft-mon.csv' )
temp <- list()
s <- as.integer( strsplit( as.character( data.usbr[1,1] ), '-' )[[1]] )
for(i in 1:length(site$names)){
	temp[[site$names[i]]] <- 
		ts(data.usbr[[site$names[i]]], start=c(s[3], s[1]), frequency=12)
	temp[[site$names[i]]] <- window(temp[[site$names[i]]],c(1971,1), c(2000,12))
}

data.usbr <- temp	
names(data.usbr) <- site$names[!is.na(site$names)]	

# Plot the comparisons

pdf('usbr-cbrfc-natcomp.pdf',width=12,height=4)
for( i in 1:length(site$names) ){
	if( !is.na(site$codes[i]) && !is.na(site$names[i]) ){
		
		plot(data.cbrfc[[site$names[i]]], col='black', 
			xlab='', ylab='Flow (ac-ft/month)', 
			main = paste(site$names[i],'vs.',site$codes[i]) )
		lines(data.usbr[[site$names[i]]], col='steelblue', lty = 'solid')
		lines(data.usbr[[site$names[i]]] - data.cbrfc[[site$names[i]]], col='green')
		legend("topright", c("CBRFC","USBR", "Difference"), 
			col=c(1,'steelblue','green'), lty=c('solid'))
			
	}
}

	# plot the cumulative difference
col <- rainbow(length(site$names), alpha=.9)
z <- diff.ts( data.usbr, data.cbrfc, cumsum = TRUE )

plot(z[[1]], xlab = '', ylab = 'Cumulative difference', col = col[1], 
	xlim=c(time(z[[1]])[1],2000),
	ylim = c(min(sapply(z, min)), max(sapply(z, max))))
for(i in 2:length(site$names))
	lines(z[[i]], col = col[i])
legend('topleft',site$names,col=col,lty='solid')

	#plot the percent differences
col <- rainbow(length(site$names), alpha=.9)
p <- diff.ts( data.usbr, data.cbrfc, percent = TRUE )
plot(p[[i]], xlab = '', ylab = 'Percent difference', col = col[1], 
	ylim = c(0,100),xlim=c(time(p[[1]])[1],2000))
	
for(i in 2:length(site$names))
	lines(p[[i]], col = col[i])
legend('bottomleft',site$names,col=col,lty='solid')

dev.off()