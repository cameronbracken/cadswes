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
	download.dir = 'downloads', from.cache = TRUE, missing.val = -9999)
{
	# This function takes a a character vector of tiver forcast center site names,
	# and downloads the unregulated flow data for that site
	
	urls <- get.cbrfc.unregulated.data.url( site.names[!is.na(site.names)] )
	
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

data.cbrfc <- download.and.read.cbrfc.unregulated.data( site$codes )
names(data.cbrfc) <- site$names[!is.na(site$names)]

#read and transform the usbr data to a list of timeseries
data.usbr <- read.csv( 'USBRLocations-acft-mon.csv' )
temp <- list()
s <- as.integer( strsplit( as.character( data.usbr[1,1] ), '-' )[[1]] )
for(i in 1:length(site$names))
	temp[[site$names[i]]] <- 
		ts(data.usbr[[site$names[i]]], start=c(s[3], s[1]), frequency=12)

data.usbr <- temp	
names(data.usbr) <- site$names[!is.na(site$names)]	

# Plot the comparisons

pdf('usbr-cbrfc-natcomp.pdf',width=10,height=5)
for( i in 1:length(site$names) ){
	if( !is.na(site$codes[i]) && !is.na(site$names[i]) ){
		
		plot(data.cbrfc[[site$names[i]]], col='darkgrey', lwd=2, 
			xlab='', ylab='Flow (ac-ft/month)', 
			main = paste(site$names[i],'vs.',site$codes[i]) )
		lines(data.usbr[[site$names[i]]], col='steelblue', lty = 'solid')
		legend("topright", c("CBRFC","USBR"), col=c('darkgrey','steelblue'), 
			lty=c('solid','solid'), lwd=c(2,1))
	}
}
	#plot the percent differences
col <- rainbow(length(site$names),alpha=.5)
x <- data.usbr[[site$names[1]]]
y <- data.cbrfc[[site$names[1]]]
plot((x - y)/(x + y), xlab = '', ylab = 'Percent difference', col = col[1], 
	ylim = c(-1,1),xlim=c(1909,2012))
for(i in 2:length(site$names)){
	x <- data.usbr[[site$names[i]]]
	y <- data.cbrfc[[site$names[i]]]
	lines((x - y)/ (x + y), col = col[i])
}
legend('bottomleft',site$names,col=col,lty='solid')
dev.off()