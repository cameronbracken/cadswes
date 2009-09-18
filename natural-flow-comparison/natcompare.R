#!/usr/bin/env Rscript

##########################
#
# Main Program
#
##########################
source('libnatcompare.R')

sComp <- 1971
eComp <- 2000

site <- list(
	# Paired lists of ubrfc and usbr names
	codes = c(
		#NA,      #Taylor Park (not online)
		"BMDC2", #Blue Mesa
		"CLSC2", #Crystal
		#"MPSC2", #Morrow Point
		"NVRN5", #Navajo
		#NA,      #Valecito (neither has data)
		"GBRW4", #Fontenelle
		"GRNU1", #Flaming Gorge
		"GLDA3" #Lees Ferry
	)
)
##############################
# Get the cbrfc data
##############################
data.cbrfc <- download.and.read.cbrfc.unregulated.data( site$codes )
# enforce the time limits
data.cbrfc <- window(data.cbrfc, c(sComp,1), c(eComp,12))

##############################
# Get the usbr data
##############################
#read and transform the usbr data to a timeseries starting at 1971, ending at 2000
data.usbr <- ts(read.table("USBR-monthly.tab",sep='\t',header=TRUE),start=c(1905,10),frequency=12)
# enforce time window and convert to thousand acre-ft/mon
data.usbr <- window(data.usbr, c(sComp,1), c(eComp,12))/1e3

site <- c(site,names = list(attributes(data.usbr)$dimnames[[2]]))

###################################
# Plot the comparisons at each site
###################################
pdf('usbr-cbrfc-natcomp.pdf',width=11,height=4)
for( i in 1:length(site$names) ){
	
	# plot a thousand acre-ft/month
	# CBRFC 
	plot(data.cbrfc[,i], col='black', 
		xlab='', ylab='Flow (thousand ac-ft/month)', 
		main = paste(site$names[i],'vs.',site$codes[i]) )
	# USBR
	lines(data.usbr[,i], col='steelblue', lty = 'solid')
	# Difference
	lines(data.usbr[,i]-data.cbrfc[,i], col='green')
	legend("topright", c("CBRFC","USBR", "Difference"), 
		col=c(1,'steelblue','green'), lty=c('solid'))	
}
dev.off()


###################################
# Plot the cumulative difference
###################################
pdf('usbr-cbrfc-natcomp-cum-diff.pdf',width=7,height=10)

#Read in Consumtive use data and convert to MAF
cul <- ts(read.table("USBR-CUL-ac-ft.tab",sep='\t',header=TRUE),start=1971+1,frequency=1)/1e6
ag <- ts(read.table("USBR-ag-loss-ac-ft.tab",sep='\t',header=TRUE),start=1971+1,frequency=1)/1e6
col <- rainbow(length(site$names))

	# compute difference in MAF from thousand ac-ft
diff <- (data.usbr - data.cbrfc)/1e3
attr(diff,"dimnames") <- attributes(data.usbr)$dimnames
diff <- cumsum.ts((data.usbr - data.cbrfc)/1e3)
		
	#add intervening uses
diff[,"FlamingGorge"] <- diff[,"FlamingGorge"] + diff[,"Fontenelle"]
diff[,"Crystal"] <- diff[,"Crystal"] + diff[,"BlueMesa"]

layout(matrix(1:length(site$names),ncol=1))
for(i in 1:length(site$names)){
	
	z <- diff[,i]
		
	x <- as.vector(time(ag)-1/12)
		#Margins
	par(mar=c(2,4,1,1))
		#Cumulative difference
	plot(z, xlab = '', ylab = 'Cum diff (MAF)', col = col[i], 
			xlim=c(time(z)[1],eComp),
			ylim = c(min(z), max(z)))
		# Total CUL
	lines(x,cumsum.ts(cul[,i]))
		#Ag CUL
	lines(x,cumsum.ts(ag[,i]),lty="dashed")
	legend('topleft',c(site$names[i],"Total CUL","Ag CUL"),
			col=c(col[i],1,1),lty=c('solid','solid','dashed'))
}
dev.off()

#########################################################
# Plot the average cumulative difference for one year
#########################################################
pdf('usbr-cbrfc-natcomp-ave-cum-diff.pdf')

	# compute difference in MAF from thousand ac-ft
diff <- (data.usbr - data.cbrfc)/1e3
	
	#get the mean difference for each season
	#s is a data.frame
s <- seasonal.stat(diff, mean)

	#Get the cumulative sums of each mean 
for(i in 1:length(s))
	s[,i] <- cumsum.ts(s[,i])

	#names of months
mon <- format(as.POSIXct(sprintf("2009-%02d-01",1:12),"%Y-%m-%d"),"%b")	

	#plot differences
par(xaxt="n")
plot(s[,1], xlab = '', ylab = 'Ave Cum Diff (MAF)', col = col[1],
	ylim = c(min(s), max(s)))
par(xaxt="s")
axis(1,seq(1:12),labels=mon)
	
for(i in 2:length(site$names))
	lines(s[,i],col=col[i])
	
legend('topleft',site$names,col=col,lty='solid')

dev.off()

###################################
# Plot the percent differences
###################################
pdf('usbr-cbrfc-natcomp-percent-diff.pdf',width=7,height=10)
col <- rainbow(length(site$names), alpha=.8)
p <- diff.ts( data.usbr, data.cbrfc, percent = TRUE )

layout(matrix(1:length(site$names),ncol=1))
for(i in 1:length(site$names)){
	par(mar=c(2,4,1,1))
	plot(p[,i], xlab = '', ylab = '% difference', col = col[i], 
		ylim = c(0,100),xlim=c(time(p)[i],eComp))
	legend('topleft',site$names[i],col=col[i],lty='solid')
}
dev.off()