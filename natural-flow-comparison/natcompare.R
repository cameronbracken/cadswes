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
	plot(data.cbrfc[,i], col='black', 
		xlab='', ylab='Flow (thousand ac-ft/month)', 
		main = paste(site$names[i],'vs.',site$codes[i]) )
	lines(data.usbr[,i], col='steelblue', lty = 'solid')
	lines(data.usbr[,i]-data.cbrfc[,i], col='green')
	legend("topright", c("CBRFC","USBR", "Difference"), 
		col=c(1,'steelblue','green'), lty=c('solid'))	
}
dev.off()


###################################
# Plot the cumulative difference
###################################
pdf('usbr-cbrfc-natcomp-cum-diff.pdf',width=7,height=10)
cul <- ts(read.table("USBR-CUL-ac-ft.tab",sep='\t',header=TRUE),start=1971,frequency=1)
ag <- ts(read.table("USBR-ag-loss-ac-ft.tab",sep='\t',header=TRUE),start=1971,frequency=1)
col <- rainbow(length(site$names))

	# z is a list of time series
diff <- data.usbr - data.cbrfc


for(i in 1:length(site$names)) z[[i]] <- z[[i]]/1e6 #change units to MAF

layout(matrix(1:length(site$names),ncol=1))
for(i in 1:length(site$names)){
	this.z <- cumsum.ts()
	par(mar=c(2,4,1,1))
	plot(z[[i]], xlab = '', ylab = 'Cum diff (MAF)', col = col[i], 
		xlim=c(time(z[[i]])[1],eComp),
		ylim = c(min(z[[i]]), max(z[[i]])))
	lines(cumsum.ts(cul[,i])/1e6)
	lines(cumsum.ts(ag[,i])/1e6,lty="dashed")
	legend('topleft',c(site$names[i],"Total CUL","Ag CUL"),
			col=c(col[i],1,1),lty=c('solid','solid','dashed'))
}
dev.off()

#########################################################
# Plot the average cumulative difference for one year
#########################################################
pdf('usbr-cbrfc-natcomp-ave-cum-diff.pdf')
s <- sapply(diff.ts( data.usbr, data.cbrfc ), seasonal.stat, mean, simplify=F)

for(i in 1:length(s))
	s[[i]] <- cumsum.ts(s[[i]])

for(i in 1:length(site$names)) 
	s[[i]] <- s[[i]]/1e3 #change units to MAF

mon <- format(as.POSIXct(sprintf("2009-%02d-01",1:12),"%Y-%m-%d"),"%b")	

par(xaxt="n")
plot(s[[1]], xlab = '', ylab = 'Ave Cum diff (MAF)', col = col[1],
	ylim = c(min(sapply(s,min)), max(sapply(s,max))))
par(xaxt="s")
axis(1,seq(1:12),labels=mon)
	
for(i in 2:length(site$names))
	lines(s[[i]],col=col[i])
	
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
	plot(p[[i]], xlab = '', ylab = '% difference', col = col[i], 
		ylim = c(0,100),xlim=c(time(p[[i]])[i],eComp))
	legend('topleft',site$names[i],col=col[i],lty='solid')
}
dev.off()