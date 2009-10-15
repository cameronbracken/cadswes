#!/usr/bin/env Rscript

##########################
#
# Main Program
#
##########################
source('libnatcompare.R')

figPrefix = 'figs'
if(!file.exists(figPrefix)) dir.create(figPrefix)

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
data.cbrfc <- downloadAndReadCbrfcUnregulatedData( site$codes )
# enforce the time limits
data.cbrfc <- window(data.cbrfc, c(sComp,1), c(eComp,12))
#remove negatives
data.cbrfc[ data.cbrfc < 0 ] = 0

##############################
# Get the usbr data
##############################
#read and transform the usbr data to a timeseries starting at 1971, ending at 2000
data.usbr <- ts(read.table("data/USBR-monthly.tab",sep='\t',header=TRUE),
		start=c(1905,10),frequency=12)
# enforce time window and convert to thousand acre-ft/mon
data.usbr <- window(data.usbr, c(sComp,1), c(eComp,12))/1e3
data.usbr[data.usbr < 0] = NA

site <- c(site,names = list(attributes(data.usbr)$dimnames[[2]]))

###################################
# Plot the comparisons at each site
###################################
pdf(file.path(figPrefix,'data.pdf'),width=6.5,height=8.5)
layout(matrix(1:length(site$names),ncol=1))

for( i in 1:length(site$names) ){
	# plot a thousand acre-ft/month
	# CBRFC 
	par(mar=c(2,4,1.5,1))
	plot(data.cbrfc[,i], col='black', 
		xlab='', ylab='Flow (KAF/mon)', 
		main = paste(site$names[i],'vs.',site$codes[i]) )
		
	# USBR
	lines(data.usbr[,i], col='steelblue', lty = 'solid')
	
	# Difference
	# lines(data.usbr[,i]-data.cbrfc[,i], col='green')
	#legend("topright", c("CBRFC","USBR", "Difference"), 
	#	col=c(1,'steelblue','green'), lty=c('solid'))	
	legend("topright", c("CBRFC","USBR"), 
		col=c(1,'steelblue'), lty=c('solid'))	
}
dev.off()


###################################
# Plot the cumulative difference
###################################

pdf(file.path(figPrefix,'cum-diff.pdf'),width=6.5,height=8.5)
layout(matrix(1:length(site$names),ncol=1))

#Read in Consumtive use data and convert to MAF
cul <- ts(read.table("data/USBR-CUL-ac-ft.tab",sep='\t',header=TRUE), start=1971+1,frequency=1)/1e6
ag <- ts(read.table("data/USBR-ag-loss-ac-ft.tab",sep='\t', header=TRUE), start=1971+1,frequency=1)/1e6
col <- rainbow(length(site$names))

	# compute difference in MAF from thousand ac-ft
diff <- (data.usbr - data.cbrfc)/1e3
	#dimnames get changed when ts's are subtracted
attr(diff,"dimnames") <- attributes(data.usbr)$dimnames
diff <- cumsum.ts(diff)
		
	#add intervening uses
ag[,"FlamingGorge"] <- ag[,"FlamingGorge"] + ag[,"Fontenelle"]
ag[,"Crystal"] <- ag[,"Crystal"] + ag[,"BlueMesa"]
cul[,"FlamingGorge"] <- cul[,"FlamingGorge"] + cul[,"Fontenelle"]
cul[,"Crystal"] <- cul[,"Crystal"] + cul[,"BlueMesa"]

for(i in 1:length(site$names)){
	
		#crystal set is shorter so truncate series if we are on that site
	if(site$names[i] == "Crystal"){
		#cat('Special case for Crystal')
		z <- window(diff[,i],1978,2001)
		this.cul <- cumsum.ts(window(cul[,i],1978,2001))
		this.ag <- cumsum.ts(window(ag[,i],1978,2001))
	} else {
		z <- diff[,i]
		this.cul <- cumsum.ts(cul[,i])
		this.ag <- cumsum.ts(ag[,i])
	}
		
		#plot the annual series on the last month of the year
	x <- as.vector(time(this.ag)-1/12)
		#Margins
	par(mar=c(2,4,1,1))
		#Cumulative difference
	plot(z, xlab = '', ylab = 'Cum diff (MAF)', col = col[i], 
			xlim=c(time(diff)[1],eComp),
			ylim = c(min(c(z,this.cul)), max(c(z,this.cul))))
		# Total CUL
	lines(x,this.cul)
		#Ag CUL
	lines(x,this.ag,lty="dashed")
	legend('topleft',c(site$names[i],"Total CUL","Ag CUL"),
			col=c(col[i],1,1),lty=c('solid','solid','dashed'))
}
dev.off()

#########################################################
# Calculate the average cumulative difference for one year
#########################################################

	# compute difference in MAF from thousand ac-ft
	
	#get the mean difference for each season
	#s is a data.frame
diff <- as.matrix((data.usbr - data.cbrfc)/1e3)
	
f <- function(x) quantile(x,0.5,na.rm=T)
s <- as.matrix(seasonal.stat(diff, f))
f <- function(x) quantile(x,0.05,na.rm=T)
smin <- as.matrix(seasonal.stat(diff, f))
f <- function(x) quantile(x,0.95,na.rm=T)
smax <- as.matrix(seasonal.stat(diff, f))

	#names of months
mon <- format(as.POSIXct(sprintf("2009-%02d-01",1:12),"%Y-%m-%d"),"%b")

#########################################################
# Plot the median cumulative difference, 
# each site in separate frame
#########################################################	

pdf(file.path(figPrefix,'median-cum-diff-err.pdf'))

layout(matrix(1:length(site$names),ncol=2))
	
for(i in 1:length(site$names)){
	par(xaxt="n",mar=c(3,4,1,1))
	boxdata <- seasonal.stat.work(diff[,i],median,giveStack=T)
	plot(s[,i], xlab = '', ylab = 'Difference (MAF)',
		ylim = c(min(c(smin[,-6],s[,-6],boxdata),na.rm=T), 
				max(c(smax[,-6],s[,-6],boxdata),na.rm=T)), 
		type = 'n')
	myboxplot(as.data.frame(boxdata),add=T)
	lines(s[,i], col = col[i])
	par(xaxt="s")
	axis(1,seq(1:12),labels=mon)
	legend('topleft',site$names[i],col=col[i],lty='solid')
}

dev.off()

#########################################################
# Plot the median cumulative difference, all sites
#########################################################
pdf(file.path(figPrefix,'med-cum-diff.pdf'), width=6,height=6)

	#Get the cumulative sums of each mean
s <- cumsum.ts(s)

	#plot differences
par(xaxt="n")
plot(s[,1], xlab = '', ylab = 'Median Cumulative Diff (MAF)', col = col[1],
	ylim = c(min(s), max(s)), type = 'l')
par(xaxt="s")
axis(1,seq(1:12),labels=mon)
	
for(i in 2:length(site$names)){
	lines(s[,i],col=col[i])
}
legend('topleft',site$names,col=col,lty='solid')

dev.off()

#########################################################
# Plot the median cumulative difference, Lees Ferry with box plot
#########################################################
pdf(file.path(figPrefix,'med-cum-diff-err-lees.pdf'), width=6,height=6)

n <- which(site$names == 'LeesFerry')

#plot differences
par(xaxt="n",mar=c(3,4,4,1))
boxdata <- seasonal.stat.work(diff[,n],mean,giveStack=T)
boxdata <- t(apply(boxdata,1,cumsum))
s <- apply(boxdata,2,median,na.rm=T)

plot(s, xlab = '', ylab = 'Difference (MAF)',
	ylim = c(min(boxdata,na.rm=T), 
			max(boxdata,na.rm=T)), 
	type = 'n')
myboxplot(as.data.frame(boxdata),add=T)
lines(s, col = col[n])
#for spaghetti plot
#for(i in 1:nrow(boxdata))
#	lines(boxdata[i,])
par(xaxt="s")
axis(1,seq(1:12),labels=mon)
legend('topleft','LeesFerry',col=col[n],lty='solid')

dev.off()


###################################
# Plot the percent differences
###################################
pdf(file.path(figPrefix,'percent-diff.pdf'),
	width=6.5,height=8.5)
	
col <- rainbow(length(site$names), alpha=.8)
p <- diff.ts( data.usbr, data.cbrfc, percent = TRUE )

layout(matrix(1:length(site$names),ncol=1))
for(i in 1:length(site$names)){
	par(mar=c(2,4,1,1))
	plot(p[,i], xlab = '', ylab = '% difference', col = col[i], 
		ylim = c(0,120),xlim=c(time(p)[i],eComp))
	legend('topleft',site$names[i],col=col[i],lty='solid')
}
dev.off()