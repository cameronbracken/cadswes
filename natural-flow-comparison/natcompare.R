#!/usr/bin/env Rscript

##########################
#
# Main Program
#
##########################
source('libnatcompare.R')

figPrefix = 'figs'
paPath = 'pa'
if(!file.exists(figPrefix)) dir.create(figPrefix)
if(!file.exists(file.path(figPrefix,paPath))) dir.create(file.path(figPrefix,paPath))

sComp <- 1971
eComp <- 2000

site <- list(
	# lists of cbrfc names to download
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
cbrfc.web <- downloadAndReadCbrfcWebData( site$codes )
# enforce the time limits
cbrfc.web <- window(cbrfc.web, c(sComp,1), c(eComp,12))
#remove negatives
cbrfc.web[ cbrfc.web < 0 ] = 0

##############################
# Get the usbr natural flow data
##############################
#read and transform the usbr data to a timeseries starting at 1971, ending at 2000
usbr.nat <- ts(read.table("data/USBRNatural-monthly.tab",sep='\t',header=TRUE),
		start=c(1905,10),frequency=12)
# enforce time window and convert to thousand acre-ft/mon
usbr.nat <- window(usbr.nat, c(sComp,1), c(eComp,12))/1e3
usbr.nat[usbr.nat < 0] = NA

site <- c(site,names = list(attributes(usbr.nat)$dimnames[[2]]))

##############################
# Get the usbr unregulated data
##############################
usbr.unreg <- ts(read.csv("data/USBRHistUnreg-acft.csv",comment.char="#"),
		start=c(1971,1),frequency=12)
	# enforce time window and convert to thousand acre-ft/mon
usbr.unreg <- window(usbr.unreg, c(sComp,1), c(eComp,12))/1e3
usbr.unreg[usbr.unreg < 0] = NA

##############################
# Get the usbr unregulated from cbrfc
##############################
cbrfc.usbr.unreg <- ts(scan('data/CBRFC-PB.csv',comment.char='#',quiet=T),
						start=c(1963,10),frequency=12)
cbrfc.usbr.unreg <- window(cbrfc.usbr.unreg, c(sComp,1), c(eComp,12))

##############################
# Get the cbrfc natural data
##############################
cbrfc.nat <- ts(scan('data/CBRFC-PA.csv',comment.char='#',quiet=T),
				start=c(1963,10),frequency=12)
cbrfc.nat <- window(cbrfc.nat, c(sComp,1), c(eComp,12))

##############################
# Add in an extra plot for PA vs. Lees
##############################
site$names <- c(site$names,'LeesFerry2')
site$codes <- c(site$codes,'PA')
usbr.nat <- cbind(usbr.nat,usbr.nat[,'LeesFerry'])
usbr.unreg <- cbind(usbr.unreg,usbr.unreg[,'LeesFerry'])
cbrfc.web <- cbind(cbrfc.web,cbrfc.nat)
	#names get messed up when adding an extra series
attributes(usbr.nat)$dimnames[[2]] <- site$names
attributes(usbr.unreg)$dimnames[[2]] <- site$names
attributes(cbrfc.web)$dimnames[[2]] <- site$codes



###################################
# Plot the comparisons at each site
###################################
pdf(file.path(figPrefix,'data.pdf'),width=6.5,height=8.5)
layout(matrix(1:length(site$names),ncol=1))

for( i in 1:length(site$names) ){
	# plot a thousand acre-ft/month
	# CBRFC 
	par(mar=c(2,4,1.5,1))
	plot(cbrfc.web[,i], col='black', 
		xlab='', ylab='Flow (KAF/mon)', 
		main = paste(site$names[i],'vs.',site$codes[i]) )
		
	# USBR
	lines(usbr.nat[,i], col='steelblue', lty = 'solid')
	
	# Difference
	# lines(usbr.nat[,i]-cbrfc.web[,i], col='green')
	#legend("topright", c("CBRFC","USBR", "Difference"), 
	#	col=c(1,'steelblue','green'), lty=c('solid'))	
	legend("topright", c("CBRFC Web","USBR Nat"), 
		col=c(1,'steelblue'), lty=c('solid'))	
}
dev.off()

###################################
# Plot the usbr unregulated data 
# comparisons at each site
###################################
pdf(file.path(figPrefix,'data-usbr-unreg.pdf'),width=6.5,height=8.5)
layout(matrix(1:length(site$names),ncol=1))

for( i in 1:length(site$names) ){
	
	# plot a thousand acre-ft/month
	# CBRFC 
	par(mar=c(2,4,1.5,1))
	plot(cbrfc.web[,i], col='black', 
		xlab='', ylab='Flow (KAF/mon)', 
		main = paste(site$names[i],'vs.',site$codes[i]) )

	# USBR
	lines(usbr.unreg[,i], col='gray', lty = 'solid')
		
	if(site$codes[i] == 'PA'){
		
		legend("topright", c("CBRFC PA","USBR Nat"), 
			col=c(1,'gray'), lty=c('solid'),bg='#FFFFFFAA')
		
	}else{
		
		legend("topright", c("CBRFC Web","USBR Unreg"), 
			col=c(1,'gray'), lty=c('solid'),bg='#FFFFFFAA')
		
	}
}
dev.off()

###################################
# Plot the usbr unregulated data 
# comparisons Lees Ferry
###################################
pdf(file.path(figPrefix,paPath,'data-lf-views.pdf'),width=7.5,height=2.5)

# plot a thousand acre-ft/month
# CBRFC 
par(mar=c(2,4,1.5,1))

plot(cbrfc.web[,'GLDA3'],col=2,lwd=2)
lines(usbr.unreg[,'LeesFerry2'],col=4)
lines(cbrfc.usbr.unreg,col=5)	
lines(usbr.nat[,'LeesFerry2'],col=6)	
lines(cbrfc.nat,col=3)

legend("topright", c("CBRFC Web", "USBR Unreg HDB", "CBRFC PB","CBRFC PA", "USBR Natural"), 
	col=c(2,4,5,6,3), lty=c('solid'),bg='#FFFFFFAA')

dev.off()

###################################
# Plot the usbr/cbrfc 
# comparisons Lees Ferry
###################################
pdf(file.path(figPrefix,paPath,'data-lf-views.pdf'),width=7.5,height=2.5)

# plot a thousand acre-ft/month
# CBRFC 
par(mar=c(2,4,1.5,1))

plot(cbrfc.web[,'GLDA3'],col=2,ylab='Flow (KAF/mon)')
lines(usbr.unreg[,'LeesFerry2'],col=3)
lines(cbrfc.usbr.unreg,col=4)	
lines(usbr.nat[,'LeesFerry2'],col=1)	
lines(cbrfc.nat,col=8)

legend("topright", c("CBRFC Web", "USBR Unreg HDB", "CBRFC PB","CBRFC Natural", "USBR Natural"), 
	col=c(2:4,1,8), lty=c('solid'),bg='#FFFFFFAA')

dev.off()

###################################
# Plot the usbr unregulated data 
# comparisons Lees Ferry
###################################
pdf(file.path(figPrefix,paPath,'data-lf-views-unreg.pdf'),width=7.5,height=2.5)

# plot a thousand acre-ft/month
# CBRFC 
par(mar=c(2,4,1.5,1))

plot(cbrfc.web[,'GLDA3'],col=2,ylab='Flow (KAF/mon)')
lines(usbr.unreg[,'LeesFerry2'],col=3,lty=2)
lines(cbrfc.usbr.unreg,col=4)	

legend("topright", c("CBRFC Web", "USBR Unreg HDB", "CBRFC PB"), 
	col=2:4, lty=c(1,2,1),bg='#FFFFFFAA')

dev.off()

###################################
# Plot the usbr natural and cbrfc natural
###################################
pdf(file.path(figPrefix,paPath,'data-lf-views-natural.pdf'),width=7.5,height=2.5)

# plot a thousand acre-ft/month
# CBRFC 
par(mar=c(2,4,1.5,1))

plot(usbr.nat[,'LeesFerry2'],ylab='Flow (KAF/mon)')	
lines(cbrfc.nat,col=8)

legend("topright", c("USBR Natural", "CBRFC Natural"), 
	col=c(1,8),lty=1,bg='#FFFFFFAA')

dev.off()


###################################
# Plot the cumulative difference
###################################

pdf(file.path(figPrefix,'cum-diff.pdf'),width=6.5,height=8.5)
layout(matrix(1:(length(site$names)-1),ncol=1))

#Read in Consumtive use data and convert to MAF
cul <- ts(read.table("data/USBR-CUL-ac-ft.tab",sep='\t',header=TRUE), 
		start=1971+1,frequency=1)/1e6

ag <- ts(read.table("data/USBR-ag-loss-ac-ft.tab",sep='\t', header=TRUE), 
		start=1971+1,frequency=1)/1e6
col <- rainbow(length(site$names)-1)

	# compute difference in MAF from thousand ac-ft
diff <- (usbr.nat - cbrfc.web)/1e3
	#dimnames get changed when ts's are subtracted
attr(diff,"dimnames") <- attributes(usbr.nat)$dimnames
diff <- cumsum.ts(diff)
		
	#add intervening uses
ag[,"FlamingGorge"] <- ag[,"FlamingGorge"] + ag[,"Fontenelle"]
ag[,"Crystal"] <- ag[,"Crystal"] + ag[,"BlueMesa"]
cul[,"FlamingGorge"] <- cul[,"FlamingGorge"] + cul[,"Fontenelle"]
cul[,"Crystal"] <- cul[,"Crystal"] + cul[,"BlueMesa"]

for(i in 1:(length(site$names)-1)){
	
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

###################################
# Plot the cumulative difference
# Lees Ferry PA
###################################
pdf(file.path(figPrefix,paPath,'cum-diff-lees.pdf'),width=7.5,height=2.5)

z <- diff[,'LeesFerry2']
this.cul <- cumsum.ts(cul[,'LeesFerry'])
this.ag <- cumsum.ts(ag[,'LeesFerry'])

x <- as.vector(time(this.ag)-1/12)
	#Margins
par(mar=c(2,4,1,1))
	#Cumulative difference
plot(z, xlab = '', ylab = 'Cum diff (MAF)', col = 'purple', 
		xlim=c(time(diff)[1],eComp),
		ylim = c(min(c(z,this.cul)), max(c(z,this.cul))))
	# Total CUL
lines(x,this.cul)
	#Ag CUL
lines(x,this.ag,lty="dashed")
legend('topleft',c('Lees Ferry',"Total CUL","Ag CUL"),
		col=c('purple',1,1),lty=c(1,1,2))

dev.off()


#########################################################
# Calculate the average cumulative difference for one year
#########################################################

	# compute difference in MAF from thousand ac-ft
	
	#get the mean difference for each season
	#s is a data.frame
diff <- as.matrix((usbr.nat - cbrfc.web)/1e3)
attr(diff,"dimnames") <- attributes(usbr.nat)$dimnames
	
f <- function(x) quantile(x,0.5,na.rm=T)
s <- as.matrix(seasonal.stat(diff, f))
f <- function(x) quantile(x,0.05,na.rm=T)
smin <- as.matrix(seasonal.stat(diff, f))
f <- function(x) quantile(x,0.95,na.rm=T)
smax <- as.matrix(seasonal.stat(diff, f))

	#names of months
mon <- month.abb

#########################################################
# Plot the median cumulative difference, 
# each site in separate frame
#########################################################	

pdf(file.path(figPrefix,'diff-err.pdf'))

layout(matrix(1:(length(site$names)-1),ncol=2))
	
for(i in 1:(length(site$names)-1)){
	par(xaxt="n",mar=c(3,4,1,1))
	boxdata <- seasonal.stat.work(diff[,i],median,giveStack=T)
	plot(s[,i], xlab = '', ylab = 'Difference (MAF)',
		ylim = c(min(c(smin[,-(6:7)],s[,-(6:7)],boxdata),na.rm=T), 
				max(c(smax[,-(6:7)],s[,-(6:7)],boxdata),na.rm=T)), 
		type = 'n')
	myboxplot(as.data.frame(boxdata),add=T)
	lines(s[,i], col = col[i])
	par(xaxt="s")
	axis(1,seq(1:12),labels=mon)
	legend('topleft',site$names[i],col=col[i],lty='solid')
}

dev.off()

#######################
# difference at lees only
#######################
pdf(file.path(figPrefix,paPath,'diff-err-lees.pdf'),height=5)
	
par(xaxt="n",mar=c(3,4,1,1))
boxdata <- seasonal.stat.work(diff[,'LeesFerry2'],median,giveStack=T)
plot(s[,7], xlab = '', ylab = 'Difference (MAF)',
	ylim = c(min(c(smin[,7],s[,7],boxdata),na.rm=T), 
			max(c(smax[,7],s[,7],boxdata),na.rm=T)), 
	type = 'n')
myboxplot(as.data.frame(boxdata),add=T)
lines(s[,7], col = 'purple')
par(xaxt="s")
axis(1,seq(1:12),labels=mon)
legend('topleft','Lees Ferry',col='purple',lty='solid')

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
pdf(file.path(figPrefix,paPath,'med-cum-diff-err-lees.pdf'), width=6,height=6)

n <- which(site$names == 'LeesFerry2')

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
lines(s, col = 'purple')
#for spaghetti plot
#for(i in 1:nrow(boxdata))
#	lines(boxdata[i,],col=col[i%%length(col)])
par(xaxt="s")
axis(1,seq(1:12),labels=mon)
legend('topleft','Lees Ferry',col='purple',lty='solid')

dev.off()


###################################
# Plot the percent differences
###################################
pdf(file.path(figPrefix,'percent-diff.pdf'),
	width=6.5,height=8.5)
	
col <- rainbow(length(site$names)-1, alpha=.8)
p <- diff.ts( usbr.nat, cbrfc.web, percent = TRUE )

layout(matrix(1:(length(site$names)-1),ncol=1))
for(i in 1:(length(site$names)-1)){
	par(mar=c(2,4,1,1))
	plot(p[,i], xlab = '', ylab = '% difference', col = col[i], 
		ylim = c(0,120),xlim=c(time(p)[1],eComp))
	legend('topleft',site$names[i],col=col[i],lty='solid')
}
dev.off()

###################################
# Plot the percent differences Lees Ferry
###################################
pdf(file.path(figPrefix,paPath,'percent-diff-lees.pdf'),width=7.5,height=2.5)
	
p <- diff.ts( usbr.nat[,'LeesFerry2'], cbrfc.web[,'PA'], percent = TRUE )

par(mar=c(2,4,1,1))
plot(p, xlab = '', ylab = '% difference', col = 'purple', 
	ylim = c(0,120),xlim=c(time(p)[i],eComp))
legend('topleft','Lees Ferry',col='purple',lty='solid')

dev.off()


###################################
# Plot the percent difference and 
# flows in a year with large percent diff
###################################
pdf(file.path(figPrefix,paPath,'percent-diff-flow-1994.pdf'))

p <- diff.ts( usbr.nat[,'LeesFerry2'], cbrfc.web[,'PA'], percent = TRUE )

a <- ts(window(usbr.nat[,'LeesFerry2'],1994,c(1994,12)),start=1994,frequency=12)
b <- ts(window(cbrfc.web[,'PA'],1994,c(1994,12)),start=1994,frequency=12)
d <- ts(window(p,1994,c(1994,12)),start=1994,frequency=12)

layout(cbind(c(1,2)))
plot(a,ylim=range(c(a,b)),col='steelblue',ylab='Flow')
lines(b)

plot(d,ylab='% Difference')
dev.off()
