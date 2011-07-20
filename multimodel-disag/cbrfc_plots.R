require(sm)

dir <- "diagnostics/plots_cbrfc"

if(!file.exists(dir)) dir.create(dir)

lf <- 20

years.retro <- years[years %in% (end(years)[1]-nback+1):end(years)[1]]
yr.2009 <- ifelse(vtype=='retro',which(years.retro == 2009),which(years == 2009))
yr.2010 <- ifelse(vtype=='retro',which(years.retro == 2010),which(years == 2010))
yr.2011 <- ifelse(vtype=='retro',which(years.retro == 2011),which(years == 2011))

vol.2009 <- apply(d$disag.tot[yr.2009,,lf,],2,sum)
vol.2010 <- apply(d$disag.tot[yr.2010,,lf,],2,sum)
vol.2011 <- apply(d$disag.tot[yr.2011,,lf,],2,sum)
vol.2010.yampa <- d$disag[yr.2010,3,12,]
vol.2011.yampa <- d$disag[yr.2011,3,12,]

pdf(file.path(dir,paste('2009-forecast-',predmonth,'-',vtype,'.pdf',sep='')))
    hist(vol.2009,freq=F,xlab='Apr-July Flow Volume [MAF]',main='2009 Seasonal Volume Forecast')
    rug(vol.2009)
    sm.density(vol.2009,add=T)
dev.off()

pdf(file.path(dir,paste('2010-forecast-',predmonth,'-',vtype,'.pdf',sep='')))
    hist(vol.2010,freq=F,xlab='Apr-July Flow Volume [MAF]',main='2010 Seasonal Volume Forecast')
    rug(vol.2010)
    sm.density(vol.2010,add=T)
dev.off()

pdf(file.path(dir,paste('2011-forecast-',predmonth,'-',vtype,'.pdf',sep='')))
    hist(vol.2011,freq=F,xlab='Apr-July Flow Volume [MAF]',main='2011 Seasonal Volume Forecast')
    rug(vol.2011)
    sm.density(vol.2011,add=T)
dev.off()

pdf(file.path(dir,paste('2010-forecast-',predmonth,'-',vtype,'-yampa','.pdf',sep='')))
    sm.density(vol.2010.yampa,xlab='Yampa Mar1 June Intervening Flow [MAF]',main='2011 Forecast')
    title(,main='2011 Forecast')
    sm.density(d$hist[,3,12],add=T,lty=2)
    legend('topright',c('Ensemble Forecast','Historical'),lty=1:2,pch=-1)
    hist(vol.2010.yampa,freq=F,add=T)
dev.off()


pdf(file.path(dir,paste('2011-forecast-',predmonth,'-',vtype,'-yampa','.pdf',sep='')))
    sm.density(vol.2011.yampa,xlab='Yampa Mar1 June Intervening Flow [MAF]',main='2011 Forecast')
    title(,main='2011 Forecast')
    sm.density(d$hist[,3,12],add=T,lty=2)
    legend('topright',c('Ensemble Forecast','Historical'),lty=1:2,pch=-1)
    hist(vol.2010.yampa,freq=F,add=T)
dev.off()