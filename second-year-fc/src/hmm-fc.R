library(HiddenMarkov)
library(tikzDevice)
library(sm)
source('lib.R')

dataset <- 'hist-annual'
meko <- read.table('data/Meko.txt')
meko <- ts(meko[,2],start=meko[1,1])/10^6

if(dataset == 'hist-seasonal'){
    x <- read.table("data/lf-seasonal-apr-jul.txt")
}else if(dataset == 'hist-annual'){
    x <- read.table("data/lf-annual-cy-af.txt") 
}else if(dataset == 'paleo'){
    x <- read.table('data/Meko.txt')
}
x <- ts(x[,2],start=x[1,1])/10^6
sy <- start(x)[1]

plot.dir <- '../hmm-sim-paper/plots/'
family <- "norm"  # underlying distribution for hmm
m <- 2            # order to fit
stationary <- F   # use a stationary distribution of mixtures
do.ar <- TRUE     # generate forecasts using ar and 
nsims <- 1000
verif.start <- 1980
H <- 4             # number of forecast steps ahead
fcdist.len <- 500  # resolution of the output probability distribution
ic <- c("same.sd")#,"same.both","both.diff")

verif.range <- verif.start:end(x)[1]
verif.length <- length(verif.range)
tf <- tempfile()
fd.name <- ifelse(family == "norm", "normal", family)
fcfile <- file.path('cache',paste('fc-',m,'-',dataset,'.Rdata',sep=''))
ensfile <- file.path('cache',paste('ens-',m,'-',dataset,'.Rdata',sep=''))

hmm.n <- hmm.g <- decoding.n <- decoding.g <- hmm.fc.n <- hmm.fc.g <- 
  hmm.fc.n.multi <- hmm.fc.g.multi <- 
  ar <- ar.fc <- vector('list',verif.length)
hmm.fc.n.traces <- hmm.fc.g.traces <- array(NA,c(nsims,verif.length,H))

bic.n <- bic.g <- aic.n <- aic.g <- numeric(verif.length)

    # loop through and fit hmm for each model order and each year in the 
    # verification range.
    # i.e. add a year fit the model again, forecast, repeat

Pi <- matrix(rep(1/m,m^2),m)
delta <- c(rep(0,m-1),1)
delta <- c(.2,.3,0.5)

cat('Fitting order',m,'Models...\n')
flush.console()
pb <- txtProgressBar(1,verif.length,style=3)

if(!file.exists(fcfile)){
    for(i in 1:verif.length){
        setTxtProgressBar(pb,i)

            # all x data up to the year before the current verification year
        this.x <- window(x,start(x),c(verif.start+i-2))
    
        pars.n <- get.named.parlist(this.x, m, 'normal', lower=.01, ic)
        pars.g <- get.named.parlist(this.x, m, 'gamma', lower=.01, ic)

        hmm.n[[i]] <- dthmm(this.x, Pi=Pi, delta=delta, 'norm', 
                pars.n, nonstat=!stationary)
        hmm.g[[i]] <- dthmm(this.x, Pi=Pi, delta=delta, 'gamma', 
                pars.g, nonstat=!stationary)        
    
       
        sink(tf)
            # estimate the parameters of the HMM model
        hmm.n[[i]] <- BaumWelch(hmm.n[[i]])
        hmm.g[[i]] <- BaumWelch(hmm.g[[i]])
        sink()

            # get the hidden states from the fittet model 
        decoding.n[[i]] <- ts(Viterbi(hmm.n[[i]]),start=sy)
        decoding.g[[i]] <- ts(Viterbi(hmm.g[[i]]),start=sy)

        hmm.fc.n[[i]] <- hmm.forecast.dist(hmm.n[[i]],len=fcdist.len)
        hmm.fc.g[[i]] <- hmm.forecast.dist(hmm.g[[i]],len=fcdist.len)
        hmm.fc.n.multi[[i]] <- hmm.forecast.dist(hmm.n[[i]],
          H=4,len=fcdist.len)
        hmm.fc.g.multi[[i]] <- hmm.forecast.dist(hmm.g[[i]],
          H=4,len=fcdist.len)
        
        #browser()
        
        hmm.fc.n.traces[,i,] <- hmm.forecast.traces(hmm.n[[i]],
          H=4,len=nsims)
        hmm.fc.g.traces[,i,] <- hmm.forecast.traces(hmm.g[[i]],
          H=4,len=nsims)
        
        
            # fit ar1 model for comparison
        ar[[i]] <- arima0(this.x,c(2,1,1))
        ar.fc[[i]] <- predict(ar[[i]],1)
    
        n <- length(this.x)
    
        bic.n[i] <- BIC(hmm.n[[i]])
        aic.n[i] <- AIC(hmm.n[[i]])
        bic.g[i] <- BIC(hmm.g[[i]])
        aic.g[i] <- AIC(hmm.g[[i]])
    
    }
    
    save(hmm.n, hmm.g, hmm.fc.n, hmm.fc.g, hmm.fc.n.multi, hmm.fc.g.multi, 
      hmm.fc.n.traces, hmm.fc.g.traces, decoding.n, decoding.g, ar, ar.fc, 
      bic.g, bic.n, aic.g, aic.n, file=fcfile)
    
}else{
  
  load(fcfile)
  
}


close(pb)

n <- length(x)

cat('AR Forecasts...\n')
flush.console()
if(do.ar){
  
  ar.pred <- sapply(sapply(ar.fc,"[",1),"[",1)
  ar.se <- sapply(sapply(ar.fc,"[",2),"[",1)

  ar.best <- vector('list',verif.length)
  range <- extendrange(x,f=.5)
  xrange <- seq(range[1],range[2],,fcdist.len)

  pb <- txtProgressBar(1,verif.length,style=3)
  for(i in 1:verif.length){
    
      setTxtProgressBar(pb,i)

      ar.best[[i]]$x <- xrange
      ar.best[[i]]$y <- dnorm(xrange,sd=ar.se[i],mean=ar.pred[i])
  }
  close(pb)
  rpss.ar <- RPSS(window(x,verif.start),ar.best,altobs=x)
  
}
    
cat('Diagnostics...\n')
flush.console()

  # this is the old way of doing it with the forecast dist
# rpss.n <- RPSS(window(x,verif.start),hmm.fc.n,altobs=x)
# rpss.g <- RPSS(window(x,verif.start),hmm.fc.g,altobs=x)

rpss.n <- RPSS(window(x,verif.start),hmm.fc.n.traces[,,1],altobs=x)
rpss.g <- RPSS(window(x,verif.start),hmm.fc.g.traces[,,1],altobs=x)

if(!file.exists(ensfile)){
  
  ens.n <- hmm.ensemble.fc(hmm.fc.n.multi, verif.start)
  ens.g <- hmm.ensemble.fc(hmm.fc.g.multi, verif.start)
  save(ens.n, ens.g, file=ensfile)
  
}else{
  
  load(ensfile)
  
}

cat('Overall RPSS---\n')
cat('\tAR:',median(rpss.ar),'\n')
cat('\tGamma:',median(rpss.g),'\n')
cat('\tNormal:',median(rpss.n),'\n\n')

ver <- x[time(x) %in% verif.range]
cat('Dry RPSS---\n')
dry <- ver < quantile(ver,1/3)
cat('\tAR:',median(rpss.ar[which(dry)]),'\n')
cat('\tGamma:',median(rpss.g[which(dry)]),'\n')
cat('\tNormal:',median(rpss.n[which(dry)]),'\n\n')

cat('Wet RPSS---\n')
wet <- ver > quantile(ver,2/3)
cat('\tAR:',median(rpss.ar[which(wet)]),'\n')
cat('\tGamma:',median(rpss.g[which(wet)]),'\n')
cat('\tNormal:',median(rpss.n[which(wet)]),'\n\n')

cat('Ave RPSS---\n')
cat('\tAR:',median(rpss.ar[which(!dry & !wet)]),'\n')
cat('\tGamma:',median(rpss.g[which(!dry & !wet)]),'\n')
cat('\tNormal:',median(rpss.n[which(!dry & !wet)]),'\n\n')

save(file='data/hmm-fc.Rdata', x, hmm.fc.g, hmm.fc.n, hmm.fc.n.multi, 
    hmm.fc.g.multi, hmm.g, hmm.n, ens.n, ens.g, hmm.fc.n.traces, 
    hmm.fc.g.traces, decoding.g, decoding.n, verif.start, rpss.g, rpss.n, 
    rpss.ar, ar.pred, ar.se, aic.g, aic.n, bic.g, bic.n, ar.best,ic)


####################
#
# Plotting
#
#####################

nfc <- length(hmm.g)


#------------------
# Boxplot forecasts
#------------------
tikz(file.path(plot.dir,'fc-hmm2.tex'),width=pc2in(20),height=pc2in(20),pointsize=11)
  layout(cbind(1:2))
  par(mar=c(3,4,1,1)+.1)
  hmm.boxplot.fc(hmm.fc.g,verif.start,ylim=range(x))
  lines(as.vector(window(x,verif.start)),col='red',lwd=2)

  hmm.boxplot.fc(hmm.fc.n,verif.start,ylim=range(x))
  lines(as.vector(window(x,verif.start)),col='red',lwd=2)
  
  #    lines(ar.pred,col='blue')
  #    lines(ar.pred+ar.se,col='blue',lty=2)
  #    lines(ar.pred-ar.se,col='blue',lty=2)
dev.off()

#tikz(file.path(plot.dir,'fc-hmm2n.tex'),width=pc2in(20),height=pc2in(12.5),pointsize=11)
#  hmm.boxplot.fc(hmm.fc.n,verif.start,ylim=range(x))
#  lines(as.vector(window(x,verif.start)),col='red',lwd=2)
#dev.off()


#------------------
# Boxplot skills
#------------------
tikz(file.path(plot.dir,'boxplot-skills.tex'),width=pc2in(20),height=pc2in(20),pointsize=11)
  rpss <- cbind(rpss.g,rpss.n,rpss.ar)
  colnames(rpss) <- c('HM2G','HM2N','AR1')
  myboxplot.matrix(rpss,outline=F,ylab='RPSS',xlab='')
  abline(h=0)
dev.off()

##------------------
## Decoding
##------------------
#pdf(file.path(path,paste('decoding-hm2g.pdf',sep='')))
#  plot(decoding.g[[nfc]],type='h',axes=F,ylab='State')
#  axis(2,1:2)
#  box()
#  axis(1)
#dev.off()
#
#pdf(file.path(path,paste('decoding-hm2n.pdf',sep='')))
#  plot(decoding.n[[nfc]],type='h',axes=F,ylab='State')
#  axis(2,1:2)
#  box()
#  axis(1)
#dev.off()
#

#------------------------------
# Forecast Distributions (Wet)
#------------------------------
yr <- 1984
yri <- which(verif.range == yr)
this.index <- yr-verif.start+1
tikz(file.path(plot.dir,paste('pdf-shift',yr,'.tex',sep='')),width=pc2in(20),height=pc2in(20),pointsize=11)
    plot(hmm.fc.g[[this.index]],type='l',col='blue',xlab='Flow [MAF]',
      ylab='Probability Density',ylim=c(0,.12))
    #lines(hmm.fc[[1]][[1]][[this.index]],col='green')
    lines(hmm.fc.n[[this.index]],col='green')
    lines(ar.best[[this.index]],col='red')
    sm.density(x,add=T)
    abline(v=window(x,yr,yr))

    #q <- quantile(x,.8)
    #xp <- hmm.fc.n[[yrsi[1]]]$x
    #yp <- hmm.fc.n[[yrsi[1]]]$y
    #filter <- xp >= q
    #xp <- xp[filter]
    #yp <- yp[filter]
    #yv <- yp[which(filter)[1]]
    #polygon(c(q,xp,q),c(yv,yp,0),col='gray',density=4)

    legend('topright',
        c('Clim','HM2G','HM2N','AR1'),
        col=c('black','blue','green','red'),lty=1)
dev.off()

# pdf.wet <- cbind(hmm.fc.g[[5]]$y, hmm.fc.n[[5]]$y, ar.best[[5]]$y)
# rownames(pdf.wet) <- hmm.fc.g[[5]]$x
# colnames(pdf.wet) <- c('HM2G','HM2N','AR1')
# pdf.wet.df <- melt(pdf.wet)
# names(pdf.wet.df) <- c('x','model','y')
# pdf.wet.df <- rbind(pdf.wet.df, cbind(rep('Climatology',length(x)),x))
# ggplot(pdf.wet.df,aes(x=x,y=y)) + geom_path(aes(color=model)) + 
# geom_density(aes(x=x,y=..density..),data=as.data.frame(x))

thresh <- quantile(x,c(.9,.8,.65))
#thresh <- c(4195,4200,4205)
arp <- hmm2gp <- hmm2np <- numeric(length(thresh))
for(i in 1:length(thresh)){

    arp[i] <- integrate(dnorm,thresh[i],Inf,mean=ar.pred[5],sd=ar.se[5])$value
    maxv <- max(hmm.fc.g[[yri]]$x)
    hmm2np[i] <- integrate(approxfun(hmm.fc.n[[yri]]),thresh[i],maxv,subdivisions=1000)$value
    maxv <- max(hmm.fc.n[[yri]]$x)
    hmm2gp[i] <- integrate(approxfun(hmm.fc.g[[yri]]),thresh[i],maxv,subdivisions=1000)$value
}
probs.wet <- data.frame(
    Climatology=1-ecdf(x)(thresh),
    HM2G=hmm2gp,
    HM2N=hmm2np,
    AR1=arp,
    row.names=round(thresh,3))


#------------------------------
# Forecast Distributions (Dry)
#------------------------------
yr <- 2004
yri <- which(verif.range == yr)
this.index <- yr-verif.start+1
tikz(file.path(plot.dir,paste('pdf-shift',yr,'.tex',sep='')),width=pc2in(20),height=pc2in(20),pointsize=11)
    plot(hmm.fc.g[[this.index]],type='l',col='blue',xlab='Flow [MAF]',
      ylab='Probability Density',ylim=c(0,.12))
    #lines(hmm.fc[[1]][[1]][[this.index]],col='green')
    lines(hmm.fc.n[[this.index]],col='green')
    lines(ar.best[[this.index]],col='red')
    sm.density(x,add=T)
    abline(v=window(x,yr,yr))

    #q <- quantile(x,.2)
    #xp <- hmm.fc.n[[yrsi[2]]]$x
    #yp <- hmm.fc.n[[yrsi[2]]]$y
    #filter <- xp <= q
    #xp <- xp[filter]
    #yp <- yp[filter]
    #yv <- yp[which(filter)[length(which(filter))]]

    #polygon(c(0,xp,q),c(0,yp,0),col='gray',density=4)

    legend('topright',
        c('Clim','HM2G','HM2N','AR1'),
        col=c('black','blue','green','red'),lty=1)
dev.off()


thresh <- quantile(x,c(.1,.2,.35))
#thresh <- c(4195,4200,4205)
arp <- hmm2gp <- hmm2np <- numeric(length(thresh))
for(i in 1:length(thresh)){

    arp[i] <- integrate(dnorm,0,thresh[i],mean=ar.pred[yri],sd=ar.se[yri])$value
    minv <- min(hmm.fc.g[[yri]]$x)
    hmm2gp[i] <- integrate(approxfun(hmm.fc.g[[yri]]),minv,thresh[i],subdivisions=1000)$value
    minv <- min(hmm.fc.n[[yri]]$x)
    hmm2np[i] <- integrate(approxfun(hmm.fc.g[[yri]]),minv,thresh[i],subdivisions=1000)$value
}
probs.dry <- data.frame(
    Climatology=ecdf(x)(thresh),
    HM2G=hmm2gp,
    HM2N=hmm2np,
    AR1=arp,
    row.names=round(thresh,3))

cat('\n1984\n')
print(round(probs.wet,2))
cat('\n2004 \n')
print(round(probs.dry,2))
#cat('Median Correlation Gamma:',cor(sapply(hmm.fc.g,median.dist),window(x,verif.start,end(x)[1])),'\n')
#cat('Median Correlation Normal:',cor(sapply(hmm.fc.n,median.dist),window(x,verif.start,end(x)[1])),'\n')
cat('Median Correlation Gamma:',cor(apply(hmm.fc.g.traces[,,1],2,median),window(x,verif.start,end(x)[1])),'\n')
cat('Median Correlation Normal:',cor(apply(hmm.fc.n.traces[,,1],2,median),window(x,verif.start,end(x)[1])),'\n')

#enso.ann <- ts.annual.mean(enso)[-142]
#summary(glm(window(decoding[[2]],1906,1996)~window(ntile.ts(enso.ann,2),1906,1996)))