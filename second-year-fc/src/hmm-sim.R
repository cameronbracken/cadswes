source('lib.R')
require(HiddenMarkov)
require(tseries)
require(sm)
require(e1071)
require(gtools)
require(forecast)
require(tikzDevice)

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

plot.dir <- '../hmm-sim-paper/plots'
sim.len <- length(x)
nsims <- 1200
count.max <- 25
ns <- 3
fd.name <- 'gamma'
dist <- 'gamma'
ics <- c("same.sd","same.both","both.diff")
ic.type <- 1
tf <- tempfile()
eval <- seq(0,13,,100)
simfile <- file.path('cache',paste('sims-',dataset,'-',dist,'.Rdata',sep=''))
statsfile <- file.path('cache',paste('sim-stats-',dataset,'-',dist,'.Rdata',sep=''))

hmm.sims.2 <- hmm.sims.3 <- ar.sims.1 <- ar.sims.0 <- ar.sims.2 <- knn.sims <- 
nhm.sims <- hmm.sims.2.norm <- hmm.sims.3.norm <- matrix(NA,ncol=sim.len,nrow=nsims)


scale <- wavelet(rep(0,length(x)),pad=0,dj=0.05)$scale

#Pi.2 <- matrix(runif(2^2),2)
#Pi.2 <- Pi.2/rowSums(Pi.2)
Pi.2 <- matrix(rep(1/2,2^2),2)
delta.2 <- c(0,1)

#Pi.3 <- matrix(runif(3^2),3)
#Pi.3 <- Pi.3/rowSums(Pi.3)
Pi.3 <- matrix(rep(1/3,3^2),3)
delta.3 <- c(.2,.3,0.5)

Pi.4 <- matrix(rep(1/4,4^2),4)
delta.4 <- c(.2,.3,0.1,.4)

Pi.5 <- matrix(rep(1/5,5^2),5)
delta.5 <- c(.18,.4,0.05,.2,.15)

pars.2 <- get.named.parlist(x, 2, fd.name, lower=.01, ics[ic.type])
pars.3 <- get.named.parlist(x, 3, fd.name, lower=.01, ics[ic.type])
pars.4 <- get.named.parlist(x, 4, fd.name, lower=.01, ics[ic.type])
pars.5 <- get.named.parlist(x, 5, fd.name, lower=.01, ics[ic.type])
pars.2.norm <- get.named.parlist(x, 2, 'normal', lower=.01, ics[ic.type])
pars.3.norm <- get.named.parlist(x, 3, 'normal', lower=.01, ics[ic.type])
pars.4.norm <- get.named.parlist(x, 4, 'normal', lower=.01, ics[ic.type])
pars.5.norm <- get.named.parlist(x, 5, 'normal', lower=.01, ics[ic.type])

hmm.2 <- dthmm(x, Pi.2, delta.2, dist, pars.2)
hmm.3 <- dthmm(x, Pi.3, delta.3, dist, pars.3)
hmm.4 <- dthmm(x, Pi.4, delta.4, dist, pars.4)
hmm.5 <- dthmm(x, Pi.5, delta.5, dist, pars.5)
hmm.2.norm <- dthmm(x, Pi.2, delta.2, 'norm', pars.2.norm)
hmm.3.norm <- dthmm(x, Pi.3, delta.3, 'norm', pars.3.norm)
hmm.4.norm <- dthmm(x, Pi.4, delta.4, 'norm', pars.4.norm)
hmm.5.norm <- dthmm(x, Pi.5, delta.5, 'norm', pars.5.norm)

sink(tf)
hmm.2 <- BaumWelch(hmm.2)
hmm.3 <- BaumWelch(hmm.3)
hmm.4 <- BaumWelch(hmm.4)
hmm.5 <- BaumWelch(hmm.5)
hmm.2.norm <- BaumWelch(hmm.2.norm)
hmm.3.norm <- BaumWelch(hmm.3.norm)
hmm.4.norm <- BaumWelch(hmm.4.norm)
hmm.5.norm <- BaumWelch(hmm.5.norm)
sink()

ar.0 <- arima(x,c(0,0,0))
ar.1 <- arima(x,c(1,0,0))
ar.2 <- arima(x,c(2,0,0))

nhm.sims <- as.matrix(read.table("NHM-cam/NM_paleo_flow(orig).txt"))[1:nsims,]/10^6

if(!file.exists(simfile)){
    pb <- txtProgressBar(1,nsims,style=3)
    for(i in 1:nsims){
    
         setTxtProgressBar(pb,i)

            # Simulate from each model, use the best fit parameters 
        hmm.sim.2 <- simulate(dthmm(NULL, hmm.2$Pi, delta.2, dist, hmm.2$pm), nsim=sim.len)
        hmm.sim.2.norm <- simulate(dthmm(NULL, hmm.2.norm$Pi, delta.2, 'norm', hmm.2.norm$pm), nsim=sim.len)
        hmm.sim.3 <- simulate(dthmm(NULL, hmm.3$Pi, delta.3, dist, hmm.3$pm), nsim=sim.len)
        hmm.sim.3.norm <- simulate(dthmm(NULL, hmm.3.norm$Pi, delta.3, 'norm', hmm.3.norm$pm), nsim=sim.len)
        ar.sim.0 <- simulate(ar.0,sim.len)
        ar.sim.1 <- simulate(ar.1,sim.len)
        ar.sim.2 <- simulate(ar.2,sim.len)
        nhm.sim <- nhm.sims[i,]
    
            # Save the simulations for later 
        hmm.sims.2[i,] <- hmm.sim.2$x
        hmm.sims.2.norm[i,] <- hmm.sim.2.norm$x
        hmm.sims.3[i,] <- hmm.sim.3$x
        hmm.sims.3.norm[i,] <- hmm.sim.3.norm$x
        ar.sims.0[i,] <- ar.sim.0
        ar.sims.1[i,] <- ar.sim.1
        ar.sims.2[i,] <- ar.sim.2
        knn.sims[i,] <- knn.sim(x)

    
    }
    close(pb)

    save(file=simfile,
        hmm.sims.2,hmm.sims.3,hmm.sims.2.norm,hmm.sims.3.norm,ar.sims.0,
        ar.sims.1,ar.sim.2,nhm.sims,knn.sims)
}else{
    load(simfile)
}

if(!file.exists(statsfile)){
    hmm.2.stats <- sim.stats(hmm.sims.2,x)
    hmm.2.norm.stats <- sim.stats(hmm.sims.2.norm,x)
    hmm.3.stats <- sim.stats(hmm.sims.3,x)
    hmm.3.norm.stats <- sim.stats(hmm.sims.3.norm,x)
    ar.0.stats <- sim.stats(ar.sims.0,x)
    ar.1.stats <- sim.stats(ar.sims.1,x)
    ar.2.stats <- sim.stats(ar.sims.2,x)
    nhm.stats <- sim.stats(nhm.sims,x)
    knn.stats <- sim.stats(knn.sims,x)

    save(file=statsfile,
        hmm.2.stats,hmm.3.stats,hmm.2.norm.stats,hmm.3.norm.stats,ar.0.stats,
        ar.1.stats,ar.2.stats,nhm.stats, knn.stats)
}else{
    load(statsfile)
}

#count.min <- 5
#count.max <- 13
#myboxplot.matrix(ar.counts.0[,count.min:count.max],outline=F,
#    at=seq(1,by=5,len=(count.max-count.min+1)),xlim=c(count.min,count.max*4),axes=F)
#myboxplot.matrix(ar.1.stats$counts[,count.min:count.max],outline=F,
#    at=seq(2,by=5,len=(count.max-count.min+1)),xlim=c(count.min,count.max*4),add=T,axes=F)
#myboxplot.matrix(hmm.counts.2[,count.min:count.max],
#    outline=F,at=seq(3,by=5,len=(count.max-count.min+1)),xlim=c(count.min,count.max*4),add=T)
#myboxplot.matrix(hmm.counts.3[,count.min:count.max],
#    outline=F,at=seq(4,by=5,len=(count.max-count.min+1)),xlim=c(count.min,count.max*4),add=T,axes=F)
#
tikz(file.path(plot.dir,paste(dataset,dist,'sim-lengths-cum-mean.tex',sep='-')),width=pc2in(21),height=pc2in(22),pointsize=11)
cumsum.after <- 5
max.plot <- 14
mean.counts <- rbind(
    #apply(ar.0.stats$counts,2,mean),
    apply(ar.1.stats$counts,2,mean),
    apply(hmm.2.stats$counts,2,mean),
    #apply(hmm.3.stats$counts,2,mean),
    apply(hmm.2.norm.stats$counts,2,mean)
    )
colnames(mean.counts) <- 1:count.max
rownames(mean.counts) <- c('AR1','HM2G','HM2N')#, 'NHM')
mean.counts.pdf <- mean.counts[,(cumsum.after + 1):max.plot]
mean.counts.cdf <- t(apply(mean.counts[,!(colnames(mean.counts) %in% 1:(cumsum.after))],1,cumsum))[,1:(max.plot-cumsum.after)]
colnames(mean.counts.cdf) <- (cumsum.after + 1):max.plot

layout(cbind(1:2))
par(mar=c(3,4,1,1)+.1)
barplot(mean.counts.pdf,beside=T,legend=T,xlab='Run Length',ylab='Mean Count',
  ylim=c(0,max(mean.counts.pdf+.051)),pointsize=11)
barplot(mean.counts.cdf,beside=T,legend=T,xlab='Run Length',ylab='Mean Count',
  ylim=c(0,max(mean.counts.cdf+.051)),legend.text=FALSE)#c('AR1','HM2G','HM2N'),
  #args.legend=list(x='bottomright',bg='white'))
mtext('Run Length', 1,2)
dev.off()

tikz(file.path(plot.dir,paste(dataset,dist,'spectrum.tex',sep='-')),width=pc2in(20),height=pc2in(20),pointsize=11)
    x.pow <- apply(wavelet(x,pad=1,dj=.05)$power,1,sum)
    plot(scale,x.pow,t='l',log='x',ylab='Wavelet Power', xlab="Period",lwd=2,ylim=c(0,7000))
    #lines(scale,apply(hmm.3.stats$pow,2,quantile,.25),col=2)
    #lines(scale,apply(hmm.3.stats$pow,2,quantile,.75),col=2)
    lines(scale,apply(hmm.2.stats$pow,2,quantile,.75),col=2)
    lines(scale,apply(hmm.2.stats$pow,2,quantile,.25),col=2)
    #lines(scale, apply(hmm.2.norm.stats$pow,2,quantile,.25),col=3)
    #lines(scale, apply(hmm.2.norm.stats$pow,2,quantile,.75),col=3)
    lines(scale, apply(ar.1.stats$pow,2,quantile,.25),col=3)
    lines(scale, apply(ar.1.stats$pow,2,quantile,.75),col=3)
    #lines(scale, apply(ar.0.stats$pow,2,quantile,.25),col=5)
    #lines(scale, apply(ar.0.stats$pow,2,quantile,.75),col=5)
    #lines(scale, apply(nhm.stats$pow,2,quantile,.25),col=6)
    #lines(scale, apply(nhm.stats$pow,2,quantile,.75),col=6)
    #lines(scale, apply(knn.stats$pow,2,quantile,.25),col=6)
    #lines(scale, apply(knn.stats$pow,2,quantile,.75),col=6)
    legend('topleft',c('AR1','HM2G','Historical'),lty=1,col=3:1,pch=-1,lwd=c(1,1,1,2))
dev.off()

tikz(file.path(plot.dir,paste(dataset,dist,'sim-lengths-density.tex',sep='-')),width=pc2in(20),height=pc2in(20),pointsize=11)  
    upper.hmm <- apply(hmm.2.stats$dens,2,quantile,.99)
    lower.hmm <- apply(hmm.2.stats$dens,2,quantile,.01)
    med.hmm <- apply(hmm.2.stats$dens,2,quantile,.5)

    d.hmm.sim <- density(hmm.2.stats$runs[1,],bw=1,from=1,to=count.max,na.rm=T)
    d.hist <- density(rl.count(as.vector(meko),3)$rl,bw=1,from=1,to=13)
    hxp <- d.hmm.sim$x
    
    plot(hxp,upper.hmm,xlim=c(1,10),type='n',xlab='Run Lengths',ylab='Probability Density')
    polygon(c(hxp,rev(d.hmm.sim$x)),c(upper.hmm,rev(lower.hmm)),col='lightgray',border=NA)
    lines(d.hist,col='red')
    legend('topright',c('HM2G','Historical'),lty=c(0,1,1),
        col=c('lightgray','red'),pch=c(22,-1,-1),pt.bg='lightgray')
dev.off()

tikz(file.path(plot.dir,paste(dataset,dist,'stats-hmm2.tex',sep='-')),width=pc2in(20),height=pc2in(12.5),pointsize=11)
    boxplot.stats.list(hmm.2.stats,x)
dev.off()

tikz(file.path(plot.dir,paste(dataset,dist,'stats-hmm3.tex',sep='-')),width=pc2in(20),height=pc2in(12.5),pointsize=11)
    boxplot.stats.list(hmm.3.stats,x)
dev.off()

tikz(file.path(plot.dir,paste(dataset,dist,'stats-nhm.tex',sep='-')),width=pc2in(20),height=pc2in(12.5),pointsize=11)
    boxplot.stats.list(nhm.stats,x)
dev.off()

tikz(file.path(plot.dir,paste(dataset,dist,'stats-all.tex',sep='-')),width=pc2in(39),height=pc2in(24.4),pointsize=11)
    boxplot.stats.n.list(list(hmm.2.stats,hmm.2.norm.stats,ar.1.stats),
        x,names=c('HM2G','HM2N','AR1'))
dev.off()

tikz(file.path(plot.dir,paste(dataset,dist,'pdo-decoding.tex',sep='-')),width=pc2in(20),height=pc2in(14.3),pointsize=11)
    decoding <- ts(Viterbi(hmm.2),1906)
    #decoding[decoding==2] <- 0
    #decoding[decoding==1] <- 2
    #decoding[decoding==0] <- 1

    pdo.ts <- ts(as.vector(t(as.matrix(read.table('data/pdo.txt',na.strings='-9.90')[,2:13]))),1948,freq=12)
    pdo.ann.ts <- ts.annual.mean(pdo.ts)
    pdo.rec <- ts(read.table('data/pdo_reconstr.txt',header=T)$Index,1873)
    pdo.5yr <- ts(running(pdo.ann.ts,w=5,align='center'),1950)
    pdo.r.5 <- ts(running(pdo.rec,w=5,align='center'),1875)
    par(mar=c(5.1, 4.1, 4.1, 5.1))
    plot(window(decoding,start(pdo.ann.ts)),type='h',axes=F)
    axis(2,at=c(1,2,3))
    axis(1)
    box()
    par(usr=c(par("usr")[1:2], extendrange(pdo.r.5)))
    lines(pdo.5yr,lwd=2,col='red')
    #lines(pdo.r.5,lwd=2)
    mtext(side=4,'PDO',line=3,srt=180)
    axis(4)
dev.off()

tikz(file.path(plot.dir,paste(dataset,dist,'hmm2-pdf.tex',sep='-')),width=pc2in(20),height=pc2in(20),pointsize=11)
    boxplot(hmm.2.stats$pdf,outline=F,axes=F)
    axis(1,at=1:length(hmm.2.stats$pdf.x),labels=round(hmm.2.stats$pdf.x,0))
    box()
    axis(2)
    d <- sm.density(x, eval.points= hmm.2.stats$pdf.x,add=T,col='red',display='none')
    lines(d$estimate,col='red')
    title(xlab='Flow Volume [MAF]',ylab='Density')
dev.off()


tikz(file.path(plot.dir,paste(dataset,dist,'hmm-components.tex',sep='-')),width=pc2in(39),height=pc2in(24.4),pointsize=11)
  layout(matrix(1:4,2))
  plot.dthmm.stat(hmm.2,xlab='Flow Volume [MAF]',main='HM2G',ylim=c(0,.1))
  plot.dthmm.stat(hmm.3,xlab='Flow Volume [MAF]',main='HM3G',ylim=c(0,.1))
  plot.dthmm.stat(hmm.2.norm,xlab='Flow Volume [MAF]',main='HM2N',ylim=c(0,.1))
  plot.dthmm.stat(hmm.3.norm,xlab='Flow Volume [MAF]',main='HM3N',ylim=c(0,.1))
dev.off()


tikz(file.path(plot.dir,'decoding-period-means.tex'),width=pc2in(20),height=pc2in(20),pointsize=11)
    layout(cbind(1:2))
    par(mar=c(3,4,1,1)+.1)
    plot(decoding,type='h',axes=F,ylab='State',xlab='')
    axis(2,1:2)
    box()
    axis(1)
   
    plot(x,ylab='Flow Volume [MAF]',xlab='')
    lines(c(1906,1929),rep(mean(window(x,1906,1929)),2),lwd=2)
    lines(c(1930,1981),rep(mean(window(x,1930,1981)),2),lwd=2)
    lines(c(1982,1986),rep(mean(window(x,1982,1986)),2),lwd=2)
    lines(c(1987,2009),rep(mean(window(x,1987,2009)),2),lwd=2)
    abline(h=hmm.2$pm$mean[1],lty=2)
    abline(h=hmm.2$pm$mean[2],lty=2)
#    legend('bottomleft','HMM(2) Means',lty=2,pch=-1)
#    legend('topleft','Period Means',lty=1,pch=-1)
dev.off()

tikz(file.path(plot.dir,'lees-ts.tex'),width=pc2in(20),height=pc2in(14),pointsize=11)
  par(mar=c(3,4,1,1)+.1)
  plot(x,ylab='Flow [MAF]',lwd=2)
dev.off()

options(tikzMetricPackages=c(getOption('tikzMetricPackages'),'\\usepackage{amsmath}\n'))
tikz(file.path(plot.dir,'statdist.tex'),width=pc2in(20),height=pc2in(25),pointsize=11,)
  layout(cbind(1:2))
  par(mar=c(3,4,1,1)+.1)
  plot.dthmm.stat(hmm.2,xlab='',main='')
  title(main=paste('$\\boldsymbol\\delta=[',
      paste(round(statdist(hmm.2$Pi),2),collapse=', '),']$',sep=''),
    bty='n')
  plot.dthmm.stat(hmm.2.norm,xlab='',main='')
  title(main=paste('$\\boldsymbol\\delta=[',
      paste(round(statdist(hmm.2.norm$Pi),2),collapse=', '),']$',sep=''),
    bty='n')
  mtext('Flow [MAF]',1)
dev.off()

options(tikzMetricPackages=c(getOption('tikzMetricPackages'),'\\usepackage{amsmath}\n'))
tikz(file.path(plot.dir,'statdist3.tex'),width=pc2in(20),height=pc2in(25),pointsize=11,)
  layout(cbind(1:2))
  par(mar=c(3,4,1,1)+.1)
  plot.dthmm.stat(hmm.3,xlab='',main='')
  title(main=paste('$\\boldsymbol\\delta=[',
      paste(round(statdist(hmm.3$Pi),2),collapse=', '),']$',sep=''),
    bty='n')
  plot.dthmm.stat(hmm.3.norm,xlab='',main='')
  title(main=paste('$\\boldsymbol\\delta=[',
      paste(round(statdist(hmm.3.norm$Pi),2),collapse=', '),']$',sep=''),
    bty='n')
  mtext('Flow [MAF]',1)
dev.off()

#layout(cbind(1:2));par(mar=c(3,4,1,1)+.1)
# plot(1:54,xxlf$spec,col='red',t='l',axes=F,ylim=c(0,140))
# axis(1,pretty(1:54,6),labels=pretty(seq(0,.6,len=54),6));axis(2);box()
# boxplot(as.data.frame(ar.1.stats$spec),outline=F,log='y',add=T,axes=F)
# plot(1:54,xxlf$spec,col='red',t='l',axes=F,ylim=c(0,140))
# axis(1,pretty(1:54,6),labels=pretty(seq(0,.6,len=54),6));axis(2);box()
# boxplot(as.data.frame(hmm.2.stats$spec),outline=F,log='y',add=T,axes=F)

#plot(ts(hmm.2$u[,2],1906),t='b',ylab='Probability')
#lines(ts(Viterbi(hmm.2)-1,1906),t='h')
#abline(h=.5)
#axis(4,at=c(0,1),labels=c(1,2))
#mtext('Decoded State',4)