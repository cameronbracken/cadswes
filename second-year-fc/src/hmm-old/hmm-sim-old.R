source('lib.R')
require(HiddenMarkov)
require(tseries)
require(sm)
require(e1071)

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

plot.dir <- 'sim-plots'
sim.len <- length(x)
nsims <- 5000
count.max <- 25
ns <- 3
fd.name <- 'gamma'
dist <- 'gamma'
ics <- c("same.sd","same.both","both.diff")
ic.type <- 1
tf <- tempfile()
eval <- seq(0,13,,100)

hmm.counts.2 <- hmm.counts.3 <- ar.counts.1 <- ar.counts.0 <- nhm.counts <- matrix(0,ncol=count.max,nrow=nsims)
hmm.runs.2 <- hmm.runs.3 <- ar.runs.1 <- ar.runs.0 <- nhm.runs <- matrix(NA,ncol=sim.len,nrow=nsims)
hmm.sims.2 <- hmm.sims.3 <- ar.sims.1 <- ar.sims.0 <- nhm.sims <- matrix(NA,ncol=sim.len,nrow=nsims)
#hmm.dens.2 <- hmm.dens.3 <- ar.dens.1 <- ar.dens.0 <- matrix(NA,ncol=length(eval),nrow=nsims)
hmm.dens.2 <- hmm.dens.3 <- ar.dens.1 <- ar.dens.0 <- nhm.dens <- matrix(NA,ncol=512,nrow=nsims)


scale <- wavelet(rep(0,length(x)),pad=0,dj=0.05)$scale
hmm.pow.2 <- hmm.pow.3 <- ar.pow.1 <- ar.pow.0 <- nhm.pow <- matrix(0,ncol=length(scale),nrow=nsims)

#Pi.2 <- matrix(runif(2^2),2)
#Pi.2 <- Pi.2/rowSums(Pi.2)
Pi.2 <- matrix(rep(1/2,2^2),2)
delta.2 <- c(1,0)

#Pi.3 <- matrix(runif(3^2),3)
#Pi.3 <- Pi.3/rowSums(Pi.3)
Pi.3 <- matrix(rep(1/3,3^2),3)
delta.3 <- c(.2,.3,0.5)

pars.2 <- get.named.parlist(x, 2, fd.name, lower=.01, ics[ic.type])
pars.3 <- get.named.parlist(x, 3, fd.name, lower=.01, ics[ic.type])

hmm.2 <- dthmm(x, Pi.2, delta.2, dist, pars.2)
#hmm.3 <- dthmm(x, Pi.3, delta.3, dist, pars.3)
hmm.3 <- dthmm(x, Pi.3, delta.3, dist, pars.3)

sink(tf)
hmm.2 <- BaumWelch(hmm.2)
hmm.3 <- BaumWelch(hmm.3)
sink()

ar.0 <- arma(x,c(0,0))
ar.1 <- arma(x,c(1,0))

nhm.sims <- as.matrix(read.table("NHM-cam/NM_paleo_flow(orig).txt"))[1:nsims,]/10^6

pb <- txtProgressBar(1,nsims,style=3)
for(i in 1:nsims){
    
     setTxtProgressBar(pb,i)

        # Simulate from each model 
    hmm.sim.2 <- simulate(dthmm(NULL, hmm.2$Pi, delta.2, dist, hmm.2$pm), nsim=sim.len)
    hmm.sim.3 <- simulate(dthmm(NULL, hmm.3$Pi, delta.3, dist, hmm.3$pm), nsim=sim.len)
    ar.sim.0 <- arima.sim(ar.0,sim.len)[-1] * sd(x) + mean(x)
    ar.sim.1 <- arima.sim(ar.1,sim.len)[-1] * sd(x) + mean(x)
    nhm.sim <- nhm.sims[i,]
        # Save the simulations for later 
    hmm.sims.2[i,] <- hmm.sim.2$x
    hmm.sims.3[i,] <- hmm.sim.3$x
    ar.sims.1[i,] <- ar.sim.0
    ar.sims.0[i,] <- ar.sim.1
    
    
        # HMM(2)
    runs.hmm.2 <- rl.count(hmm.sim.2$x,ns)
    hmm.runs.2[i,1:length(runs.hmm.2$rl)] <- runs.hmm.2$rl
    #hmm.dens.2[i,] <- sm.density(hmm.runs.2[i,!is.na(hmm.runs.2[i,])],positive=T,display="none",eval.points=eval,h=1)$estimate
    hmm.dens.2[i,] <- density(hmm.runs.2[i,],bw=1,from=1,to=count.max,na.rm=T)$y
    hmm.counts.2[i,runs.hmm.2$len] <- runs.hmm.2$count
    hmm.pow.2[i,] <- apply(wavelet(hmm.sim.2$x, pad=1, dj=.05)$power,1,sum)
    
        # HMM(3)
    runs.hmm.3 <- rl.count(hmm.sim.3$x,ns)
    hmm.runs.3[i,1:length(runs.hmm.3$rl)] <- runs.hmm.3$rl
    #hmm.dens.3[i,] <- sm.density(hmm.runs.3[i,!is.na(hmm.runs.3[i,])],positive=T,display="none",eval.points=eval)$estimate
    hmm.dens.3[i,] <- density(hmm.runs.3[i,],bw=1,from=1,to=count.max,na.rm=T)$y
    hmm.counts.3[i,runs.hmm.3$len] <- runs.hmm.3$count
    hmm.pow.3[i,] <- apply(wavelet(hmm.sim.3$x, pad=1, dj=.05)$power,1,sum)
    
        # AR(0)
    runs.ar.0 <- rl.count(ar.sim.0,ns)
    ar.runs.0[i,1:length(runs.ar.0$rl)] <- runs.ar.0$rl
    #ar.dens.0[i,] <- sm.density(ar.runs.0[i,!is.na(ar.runs.0[i,])],positive=T,display="none",eval.points=eval)$estimate
    ar.dens.0[i,] <- density(ar.runs.0[i,],bw=1,from=1,to=count.max,na.rm=T)$y
    ar.counts.0[i,runs.ar.0$len] <- runs.ar.0$count
    ar.pow.0[i,] <- apply(wavelet(ar.sim.0, pad=1, dj=.05)$power,1,sum)
    
        # AR(1)
    runs.ar.1 <- rl.count(ar.sim.1,ns)
    ar.runs.1[i,1:length(runs.ar.1$rl)] <- runs.ar.1$rl
    #ar.dens.1[i,] <- sm.density(ar.runs.1[i,!is.na(ar.runs.1[i,])],positive=T,display="none",eval.points=eval)$estimate
    ar.dens.1[i,] <- density(ar.runs.1[i,],bw=1,from=1,to=count.max,na.rm=T)$y
    ar.counts.1[i,runs.ar.1$len] <- runs.ar.1$count
    ar.pow.1[i,] <- apply(wavelet(ar.sim.1, pad=1, dj=.05)$power,1,sum)
    
        # NHM
    runs.nhm <- rl.count(nhm.sims[i,],ns)
    nhm.runs[i,1:length(runs.nhm$rl)] <- runs.nhm$rl
    #ar.dens.1[i,] <- sm.density(ar.runs.1[i,!is.na(ar.runs.1[i,])],positive=T,display="none",eval.points=eval)$estimate
    nhm.dens[i,] <- density(nhm.runs[i,],bw=1,from=1,to=count.max,na.rm=T)$y
    nhm.counts[i,runs.nhm$len] <- runs.nhm$count
    nhm.pow[i,] <- apply(wavelet(nhm.sim, pad=1, dj=.05)$power,1,sum)
    
}
close(pb)

#count.min <- 5
#count.max <- 13
#myboxplot.matrix(ar.counts.0[,count.min:count.max],outline=F,
#    at=seq(1,by=5,len=(count.max-count.min+1)),xlim=c(count.min,count.max*4),axes=F)
#myboxplot.matrix(ar.counts.1[,count.min:count.max],outline=F,
#    at=seq(2,by=5,len=(count.max-count.min+1)),xlim=c(count.min,count.max*4),add=T,axes=F)
#myboxplot.matrix(hmm.counts.2[,count.min:count.max],
#    outline=F,at=seq(3,by=5,len=(count.max-count.min+1)),xlim=c(count.min,count.max*4),add=T)
#myboxplot.matrix(hmm.counts.3[,count.min:count.max],
#    outline=F,at=seq(4,by=5,len=(count.max-count.min+1)),xlim=c(count.min,count.max*4),add=T,axes=F)

pdf(file.path(plot.dir,paste(dataset,dist,'sim-lengths-mean.pdf',sep='-')))
mean.counts <- rbind(
    apply(ar.counts.0,2,mean),
    apply(ar.counts.1,2,mean),
    apply(hmm.counts.2,2,mean),
    apply(hmm.counts.3,2,mean),
    apply(nhm.counts,2,mean))[,5:12]
colnames(mean.counts) <- 5:12
rownames(mean.counts) <- c('AR0','AR1','HM2','HM3', 'NHM')
barplot(mean.counts,beside=T,legend=T,xlab='Run Length',ylab='Mean Count',ylim=c(0,1))
dev.off()

pdf(file.path(plot.dir,paste(dataset,dist,'spectrum.pdf',sep='-')))
    x.pow <- apply(wavelet(x,pad=1,dj=.05)$power,1,sum)
    plot(scale,x.pow,t='l',log='x',ylab='Wavelet Power', xlab="Calendar Year",lwd=2,ylim=c(0,7000))
    lines(scale,apply(hmm.pow.3,2,quantile,.25),col=2)
    lines(scale,apply(hmm.pow.3,2,quantile,.75),col=2)
    lines(scale,apply(hmm.pow.2,2,quantile,.75),col=3)
    lines(scale,apply(hmm.pow.2,2,quantile,.25),col=3)
    lines(scale, apply(ar.pow.1,2,quantile,.25),col=4)
    lines(scale, apply(ar.pow.1,2,quantile,.75),col=4)
    lines(scale, apply(ar.pow.0,2,quantile,.25),col=5)
    lines(scale, apply(ar.pow.0,2,quantile,.75),col=5)
    lines(scale, apply(nhm.pow,2,quantile,.25),col=6)
    lines(scale, apply(nhm.pow,2,quantile,.75),col=6)
    legend('topleft',c('NHM','AR(0)','AR(1)','HM(2)','HM(3)','Historical'),lty=1,col=6:1,pch=-1,lwd=c(1,1,1,1,1,2))
dev.off()

pdf(file.path(plot.dir,paste(dataset,dist,'sim-lengths-density.pdf',sep='-')))
    upper.ar <- apply(hmm.dens.2,2,quantile,.99)
    lower.ar <- apply(hmm.dens.2,2,quantile,.01)
    med.ar <- apply(ar.dens.1,2,quantile,.5)
    
    upper.hmm <- apply(ar.dens.1,2,quantile,.99)
    lower.hmm <- apply(ar.dens.1,2,quantile,.01)
    med.hmm <- apply(hmm.dens.2,2,quantile,.5)
    #upper.ar <- apply(ar.dens.1,2,quantile,.99)
    #lower.ar <- apply(ar.dens.1,2,quantile,.01)
    d.hmm.sim <- density(hmm.runs.3[i,],bw=1,from=1,to=count.max,na.rm=T)
    d.ar.sim <- density(ar.runs.1[i,],bw=1,from=1,to=count.max,na.rm=T)
    d.hist <- density(rle(as.vector(ntile.ts(meko,3)))$lengths,bw=1,from=1,to=13)

    plot(d.hmm.sim$x,upper.hmm,xlim=c(1,10),type='n',xlab='Run Lengths',ylab='Probability Density')
    polygon(c(d.hmm.sim$x,rev(d.hmm.sim$x)),c(upper.hmm,rev(lower.hmm)),col='lightgray',border=NA)
    polygon(c(d.ar.sim$x,rev(d.ar.sim$x)),c(upper.ar,rev(lower.ar)),col='lightblue',border=NA)
    lines(d.hmm.sim$x,med.hmm,col='blue')
    lines(d.ar.sim$x,med.ar,col='green')
    #polygon(c(eval,rev(eval)),c(upper.ar,rev(lower.ar)))
    lines(d.hist,col='red')
    legend('topright',c('HMM(3)','Historical','AR(1) Median'),lty=c(0,1,1),
        col=c('lightgray','red','green'),pch=c(22,-1,-1),pt.bg='lightgray')
dev.off()

pdf(file.path(plot.dir,paste(dataset,dist,'stats-hmm2.pdf',sep='-')),width=7,height=5)
    boxplot.stats(hmm.sims.2,x)
dev.off()

pdf(file.path(plot.dir,paste(dataset,dist,'stats-hmm3.pdf',sep='-')),width=7,height=5)
    boxplot.stats(hmm.sims.3,x)
dev.off()

pdf(file.path(plot.dir,paste(dataset,dist,'stats-nhm.pdf',sep='-')),width=7,height=5)
    boxplot.stats(nhm.sims,x)
dev.off()

pdf(file.path(plot.dir,paste(dataset,dist,'stats-all.pdf',sep='-')),width=8,height=7)
    boxplot.stats3(hmm.sims.3,nhm.sims,ar.sims.1,x,names=c('HM2','NHM','AR1'))
dev.off()

#hist(x,freq=F,ylim=c(0,.11),main='',xlab='Lees Ferry Total Flow [MAF]')
#lines(xrange,dnorm(xrange,mean=pm$mean[1],sd=pm$sd[1]),t='l')
#lines(xrange,dnorm(xrange,mean=pm$mean[2],sd=pm$sd[2]),t='l')

#plot(fin$scale,apply(fin$power,1,sum),t='l',log='x')
save(file=file.path('cache',paste('sim-counts-',dataset,'-',dist,'.Rdata',sep='')),
    ar.counts.0, ar.pow.0 ,ar.counts.1, ar.pow.1, hmm.counts.2, hmm.pow.2, 
    hmm.counts.3, hmm.pow.3, nhm.counts, nhm.pow, hmm.dens.2, hmm.dens.3, 
    ar.dens.0, ar.dens.1, nhm.dens, hmm.sims.2, hmm.sims.3, 
    ar.sims.0, ar.sims.1, nhm.sims, mean.counts)
    
    