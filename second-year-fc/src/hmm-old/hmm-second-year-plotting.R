load('data/hmm-fc.RData')
source('lib.R')
require(sm)

path <- 'plots'
nfc <- length(hmm[[1]][[1]])

for(k in 1:(max.order-1)){
    
    flush.console()
    
    pdf(paste(file.path(path,'hmm-order-'),k+1,'.pdf',sep=''),width=7,height=4)
    hmm.boxplot.fc(hmm.fc[[k]][[1]],verif.start,ylim=range(x))
    lines(as.vector(window(x,verif.start)),col='red',lwd=2)

    #    lines(ar.pred,col='blue')
    #    lines(ar.pred+ar.se,col='blue',lty=2)
    #    lines(ar.pred-ar.se,col='blue',lty=2)
    dev.off()
}

pdf(file.path(path,'boxplot-skills.pdf'))
colnames(rpss[[1]]) <- 2:max.order
myboxplot.matrix(cbind(rpss[[1]]),outline=F,ylab='RPSS',xlab='HMM Order')
abline(h=0)
dev.off()
print(apply(rpss[[1]],2,median))

for(i in 1:(max.order-1)){
    pdf(file.path(path,paste('decoding-order-',i+1,'.pdf',sep='')))
    plot(decoding[[i]][[1]][[nfc]],type='h',axes=F,ylab='State')
    axis(2,1:(i+1))
    box()
    axis(1)
    dev.off()
}

pdf(file.path(path,'decoding-period-means.pdf'),width=7,height=5)
    layout(cbind(1:2))
    par(mar=c(3,4,1,1)+.1)
    plot(decoding[[1]][[1]][[nfc]],type='h',axes=F,ylab='State',xlab='')
    axis(2,1:(i+1))
    box()
    axis(1)
   
    plot(x,ylab='Flow Volume [MAF]',xlab='')
    lines(c(1906,1929),rep(mean(window(x,1906,1929)),2),lwd=2)
    lines(c(1930,1981),rep(mean(window(x,1930,1981)),2),lwd=2)
    lines(c(1982,1986),rep(mean(window(x,1982,1986)),2),lwd=2)
    lines(c(1987,2009),rep(mean(window(x,1987,2009)),2),lwd=2)
    abline(h=hmm[[1]][[1]][[31]]$pm$mean[1],lty=2)
    abline(h=hmm[[1]][[1]][[31]]$pm$mean[2],lty=2)
    legend('bottomleft','HMM(2) Means',lty=2,pch=-1)
#    legend('topleft','Period Means',lty=1,pch=-1)
dev.off()

yrs <- c(1984,2004)
yrsi <- which(1980:2010 %in% yrs)
for(i in 1:length(yrs)){
    this.index <- yrs[i]-verif.start+1
    pdf(file.path(path,paste('pdf-shift',yrs[i],'.pdf',sep='')))
        plot(hmm.fc[[1]][[1]][[this.index]],type='l',col='blue',xlab='Flow [MAF]',ylab='Probability Density',ylim=c(0,.12))
        #lines(hmm.fc[[1]][[1]][[this.index]],col='green')
        sm.density(x,add=T)
        #abline(v=c(11,window(x,yrs[i],yrs[i])),lty=2:1)
        if(i == 1){
            q <- quantile(x,.8)
            xp <- hmm.fc[[1]][[1]][[yrsi[1]]]$x
            yp <- hmm.fc[[1]][[1]][[yrsi[1]]]$y
            filter <- xp >= q
            xp <- xp[filter]
            yp <- yp[filter]
            yv <- yp[which(filter)[1]]
    
            polygon(c(q,xp,q),c(yv,yp,0),col='gray',density=4)
            
        }
        
         if(i == 2){
                q <- quantile(x,.2)
                xp <- hmm.fc[[1]][[1]][[yrsi[2]]]$x
                yp <- hmm.fc[[1]][[1]][[yrsi[2]]]$y
                filter <- xp <= q
                xp <- xp[filter]
                yp <- yp[filter]
                yv <- yp[which(filter)[length(which(filter))]]

                polygon(c(0,xp,q),c(0,yp,0),col='gray',density=4)

        }
        #xrange <- seq(0,30,,1000)
        #lines(xrange,dnorm(xrange,mean=ar.pred[yrsi[i]],sd=ar.se[yrsi[i]]))
        legend('topright',
            c('Climatology','HMM 2nd order'),
            col=c('black','green'),lty=1)
    dev.off()
}
    

thresh <- quantile(x,c(.9,.8,.65))
#thresh <- c(4195,4200,4205)
arp <- hmm2p <- hmm3p <- hmm4p <- numeric(length(thresh))
for(i in 1:length(thresh)){
    
    arp[i] <- integrate(dnorm,thresh[i],Inf,mean=ar.pred[5],sd=ar.se[5])$value
    maxv <- max(hmm.fc[[1]][[1]][[yrsi[1]]]$x)
    hmm2p[i] <- integrate(approxfun(hmm.fc[[1]][[1]][[yrsi[1]]]),thresh[i],maxv,subdivisions=1000)$value
    #hmm3p[i] <- integrate(approxfun(hmm.fc[[2]][[1]][[yrsi[1]]]),thresh[i],maxv,subdivisions=1000)$value
}
probs84 <- data.frame(
    Climatology=1-ecdf(x)(thresh),
    HMM2=hmm2p,
    #HMM3=hmm3p,
    AR1=arp,
    row.names=thresh)
    
    
thresh <- quantile(x,c(.1,.2,.35))
#thresh <- c(4195,4200,4205)
arp <- hmm2p <- hmm3p <- hmm4p <- numeric(length(thresh))
for(i in 1:length(thresh)){

    arp[i] <- integrate(dnorm,0,thresh[i],mean=ar.pred[yrsi[2]],sd=ar.se[yrsi[2]])$value
    mv <- max(hmm.fc[[1]][[1]][[yrsi[2]]]$x)
    minv <- min(hmm.fc[[1]][[1]][[yrsi[1]]]$x)
    hmm2p[i] <- integrate(approxfun(hmm.fc[[1]][[1]][[yrsi[2]]]),minv,thresh[i],subdivisions=1000)$value
    #hmm3p[i] <- integrate(approxfun(hmm.fc[[2]][[1]][[yrsi[2]]]),minv,thresh[i],subdivisions=1000)$value
}
probs02 <- data.frame(
    Climatology=ecdf(x)(thresh),
    HMM2=hmm2p,
    #HMM3=hmm3p,
    AR1=arp,
    row.names=thresh)
    
cat('\n1984\n')
print(round(probs84,2))
cat('\n2004 \n')
print(round(probs02,2))