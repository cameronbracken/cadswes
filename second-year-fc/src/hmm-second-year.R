library(HiddenMarkov)
source('lib.R')

datafile <- "data/lf-annual-cy-af.txt" # "data/Meko.txt" or "data/lf-annual-cy-af.txt"
x <- read.table(datafile) #read in data
sy <- x[1,1]
x <- ts(x[,2]/10^6,start=sy,freq=1)
    #load mixture model parameters for initial conditions
#load('data/mixmod-lf-wy.RData')

family <- "norm"
order <- 5
stationary <- F
nsims <- 1000
verif.start <- 1980
fcdist.len <- 100  # resolution of the output pdf 
verif.range <- verif.start:end(x)[1]
verif.length <- length(verif.range)
tf <- tempfile()

hmm <- decoding <- hmm.fc <- ar <- ar.fc <- list()
#layout(cbind(1:(order-1)))

    # loop through and fit hmm for each model order and each year in the 
    # verification range.
    # i.e. add a year fit the model again, forecast, repeat
for(k in 1:(order-1)){
    
    m <- k + 1
    #hmm[[o]] <- dthmm(x, Pi=matrix(rep(1/o,o^2),o), delta=rep(1/o,o), "norm",
    #           list(mean=parameters(mm[[o]])[1,], sd=parameters(mm[[o]])[2,]))
    #Pi <- matrix(rep(1/m,m^2),m)
    Pi <- matrix(runif(m^2),m)
    Pi <- Pi/rowSums(Pi)
    delta <- rep(1/m,m)
    
    #hmm.fc[[o]] <- matrix(NA,ncol=verif.length,nrow=fcdist.len)
    
    hmm[[k]] <- decoding[[k]] <- hmm.fc[[k]] <- vector('list',verif.length)
    
    cat('Fitting order',m,'Models...\n')
    flush.console()
    pb <- txtProgressBar(1,verif.length,style=3)
    
    for(i in 1:verif.length){
        setTxtProgressBar(pb,i)
    
        this.x <- window(x,start(x),c(verif.start+i-1))
        
        pars <- list()
        for(j in 1:m){
            pars$mean[j] <- mean(this.x[ntile.ts(this.x,m) == (j-1)])
            pars$sd[j] <- sd(this.x[ntile.ts(this.x,m) == (j-1)])

        }
        
        hmm[[k]][[i]] <- dthmm(this.x, Pi=Pi, delta=delta, family, 
                pars, nonstat=!stationary)
               
        sink(tf)
        hmm[[k]][[i]] <- BaumWelch(hmm[[k]][[i]])
        sink()
        
        decoding[[k]][[i]] <- ts(Viterbi(hmm[[k]][[i]]),start=sy)
    
        hmm.fc[[k]][[i]] <- hmm.forecast.dist(hmm[[k]][[i]])
        
            # fit ar1 model for comparison
        if(k ==1){
            ar[[i]] <- arima0(this.x,c(1,0,0))
            ar.fc[[i]] <- predict(ar[[i]],1)
        }
    }
    close(pb)

    #plot(as.vector(time(x)),decoding[[o]],type='h',ylim=c(0,o),lwd=.5,
    #    main=paste('HMM order',o),xlab='',ylab='Decoded State')

}

ar.pred <- sapply(sapply(ar.fc,"[",1),"[",1)
ar.se <- sapply(sapply(ar.fc,"[",2),"[",1)

    # combine pdf's from each of the models based in their liklihood value
hmm.best <- ar.best <- matrix(NA,ncol=verif.length,nrow=nsims)
for(i in 1:verif.length){
    
    LL <- numeric(order-1)
    
    for(k in 1:(order-1))
        LL[k] <- hmm[[k]][[i]]$LL
    
    for(j in 1:nsims){
        w <- cumsum(1/LL/sum(1/LL))
        rand <- runif(1)
    
        which.model <- rank(c(rand,w))[1]
        
        hmm.best[j,i] <- hmm.fcdist.sim(hmm.fc[[which.model]][[i]],1)
    }
    ar.best[,i] <- rnorm(nsims,sd=ar.se[i],mean=ar.pred[i])
}

    # plot the original data as boxplots with histprical overlayed
ar.pred <- sapply(sapply(ar.fc,"[",1),"[",1)
ar.se <- sapply(sapply(ar.fc,"[",2),"[",1)
ens <- list()
for(k in 1:(order-1)){
    
    cat('Order',k+1,'diagnostics...\n')
    flush.console()
    
    pdf(paste('plots/hmm-order-',k+1,'.pdf',sep=''),width=7,height=4)
    hmm.boxplot.fc(hmm.fc[[k]],verif.start,ylim=range(x))
    lines(as.vector(window(x,verif.start)),col='red',lwd=2)
    lines(ar.pred,col='blue')
    lines(ar.pred+ar.se,col='blue',lty=2)
    lines(ar.pred-ar.se,col='blue',lty=2)
    dev.off()
    
    ens[[k]] <- hmm.ensemble.fc(hmm.fc[[k]],verif.start)
}

#enso.ann <- ts.annual.mean(enso)[-142]
#summary(glm(window(decoding[[2]],1906,1996)~window(ntile.ts(enso.ann,2),1906,1996)))