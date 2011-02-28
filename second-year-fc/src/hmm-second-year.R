library(HiddenMarkov)
source('lib.R')

datafile <- "data/lf-annual-cy-af.txt" # "data/Meko.txt" or "data/lf-annual-cy-af.txt"
x <- read.table(datafile) #read in data
sy <- x[1,1]
x <- ts(x[,2]/10^6,start=sy,freq=1)
#x <- ts(rnorm(length(x)),start=sy,freq=1)
    #load mixture model parameters for initial conditions
#load('data/mixmod-lf-wy.RData')

family <- "norm"  # underlying distribution for hmm
max.order <- 4        # max order to fit
stationary <- F   # use a stationary 
do.ar <- TRUE     # generate forecasts using ar and 
nsims <- 3000
verif.start <- 1980
fcdist.len <- 200  # resolution of the output pdf 
ic <- c("same.sd","same.both","both.diff")

verif.range <- verif.start:end(x)[1]
verif.length <- length(verif.range)
tf <- tempfile()
fd.name <- ifelse(family == "norm", "normal", family)

hmm <- decoding <- hmm.fc <- ar <- ar.fc <- list()
bic <- aic <- gcv <- vector('list',length(ic))
for(i in 1:length(ic))
    bic[[i]] <- aic[[i]] <- gcv[[i]] <- matrix(NA,ncol=max.order-1,nrow=verif.length)

    # loop through and fit hmm for each model order and each year in the 
    # verification range.
    # i.e. add a year fit the model again, forecast, repeat
for(k in 1:(max.order-1)){
        
        # model order
    m <- k + 1

    #Pi <- matrix(rep(1/m,m^2),m)
        # random guess for the transition matrix
    Pi <- matrix(runif(m^2),m)
    Pi <- Pi/rowSums(Pi)
    delta <- c(1,rep(0,m))
    
    hmm[[k]] <- decoding[[k]] <- hmm.fc[[k]] <- vector('list',length(ic))
    for(i in 1:length(ic))
        hmm[[k]][[i]] <- decoding[[k]][[i]] <- hmm.fc[[k]][[i]] <- vector('list',verif.length)
    
    cat('Fitting order',m,'Models...\n')
    flush.console()
    pb <- txtProgressBar(1,verif.length,style=3)
    
    for(i in 1:verif.length){
        setTxtProgressBar(pb,i)
    
            # all x data up to the year before the current verification year
        this.x <- window(x,start(x),c(verif.start+i-2))
        
        for(j in 1:length(ic)){
            pars <- get.named.parlist(this.x, m, fd.name, lower=.01, ic[j])
        
            hmm[[k]][[j]][[i]] <- dthmm(this.x, Pi=Pi, delta=delta, family, 
                    pars, nonstat=!stationary)
               
            sink(tf)
                # estimate the parameters of the HMM model
            hmm[[k]][[j]][[i]] <- BaumWelch(hmm[[k]][[j]][[i]])
            sink()
        
                # get the hidden states from the fittet model 
            decoding[[k]][[j]][[i]] <- ts(Viterbi(hmm[[k]][[j]][[i]]),start=sy)
    
            hmm.fc[[k]][[j]][[i]] <- hmm.forecast.dist(hmm[[k]][[j]][[i]])
        
                # fit ar1 model for comparison
            if(k ==1 & j==1 & do.ar){
                ar[[i]] <- arima0(this.x,c(1,0,0))
                ar.fc[[i]] <- predict(ar[[i]],1)
            }
            n <- length(this.x)
            ss <- sum(residuals(hmm[[k]][[j]][[i]])^2)
            se <- sd(residuals(hmm[[k]][[j]][[i]]))
            np <- (2*m+1+m*(m-1))
            LL <- hmm[[k]][[j]][[i]]$LL
            gcv[[j]][i,k] <- (ss/n)/(1-np/n)^2
            bic[[j]][i,k] <- n*log(se)+np*log(n)
            aic[[j]][i,k] <- 2*np-2*log(LL+1000)
        }
    }
    
    close(pb)

}

m <- 2:max.order
n <- length(x)

cat('Sampling Best Models...\n')
flush.console()
pb <- txtProgressBar(1,verif.length,style=3)
ar.best <- matrix(NA,nrow=nsims,ncol=verif.length)
for(i in 1:verif.length){
    
    setTxtProgressBar(pb,i)

    if(do.ar)
        ar.best[,i] <- rnorm(nsims,sd=ar.se[i],mean=ar.pred[i])
}
close(pb)


if(do.ar){
    ar.pred <- sapply(sapply(ar.fc,"[",1),"[",1)
    ar.se <- sapply(sapply(ar.fc,"[",2),"[",1)
    rpss.ar <- RPSS(window(x,verif.start),ar.best,alt=T,altobs=x)
}
ens <- rpss <- list()
for(i in 1:length(ic)){
    rpss[[i]] <- matrix(NA,ncol=max.order-1,nrow=verif.length)
    ens[[i]] <- list()
}
for(k in 1:(max.order-1)){
    
    cat('Order',k+1,'diagnostics...\n')
    flush.console()
    for(i in 1:length(ic)){
        ens[[k]][[i]] <- hmm.ensemble.fc(hmm.fc[[k]][[i]],verif.start)
        rpss[[i]][,k] <- RPSS(window(x,verif.start),ens[[k]][[i]],alt=T,altobs=x)
    }
}

for( i in 1:length(ic))
    print(apply(rpss[[i]],2,median))

save(file='data/hmm-fc.Rdata', hmm.fc, hmm, decoding, ens, 
    rpss, rpss.ar, max.order, ar.pred, ar.se, gcv, aic, bic)

#enso.ann <- ts.annual.mean(enso)[-142]
#summary(glm(window(decoding[[2]],1906,1996)~window(ntile.ts(enso.ann,2),1906,1996)))