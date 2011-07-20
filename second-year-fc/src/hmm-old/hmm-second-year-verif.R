library(HiddenMarkov)
source('lib.R')

datafile <- "data/lf-annual-cy-af.txt"#"data/LeesFerry.txt" # "data/Meko.txt" or "data/lf-annual-cy-af.txt"
#x <- as.matrix(read.table('data/LeesFerry.txt'))
x <- read.table(datafile)
sy <- x[1,1]
#x <- cbind(x[,1],apply(x[,5:8],1,sum))
x <- ts(x[,2]/10^6,start=sy,freq=1)
#x <- el.ts.ann
#x <- ts(rnorm(length(x)),start=sy,freq=1)
    #load mixture model parameters for initial conditions
#load('data/mixmod-lf-wy.RData')

family <- "norm"  # underlying distribution for hmm
max.order <- 2        # max order to fit
stationary <- F   # use a stationary distribution of mixtures
do.ar <- TRUE     # generate forecasts using ar and 
nsims <- 3000
verif.start <- 1980
fcdist.len <- 500  # resolution of the output probability distribution
ic <- c("same.sd")#,"same.both","both.diff")

verif.range <- verif.start:end(x)[1]
verif.length <- length(verif.range)
tf <- tempfile()
fd.name <- ifelse(family == "norm", "normal", family)

hmm <- decoding <- hmm.fc <- ar <- ar.fc <- list()
bic <- aic <- gcv <- vector('list',length(ic))
for(i in 1:length(ic))
    bic[[i]] <- aic[[i]] <- gcv[[i]] <- numeric(verif.length)

    # loop through and fit hmm for each model order and each year in the 
    # verification range.
    # i.e. add a year fit the model again, forecast, repeat
        
      # model order
  m <- max.order

  Pi <- matrix(rep(1/m,m^2),m)
      # random guess for the transition matrix
  #Pi <- matrix(runif(m^2),m)
  #Pi <- Pi/rowSums(Pi)
  delta <- c(rep(0,m-1),1)
  #delta <- c(.3,.5,.2)
  
  hmm <- decoding <- hmm.fc <- vector('list',length(ic))
  for(i in 1:length(ic))
      hmm[[i]] <- decoding[[i]] <- hmm.fc[[i]] <- vector('list',verif.length)
  
  cat('Fitting order',m,'Models...\n')
  flush.console()
  pb <- txtProgressBar(1,verif.length,style=3)

  for(i in 1:verif.length){
      setTxtProgressBar(pb,i)
  
          # all x data up to the year before the current verification year
      this.x <- window(x,start(x),c(verif.start+i-2))
      
      for(j in 1:length(ic)){
          pars <- get.named.parlist(this.x, m, fd.name, lower=.01, ic[j])
      
          hmm[[j]][[i]] <- dthmm(this.x, Pi=Pi, delta=delta, family, 
                  pars, nonstat=!stationary)
             
          sink(tf)
              # estimate the parameters of the HMM model
          hmm[[j]][[i]] <- BaumWelch(hmm[[j]][[i]])
          sink()
      
              # get the hidden states from the fittet model 
          decoding[[j]][[i]] <- ts(Viterbi(as.vector(hmm[[j]][[i]])),start=sy)
  
          hmm.fc[[j]][[i]] <- hmm.forecast.dist(hmm[[j]][[i]],H=4,len=fcdist.len)
      
              # fit ar1 model for comparison
          if(j==1 & do.ar){
              ar[[i]] <- arima0(this.x,c(2,1,1))
              ar.fc[[i]] <- predict(ar[[i]],1)
          }
          n <- length(this.x)
          ss <- sum(residuals(hmm[[j]][[i]])^2)
          se <- sd(residuals(hmm[[j]][[i]]))
          np <- (2*m+1+m*(m-1))
          LL <- hmm[[j]][[i]]$LL
          gcv[[j]][i] <- (ss/n)/(1-np/n)^2
          bic[[j]][i] <- n*log(se)+np*log(n)
          aic[[j]][i] <- 2*np-2*log(LL+1000)
      }
  }
  
  close(pb)


m <- 2:max.order
n <- length(x)

if(do.ar){
    ar.pred <- sapply(sapply(ar.fc,"[",1),"[",1)
    ar.se <- sapply(sapply(ar.fc,"[",2),"[",1)
}

cat('Sampling Best Models...\n')
flush.console()
ar.best <- vector('list',verif.length)
#ar.best <- matrix(NA,nrow=nsims,ncol=verif.length)
range <- extendrange(x,f=.2)
xrange <- seq(range[1],range[2],,fcdist.len)

pb <- txtProgressBar(1,verif.length,style=3)
for(i in 1:verif.length){
    
    setTxtProgressBar(pb,i)

    if(do.ar){
        ar.best[[i]]$x <- xrange
        ar.best[[i]]$y <- dnorm(xrange,sd=ar.se[i],mean=ar.pred[i])
        #ar.best[,i] <- rnorm(nsims,sd=ar.se[i],mean=ar.pred[i])
    }
}
close(pb)
if(do.ar) rpss.ar <- RPSS(window(x,verif.start),ar.best,altobs=x)


ens <- rpss <- list()
for(i in 1:length(ic)){
    rpss[[i]] <- matrix(NA,ncol=max.order-1,nrow=verif.length)
}
#for(k in m){
    
#    cat('Order',k+1,'diagnostics...\n')
#    flush.console()
    for(i in 1:length(ic)){
        ens <- hmm.ensemble.fc(hmm.fc[[i]],verif.start)
        rpss[[i]] <- RPSS(window(x,verif.start),hmm.fc[[i]],altobs=x)
    }
#}

for( i in 1:length(ic))
    print(median(rpss[[i]]))

save(file='data/hmm-fc.Rdata', x, hmm.fc, hmm, decoding, verif.start, #ens
    rpss, rpss.ar, max.order, ar.pred, ar.se, gcv, aic, bic,ar.best,ic, ens)

#enso.ann <- ts.annual.mean(enso)[-142]
#summary(glm(window(decoding[[2]],1906,1996)~window(ntile.ts(enso.ann,2),1906,1996)))