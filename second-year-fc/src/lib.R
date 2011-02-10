
laglead <- 
function(x,q,r){

    ## laglead:
    ##   This function gives all the chunks of a vector of length q 
    ##   followed by chunks of length r. It gives back a list of two 
    ##   matricies, lag and lead, essentially the lag q matrix 
    ##   and the subsequent r values. 
    ##
    ##   Inputs:
    ##      x - A vetor 
    ##      q - lead length
    ##      r - lag length 
    ##
    ## Ex.
    ## 
    ##    laglead(1:10,3,2)
    ##   $x
    ##        [,1] [,2] [,3]
    ##   [1,]    1    2    3
    ##   [2,]    2    3    4
    ##   [3,]    3    4    5
    ##   [4,]    4    5    6
    ##   [5,]    5    6    7
    ##   [6,]    6    7    8
    ##   
    ##   $y
    ##        [,1] [,2]
    ##   [1,]    4    5
    ##   [2,]    5    6
    ##   [3,]    6    7
    ##   [4,]    7    8
    ##   [5,]    8    9
    ##   [6,]    9   10
    
    
    n <- length(x)

  lag <- matrix(NA, n-q-r+1, q)
    lead <- matrix(NA, n-q-r+1, r)

    for(i in 1:(n-q-r+1)){
        lag[i,] <- x[i:(i+q-1)]
        lead[i,] <- x[(i+q):(i+q+r-1)]
    }
    return(list(lag=lag,lead=lead))
}


ts.annual.mean <- function(x){

    options(warn=-1)
    nyears <- length(unique(floor(time(x))))
    syear <- start(x)[1]
    nst = vector('numeric',nyears)

    for(i in 1:nyears)  nst[i] <- mean(window(x,syear - 1 + i,
                                        c(syear - 1 + i, frequency(x))))
   
    nst <- ts(nst, start = c(syear,1), frequency = 1) 
    return(nst)

}


ts.next.time <- function(x){
    en <- end(x)
    en <- if(en[2] == frequency(x))
            c(en[1] + 1, 1)
        else
            c(en[1], en[2] + 1)
    return(en)
}

ts.order.start <- function(x,start.per){

        f = frequency(x)
        ord <- if(start.per == 1) 
                start.per:f
           else if(start.per == f) 
                c(f, 1:(f - 1))
           else
                c(start.per:f, 1:(start.per - 1))
        return(ord)
}

ts.sim.and.calc <- function(x, order, annual.mean = TRUE, meanvec=NULL, nsims = 10000){

    x.fit <- arima(x,order=order)
    x.sim <- arima.sim(list(ar = coef(x.fit)), nsims, n.start = 100,
                                sd = sqrt(x.fit$sigma2))
    
    x.sim <- ts(x.sim, start = ts.next.time(x), frequency = frequency(x))
    if(annual.mean) x.sim <- ts.annual.mean(x.sim + meanvec)
    
    return(x.sim)
}

wapply <- 
function(x, fun, win.len = length(x), ...){
  
    ##  wapply:
    ##
    ##   applies a function to each succesive window of a time series;
    ##   good for things like annual means from monthly values.
    ##   note: does not return a ts object
    ##
    ##  Inputs:
    ##
    ##    x       - A timeseries 
    ##    fun     - function name to apply
    ##    win.len - length of window to apply fun to, defaults to length(x)
    ##
    ##  Ex.
    ##
    ##  wapply(ts(rnorm(12*10),frequency = 12),mean,12)
    ##  [1] -0.10910749  0.20212615  0.89948987 -0.27514597  0.18479893  0.55860823
    ##  [7]  0.38323747  0.08271312  0.26844408  0.03115793
    ##
    
    r <- (length(x) %% win.len)
    if(r > 0) x <- x[1:(length(x)-r)]
    stack <- matrix(x,nrow = win.len)
    return(apply(stack,2,fun,...))
    
}

binary.ts <- 
function(x, limit = median(x), tie = 1 ){
    # returns a binary representation of x, 1 above limit, 0 below
    #
    # Ex. 
    #
    # binary.ts(1:10)
    ## [1] 0 0 0 0 0 1 1 1 1 1
    # binary.ts(1:10,8)
    ## [1] 0 0 0 0 0 0 0 1 1 1
    # binary.ts(1:10,8,tie=0)
    ## [1] 0 0 0 0 0 0 0 0 1 1
    
    b <- integer(length(x))
    
    filter <- if(tie == 1) x >= limit else x > limit
    
    #only need to set the 1's because b is already 0's
    b[filter] <- 1L
    
    if(class(x) == 'ts') 
        return(ts(b,start=start(x),end=end(x))) 
    else 
        return(b)
}

ntile.ts <- 
function(x, n, limit.type = 'prob', tie = 1 ){
    # returns an integer vector corresponding to n states broken by equal 
    # probability or equal distance
    #
    limit <- 
    if(limit.type == 'prob') 
      quantile(x,seq(0,1,1/n))
    else 
      seq(min(x),max(x),by=diff(range(x))/n)
    
    b <- integer(length(x))
    
    for(i in 1:(n+1)){
        filter <- 
        if(tie == 1) 
            x >= limit[i] & x <= limit[i+1]
        else 
            x > limit[i] & x <= limit[i+1]
    
        #only need to set the 1's because b is already 0's
        b[filter] <- as.integer(i-1)
    }
    
    if(class(x) == 'ts') 
        return(ts(b,start=start(x),end=end(x))) 
    else 
        return(b)
}

binarys <- function(i,align=F){
    # returns the binary representation of integer vector i (coerced) 
    # as a character vector
    #
    # Ex.
    #
    # binarys(1:10)
    ## [1] "1"    "10"   "11"   "100"  "101"  "110"  "111"  "1000" "1001" "1010"
    # binarys(1:10,align=T)
    ## [1] "0001" "0010" "0011" "0100" "0101" "0110" "0111" "1000" "1001" "1010"
    #
    
    bb <- function(i) if (i) paste(bb(i %/% 2), i %% 2, sep="") else ""
        
        #number of binary digits of largest number
    width <- ceiling(log(max(i)+1,base=2))
    
        #get the character vector
    i <- sapply(as.integer(i),bb)
    
    if(align)
        return(sprintf(paste('%0',width,'d',sep=''),as.integer(i)))
    else
        return(i)
}

rows.equal <- function(x, y){
    
    if(is.null(dim(x))) x <- rbind(x)
    which( as.logical( 
            apply(x, 1, all.equal, y)))
}

trprob <- function(lag,lead,ns = max(lead)+1){
    
    require(gtools)
    
    q <- ncol(lag)
    r <- ncol(lead)
    
    states.lag <- permutations(ns,q,0:(ns-1),repeats.allowed=T)
    states.lead <- permutations(ns,r,0:(ns-1),repeats.allowed=T)
    nfrom <- nrow(states.lag)
    nto <- nrow(states.lead)
    
    nc <- ns^(q+r)
    
    tpm <- list(
        from = matrix(NA,nrow=nc,ncol=q),
        to = matrix(NA,nrow=nc,ncol=r), 
        p = matrix(NA,nrow=nfrom,ncol=nto),
        cor = matrix(NA,nrow=nfrom,ncol=nto),
        pts = matrix(NA,nrow=nfrom,ncol=nto),
        sig = matrix(NA,nrow=nfrom,ncol=nto),
        pval = matrix(NA,nrow=nfrom,ncol=nto),
        gcv = matrix(NA,nrow=nfrom,ncol=nto),
        pools = list()
        )
        
    pb <- txtProgressBar(1,nrow(states.lag),style=3)
    
    n <- 0
    tpm$pools$to <- tpm$pools$from <- list()
    rownames(tpm$p) <- character(nfrom)
    colnames(tpm$p) <- character(nto)
    
    for(i in 1:nfrom){
        setTxtProgressBar(pb,i)
        
            #all rows starting in a particular state
        pool <- rows.equal(lag,states.lag[i,])
        tpm$pools$from[[i]] <- pool
        tpm$pools$to[[i]] <- list()
    
        attr(tpm$pools$from[[i]],'state.from') <- states.lag[i,]
        
        for(j in 1:nto){
          
            n <- n + 1
              # record the current to and from states
            tpm$from[n,] <- states.lag[i,]
            tpm$to[n,] <- states.lead[j,]
            
            #matches <- rows.equal(cbind(lead[pool,]), states.lead[j,])
            this.state <- rbind(states.lead[j,])
            
              # turn vectors (single state to) into matricies, 
              # rbind handles the case of one entry only in the pool, 
              # but multiple to states 
            this.pool <- if(r==1) cbind(lead[pool,]) else rbind(lead[pool,])
            
              # If we get this condition, something is wrong.
            if(ncol(this.state) != ncol(this.pool))browser()
            
              # find the matching rows
            matches <- 
            if(nrow(this.pool) == 0){
              # then we're empty
              integer(0)
            }else{
              find.matches(this.state, this.pool, maxmatch=nrow(lag))$matches
            }
            
              # find.matches returns a single 0 if there are no matches
          if(length(matches) == 1) if( matches == 0 ) matches <- integer(0)
          
            # record the transition probability
            possible <- nrow(this.pool)
            tpm$p[i,j] <- length( matches ) / possible
              # set the transition state names, this sets them way too much
              # but it doesnt hurt anything
            rownames(tpm$p)[i] <- paste(tpm$from[n,],collapse='')
            colnames(tpm$p)[j] <- paste(tpm$to[n,],collapse='')
                
                #save the indexes of the states from and to
            tpm$pools$to[[i]][[j]] <- as.vector(matches)
          attr(tpm$pools$to[[i]],'state.from') <- states.lag[i,]
          attr(tpm$pools$to[[i]][[j]],'state.to') <- states.lead[j,]
           #browser()
        }
    }
    close(pb)
    return(tpm)
    
}

binary.combos <- function(n){
    as.matrix( expand.grid( rep( list(c(T,F)), n ) ) )
}

sim.pqr <- function(tr.paleo, conditional.pool = FALSE){
    
    if(conditional.pool){
            #simulate transition
        rand <- runif(1)
        which.to <- rank(c(rand,cumsum(this.p)))[1]
        state.to <- tr.paleo$to[which.to,]


            #conditional pool, quantiles given state.to
        pool.from <- tr.paleo$pools$from[[ which.from[which.to] ]]
        pool.to <- tr.paleo$pools$to[[ which.from[which.to] ]]
        pool.from <- matrix(pool.from,,q)
        pool.to <- matrix(pool.to,,r)
    }
    
}

mylag <- 
function(x,lag,docor=FALSE){

    if(lag>length(x)) 
        warning("Lag is larger than input vector length, returning NA's") 

    if(lag<0)  lagn = c(rep(NA,abs(lag)),x[-(length(x):(length(x)+lag+1))])
    if(lag==0) lagn = x    
    if(lag>0)  lagn = c(x[-(1:lag)],rep(NA,lag))

    remove = !is.na(lagn) & !is.na(x)
    if(docor){
        return(cor(x[remove],lagn[remove]))
    }else{
        return(lagn)
    }
}
searchCriteria <- function(x, lag.max.ami = 200, lag.max.acf = 400, d.max = 7, show = T){
    
    require(tseriesChaos)

    acf <- acf(x, lag.max = lag.max.acf,plot=F)
    acf <- acf$acf[,,1]

    ami <- mutual(x, lag.max = lag.max.ami, plot=F)

    T.acf.picked <- T.ami.picked <- FALSE
    for(i in 1:max(c(lag.max.acf,lag.max.ami))){
        if(i == max(c(lag.max.acf,lag.max.ami))) 
            stop('One of the T criteria is monotonically 
    decreasing, pick a lager lag.max.')
    
        if(!T.acf.picked){
            if(acf[i+1] > acf[i]){
                T.acf <- i
                T.acf.picked <- TRUE
            }
        }
        if(!T.ami.picked){
            if(ami[i+1] > ami[i]){ 
                T.ami <- i
                T.ami.picked <- TRUE
            }
        }
        if(T.acf.picked & T.ami.picked) break
    }
    cat('The lag suggested by the ACF criteria is:', T.acf,'\n')
    cat('The lag suggested by the AMI criteria is:', T.ami,'\n')

    fnn.acf <- false.nearest(x, m = d.max, d = T.acf, t=length(x)/10, eps=sd(x))
    fnn.ami <- false.nearest(x, m = d.max, d = T.ami, t=length(x)/10, eps=sd(x))

    d.acf <- order(abs(fnn.acf[1,1:(d.max-1)]-fnn.acf[1,2:d.max]))[1]
    d.ami <- order(abs(fnn.ami[1,1:(d.max-1)]-fnn.ami[1,2:d.max]))[1]

    cat('The FNN embedding dimension suggested by the ACF lag is:', d.acf,'\n')
    cat('The FNN embedding dimension suggested by the AMI lag is:', d.ami,'\n')

    #calculate correlation integrals
    cint <- d2(x, m=d.max, d=T.ami, t=.1, eps.min=1)
    
    invisible(list(T.acf=T.acf,T.ami=T.ami,d.acf=d.acf,d.ami=d.ami,cint=cint))
}

plot.d2.cam <- function (x, ...){
    m <- ncol(x)
    plot(x[, c(1, m)], type = "l", log = "xy",...)
    for (i in (m - 1):2) lines(x[, c(1, i)])
}


myscale <- function(x){
  
  (x-mean(x))/sd(x)
  
}


build.pools.from <- function(tr, qr){
  
  # Builds from pools in terms of actual values.
  # tr is an object from trprob()
  # qr is an object from laglead()
  nfrom <- nrow(unique(tr$from))
  
  pools.from <- vector('list',nfrom)
  
  for( i in 1:nfrom )
        pools.from[[i]] <- cbind(qr$lag[ tr$pools$from[[i]], ])
    
    return(pools.from)
  
}

build.pools.to <- function(tr, qr){
  
  # Builds to pools in terms of actual values.
  # tr is an object from trprob()
  # qr is an object from laglead()
  
  nfrom <- nrow(unique(tr$from))
  nto <- nrow(unique(tr$to))
  q <- ncol(tr$from)
  r <- ncol(tr$to)
  
  pools.to <- vector('list',nto)
  
  
  for( i in 1:nfrom ){
    
    pools.to[[i]] <- list()
    
    for( j in 1:nto ){
        #ito <- (i-1)*nto+j
        #browser()
          # filter first by the from state then the to state
        pools.to[[i]][[j]] <- 
          if(r == 1)
            cbind(cbind(qr$lead[ tr$pools$from[[i]], ])[tr$pools$to[[i]][[j]], ])
          else
            rbind(rbind(qr$lead[ tr$pools$from[[i]], ])[tr$pools$to[[i]][[j]], ])
    }
  }
    
    return(pools.to)
   
}

RPSS <- function(histobs, ensemble, alt=F, altobs=0){
    # This function calculates the Rank Probability Skill Score(RPSS)
    # for each prediction year and gives the RPSS for 'wet', 'dry' and all years
    # 'wet' is in upper 67th percentile, dry is in lower 33rd percentile

    nyr <- length(histobs) #Number of Years Record
    rpss <- 1:nyr          #Equivalent size to number of years

    for(j in 1:nyr) { 
        isim <- ensemble[,j]       # Ensemble of a particular year
        obsval <- histobs[j]   #Observed value of that particular year
 
        #remove the year we are on to get calibrated data
        calibdata <- histobs[-j]
    
        # Find 33rd and 67th percentile of calibrated data
        lq <- ifelse(alt, quantile(altobs,1/3), quantile(calibdata,1/3))
        uq <- ifelse(alt, quantile(altobs,2/3), quantile(calibdata,2/3))
             
        ncat <- 3      #Number of categories
        probs <- pclim <- obscat <- numeric(ncat)

        # assigns which category each of the simulations 
        # falls in for whole ensemble       
        nsim <- length(isim)       #Number of Simulations
        simcat <- numeric(nsim)
        simcat[isim >= uq] <- 3
        simcat[isim <= lq] <- 1
        simcat[isim > lq & isim <uq] <- 2

        # probability of simulations falling into each category        
        for(i in 1:ncat)       
            probs[i] <- length(simcat[simcat == i])/nsim 
        #print(probs)
        probs <- cumsum(probs)

        # raw probability of falling into each category   
        # 33% chance to fall in each one of 3 categories             
        pclim <- cumsum(rep(1,ncat)/ncat)
                        
        # where the obsevation actually falls        
        obscat <- rep(0,ncat)
        if(obsval >= uq) #in third category(quantile)
            obscat[3]=1
        if(obsval > lq & obsval < uq)   #obs in second quantile
            obscat[2]=1
        if(obsval <= lq) #obs in first quanitle
            obscat[1]=1
        
        obscat <- cumsum(obscat)

        rpss[j]=1-(sum((obscat-probs)^2))/(sum((obscat-pclim)^2))
  }
  
            

    quw=quantile(histobs,2/3)  
    zzw=rpss[histobs>quw]           
    
    qld=quantile(histobs,1/3)      
    zzd=rpss[histobs<qld]  

  RPSS=median(rpss)
  return(rpss)
}


oneoverk <- function(k){
    
    W <- numeric(k)
    for(j in 1:k)
        W[j] <- 1/j
    W <- W/sum(W)
    W
    
}

############################################################
############################################################
# sigcor:
#       Returns the minimum significant correlation at the 
#       given alpha value
sigcor <- function(n,alpha=.05){
 
    k <- qnorm(alpha)^2/(n-2)
    sqrt(k/(k+1))   
    
}

    #returns the best alpha and degree
best.par <- 
function(x,y, a = seq(0.2,1.0,by=0.05), n = length(a), f=c(gcvplot,aicplot),...){
    
    require(locfit)
    options(warn=-1)
        # get the gcv values for all combinations of deg and alpha
    d1 <- try(f(y~x, deg=1, alpha=a, kern='bisq', scale=T,...)$values,silent=T)
    if(class(d1) == "try-error") d1 <- numeric()
    d2 <- try(f(y~x, deg=2, alpha=a, kern='bisq', scale=T,...)$values,silent=T)
    if(class(d2) == "try-error") d2 <- numeric()
    options(warn=0)
    
    gcvs <- c(d1,d2)
    
    if(length(gcvs)==0){
        bestalpha <- bestd <- 1
        gcv <- NA
    }else{
        best <- order(gcvs)[1]
            #get the best alpha and degree
        bestd <- c(rep(1,n),rep(2,n))[best]
        bestalpha <- c(a,a)[best]
        gcv <- gcvs[best]
    }
    
    return(list(p=bestd,a=bestalpha,gcv=gcv))
}


plot.sy.sims <- function(sims,hist,q,r,fn=NULL){
    
    if(!is.null(fn)) pdf(fn,width=10,height=4)
    hist.plot <- list()
    layout(cbind(1:r))
    for( s in 1:r ){

        # the forst year simualted is the start year + q - 1 + s 
        # (the number of years forecasted ahead), for example 
        # if q = 3, we need three years back to initilize, so if the first year
        # year in the record is 1906 then the forst year we can forecast is 
        # 1906 + 3 - 1 + 1 = 1909
        colnames(sim[[s]])  <- (start(hist)[1]+q -1 ):(end(hist)[1]) + s
        hist.plot[[s]] <- window(hist,start(hist)[1] + q + s - 1)
        boxplot(sim[[s]],outline=F) #[,2:ncol(sim[[s]])]
        lines(1:length(hist.plot[[s]]),hist.plot[[s]],col='red',lwd=2,type='b',cex=.5)

    }
    if(!is.null(fn)) dev.off()
    invisible(hist.plot)
}

rank.histogram <- function(sim,hist){
    ranks <- integer(length(hist))
    for(i in 1:length(hist)){
       ranks[i] <- 
        rank(c(hist[i],sim[,i]),ties.method='first')[1]
    }
    hist(ranks)
    invisible(ranks)
}

# Read monthly data file with year in first column and months along rows 
# create a yearly timeseries aggregated april <---> march, the "runoff year"
make.runoff.year <- function(file='data/LeesFerry.txt',start=c(1906,1)){
    
    x <- ts(as.vector(t(as.matrix(read.table(file)[,-1]))),start=start,frequency=12)
    y <- numeric(0)
    chunk <- 4:15
    for(i in 1:(length(unique(floor(time(x))))-1)){
        
        y[i] <- sum(x[chunk])
        chunk <- chunk + 12
    }
    y <- ts(y,start=start,frequency=1)
    y
    
}

    # modified from code Zucchini and McDonald (2009) to work with the 
    # the HiddenMarkov package to do state prediciton
hmm.state.prediction <- function(x, H = 1, ...) {
    require(HiddenMarkov)
    n <- length(x$x)
    m <- ncol(x$Pi)
    fb <- forwardback(x$x, x$Pi, x$delta, x$distn, x$pm)
    la <- t(fb$logalpha)
    c <- max(la[, n])
    llk <- c + log(sum(exp(la[, n] - c)))
    statepreds <- matrix(NA, ncol = H, nrow = m)
    foo1 <- exp(la[, n] - llk)
    foo2 <- diag(m)
    for (i in 1:H) {
        foo2 <- foo2 %*% x$Pi
        statepreds[, i] <- foo1 %*% foo2
    }
    statepreds
}


    # generate forcest distributions for H steps out for a given hmm
    # model x, this function assumes a normal distributions for the 
    # components
hmm.forecast.dist <- function(x, H = 1, len=100, ...) {

    #lower <- qnorm(0.001, min(x$pm$mean),x$pm$sd[which.min(x$pm$mean)])
    #upper <- qnorm(0.999, max(x$pm$mean),x$pm$sd[which.max(x$pm$mean)])
    range <- extendrange(x$x,f=.1)
    xrange <- seq(range[1],range[2],,len)
        
    n <- length(x$x)
    m <- ncol(x$Pi)
    
    allprobs <- matrix(NA,nrow=length(x$x),ncol=m)
    for(i in 1:m)
        allprobs[,i] <- dnorm(x$x, x$pm$mean[i], x$pm$sd[i])
        
    allprobs <- outer(x$x, x$pm$mean, dnorm, x$pm$sd)
    allprobs <- ifelse(!is.na(allprobs), allprobs, 1)
    foo <- x$delta * allprobs[1, ]
    sumfoo <- sum(foo)
    lscale <- log(sumfoo)
    foo <- foo/sumfoo
    
    for (i in 2:n) {
        foo <- foo %*% x$Pi * allprobs[i, ]
        sumfoo <- sum(foo)
        lscale <- lscale + log(sumfoo)
        foo <- foo/sumfoo
    }
    xi <- matrix(NA, nrow = m, ncol = H)
    for (i in 1:H) {
        foo <- foo %*% x$Pi
        xi[, i] <- foo
    }
    
    allprobs.out <- matrix(NA,nrow=len,ncol=m)
    for(i in 1:m)
        allprobs.out[,i] <- dnorm(xrange, x$pm$mean[i], x$pm$sd[i])
    
    fdists <- allprobs.out %*% xi[, 1:H]
    list(x = xrange, y = fdists)
}

    #boxplot the forecast distributions bases on their summary stats
hmm.boxplot.fc <- function(hmm.fc,sy,...){
    
    bx <- boxplot(matrix(0,nrow=1,ncol=length(hmm.fc)), plot = FALSE)
    
    for(i in 1:length(hmm.fc)){
        
        bx$stats[,i] <- hmm.fcdist.fivenum(hmm.fc[[i]])
        
    }
    
    bx$names <- paste(sy:(sy+length(hmm.fc)-1))
    bxp(bx,...)
    
}


    # takes a list of the forecast distribution for each year and 
    # generated ensembles from them by sampling
hmm.ensemble.fc <- function(hmm.fc,sy,nsims=1000,...){
    
    years <- sy:(sy+length(hmm.fc)-1)
    ens <- matrix(NA,nrow=nsims,ncol=length(hmm.fc))
    
    pb <- txtProgressBar(1,length(hmm.fc),style=3)
    
    for(i in 1:length(hmm.fc)){
        
        setTxtProgressBar(pb,i)
        ens[,i] <- hmm.fcdist.sim(hmm.fc[[i]],nsims)
        
    }
    close(pb)
    
    names(ens) <- paste(years)
    ens
}

    # returns 5th, 25th, 50th, 75th and 95th percentile 
    # of a given forecast distribution
hmm.fcdist.fivenum <- function(fcdist){
    
    pts <- c(.05,.25,.5,.75,.95)
    fivenum <- numeric(5)
    
    for(i in 1:5){
        
        cdf <- cumsum(fcdist$y)
        cdf <- cdf/max(cdf)
        
        fivenum[i] <- approx(cdf,fcdist$x,pts[i])$y
        
    }
    
    fivenum
    
}

    # simulate nsim values from a forecast distribution
hmm.fcdist.sim <- function(fcdist,nsim=1000){
    
    pts <- runif(nsim)
    
    cdf <- cumsum(fcdist$y)
    cdf <- cdf/max(cdf)
    
    sim <- numeric(nsim)
    
    for(i in 1:nsim){
        
        sim[i] <- approx(cdf,fcdist$x,pts[i])$y
        
    }
    
    sim
    
}

    # plot transition probabilities over time for the 2 state models only
    # hmm is a list of fitted models
    # sy is the start year for plotting
plot.hmm.trprob.2state <- function(hmm,sy){
    
    #ww <- wd <- dw <- dd <- numeric(length(hmm))
    require(ggplot2)
    
    theme_set(theme_bw())
    myopts <- opts(
    		panel.grid.major = theme_blank(), 
    		panel.grid.minor = theme_blank(),
    	  	axis.text.x = theme_text(size =  11), 
    		axis.text.y = theme_text(size =  11,angle=90),
    	 	axis.title.x = theme_text(size = 11), 
    		axis.title.y = theme_text(size = 11,angle=90),
    		panel.background = theme_rect())
    		
    tr <- matrix(NA,ncol=4,nrow=length(hmm))
    
    for(i in 1:length(hmm)){
        tr[i,1] <- hmm[[i]]$Pi[1,1]
        tr[i,2] <- hmm[[i]]$Pi[1,2]
        tr[i,3] <- hmm[[i]]$Pi[2,1]
        tr[i,4] <- hmm[[i]]$Pi[2,2]
    }
    
    rownames(tr) <- sy:(sy+length(hmm)-1)
    colnames(tr) <- c("ww", "wd", "dw", "dd" )
    tr <- melt(tr)
    names(tr) <- c('Time','Transition','Probability')
    
    qplot(Time,Probability,data=tr,geom="line",col=Transition,linetype=Transition) + 
        myopts +
        opts(legend.position = c(0.2, 0.5)) + 
        ylab('Hidden State Transition Probability') +
        xlab('Time')
    
}
