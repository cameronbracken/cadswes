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
function(x, n, limit.type = 'prob', tie = 1, altobs = NULL ){
    # returns an integer vector corresponding to n states broken by equal 
    # probability or equal distance
    #
    limit <- 
    if(limit.type == 'prob') 
      quantile(x,seq(0,1,1/n))
    else if(limit.type == 'equal')
      seq(min(x),max(x),by=diff(range(x))/n)

    if(!is.null(altobs)) limit <- quantile(altobs,seq(0,1,1/n))
    
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

#' Returns a first order transition probability matrix 
#'  for the given data and number of states
#' 
#' @param x a vector 
#' @param ns the number of states for the tpm. If NULL, it is assumed that the
#'           series is already converted to states.
tpm <- function(x,ns=NULL){
    
    require(msm)
    
    if(is.null(ns)){
      ns <- max(x)
      states <- x
      if(length(unique(states)) > 20) stop('Too many states, specify a smaller number.')
    }else{
      states <- ntile.ts(x,ns)
    }
    
    st <- statetable.msm(state,data=list(state=states))
    st/apply(st,1,sum)
    
}

trprob <- function(lag,lead,ns = max(lead)+1){
    
    require(gtools)
    require(Hmisc)
    
    q <- ncol(lag)
    r <- ncol(lead)
    
    states.lag <- gtools::permutations(ns,q,0:(ns-1),repeats.allowed=T)
    states.lead <- gtools::permutations(ns,r,0:(ns-1),repeats.allowed=T)
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

lagc <- 
function(x,lag=1){

    if(lag>length(x)) 
        warning("Lag is larger than input vector length, returning NA's") 

    if(lag<0)  lagn = c(rep(NA,abs(lag)),x[-(length(x):(length(x)+lag+1))])
    if(lag==0) lagn = x    
    if(lag>0)  lagn = c(x[-(1:lag)],rep(NA,lag))

    remove = !is.na(lagn) & !is.na(x)
    return(cor(x[remove],lagn[remove]))
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

#WAVELET  1D Wavelet transform with optional singificance testing
#
#   [WAVE,PERIOD,SCALE,COI] = wavelet(Y,DT,PAD,DJ,S0,J1,MOTHER,PARAM)
#
#   Computes the wavelet transform of the vector Y (length N),
#   with sampling rate DT.
#
#   By default, the Morlet wavelet (k0=6) is used.
#   The wavelet basis is normalized to have total energy=1 at all scales.
#
#
# INPUTS:
#
#    Y = the time series of length N.
#    DT = amount of time between each Y value, i.e. the sampling time.
#
# OUTPUTS:
#
#    WAVE is the WAVELET transform of Y. This is a complex array
#    of dimensions (N,J1+1). FLOAT(WAVE) gives the WAVELET amplitude,
#    ATAN(IMAGINARY(WAVE),FLOAT(WAVE) gives the WAVELET phase.
#    The WAVELET power spectrum is ABS(WAVE)^2.
#    Its units are sigma^2 (the time series variance).
#
#
# OPTIONAL INPUTS:
# 
# *** Note *** setting any of the following to -1 will cause the default
#               value to be used.
#
#    PAD = if set to 1 (default is 0), pad time series with enough zeroes to get
#         N up to the next higher power of 2. This prevents wraparound
#         from the end of the time series to the beginning, and also
#         speeds up the FFT's used to do the wavelet transform.
#         This will not eliminate all edge effects (see COI below).
#
#    DJ = the spacing between discrete scales. Default is 0.25.
#         A smaller # will give better scale resolution, but be slower to plot.
#
#    S0 = the smallest scale of the wavelet.  Default is 2*DT.
#
#    J1 = the # of scales minus one. Scales range from S0 up to S0*2^(J1*DJ),
#        to give a total of (J1+1) scales. Default is J1 = (LOG2(N DT/S0))/DJ.
#
#    MOTHER = the mother wavelet function.
#             The choices are 'MORLET', 'PAUL', or 'DOG'
#
#    PARAM = the mother wavelet parameter.
#            For 'MORLET' this is k0 (wavenumber), default is 6.
#            For 'PAUL' this is m (order), default is 4.
#            For 'DOG' this is m (m-th derivative), default is 2.
#
#
# OPTIONAL OUTPUTS:
#
#    PERIOD = the vector of "Fourier" periods (in time units) that corresponds
#           to the SCALEs.
#
#    SCALE = the vector of scale indices, given by S0*2^(j*DJ), j=0...J1
#            where J1+1 is the total # of scales.
#
#    COI = if specified, then return the Cone-of-Influence, which is a vector
#        of N points that contains the maximum period of useful information
#        at that particular time.
#        Periods greater than this are subject to edge effects.
#        This can be used to plot COI lines on a contour plot by doing:
#
#              contour(time,log(period),log(power))
#              plot(time,log(coi),'k')
#
#----------------------------------------------------------------------------
#   Copyright (C) 1995-2004, Christopher Torrence and Gilbert P. Compo
#
#   This software may be used, copied, or redistributed as long as it is not
#   sold and this copyright notice is reproduced on each copy made. This
#   routine is provided as is without any express or implied warranties
#   whatsoever.
#
# Notice: Please acknowledge the use of the above software in any publications:
#    ``Wavelet software was provided by C. Torrence and G. Compo,
#      and is available at URL: http://paos.colorado.edu/research/wavelets/''.
#
# Reference: Torrence, C. and G. P. Compo, 1998: A Practical Guide to
#            Wavelet Analysis. <I>Bull. Amer. Meteor. Soc.</I>, 79, 61-78.
#
# Please send a copy of such publications to either C. Torrence or G. Compo:
#  Dr. Christopher Torrence               Dr. Gilbert P. Compo
#  Research Systems, Inc.                 Climate Diagnostics Center
#  4990 Pearl East Circle                 325 Broadway R/CDC1
#  Boulder, CO 80301, USA                 Boulder, CO 80305-3328, USA
#  E-mail: chris[AT]rsinc[DOT]com         E-mail: compo[AT]colorado[DOT]edu
#----------------------------------------------------------------------------
wavelet = function(Y, DT = 1, pad = 0, dj = 0.25, param = 6) {
    #Y is time series to be analyzed
    #DT is timestep for annual data, 1
    #pad data ? 0=F, 1=T
    #dj= spacing between discrete scales (.25)
    #param = wavenumber (6)
    s0 = 2 * DT
    #s0=DT
    n1 = length(Y)
    J1 = floor((log2(n1 * DT/s0))/dj)
    #....construct time series to analyze, pad if necessary
    x = Y - mean(Y)
    if (pad == 1) {
        base2 = trunc(log(n1)/log(2) + 0.4999)  # power of 2 nearest to N
        x = c(x, rep(0, 2^(base2 + 1) - n1))
    }
    n = length(x)
    #....construct wavenumber array used in transform [Eqn(5)]
    k = (1:trunc(n/2))
    k = k * ((2 * pi)/(n * DT))
    k = c(0, k, -rev(k[1:floor((n - 1)/2)]))
    #....compute FFT of the (padded) time series
    f = fft(x)  # [Eqn(3)]
    #....construct SCALE array & empty PERIOD & WAVE arrays
    scale = s0 * 2^((0:J1) * dj)
    period = scale
    wave = matrix(data = 0, ncol = n, nrow = J1 + 1)  # define the wavelet array
    wave = as.complex(wave)  # make it complex
    wave = matrix(data = wave, ncol = n, nrow = J1 + 1)
    # loop through all scales and compute transform
    for (a1 in 1:(J1 + 1)) {
        scl = scale[a1]
        #source('wave_bases_r.txt')
        nn = length(k)
        k0 = param
        expnt = -(scl * k - k0)^2/(2 * (k > 0))
        norm = sqrt(scl * k[2]) * (pi^(-0.25)) * sqrt(nn)  # total energy=N   [Eqn(7)]
        daughter = norm * exp(expnt)
        daughter = daughter * (k > 0)  # Heaviside step function
        fourier_factor = (4 * pi)/(k0 + sqrt(2 + k0^2))  # Scale-->Fourier [Sec.3h]
        coi = fourier_factor/sqrt(2)  # Cone-of-influence [Sec.3g]
        dofmin = 2  # Degrees of freedom
        out <- list(daughter = daughter, fourier_factor = fourier_factor, coi = coi, 
            dofmin = dofmin)
        daughter = out$daughter
        fourier_factor = out$fourier_factor
        coi = out$coi
        dofmin = out$dofmin
        wave[a1, ] = fft((f * daughter), inverse = TRUE)/(length(f * daughter))  # wavelet transform[Eqn(4)]
    }
    period = fourier_factor * scale
    coi = coi * c(1:(floor(n1 + 1)/2), rev(1:floor(n1/2))) * DT
    wave = wave[, 1:n1]  # get rid of padding before returning
    power = abs(wave)^2
    list(wave = wave, period = period, scale = scale, power = power, coi = coi)
    # end of code
} 

rl.count <- function(x,ns){
    
    rl <- rle(ntile.ts(x,ns))$lengths
    df <- as.data.frame(table(rl))
    count <- df$Freq
    lens <- as.numeric(levels(df$rl))
    
    return(list(rl=rl,len=lens,count=count))
}

rl.count.lower <- function(x,ns){
    
    ql <- quantile(x,1/ns)
    x[x > ql] <- NA
    x[!is.na(x)] <- 1
    rl <- rle(as.vector(x))
    df <- as.data.frame(table(rl$lengths[!is.na(rl$values)]))
    count <- df$Freq
    lens <- as.numeric(levels(df[,1]))
    
    return(list(rl=rl$lengths,len=lens,count=count))
}

rl.count.upper <- function(x,ns){
    
    qu <- quantile(x,1-1/ns)
    x[x < qu] <- NA
    x[!is.na(x)] <- 1
    rl <- rle(as.vector(x))
    df <- as.data.frame(table(rl$lengths[!is.na(rl$values)]))
    count <- df$Freq
    lens <- as.numeric(levels(df[,1]))
    
    return(list(rl=rl$lengths,len=lens,count=count))
}


boxplot.stats <- function(sims,x){
    
    
    layout(rbind(1:5))
    boxplot(apply(sims,1,mean))
    points(mean(x),col='Blue',pch=2)
    mtext("Mean",1,1)
    boxplot(apply(sims,1,sd))
    points(sd(x),col='Blue',pch=2)
    mtext('SD',1,1)
    boxplot(apply(sims,1,min))
    points(min(x),col='Blue',pch=2)
    mtext('Min',1,1)
    boxplot(apply(sims,1,max))
    points(max(x),col='Blue',pch=2)
    mtext('Max',1,1)
    boxplot(apply(sims,1,mylag,1,docor=T))
    points(mylag(x,1,docor=T),col='Blue',pch=2)
    mtext('Lag 1 AC',1,1)
    abline(h=)
    
}

boxplot.stats.basic <- function(stats,x){
    
    par(mar=c(5,3.2,3,.5)+.1)
    layout(rbind(1:6))
    boxplot(stats$mean)
    points(mean(x),col='Blue',pch=2)
    mtext("Mean",1,1)
    mtext("Flow Volume (KAF)",2,2,cex=.7)
    
    boxplot(stats$sd)
    points(sd(x),col='Blue',pch=2)
    mtext('SD',1,1)
    mtext("Flow Volume (KAF)",2,2,cex=.7)
    
    boxplot(stats$skew)
    points(skewness(x),col='Blue',pch=2)
    mtext('Skewness',1,1)
    
    boxplot(stats$min)
    points(min(x),col='Blue',pch=2)
    mtext('Min',1,1)
    mtext("Flow Volume (KAF)",2,2,cex=.7)
    
    boxplot(stats$max)
    points(max(x),col='Blue',pch=2)
    mtext('Max',1,1)
    mtext("Flow Volume (KAF)",2,2,cex=.7)
    
    boxplot(stats$lag1)
    points(mylag(x,1,docor=T),col='Blue',pch=2)
    mtext('Lag 1 AC',1,1)
    
}

boxplot.stats.drought <- function(stats,x){
    
    obs.stats <- stats.drought(x,median(x))
    
    par(mar=c(5,3.2,3,.5)+.1)
    layout(rbind(1:4))
    
    boxplot(stats$dri)
    points(obs.stats$avgdri,col='Blue',pch=2)
    mtext("Ave Drought\n Run Intensity",1,2)
    mtext("Flow Volume (KAF)",2,2,cex=.7)
    
    boxplot(stats$max.runlen)
    points(obs.stats$maxrunlen,col='Blue',pch=2)
    mtext("Max Run Length",1,1)
    
    boxplot(stats$max.def)
    points(obs.stats$maxdeficit,col='Blue',pch=2)
    mtext("Max Deficit",1,1)
    mtext("Flow Volume (KAF)",2,2,cex=.7)
    
    boxplot(stats$ave.def)
    points(obs.stats$avgdeficit,col='Blue',pch=2)
    mtext("Average Deficit",1,1)
    mtext("Flow Volume (KAF)",2,2,cex=.7)
    
}


matexp <- function(x,m){
    mat <- x
    for(i in 1:m)
        mat <- mat %*% x
    mat
}

count.run.lengths <- function(sims, count.max=25, ns=3){
    
    nsims <- nrow(sims)
    sim.len <- ncol(sims)
    runs.mat <- matrix(NA,ncol=sim.len,nrow=nsims)
    for(i in 1:nsims){
        runs <- rl.count(sims[i,],ns)
        runs.mat[i,1:length(runs$rl)] <- runs$rl
    }
    runs.mat   
}

sim.stats.basic <- function(sims, x, count.max=25, ns=3, wavelet.res = 115){
    
    require(e1071)
    
    stats <- list()
    
    stats$mean <- apply(sims,1,mean)
    stats$sd <- apply(sims,1,sd)
    stats$skew <- apply(sims,1,skewness)
    stats$min <- apply(sims,1,min)
    stats$max <- apply(sims,1,max)
    stats$lag1 <- apply(sims,1,mylag,1,docor=T)
    stats$lag2 <- apply(sims,1,mylag,2,docor=T)
    
    stats
}

sim.stats <- function(sims, x, count.max=25, ns=3, wavelet.res = 115){
    
    require(sm)
    require(e1071)
    
    wavelet.res <- length(wavelet(sims[1,], pad=1, dj=.05)$scale)
    nsims <- nrow(sims)
    sim.len <- ncol(sims)
    stats <- list()
    stats$runs <- stats$runs.upper <- stats$runs.lower <- 
      matrix(NA,ncol=sim.len,nrow=nsims)
    stats$pdf.x <- seq(from=0,to=max(x)*1.25,len=50)
    stats$pdf <- matrix(NA,ncol=length(stats$pdf.x),nrow=nsims)
    stats$spec <- stats$freq <- matrix(NA,ncol=length(spectrum(sims[1,],plot=F)$spec),nrow=nsims)
    stats$dens <- stats$dens.lower <- stats$dens.upper <- 
      matrix(NA,ncol=512,nrow=nsims)
    stats$dens.x <- stats$dens.x.lower <- stats$dens.x.upper <- numeric(512)
    stats$counts <- matrix(0,ncol=count.max,nrow=nsims)
    stats$pow <- matrix(0,ncol=wavelet.res,nrow=nsims)

    pb <- txtProgressBar(1,nsims,style=3)
    for(i in 1:nsims){
        setTxtProgressBar(pb,i)
        runs <- rl.count(sims[i,],ns)
        runs.upper <- rl.count.upper(sims[i,],ns)
        runs.lower <- rl.count.lower(sims[i,],ns)
        #browser()
        stats$runs[i,1:length(runs$rl)] <- runs$rl
        stats$runs.upper[i,1:length(runs.upper$rl)] <- runs.upper$rl
        stats$runs.lower[i,1:length(runs.lower$rl)] <- runs.lower$rl
        d <- density(stats$runs[i,],bw=1,from=1,to=count.max,na.rm=T)
        stats$dens[i,] <- d$y
        d <- density(stats$runs.lower[i,],bw=1,from=1,to=count.max,na.rm=T)
        stats$dens.lower[i,] <- d$y
        d <- density(stats$runs.upper[i,],bw=1,from=1,to=count.max,na.rm=T)
        stats$dens.upper[i,] <- d$y
        d.sm <- sm.density(sims[i,], display='none', eval.points = stats$pdf.x)
        stats$pdf[i,] <- d.sm$estimate
        stats$counts[i,runs$len] <- runs$count
        stats$pow[i,] <- apply(wavelet(sims[i,], pad=1, dj=.05)$power,1,sum)
        #browser()
        spec <- spectrum(sims[i,],plot=F)
        stats$spec[i,] <- spec$spec
        stats$freq[i,] <- spec$freq
    }
    close(pb)
    
    stats$dens.x <- d$x
    
    stats$mean <- apply(sims,1,mean)
    stats$sd <- apply(sims,1,sd)
    stats$skew <- apply(sims,1,skewness)
    stats$min <- apply(sims,1,min)
    stats$max <- apply(sims,1,max)
    stats$lag1 <- apply(sims,1,mylag,1,docor=T)
    stats$lag2 <- apply(sims,1,mylag,2,docor=T)
    
    stats
}

knn.sim <- function(x,sim.len=length(x),nn=floor(sqrt(length(x)))){
    
    W <- cumsum(oneoverk(nn))
    sim <- numeric(sim.len)
    ll <- laglead(x,1,1)
        # random srarting year
    this.neighbor <- sample(1:length(x),1)
    sim[1] <- x[this.neighbor]
    for(i in 2:sim.len){
            # neighbors to the current value
            # dont use the last value because there is no following value
        this.x <- ll$lag[!(ll$lag == sim[i-1])]
        xsample <- c(sim[i-1],this.x)
        neighbors <- order(as.matrix(dist(xsample))[,1])[-1][1:nn]
        
        r1 <- runif(1)
        this.neighbor <- neighbors[rank(c(r1,W))[1]]
        
            #choose following value to neighbor selected
        val <- xsample[this.neighbor]
        if(is.na(val))browser()
        sim[i] <- ll$lead[ll$lag == val]
    }
    sim
}

statdist <- function(tpm){
    
    m <- nrow(tpm)
    ones <- rbind(rep(1,m))
    I <- diag(rep(1,m))
    U <- matrix(rep(1,m^2),m)
    as.vector(ones %*% solve(I - tpm + U))
    
}

simulate.mc.paleo <- function(x, tpm, pools.to, sim.len, index = TRUE, 
    weight = 'oneoverk', burn = 20, stationary = TRUE){
  
  delta <- 
    if(stationary)
      statdist(tpm)
    else
      delta <- c(1,rep(0,m-1))
  
  m <- nrow(tpm)
  
  cumsums <- tpm
  x.pools <- pools.to
  npool <- integer(m)
  for(i in 1:m){
    cumsums[i,] <- cumsum(tpm[i,])
    x.pools[[i]] <- x[pools.to[[i]]]
    npool[i] <- length(x.pools[[i]])
  }
  
  sim <- ind <- numeric(sim.len)
  
    # initial state
  from <- rank(c(runif(1),cumsum(delta)))[1]
  x.from <- sample(x[ntile.ts(x,m) == (from-1)],1)
  
  for(i in (burn+1):(sim.len+burn)){
    
    to <- rank(c(runif(1),cumsums[from,]))[1]
    
    np <- npool[to]
    x.to.ind <-
      if(weight == 'equal')
        knn(x.from,x.pools[[to]],k=np,W=cumsum(rep(1/np,np)), ret = 'ind')
      else
        knn(x.from,x.pools[[to]],k=np, ret = 'ind')
    
    x.to <- x.pools[[to]][x.to.ind]
    ind[i - burn] <- which(x == x.to)
    sim[i - burn] <- x.to
    
    from <- to
    x.from <- x.to
    
  }
  if(index) return(ind) else return(sim)
  
}

knn <- function(val, x, n = 1, k = NULL, W = NULL, ret='val'){
  
  if(is.null(k)) k <- floor(sqrt(length(x)))
  if(is.null(W)) W <- cumsum(oneoverk(k))
  
  d <- sqrt((x-val)^2)
  neighbors <- order(d)[1:k]
  
  vals <- numeric(n)
  for(i in 1:n){
    rand <- runif(1)
    this.neighbor <- rank(c(rand,W))[1]
    vals[i] <- if(ret == 'val') x[neighbors][this.neighbor] else which(x == x[neighbors][this.neighbor])
  }
  vals
  
}

sim.stats.drought <- function(sims){
    
    nsim <- nrow(sims)
    #tpm <- lapply(as.data.frame(t(sims)),tpm,3)
    
    dri <- max.runlen <- max.def <- ave.def <- numeric(nsim)
    for (j in 1:nsim){
      z <- stats.drought(sims[j,],median(sims[j,]))
     dri[j] <- z$avgdri
     max.runlen[j] <- z$maxrunlen
     max.def[j] <- z$maxdeficit
     ave.def[j] <- z$avgdeficit
    }
    
    #list(tpm=tpm, 
    list(dri = dri,  max.runlen = max.runlen, max.def = max.def, 
        ave.def = ave.def)
}


stats.drought <- function(datavec, threshold) {
    #threshold -- threshold for defining drought
    #calculate spell length
    #y <- datavec - mean(datavec)
    y <- datavec - threshold
    n <- length(datavec)
    z <- rep(0, n)  #0 implies a surplus
    ysaved <- rep(0, n)  #save y values to calculate maximum deficit
    dryr = 0  #counter for number of dry/below average years
    for (i in 1:n) {
        if (y[i] < 0) {
            z[i] = i
            dryr = dryr + 1
            ysaved[dryr] = abs(y[i])
        }
    }
    runlen = 0
    store = rep(NA, n)
    z1 <- z2 <- rep(NA, n)
    k = 0
    for (i in 1:n) {
        if (z[i] == i) {
            runlen = runlen + 1
            store[i] = runlen
        }
        else {
            runlen = 0
        }
        if ((runlen == 0) && (i != 1)) {
            #if ((runlen == 0 ) && (z[i-1] != 0)) {
            if (z[i - 1] != 0) {
                k = k + 1
                #print(cbind((i-1),store[i-1]))
                z1[k] <- (i - 1)
                z2[k] <- store[i - 1]
            }
        }
    }  #i
    #zz1 <- subset(z1,z1) #end index of drought
    #zz2 <- subset(z2,z2) #spell length
    zz1 = na.omit(z1)
    zz2 = na.omit(z2)
    nspell = k  #number of drought spells
    #print(cbind(zz1,zz2))
    #calculate average drought intensity
    dintensity <- rep(NA, nspell)
    for (i in 1:nspell) {
        sumdef = 0
        endidx = zz1[i]
        begidx = endidx - zz2[i] + 1
        for (j in begidx:endidx) {
            sumdef = sumdef + abs(y[j])
        }
        dintensity[i] <- sumdef/zz2[i]
    }
    avgdri <- mean(dintensity)  #average drought run intensity
    #avgdri <- median(dintensity) #median drought run intensity
    maxrunlen <- max(zz2)  #maximum drought run length
    maxdeficit <- max(ysaved)  #maximum deficit
    avgdeficit <- sum(ysaved)/dryr  #average deficit
    outlist = list(dintensity = dintensity, avgdri = avgdri, dryr = dryr, maxrunlen = maxrunlen, 
        maxdeficit = maxdeficit, avgdeficit = avgdeficit, sumy = sum(ysaved), leny = length(ysaved))
    #outlist=list('dryr'=dryr,'maxrunlen'=maxrunlen)
    return(outlist)
} 


