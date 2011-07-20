############################################################
############################################################
# myboxplot.matrix:
#   Modified version of the boxplot.matrix function to call myboxplot
#   which in turn calls myboxplot.stats to to 5th and 95th percentiles
myboxplot.matrix <- function (x, use.cols = TRUE, ...) 
{
    groups <- if (use.cols) 
        split(x, rep.int(1L:ncol(x), rep.int(nrow(x), ncol(x))))
    else split(x, seq(nrow(x)))
    if (length(nam <- dimnames(x)[[1 + use.cols]])) 
        names(groups) <- nam
    invisible(myboxplot(groups, ...))
}

############################################################
############################################################
# myboxplot:
#   Modified version of the boxplot function which calls myboxplot.stats for 
#   5th and 95th percentiles instead if default 1.5*IQR
myboxplot <- function (x, ..., range = 1.5, width = NULL, varwidth = FALSE, 
    notch = FALSE, outline = TRUE, names, plot = TRUE, border = par("fg"), 
    col = NULL, log = "", pars = list(boxwex = 0.8, staplewex = 0.5, 
        outwex = 0.5), horizontal = FALSE, add = FALSE, at = NULL) 
{
    args <- list(x, ...)
    namedargs <- if (!is.null(attributes(args)$names)) 
        attributes(args)$names != ""
    else rep(FALSE, length.out = length(args))
    groups <- if (is.list(x)) 
        x
    else args[!namedargs]
    if (0L == (n <- length(groups))) 
        stop("invalid first argument")
    if (length(class(groups))) 
        groups <- unclass(groups)
    if (!missing(names)) 
        attr(groups, "names") <- names
    else {
        if (is.null(attr(groups, "names"))) 
            attr(groups, "names") <- 1L:n
        names <- attr(groups, "names")
    }
    cls <- sapply(groups, function(x) class(x)[1L])
    cl <- if (all(cls == cls[1L])) 
        cls[1L]
    else NULL
    for (i in 1L:n) groups[i] <- list(myboxplot.stats(unclass(groups[[i]]), 
        range))
    stats <- matrix(0, nrow = 5L, ncol = n)
    conf <- matrix(0, nrow = 2L, ncol = n)
    ng <- out <- group <- numeric(0L)
    ct <- 1
    for (i in groups) {
        stats[, ct] <- i$stats
        conf[, ct] <- i$conf
        ng <- c(ng, i$n)
        if ((lo <- length(i$out))) {
            out <- c(out, i$out)
            group <- c(group, rep.int(ct, lo))
        }
        ct <- ct + 1
    }
    if (length(cl) && cl != "numeric") 
        oldClass(stats) <- cl
    z <- list(stats = stats, n = ng, conf = conf, out = out, 
        group = group, names = names)
    if (plot) {
        if (is.null(pars$boxfill) && is.null(args$boxfill)) 
            pars$boxfill <- col
        do.call("bxp", c(list(z, notch = notch, width = width, 
            varwidth = varwidth, log = log, border = border, 
            pars = pars, outline = outline, horizontal = horizontal, 
            add = add, at = at), args[namedargs]))
        invisible(z)
    }
    else z
}

#This function replaces boxplot.stats --- it uses the middle 50% and
#middle 90% instead of middle 50%(IQR) and 1.5*IQR.
myboxplot.stats <- function (x, coef = NULL, do.conf = TRUE, do.out =
TRUE)
{
  nna <- !is.na(x)
  n <- sum(nna)
  stats <- quantile(x, c(.05,.25,.5,.75,.95), na.rm = TRUE)
  iqr <- diff(stats[c(2, 4)])
  out <- x < stats[1] | x > stats[5]
  conf <- if (do.conf)
    stats[3] + c(-1.58, 1.58) * diff(stats[c(2, 4)])/sqrt(n)
  list(stats = stats, n = n, conf = conf, out = x[out & nna])
}

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

ts.seasonal.mean <- function(x, s=1, e=frequency(x), include.incomplete=FALSE){

    options(warn=-1)
    nyears <- length(unique(floor(time(x))))
    ind <- 1:nyears
    if(start(x)[2] > e) ind <- ind[-1]
    if(end(x)[2] < s) ind <- ind[-length(ind)]
    syear <- start(x)[1]
    eyear <- end(x)[1]
    years <- (syear:eyear)[ind]
    nst <- vector('numeric',length(ind))
    interyear <- ifelse(e < s, TRUE, FALSE)
    len <- ifelse(e < s, e+frequency(x)-s+1,(e-s+1))
    #first.year.complete <- length(which(floor(time(x)) == syear)) == frequency(x)
    #last.year.complete <- length(which(floor(time(x)) == eyear)) == frequency(x)

    for(i in ind){
    #browser()
        this.season <- window(x,c(syear - 1 + i, s),c(syear + ifelse(interyear,0,-1) + i, e))
        
        nst[i] <- ifelse(length(this.season)==len,mean(this.season),NA)
    }

    nst <- ts(nst, start = c(years[1],1), frequency = 1) 
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

wapply <- function(x, fun, win.length = length(x), ...){
	
	if(is.null(ncol(x))){
		return(wapply.single(x, fun, win.length, ...))
	}else{
		for(i in 1:ncol(x)){
			if(i == 1){
				z <- wapply.single(x[,1], fun, win.length, ...)
			}else{
				z <- cbind(z,wapply.single(x[,i], fun, win.length, ...))
			}
			
		}
		attributes(z)$dimnames[[2]] <- attributes(x)$dimnames[[2]]
		return(z)
	}
		
	
}

wapply.single <- 
function(x, fun, win.len = length(x), ...){
    
    r <- (length(x) %% win.len)
    if(r > 0) x <- x[1:(length(x)-r)]
    stack <- matrix(x,nrow = win.len)
    return(apply(stack,2,fun,...))
    
}

stack.ts <- function(x){

	matrix(pad.ts(x),ncol=frequency(x),byrow=T)

}

pad.ts <- function(x){
	
	ts(c(rep(NA,start(x)[2]-1),x,rep(NA,frequency(x)-end(x)[2])),
		start=c(start(x)[1],1),
		frequency=frequency(x))
	
}


fcontour <- function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
    length.out = ncol(z)), z, left=TRUE, xlim = range(x, finite = TRUE), 
    ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
    levels = pretty(zlim, nlevels), nlevels = 20, color.palette = terrain.colors, 
    col = color.palette(length(levels) - 1), plot.title, plot.axes, 
    xaxs = "i", yaxs = "i", las = 1, axes = TRUE, frame.plot = axes, ...) 
{
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                z <- x
                x <- seq.int(0, 1, length.out = nrow(z))
            }
        }
        else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
        stop("increasing 'x' and 'y' values expected")
	
	mar.orig <- (par.orig <- par(c("mar", "las")))$mar
	par(las = las)
	mar <- mar.orig
	on.exit(par(par.orig))
	if (left){
		mar[4]<- 1	
	}
	else{
		mar[2]<- 1
		mar[4]<- 4.1
	}
	par(mar=mar)
	plot.new()
   plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs)
	if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
		stop("no proper 'z' matrix specified")
	if (!is.double(z)) 
	storage.mode(z) <- "double"
	.Internal(filledcontour(as.double(x), as.double(y), z, as.double(levels), 
		col = col))
    if (missing(plot.axes)) {
        if (axes && left) {
            title(main = "", xlab = "", ylab = "")
            Axis(x, side = 1)
            Axis(y, side = 2)
        }
        else if(axes){
            title(main = "", xlab = "", ylab = "")
            Axis(x, side = 1)
            Axis(y, side = 4) 	
        	}
    }
    else plot.axes
    if (frame.plot) 
        box()
    if (missing(plot.title)) 
        title(...)
    else plot.title
    invisible()
}

cbar <- function ( nlevels, zlim, rotate=TRUE, levels = pretty(zlim, nlevels), 
    color.palette = terrain.colors, col = color.palette(length(levels) - 1), 
    key.title, key.axes, xaxs = "i", yaxs = "i", las = 1, axes = TRUE) 
{
	mar.orig <- (par.orig <- par(c("mar", "las")))$mar
	par(las = las)
	mar <- mar.orig
	on.exit(par(par.orig))
	plot.new()
	if(rotate){
		mar[4] <- 2
		mar[2] <- 2
		par(mar = mar)
    	plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
        yaxs = "i")
    	rect(0, levels[-length(levels)], 1, levels[-1], col = col)
    	if (missing(key.axes)) {
       	 if (axes) 
    	        axis(4)
    	}
    	else key.axes
    }
    else {
    	mar[1] <- mar[3]
		mar[3] <- 2
		par(mar = mar)
    	plot.window(xlim = range(levels), ylim = c(0,1), xaxs = "i", 
    	yaxs = "i")
    	rect( levels[-length(levels)], 0, levels[-1], 1, col = col)
    	if (missing(key.axes)) {
       	 if (axes) 
    	        axis(1)
    	}
    	else key.axes
    }
    box()
    if (!missing(key.title)) 
        key.title
}

sizer <- function(rotate=TRUE){
	
	mar <- par("mar")
	
	if(rotate){		
		w <- (3 + mar[2]) * par("csi") * 2.54
	}
	else{
		w <- (3+mar[3]) * par("csi") * 2.54
	}
	return(w)
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

RPSS <- function(histobs, ensemble, altobs=NULL, ncat=3){
    # This function calculates the Rank Probability Skill Score(RPSS)
    # for each prediction year and gives the RPSS for 'wet', 'dry' and all years
    # 'wet' is in upper 67th percentile, dry is in lower 33rd percentile

    nyr <- length(histobs) #Number of Years Record
    rpss <- 1:nyr          #Equivalent size to number of years

    for(j in 1:nyr) { 
        obsval <- histobs[j]   #Observed value of that particular year
 
        #remove the year we are on to get calibrated data
        calibdata <- histobs[-j]
    
        # Find 33rd and 67th percentile of calibrated data
        breaks <- seq(0,1,1/ncat)[-c(1,ncat+1)]
        quants <- if(!is.null(altobs)) quantile(altobs,breaks) else quantile(calibdata,breaks)
        lq <- quants[1]
        uq <- quants[2]
             
        probs <- pclim <- obscat <- numeric(ncat)
        
        # assigns which category each of the simulations 
        # falls in for whole ensemble   
        if(typeof(ensemble) == 'list'){
        
            for(i in 1:ncat){
                
                fun <- if(ncol(ensemble[[j]]$y) == 1 || is.null(ncol(ensemble[[j]]$y))){
                  approxfun(x=ensemble[[j]]$x,y=ensemble[[j]]$y)
                }else{
                  approxfun(x=ensemble[[j]]$x,y=ensemble[[j]]$y[,1])
                }
                
                maxv <- max(ensemble[[j]]$x)
                minv <- min(ensemble[[j]]$x)
                lower <- ifelse(i==1, max(minv,0), max(minv,quants[i-1]))
                upper <- ifelse(i==ncat, maxv, min(maxv,quants[i]))
                #print(j)
                #browser()
                probs[i] <- 
                if(lower > upper || upper < lower)
                    0
                else
                    integrate(fun,lower,upper,subdivisions=1000)$value
                #probs[i] <- integrate(fun,lower,upper,subdivisions=1000)$value
            }
            
        }else{

            probs <- table(ntile.ts(ensemble[,j],ncat,altobs=altobs))/length(ensemble[,j])
        
        }
        
        probs <- cumsum(probs)
        
        # raw probability of falling into each category   
        # 33% chance to fall in each one of 3 categories             
        pclim <- cumsum(rep(1,ncat)/ncat)
                        
        # where the obsevation actually falls        
        obscat <- rep(0,ncat)
        obscat[rank(c(obsval,quants))[1]] <- 1
        obscat <- cumsum(obscat)
        rpss[j]=1-(sum((obscat-probs)^2))/(sum((obscat-pclim)^2))
  }
  
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
hmm.forecast.dist <- function(x, H = 1, len=300, ...) {

    #lower <- qnorm(0.001, min(x$pm$mean),x$pm$sd[which.min(x$pm$mean)])
    #upper <- qnorm(0.999, max(x$pm$mean),x$pm$sd[which.max(x$pm$mean)])
    range <- extendrange(x$x,f=.4)
    if(range[1] < 0) range[1] <- 0
    xrange <- seq(range[1],range[2],,len)

    n <- length(x$x)
    m <- ncol(x$Pi)
    
        # the probability density for each observation and each state
    
    dfun.hmm <- function(x,xrange=x$x){
        
        m <- ncol(x$Pi)
        allprobs <- matrix(NA,nrow=length(xrange),ncol=m)
        np <- length(x$pm)
        fun <- paste('d',x$distn,sep='')
        for(i in 1:m){
            allprobs[,i] <- 
                if(np == 1){
                    eval(call(fun, xrange, x$pm[[1]][i]))
                }else if(np == 2){
                    #browser()
                    if(x$distn == 'gamma'){
                        eval(call(fun, xrange, x$pm[[2]][i], x$pm[[1]][i]))
                    }else{
                        eval(call(fun, xrange, x$pm[[1]][i], x$pm[[2]][i]))
                    }
                }else if(np == 3){
                    eval(call(fun, xrange, x$pm[[1]][i], x$pm[[2]][i], x$pm[[3]][i]))
                }
        }
        allprobs
        
    }
    
    allprobs <- dfun.hmm(x)

    #allprobs <- outer(x$x, x$pm$mean, dnorm, x$pm$sd)
    allprobs <- ifelse(!is.na(allprobs), allprobs, 1)
    
        # do first calculation, alpha_T/(alpha_T * 1')
        # which uses the initial distribution
    foo <- x$delta * allprobs[1, ]
    sumfoo <- sum(foo)
    lscale <- log(sumfoo)
    foo <- foo/sumfoo
    
        # phi term from Zucchini (2009) equation 5.4
    for (i in 2:n) {
        foo <- foo %*% x$Pi * allprobs[i, ]
        sumfoo <- sum(foo)
        lscale <- lscale + log(sumfoo)
        foo <- foo/sumfoo
    }
        # fc state probabilities, these are the epsilons in equation 5.5
    xi <- matrix(NA, nrow = m, ncol = H)
    for (i in 1:H) {
        foo <- foo %*% x$Pi
        xi[, i] <- foo
    }
    allprobs.out <- dfun.hmm(x,xrange)
    
        # total probability of being in 
    fdists <- allprobs.out %*% xi[, 1:H]
    list(x = xrange, y = fdists)
}

    #boxplot the forecast distributions bases on their summary stats
hmm.boxplot.fc <- function(hmm.fc,sy,H=1,...){
    
    bx <- boxplot(matrix(0,nrow=1,ncol=length(hmm.fc)), plot = FALSE)
    
    for(i in 1:length(hmm.fc)){
        
        fc <- 
        if(ncol(hmm.fc[[i]]$y) == 1 || is.null(ncol(hmm.fc[[i]]$y))){
          hmm.fc[[i]]
        }else{
          list(x=hmm.fc[[i]]$x,y=hmm.fc[[i]]$y[,H])
        }
        bx$stats[,i] <- hmm.fcdist.fivenum(fc)
        
    }
    
    bx$names <- paste(sy:(sy+length(hmm.fc)-1))
    bxp(bx,...)
    
}


    # takes a list of the forecast distribution for each year and 
    # generated ensembles from them by sampling
hmm.ensemble.fc <- function(hmm.fc,sy,nsims=1000,...){
    
    years <- sy:(sy+length(hmm.fc)-1)
    H <- ncol(hmm.fc[[1]]$y)
    ens <- 
    if(H ==1) 
      array(NA,c(nsims,length(hmm.fc)))
    else
      array(NA,c(nsims,length(hmm.fc),H))
    
    pb <- txtProgressBar(1,length(hmm.fc),style=3)
    
    for(i in 1:length(hmm.fc)){
        setTxtProgressBar(pb,i)
        if( H == 1 )
          ens[,i] <- hmm.fcdist.sim(hmm.fc[[i]],nsims)
        else
          for(j in 1:H)
            ens[,i,j] <- hmm.fcdist.sim(list(x=hmm.fc[[i]]$x,y=cbind(hmm.fc[[i]]$y[,j])),nsims)

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
    
    cdf <- cumsum(fcdist$y[,1])
    cdf <- cdf/max(cdf)
    
    sim <- numeric(nsim)
    
    for(i in 1:nsim){
        
        sim[i] <- approx(cdf,fcdist$x,pts[i])$y
        while(is.na(sim[i]))
            sim[i] <- approx(cdf,fcdist$x,runif(1))$y
        
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

get.named.parlist <- function(x,m,dist,ic,...){
    require(MASS)
    fit <- fitdistr(x,dist,...)
    np <- length(fit$estimate)
    pars <- vector('list',np)
    names(pars) <- names(fit$estimate)

    for(j in 1:m){
        this.fit <- fitdistr(x[ntile.ts(x,m) == (j-1)],dist,...)
        #for(k in 1:np)
        #    pars[[k]][j] <- this.fit$estimate[k]
        for(k in 1:np)
            pars[[k]][j] <- fit$estimate[k]
        if(dist == 'normal'){
            if(ic == 'same.both'){
                pars[[k]][j] <- mean(x)
                pars[[k]][j] <- sd(x)
            } else if( ic == 'same.sd'){
                pars[[k]][j] <- mean(x[ntile.ts(x,m) == (j-1)])
                pars[[k]][j] <- sd(x)
            }else{
                pars[[k]][j] <- mean(x[ntile.ts(x,m) == (j-1)])
                pars[[k]][j] <- sd(x[ntile.ts(x,m) == (j-1)])
            }
        }
    }
    pars
}

plot.components <- function(x,m,dist,pars=NULL,...){
    
    fd.name <- ifelse(dist == "norm", "normal", dist)
    pars <- if(is.null(pars)) get.named.parlist(x,m,fd.name,...)
    hist(x,freq=F)
    
    r <- extendrange(x,f=.1)
    r <- seq(r[1],r[2],,1000)
    
    for(i in 1:m){
        
        dens <- eval(call(paste('d',dist,sep=''),r,pars[[2]][i],pars[[1]][i]))
        lines(r,1/m*dens)
    }
    
}

fit.fourier <- function(u, curves = NULL, plot=F, ...){

    w <- length(u)
        # maximum number of harmonics
    h <- if(w%%2 == 1) (w-1)/2
         else w/2 

    t <- 1:w

    pgram <- A <- B <- ranked.harm <- integer(h)
    A <- B <- MSD <- numeric(h)

    ubar <- mean(u)

        # Estimate fourier coefficients
    for(j in 1:h){
        A[j] <- 2/w * sum(u * cos(2 * pi * j * t/w ))
        B[j] <- 2/w * sum(u * sin(2 * pi * j * t/w ))
    }

    MSDu <- 1/w * sum( (u - ubar)^2 )
    MSD <- 1/2 * ( A^2 + B^2 )

    ranked.harm <- order(MSD,decreasing=T)

    pgram <- cumsum(MSD[ranked.harm]) / MSDu
    
    f.curves <- list()
    if(!is.null(curves)){
        for(i in curves){
            f.curves[[i]] = numeric(w)    
            
            for(j in ranked.harm[1:i])
                f.curves[[i]] = f.curves[[i]] + A[j]*cos(2*pi*j*t/w) + B[j]*sin(2*pi*j*t/w)
                
            f.curves[[i]] = f.curves[[i]] + ubar
        }
    }
    if(plot) plot(0:length(pgram),c(0,pgram),
                    xlab="Ranked Harmonic",
                    ylab="Percent of Explained Variation",
                    main="Cumulative Periodogram",
                    type='l',
                    col="steelblue",
                    ...)
    
    return(list(u=u,w=w,h=h,A=A,B=B,ranked.harm=ranked.harm,MSD=MSD,MSDu=MSDu,pgram=pgram,curves=f.curves))
}

plot.harmonics <- function(ff,h=ff$h,...){
    
    w <- ff$w
    A <- ff$A
    B <- ff$B
    u <- ff$u
    
    t <- seq(1,w,length.out=10*ff$w)
    f.curve <- list(x = numeric(length(t)),y = numeric(length(t)))
    f.curve$x <- t
    
    for(j in ff$ranked.harm[1:h])
        f.curve$y = f.curve$y + A[j]*cos(2*pi*j*t/w) + B[j]*sin(2*pi*j*t/w)
    f.curve$y = f.curve$y + mean(u)
    
    plot(f.curve,type='l',...)
    invisible(f.curve)
    
}

fit.harmonic.ar <- function(ff,h=ff$h,res=1){
    require(forecast)
    
    w <- ff$w
    A <- ff$A
    B <- ff$B
    u <- ff$u 
    if(w%%2 == 1){
        if( h > (w-1)/2 ) h <-(w-1)/2
    }else{
        if( h > w/2 ) h <- w/2
    }
    t <- seq(1,w,length.out=res*ff$w)
    
    harmonic <- ar.fit <- ar.fc <- vector('list',h)
    
    for(k in 1:length(ff$ranked.harm[1:h])){
        j <- ff$ranked.harm[k]
        harmonic[[k]] = A[j]*cos(2*pi*j*t/w) + B[j]*sin(2*pi*j*t/w)
        #print(k)
        #if(k==23)browser()
        ar.fit[[k]] <- ar(harmonic[[k]])#,method='mle')
        ar.fc[[k]] <- predict(ar.fit[[k]],n.ahead=res)
    }
    ar.pred <- sapply(ar.fc,"[[",'pred')
    ar.se <- sapply(ar.fc,"[[",'se')
    ar.upper <- qnorm(.95,sd=ar.se,mean=ar.pred)
    #ar.sd <- qnorm(.683,sd=ar.se,mean=ar.pred)
    ar.lower <- qnorm(.05,sd=ar.se,mean=ar.pred)
    #browser()
    return(list(mean=sum(ar.pred) + mean(u),
        lower=sum(ar.lower) + mean(u),
        sd=sum(ar.se),
        upper=sum(ar.upper) + mean(u)))
    
    
}

fit.harmonic.mvhmm <- function(ff,h=ff$h,res=1){
    
    w <- ff$w
    A <- ff$A
    B <- ff$B
    u <- ff$u 
    if(w%%2 == 1){
        if( h > (w-1)/2 ) h <-(w-1)/2
    }else{
        if( h > w/2 ) h <- w/2
    }
    t <- seq(1,w,length.out=res*ff$w)
    
    harmonic <- ar.fit <- ar.fc <- vector('list',h)
    
    for(k in 1:length(ff$ranked.harm[1:h])){
        j <- ff$ranked.harm[k]
        harmonic[[k]] = A[j]*cos(2*pi*j*t/w) + B[j]*sin(2*pi*j*t/w)
        #print(k)
        #if(k==23)browser()
        
        ar.fit[[k]] <- ar(harmonic[[k]])#,method='mle')
        ar.fc[[k]] <- predict(ar.fit[[k]],n.ahead=res)
    }
    ar.pred <- sapply(ar.fc,"[[",'pred')
    ar.se <- sapply(ar.fc,"[[",'se')
    ar.upper <- qnorm(.95,sd=ar.se,mean=ar.pred)
    #ar.sd <- qnorm(.683,sd=ar.se,mean=ar.pred)
    ar.lower <- qnorm(.05,sd=ar.se,mean=ar.pred)
    #browser()
    return(list(mean=sum(ar.pred) + mean(u),
        lower=sum(ar.lower) + mean(u),
        sd=sum(ar.se),
        upper=sum(ar.upper) + mean(u)))
    
    
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
wavelet=function(Y,DT=1,pad=0,dj=0.25,param=6){

#Y is time series to be analyzed
#DT is timestep for annual data, 1
#pad data ? 0=F, 1=T
#dj= spacing between discrete scales (.25)
#param = wavenumber (6)

s0=2*DT
#s0=DT

n1 = length(Y)
J1=floor((log2(n1*DT/s0))/dj)


#....construct time series to analyze, pad if necessary
x = Y - mean(Y)


if (pad == 1){
	base2 = trunc(log(n1)/log(2) + 0.4999)   # power of 2 nearest to N
	x = c(x, rep(0, 2^(base2 + 1) - n1))
}
n = length(x)

#....construct wavenumber array used in transform [Eqn(5)]
k = (1:trunc(n/2))
k = k*((2*pi)/(n*DT))
k = c(0, k, -rev(k[1:floor((n-1)/2)]))

#....compute FFT of the (padded) time series
f = fft(x)    # [Eqn(3)]

#....construct SCALE array & empty PERIOD & WAVE arrays
scale = s0*2^((0:J1)*dj)
period = scale;
wave = matrix(data=0, ncol=n, nrow=J1+1)  # define the wavelet array
wave = as.complex(wave)  # make it complex
wave=matrix(data=wave, ncol=n, nrow=J1+1)

# loop through all scales and compute transform
for(a1 in 1:(J1+1)){
	scl=scale[a1]		
		
	#source("wave_bases_r.txt")	
	

nn = length(k);
  	
	k0 = param
	expnt = -(scl*k - k0)^2/(2*(k > 0))
	norm = sqrt(scl*k[2])*(pi^(-0.25))*sqrt(nn)    # total energy=N   [Eqn(7)]
	daughter = norm*exp(expnt)
	daughter = daughter*(k > 0)    # Heaviside step function
	fourier_factor = (4*pi)/(k0 + sqrt(2 + k0^2)) # Scale-->Fourier [Sec.3h]
	coi = fourier_factor/sqrt(2)                  # Cone-of-influence [Sec.3g]
	dofmin = 2                                   # Degrees of freedom

out <- list(daughter=daughter, fourier_factor=fourier_factor,coi=coi,dofmin=dofmin)






	daughter=out$daughter
	fourier_factor=out$fourier_factor
	coi=out$coi
	dofmin=out$dofmin	
	wave[a1,] = fft((f*daughter), inverse = TRUE)/(length(f*daughter))  # wavelet transform[Eqn(4)]
}

period = fourier_factor*scale

 coi = coi*c(1:(floor(n1 + 1)/2), rev(1:floor(n1/2))) * DT
  

wave = wave[,1:n1]  # get rid of padding before returning
power=abs(wave)^2
list(wave=wave, period=period, scale=scale, power=power, coi=coi)
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

boxplot.stats.list <- function(stats,x){
    
    par(mar=c(5,3,3,.5)+.1)
    layout(rbind(1:6))
    boxplot(stats$mean)
    points(mean(x),col='Blue',pch=2)
    mtext("Mean",1,1)
    
    boxplot(stats$sd)
    points(sd(x),col='Blue',pch=2)
    mtext('SD',1,1)
    
    boxplot(stats$skew)
    points(skewness(x),col='Blue',pch=2)
    mtext('Skew',1,1)
    
    boxplot(stats$min)
    points(min(x),col='Blue',pch=2)
    mtext('Min',1,1)
    
    boxplot(stats$max)
    points(max(x),col='Blue',pch=2)
    mtext('Max',1,1)
    
    boxplot(stats$lag1)
    points(mylag(x,1,docor=T),col='Blue',pch=2)
    mtext('Lag 1 AC',1,1)
    
}

boxplot.stats3 <- function(sims1,sims2,sims3,x,...){
    
    require(e1071)
    layout(rbind(1:3,4:6))
    boxplot(cbind(apply(sims1,1,mean),apply(sims2,1,mean),apply(sims3,1,mean)),...)
    abline(h=mean(x),col='Blue')
    mtext("Mean",3,1)
    
    boxplot(cbind(apply(sims1,1,sd),apply(sims2,1,sd),apply(sims3,1,sd)),...)
    abline(h=sd(x),col='Blue')
    mtext('SD',3,1)
    
    boxplot(cbind(apply(sims1,1,skewness),apply(sims2,1,skewness),apply(sims3,1,skewness)),...)
    abline(h=skewness(x),col='Blue')
    mtext('Skew',3,1)
    
    boxplot(cbind(apply(sims1,1,min),apply(sims2,1,min),apply(sims3,1,min)),...)
    abline(h=min(x),col='Blue')
    mtext('Min',3,1)
    
    boxplot(cbind(apply(sims1,1,max),apply(sims2,1,max),apply(sims3,1,max)),...)
    abline(h=max(x),col='Blue')
    mtext('Max',3,1)
    
    boxplot(cbind(apply(sims1,1,mylag,1,docor=T),apply(sims2,1,mylag,1,docor=T),apply(sims3,1,mylag,1,docor=T)),...)
    abline(h=mylag(x,1,docor=T),col='Blue')
    mtext('Lag 1 AC',3,1)
    
}


boxplot.stats.n.list <- function(stats,x,...){
    
    require(e1071)
    layout(rbind(1:3,4:6))
    
    boxplot(sapply(stats,'[[','mean'),...)
    abline(h=mean(x),col='Blue')
    mtext("Mean",3,1)
    
    boxplot(sapply(stats,'[[','sd'),...)
    abline(h=sd(x),col='Blue')
    mtext('SD',3,1)
    
    boxplot(sapply(stats,'[[','skew'),...)
    abline(h=skewness(x),col='Blue')
    mtext('Skew',3,1)
    
    boxplot(sapply(stats,'[[','min'),...)
    abline(h=min(x),col='Blue')
    mtext('Min',3,1)
    
    boxplot(sapply(stats,'[[','max'),...)
    abline(h=max(x),col='Blue')
    mtext('Max',3,1)
    
    boxplot(sapply(stats,'[[','lag1'),...)
    abline(h=mylag(x,1,docor=T),col='Blue')
    mtext('Lag 1 AC',3,1)
    
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

simulate.mc.paleo <- function(x, tpm, pools.to, sim.len, weight = 'oneoverk', burn = 20, stationary = TRUE){
  
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
  
  sim <- numeric(sim.len)
  
    # initial state
  from <- rank(c(runif(1),cumsum(delta)))[1]
  x.from <- sample(x[ntile.ts(x,m) == (from-1)],1)
  
  for(i in (burn+1):(sim.len+burn)){
    
    to <- rank(c(runif(1),cumsums[from,]))[1]
    
    np <- npool[to]
    x.to <-
      if(weight == 'equal')
        knn(x.from,x.pools[[to]],k=np,W=cumsum(rep(1/np,np)))
      else
        knn(x.from,x.pools[[to]],k=np)
    
    sim[i - burn] <- x.to
    
    from <- to
    x.from <- x.to
    
  }
  sim
  
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
    vals[i] <- if(ret == 'val') x[neighbors][this.neighbor] else neighbors[this.neighbor]
  }
  vals
  
}