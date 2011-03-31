############################################################
############################################################
# get.predictors:
#   Reads a specifically formatted data files containing predictors
get.predictors <- function(climatefile, swefile, pdsifile ,predmonth, 
    leadtimes=c('nov','dec','jan','feb','mar','apr')){
    
    climate <- as.matrix(read.table(climatefile,header=T))
    swe <- as.matrix(read.table(swefile,header=T))
    pdsi <- as.matrix(read.table(pdsifile,header=T))
    years <- as.numeric(rownames(climate))
    nrows <-min( c(nrow(climate), nrow(swe), nrow(pdsi)) )
    data <- cbind(climate[1:nrows,],swe[1:nrows,],pdsi[1:nrows,])
    pnames <- colnames(data)
        # The data file is arranged in colums by month 
        # but the months are not necessarily in order
        # so find all the months before and including the 
        # current predmonth
    prevs <- 1:max(grep(predmonth,leadtimes))
    cols <- c()
    for(i in prevs)
        cols <- c(cols,grep(leadtimes[i],pnames))

    any.is.na <- function(x)any(is.na(x))

    data <- data[,cols]
#    browser()
    data <- data[!apply(data,1,any.is.na),]
    
    pnames <- colnames(data)
    swecols <- grep("swe",pnames)
    if(length(swecols) > 1){
        swepc <- prcomp(cbind(data[,swecols]))
        swep <- t(t(swepc$rotation[,1])%*%t(cbind(data[,swecols])))
        data <- data[,-swecols]
        data <- cbind(data,swep)
        colnames(data)[ncol(data)] <- 'swe_pc1'
    }

      # principal component on pdsi, not really necessary. 
    #pnames <- colnames(data)
    #pdsicols <- grep("pdi",pnames)
    #if(length(pdsicols) > 1){
    #    pdsipc <- prcomp(cbind(data[,pdsicols]))
    #    pdsip <- t(t(pdsipc$rotation[,1])%*%t(cbind(data[,pdsicols])))
    #    data <- data[,-pdsicols]
    #    data <- cbind(data,pdsip)
    #    colnames(data)[ncol(data)] <- 'pdi_pc1'
    #}
    
    return(ts(data,start=years[1],frequency=1))
    
}

############################################################
############################################################
# combinationfilter:
#
combinationfilter=function(AllPredictors,selpredsetcombi,verbose){
  #This function will discard combinations which are highly
  #correlated among themselves (ie. multicolinear)
    predpos=0   
    combinationkept=vector(length(selpredsetcombi),mode='numeric')  # Vector to store combinations
    #print("Correlation at 95% level:::",quote=F)
    k <- qnorm(0.05)^2/(nrow(AllPredictors)-2)
    sigcor <- sqrt(k/(k+1))
    #print(sigcor,quote=F)
    #print('',quote=F)
    
    for(selpredset1 in 1:nrow(selpredsetcombi) ) {

        selpredset=as.logical(selpredsetcombi[selpredset1,])
        preddata=data.frame(AllPredictors[,selpredset])    
        corpos=0
        corvalues=1:( ncol(preddata) * ncol(preddata) )
    
        if(ncol(preddata)>1){
            cortable=matrix(-9999,nrow=ncol(preddata),ncol=ncol(preddata))
            for( i in 1:(ncol(preddata)-1) ) {
                for(j in (i+1):ncol(preddata) ) {
                    corpos=corpos+1
                    corvalues[corpos]=cor(preddata[,i],preddata[,j])
                }
            }
            corvalues=corvalues[1:corpos]

            if((max(corvalues)>sigcor) || (min(corvalues)<(-1*sigcor))){
                decision=0
                if(verbose > 1) cat("Combination",selpredset1, "NOT Selected\n")
            }else{
                decision=1
                if(verbose > 1) cat("Combination",selpredset1, "Selected\n")
            }
        }else{
            decision=1
            if(verbose > 1) cat("Combination",selpredset1, "Selected\n")
        }
        
        if(decision==1){predpos=predpos+1;combinationkept[predpos]=selpredset1}                 
    }
    combinationkept=combinationkept[1:predpos]
    combinationkept
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


############################################################
############################################################
# leaps:
#	  Generally leaps function will not do extensive search but here I modified
# 	  such that it will do extensive search. I included "really.big=TRUE"
#	  Also this program limits the number of variables to be "4" 
#	  by changing nvmax=4
leaps = function (x, y, wt = rep(1, NROW(x)), int = TRUE, method = c("Cp", 
    "adjr2", "r2"), nbest = 10, names = NULL, df = NROW(x), 
    strictly.compatible = TRUE, nvmax = 5) 
{
    if (!is.logical(int)) 
        stop("int should be TRUE or FALSE")
    if (!is.null(names)) 
        colnames(x) <- names
    method <- method[1]
    if (pmatch(method, c("Cp", "adjr2", "r2"), nomatch = 0) == 
        0) 
        stop("Ambiguous or unrecognised method name")
    if (strictly.compatible) {
        if (NCOL(x) > 31) 
            stop("leaps does not allow more than 31 variables; use regsubsets()")
        if (is.null(names)) 
            colnames(x) <- c(as.character(1:9), LETTERS)[1:NCOL(x)]
    }
    a <- leaps:::leaps.setup(x, y, wt = wt, nbest = nbest, nvmax = nvmax, 
		intercept = int, warn.dep = FALSE)
    if (strictly.compatible & any(a$lindep)) 
        stop("leaps requires full-rank design matrix; use regsubsets()")
    b <- leaps:::leaps.exhaustive(a,really.big=TRUE)
    d <- summary(b)
    rval <- list(which = d$which)
    if (int) 
        rval$which <- rval$which[, -1, drop = FALSE]
    rval$label <- colnames(d$which)
    rval$size <- as.numeric(rownames(d$which)) + int
    if (pmatch(method, c("Cp"), nomatch = 0) == 1) {
        rval$Cp <- d$cp
    }
    if (pmatch(method, c("r2"), nomatch = 0) == 1) {
        rval$r2 <- d$rsq
    }
    if (pmatch(method, c("adjr2"), nomatch = 0) == 1) {
        rval$adjr2 <- d$adjr2
    }
    rval
}

############################################################
############################################################
# gcvplot:
#   Modified version of the locfit gcvplot function
gcvplot=function (..., alpha, df = 2) 
{
    m <- match.call()
    m[[1]] <- as.name("gcv")
    m$df <- NULL
    if (!is.matrix(alpha)) 
        alpha <- matrix(alpha, ncol = 1)
    k <- nrow(alpha)
    z <- matrix(nrow = k, ncol = 4)
    for (i in 1:k) {
        m$alpha <- alpha[i, ]
        z[i, ] <- eval(m, sys.frame(sys.parent()))
    }
    ret <- list(alpha = alpha, cri = "GCV", df = z[, df], values = z[, 
        4])
    #class(ret) <- "gcvplot"
    ret
}

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

# 

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

############################################################
############################################################
# intervening2total:
#   Converts intervening flow to total flow on a river network
#
#  Inputs:
#   data - Must be a n x m matrix where n is the mumber of data points and 
#          m is the number of sites
#
#   mat  - Must be a m x m binary matrix (1 or 0).  In each row all of the 
#          columns with a 1 will be summed together using apply. 
#
intervening2total <- function(data,mat){
    
    tot <- data

    for(i in 1:nrow(mat))
        tot[,i] <- apply(cbind(data[,as.logical(mat[i,])]),1,sum)
     
    tot   
    
}

############################################################
############################################################
# intervening2total:
#   Converts total flow to intervening flow on a river network
#
#  Inputs:
#   data - Must be a n x m matrix where n is the mumber of data points and 
#          m is the number of sites
#
#   mat  - Must be a m x m binary matrix (1 or 0).  In row i, all of the 
#          columns with a 1 in them contribute directly to the total flow at 
#          site i.  The interveneing flow at site i is computed as the flow at 
#          site i minus the sum of the all the sites with a 1 (i.e. all the 
#          sites that directly contribute).  
#          NOTE: A 1 in the diagonal is implicit. 
#          
#
total2intervening <- function(data,mat){
    
    intv <- data
    #browser()
    for(i in 1:nrow(mat))
        intv[,i] <- data[,i] - apply(cbind(data[,as.logical(mat[i,])]),1,sum)
     
    intv
    
}

ts.annual.mean <- function(x,include.incomplete=FALSE){

    options(warn=-1)
    nyears <- length(unique(floor(time(x))))
    syear <- start(x)[1]
    nst = vector('numeric',nyears)

    for(i in 1:nyears){
        
        this.year <- window(x,syear - 1 + i,c(syear - 1 + i, frequency(x)))
        
        nst[i] <- ifelse(length(this.year)==frequency(x),mean(this.year),NA)
    }
   
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
    #first.year.complete <- length(which(floor(time(x)) == syear)) == frequency(x)
    #last.year.complete <- length(which(floor(time(x)) == eyear)) == frequency(x)

    for(i in ind){
    
        this.season <- window(x,c(syear - 1 + i, s),c(syear - 1 + i, e))
        
        nst[i] <- ifelse(length(this.season)==(e-s+1),mean(this.season),NA)
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

CapFirst <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
}

print.xtable.booktabs <- function(mat) {
    print(mat, 
          sanitize.text.function = function(x){x},
          #floating=FALSE, 
          NA.string = "*",
          hline.after=NULL, 
          add.to.row=list(pos=list(-1,0, nrow(mat)), 
          command=c('\\toprule ',
                    '\\midrule ',
                    '\\bottomrule ')),
          table.placement = "!ht",
          caption.placement="top")
}

latexSummary <- function(d,file){
    
    tables <- d$xtables
    sink(file)
    cat('\\documentclass[11pt]{article}\n')
    cat('\\usepackage{mathpazo}\n')
    cat('\\usepackage{booktabs}\n')
    cat('\\begin{document}\n')
    for(i in 1:length(tables)){
        print.xtable.booktabs(tables[[i]])
    }
    cat('\\end{document}\n')
    sink()
    
}

as.integer.matrix <- function(m){
    
    x <- matrix(as.integer(m),ncol=ncol(m))
    colnames(x) <- colnames(m)
    rownames(x) <- rownames(m)
    x
    
}

modelSummary <- function(d,file){
    
    w <- getOption("width")
    options(width=1000)
    sink(file)
    print(as.integer.matrix(d$model$psets))
    print(d$model$info)
    sink()
    options(width=w)
}

# disag sims to intervening
disagSimsToIntervening <- function(d,t2i){
    
    dims <- dim(d$disag)
    d$disag.tot <- array(NA,dims)
    d$disag.tot <- d$disag
    cat('Converting Flows to Intervening...\n')
    flush.console()
    pb <- txtProgressBar(1,dims[2]*dims[4],style=3)
    b <- 0
    for(i in 1:dims[2]){
        for(j in 1:dims[4]){
            b <- b + 1
            d$disag[,i,,j] <- total2intervening(d$disag[,i,,j],t2i)
            setTxtProgressBar(pb, b)
        }
    }
    close(pb)
    d
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
