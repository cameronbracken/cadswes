
pdisag <- function(hist, nsim=1000, sims = NULL){

	# hist - a matrix with years down the rows and 
	#        months along the columns, optionally sites as layers
	
	
	agg <- rowSums(hist)
		#create the proportion matrix by dividing all the rows by the row sums
	p <- hist/agg 
		# check if it works:
		#all.equal(rep(1,nrow(hist)),rowSums(p))
	
	# simulate nsim annual sequences to disaggregate
	if(is.null(sims))
	    sims <- sim.knn(agg, nsim=nsim )
	
	pdisag <- list(hist=hist,agg=agg,p=p,nsim=nsim,sims=sims)
	class(pdisag) <- 'pdisag'
	pdisag
	
}

disag <- function (object, ...) {
	UseMethod("disag")
}

diagnostics <- function (object, ...) {
	UseMethod("diagnostics")
}

print.pdisag <- function(x){
	
	cat('Historical Data: \n')
	cat('\t',dim(x$hist)[1],'Years\n')
	cat('\t',dim(x$hist)[2],'Periods\n')
	cat('\t',dim(x$hist)[3],'Sites\n')
	cat('Simulations: ',x$nsim,'\n')
}

disag.pdisag <- function(obj, quiet=FALSE,plot=T){
	
	# simdata, a 
	d <- dim(obj$hist)
	ny <- d[1]
	nt <- d[2]
	ns <- obj$nsim
	
	# we are doing space and time disag.

	# when we disag the simulation matrix (nyears by nsims), we want a 
	# disagregated matrix out for each simulated series that is the same 
	# dimensions as the historical data
	disag <- array(NA,c(d,ns))

	if(!quiet) {
		cat('Disaggregating...\n')
		flush.console()
		pb <- txtProgressBar(1,ny,style=3)
	}
	#browser()
		# the disag
	for(i in 1:ny){
		if(!quiet) setTxtProgressBar(pb, i)
		for(j in 1:ns){

				z <- obj$sims[i,j]
				neighbor <- nn(obj$agg,z,index=T)
				#browser()
				
				if(length(d) == 3)	
					disag[i,,,j] <- obj$p[neighbor,,]*z
					
				if(length(d) == 2)	
					disag[i,,j] <- obj$p[neighbor,]*z
		}
	}
			
	if(!quiet){
		close(pb)
	}
	
	obj$disag <- disag
	obj
}

plot.pdisag <- function(obj,mon,site){
    
    if(is.null(obj$disag)) stop("No disag data available, run disag() first.")

	#find the max density for y limits of plotting
	m <- 0
	for(i in 1:obj$nsim){
		x <- density(obj$disag[,mon,site,i])
		m <- ifelse(max(x$y)>m,max(x$y),m)
	}
	plot(z <- density(obj$hist[,mon,site]),ylim=c(0,m))
	for(i in 1:obj$nsim){
		x <- density(obj$disag[,mon,site,i])
		lines(x$x,x$y,col=rgb(.5,.5,.5,.2))
	}
	lines(z)

	
}

nn <- function(x, val, k = ceiling(sqrt(length(x))), index = FALSE){
	
	
		#Weight function
	W <- oneoverk(k)
		# distances
	dist <- abs(x - val)
		# nearest neighbors
	neighbors <- order(dist)[1L:k]
	
		#randomly select a year based on the weight function
	nn <- if(index)
		sample(neighbors, 1, replace = TRUE, prob=W) 
	else
		sample(x[neighbors], 1, replace = TRUE, prob=W) 
		
	nn
	
	
}

oneoverk <- function(k){
	
	W <- numeric(k)
	for(j in 1:k)
		W[j] <- 1/j
	W <- W/sum(W)
	W
	
}

diagnostics.pdisag <- function(d, main='', time.names=NULL, site.names=NULL, 
    cn = NULL, dir='diagnostics', density=T,ylab=''){
    
    if(is.null(d$disag)) stop("No disag data available, run disag() first.")
    if(!file.exists(dir)) dir.create(dir)
    
    # year, month, site, sim
    dims <- dim(d$disag)
    cat('Diagnostics...\n')
    pb <- txtProgressBar(1,dims[2]*dims[3],style=3)
    b <- 0
    for(time in 1:dims[2]){
        for(site in 1:dims[3]){
            b <- b +  1
            this.time <- ifelse(is.null(time.names),paste('Time',time),time.names[time])
            this.site <- ifelse(is.null(site.names),paste('Site',site),site.names[site])
            this.data <- t(d$disag[,time,site,])
            colnames(this.data) <- cn
            
            pdf(file.path(dir,paste(main,this.time,this.site,'box.pdf')),
                width=8,height=5)
                
                myboxplot.matrix(this.data,cex=.3,main=main,outline=F, 
                    ylab=ylab,xlab='Time')
                mtext(paste(this.time,this.site))
                lines(d$hist[,time,site])
                
            dev.off()
            
            if(density){
            	#find the max density for y limits of plotting
            	m <- 0
            	for(i in 1:d$nsim){
            		x <- density(d$disag[,time,site,i])
            		m <- ifelse(max(x$y)>m,max(x$y),m)
            	}
            	pdf(file.path(dir,paste(main,this.time,this.site,'density.pdf')),
            	    width=8,height=5)
        	    
            	    plot(z <- density(d$hist[,time,site]),ylim=c(0,m), main=main,
            	    xlab=ylab)
            	    
        	        mtext(paste(this.time,this.site))
        	        
                	for(i in 1:d$nsim){
                		x <- density(d$disag[,time,site,i])
                		lines(x$x,x$y,col=rgb(.5,.5,.5,.2))
                	}
                	lines(z)
        	
            	dev.off()
    	    }
    	    setTxtProgressBar(pb, b)
            
        }
    }
    close(pb)
    
}