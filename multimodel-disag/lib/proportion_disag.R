
pdisag <- function(obj, hist, agg = NULL, simname=NULL, nsim=1000){

	# hist - a matrix with years down the rows and 
	#        months along the columns, optionally sites as layers	
	
	# Create an aggregate series along the first index of the data,
	# for example if your data is years X months X sites then the aggregate
	# series will be yearly summed values across all months and sites. 
	if(is.null(agg))
	    agg <- rowSums(hist)
	
		#create the proportion matrix by dividing all the rows by the row sums
	p <- hist/agg 
		# check if it works:
		#all.equal(rep(1,nrow(hist)),rowSums(p))
	
	# simulate nsim annual sequences to disaggregate
	if(is.null(simname)){
	    sims <- t(sim.knn(agg, nsim=nsim ))
	}else{
	    sims <- obj[[simname]]
	    obj[[simname]] <- NULL
	    #model <- obj[-which(names(obj)==simname)]
	}
	model <- obj
	
	pdisag <- list(hist=hist,agg=agg,p=p,nsim=nsim,sims=sims, model=model)
	class(pdisag) <- 'pdisag'
	pdisag
	
}

disag <- function (object, ...) {
	UseMethod("disag")
}

diagnostics <- function (object, ...) {
	UseMethod("diagnostics")
}

rpss <- function (object, ...) {
	UseMethod("rpss")
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

				z <- obj$sims[j,i]
				neighbor <- nn(obj$agg,z,index=T, k = 12)
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

diagnostics.pdisag <- function(d, calibration, pred, main='', 
    time.names=NULL, site.names=NULL, 
    cn = NULL, dir='diagnostics', vtype, nback, density=T,ylab=''){
    
    if(is.null(d$disag)) stop("No disag data available, run disag() first.")
    if(!file.exists(dir)) dir.create(dir)
    if(!file.exists(file.path(dir,'plots'))) 
        dir.create(file.path(dir,'plots'))
    if(!file.exists(file.path(dir,'reports'))) 
        dir.create(file.path(dir,'reports'))
    
    cat('Diagnostics...\n')
    flush.console()
    
    # year, month, site, sim
    dims <- dim(d$disag)
    time.names <- month.name[if(dims[2]==4) 4:7 else 1:12]
    
    #browser()
    rpss.ann <- median(RPSS(d$agg,d$sims))
    mc.ann <- cor(apply(d$sims,2,median)[1:length(d$agg)],d$agg)
    pdf(file.path(dir,'plots',paste(main,"Annual","Lees Ferry",vtype,'box.pdf')),
        width=8,height=5)
        
        colnames(d$sims) <- cn
        
        myboxplot.matrix(d$sims,cex=.3,main=main,outline=F, 
            ylab=ylab,xlab='Time',ylim=range(c(d$sims,d$agg)))
        mtext(paste("Annual","Lees Ferry"))
        lines(d$agg,type='b',cex=.5)
        abline(h=quantile(d$model$response,1/3))
        abline(h=quantile(d$model$response,2/3))
        title(sub=paste("RPSS = ",round(rpss.ann,2), "MC = ",round(mc.ann,2)))
        
    dev.off()
    
    pb <- txtProgressBar(1,dims[2]*dims[3],style=3)
    b <- rpss <- mc <- 0
    for(time in 1:dims[2]){
        for(site in 1:dims[3]){
            b <- b +  1
            this.time <- ifelse(is.null(time.names),paste('Time',time),time.names[time])
            this.site <- ifelse(is.null(site.names),paste('Site',site),site.names[site])
    
            this.data <- t(d$disag[,time,site,])
            colnames(this.data) <- cn
            
            #if(this.time == "July" && this.site == "San Juan River Near Archuleta,NM")
            #    browser()
            rpss[b] <- median(RPSS(d$hist[,time,site],this.data))
            mc[b] <- cor(apply(this.data,2,median),d$hist[,time,site])

            
            pdf(file.path(dir,'plots',paste(main,this.time,this.site,vtype,'box.pdf')),
                width=8,height=5)
                
                myboxplot.matrix(this.data,cex=.3,main=main,outline=F, 
                    ylab=ylab,xlab='Time')
                mtext(paste(this.time,this.site))
                lines(d$hist[,time,site],type='b',cex=.5)
                abline(h=quantile(d$hist[,time,site],1/3))
                abline(h=quantile(d$hist[,time,site],2/3))
                #browser()
                mc[b] <- ifelse(abs(mc[b]) < sigcor(length(d$hist[,time,site])),
                    NA, round(mc[b],2))
                title(sub=paste("RPSS = ",round(rpss[b],2), "MC = ",mc[b]))
                
                
            dev.off()
            
            if(density){
            	#find the max density for y limits of plotting
            	m <- 0
            	for(i in 1:d$nsim){
            		x <- density(d$disag[,time,site,i])
            		m <- ifelse(max(x$y)>m,max(x$y),m)
            	}
            	pdf(file.path(dir,'plots',paste(main,this.time,this.site,vtype,'density.pdf')),
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
    
    nt <- dims[2]
    d$stats$rpss <- matrix(rpss,ncol=nt,byrow=T)
    d$stats$mc <- matrix(mc,ncol=nt,byrow=T)
    colnames(d$stats$rpss) <- colnames(d$stats$mc) <- time.names
    rownames(d$stats$rpss) <- rownames(d$stats$mc) <- site.names
    
    d$xtables$rpss <- xtable(d$stats$rpss,digits=rep(2,ncol(d$stats$rpss)+1),
        caption=paste(main,'RPSS after disaggregation',vtype))             
    d$xtables$mc <- xtable(d$stats$mc,digits=rep(2,ncol(d$stats$mc)+1),
        caption=paste(main,'MC after disaggregation',vtype,
            '(* indicates non-significant correlation)'))
    

    file <- file.path(dir,'reports',gsub(' ','',main))
    latexSummary(d, file = paste(file,'_',vtype,'.tex',sep=''))
    modelSummary(d, file = paste(file,'_',vtype,'.txt',sep=''))
    
    return(d)
    
}