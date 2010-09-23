proportion.disag <- function(x,hist,n.sim=250,l=nrow(x)){
	
	#takes a matrix of simulations
	
	# begin disag
	cat('Disaggregating...')
	
		# length of each simulation (yrs)
	l <- nrow(x)
	n.sites <- dim(x)[3]
	n.times <- ncol(x)

		#matrix for recording scale factors (if need be)
	sf_mat <- matrix(ncol=n.sim, nrow=l) 

		#matrix for disag values - row(yr), col (month), index1 (sim), index2 (site)
	disag <- array(data=NA,dim=c(l,n.times,n.sim,n.sites))
	
		#number of neighbors
	k <- sqrt(l) 
	
	for(j in 1:n.sim){

		# this picks the 1st year for disag based only on 
		# the annual flow b/c there is no prev. Dec
			
			#first annual value in each trace to be disagged
		this.flow <- seqs[1,j]
			# distance between observed years and simulated value
		D=abs(an_flw[,2]-this.flow) 
			#combines difference and corresponding year into one matrix
		Delta=cbind(an_flw[,1],D)
			#reorders the delta matrix based on distances
		Delta_sort=cbind(Delta[,1][order(Delta[,2])], sort(Delta[,2]))
			#selects the "k-nearest-neighbors" from Delta_sort 
		kmatrix = Delta_sort[1:k,1:2]
			# defines matrix for weights
		weight = matrix(nrow=k, ncol=1) 
			# ranks distances for purpose of generating weights
		rnk=rank(kmatrix[,2])
		for(i in 1:k)	weight[i,1] = 1/(rnk[i])
			# sums weights 
		z = sum(weight) 
			#divides weights by sum of weights so cumulative probability = 1
		weights = weight/z
			#Selects a year to be "nearest neighbor"
		N=sample(kmatrix[,1], 1, replace = TRUE, prob=weights) 
			# index for selected yr
		pos=N-(an_flw[1,1]-1) 
			# scaling factor to apply for disag
		SF=this.flow/(an_flw[pos,2])
			# records scale factor
		sf_mat[1,j]=SF
		index_mat[1,j]=pos
			# "disaggregation"
        disag[1, ,j,]=data[pos,,]*SF 

		# now that one year has been disaggregated, the remaining years 
		# in the trace use annual flow and also december of last yr (CY data)
		for(h in 2:l){

				#number of neighbors
			k=sqrt(len) 
				# annual flow value for disaggregation
			Flow=seqs[h,j]
				#matrix for distances, length is (len-1) 
				# b/c first observed year has no prev. december
			D=2:len
			###distance based only on annual flow
			D=abs(an_flw[,2]-Flow)
				# combines difference and corresponding year 
				# into one matrix mean flow 
			Delta=cbind(an_flw[,1],D) 
				#reorders the delta matrix based on distances
			Delta_sort=cbind(Delta[,1][order(Delta[,2])], sort(Delta[,2])) 
				#selects the "k-nearest-neighbors" from Delta_sort 
			kmatrix = Delta_sort[1:k,1:2]

				# defines matrix for weights
			weight = matrix(nrow=k, ncol=1) 
	
				#ranks distances for purpose of generating weights
			rnk=rank(kmatrix[,2]) 
				#fills weighting matrix
			for(i in 1:k){
					weight[i,1] = 1/(rnk[i])

				# sums weights 
			z = sum(weight) 
				#divides weights by sum of weights so cumulative probability = 1
			weights = weight/z
			
				#Selects a year to be "nearest neighbor"
			N=sample(kmatrix[,1], 1, replace = TRUE, prob=weights) 

			pos=N-(an_flw[1,1]-1) # index for selected yr

			SF=Flow/(an_flw[pos,2]) # scaling factor to apply for disag

			sf_mat[h,j]=SF
     

			disag[h, , j,]=data[pos, ,]*SF #disaggregation
		}
     			
		}
	}
		


	## output to "flat" file


	disagmat=matrix(ncol=4, nrow=(l*n.sim*365))


	for(j in 1:4){
	p=1
	P=(l*365)
	for(k in 1:n.sim){

	disagmat[p:P,j]= as.vector(t(disag[,,k,j]))
	p=p+(l*365)
	P=P+(l*365)
	}

	}

	cat('Done.')
}

pdisag <- function(hist, nsim=1000, sims = NULL){

	# hist - a matrix with years down the rows and 
	#        months along the columns
	
	
	agg <- apply(hist,1,sum)
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
		pb <- txtProgressBar(1,ny,style=3)
	}
	#browser()
		# the disag
	for(i in 1:ny){
		if(!quiet) setTxtProgressBar(pb, i)
		for(j in 1:ns){

				z <- obj$sims[i,j]
				neighbor <- nn(obj$agg,z,index=T)
				
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