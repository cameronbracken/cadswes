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

	fnn.acf <- false.nearest(x, m = d.max, d = T.acf, t=180, eps=sd(x)/10)
	fnn.ami <- false.nearest(x, m = d.max, d = T.ami, t=180, eps=sd(x)/10)

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
