selectpredictors <- function(rawpredictors, response, verbose=1, debug=F, 
    min.alpha = .3, nvmax=3){

	# verbose 0 = nothing
	# verbose 1 = only status on long running computations
	# verbose 2 = output reggarding results
	# verbose 3 = Tons 
	options(warn=-1)
	
	    # Calculate the 15 highest correlated subsets of each size (# predictors)
	    # in practice, more than 3 predictors are never actually used becuse 
	    # they end up being multicolinear.
	    # leaps uses the Cp statistic to compare subsets and chooses a set of the best 
        # subsets of predictors and stores them in lps$which
        # nbest-max number of subsets of each size to choose
	lps <- leaps(rawpredictors,response,nbest=15,nvmax=nvmax,method="Cp",int=F,
	    strictly.compatible=F)
		
	allsubsets <- lps$which
	nsubsets <- length(allsubsets[,1])
	degs <- alphas <- gcvs <- numeric(nsubsets)
	fitobj <- list()
	
	    # create a sequence of alphas and repeat it for degree 1 and 2
	alphaseq <- seq(min.alpha,1,by=0.05)  
	npar <- length(rep(alphaseq,2))
	pars <- data.frame(alpha = rep(alphaseq,2),
	    deg = c(rep(1,npar/2),rep(2,npar/2)))
  
    b <- 0
    if(verbose > 0){ 
        cat('Selecting best predictors...\n')
        flush.console()
        pb <- txtProgressBar(1,nsubsets*npar,style=3)
    }
	for(i in 1:nsubsets){ 
          # this loop determines the smoothing parameter and the 
          # corresponding locfit degree which minimizes the GCV statistic  
        max.gcv <- Inf          
		predset <- rawpredictors[,allsubsets[i,]]     
          #pulls predictors out for particular subset
		
		for(p in 1:npar){
		    b <- b + 1
		    this.alpha <- pars$alpha[p]
		    this.deg <- pars$deg[p]
		    
		        # Must unclass the data because warnings occur 
		        # if ts objects are passed in
    		fit <- locfit.raw(x=unclass(predset), y=unclass(response), alpha=this.alpha, 
			    deg=this.deg, kern="bisq", maxk=200)

            #if(lps$size[i] > 1) browser()
            
			z <- fit$dp[c("lk", "df1", "df2")]
			N <- fit$mi["n"]
            
            this.gcv <- (-2 * N * z[1])/(N - z[2])^2
            if(this.gcv < max.gcv){
                best.fitobj <- fit
                max.gcv <- this.gcv
                best.pos <- p
            }
		}
		
		string <- 
		if(is.null(ncol(predset)))
		    paste('Set',i,'out of',nsubsets,'with',1,'predictors')
	    else
		    paste('Set',i,'out of',nsubsets,'with',ncol(predset),'predictors')
		
		if(verbose > 0){
		    setTxtProgressBar(pb, b)
	    }
        
        degs[i] <- pars[["deg"]][best.pos]
        alphas[i] <- pars[["alpha"]][best.pos]
        gcvs[i] <- max.gcv
        fitobj <- c(fitobj, list(best.fitobj))
        
	}
    if(verbose > 0) close(pb)
    if(verbose > 2){
    	for(i in 1:nsubsets){
    		cat('For subset',as.integer(i),'\n\t\tGCV=',round(gcvs[i],2),
    				'\n\t\talpha=',alphas[i],'\n')
    	}
    	cat('\n')
    }
  
	a=1
	for(n in 1:max(lps$size)){ 
			#loop through set w/ same number of predictors
			#this whole loop is really just book keeping for large
			#number of predictors and not really necessary for most cases
			#It will discard predictor sets if there are too many 
		if(verbose > 2)
		    cat('There are',length(lps$size[lps$size==n]),
		        'sets with',n,'predictor(s)\n')
		
		    #indev numbers of sets w/ n predictors
		b=a+length(lps$size[lps$size==n])-1
		
			#luckily leaps already orders subsets by # predictors
		gcvswithnpredictors <- gcvs[a:b] 
		setswithnpredictors <- allsubsets[a:b,]
		
		    #order just subsets with same n predictors
		gcvsordered <- order(gcvswithnpredictors)
		 #sort just subsets with same n predictors
		gcvssorted <- sort(gcvswithnpredictors)  
		
			#determine how many subsets to use from set with n predictors
			#probably not very useful if there are less than 5 predictors
			#the value we are determining (keepsets) is the number of sets 
			#to keep from the subset that has the same number of predictors
		nsets <- length(gcvssorted)
		    # the min number of sets to keep
		base <- 5
		if((range(gcvssorted)[2]/gcvssorted[1]) >1.5 ){
			keepsets=0
			while((gcvssorted[keepsets+1])<=(1.5*gcvssorted[1]))
			    keepsets <- keepsets + 1
			if(keepsets<base) keepsets <- base
			if(nsets<base) keepsets <- nsets 
		} else{
			keepsets <- base
			if(nsets<base) keepsets=nsets 
		}

		if(verbose > 2)
		    cat('\tKeeping',keepsets,'out of',
		        nsets,'sets;','indicies are',a,b,'\n\n')
		
		
		# These conditions are necessary because if there is only one member 
		# of a set then it is represented as a vector and not an array
		if( length(lps$size[lps$size==n])==1 ){
                # only one predictor so 
			keptpredictors <- setswithnpredictors
			keptpredictorset <- c(keptpredictors,gcvssorted)
			if(verbose > 1){
			    cat(n,'\n')
			    write(t(keptpredictorset),file=stdout(),
			        ncol=length(keptpredictorset))
			}
		}else{
			keptpredictors <- setswithnpredictors[gcvsordered[1:keepsets],]
			keptpredictorset <- cbind(keptpredictors,gcvssorted[1:keepsets])
			if(verbose > 1){
			    cat(n,'\n')
			    write(t(keptpredictorset),file=stdout(),
			        ncol=ncol(keptpredictorset))
			}
		}
		a <- b+1
	}
	#Now finally the last step 
	allgcvsordered=order(gcvs)
	
		#Filters the combinations based on correlation between predictor 
		# varibles(multicolinearity)
		# returns an array that is the same size or smaller than all subsets
		# where rows corresponding to multicolinear sets have been discarded
	combinationskept <- combinationfilter(rawpredictors, 
	    allsubsets[allgcvsordered,], verbose = verbose)
	
	if(verbose > 1){
	    cat("----------------------------------------\n")
	    cat("Best Combinations that are not correlated among themselves\n")
	    cat(combinationskept,'\n')
	    cat("----------------------------------------\n")
    }
	
	
	finalselection <- allgcvsordered[combinationskept]	
	
	
	gcvratio <- gcvs[finalselection] / gcvs[finalselection][1]
	finalselection <- finalselection[which(gcvratio < 1.5)]
	gcvratio <- gcvs[finalselection] / gcvs[finalselection][1]
	
		
	if(verbose > 2)
	    print(finalselection)
	finalpredictors <- allsubsets[finalselection,]
  
	predset <- cbind(lps$size[finalselection],finalpredictors,
	            gcvs[finalselection], degs[finalselection],
	            alphas[finalselection],gcvratio)
	info <- cbind(lps$size[finalselection], gcvs[finalselection],
				degs[finalselection], alphas[finalselection], gcvratio)
	
	rownames(info) <- 1:nrow(predset)	
	colnames(info) <- c('np', 'gcv', 'deg', 'alpha', 'gcvratio')
	info <- as.data.frame(info)		
	
	hold <- fitobj
	fitobj <- list()
	for(i in 1:length(finalselection))
		fitobj <- c(fitobj,list(hold[[finalselection[i]]]))
	
	if(verbose > 1){
	    cat("Finally selcted Combinations\n")
	    cat("npredictors, predictors selected, GCV vals, deg, alpha, GCV ratio\n")
    	for(i in 1:nrow(predset))
    	    cat(lps$size[finalselection][i],
    	        colnames(rawpredictors)[finalpredictors[i,]],
    	        gcvs[finalselection][i], degs[finalselection][i], 
    	        alphas[finalselection][i], gcvratio[i],'\n')
	}
	#write(t(predset),file=outputfile,append=TRUE,ncol=ncol(predset))
	return(list(psets=finalpredictors,info=info,fitobj=fitobj,
	    rawpredictors=rawpredictors, response=response))
	
	
	
}
