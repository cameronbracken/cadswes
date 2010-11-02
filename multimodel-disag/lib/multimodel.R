multimodel <- function(obj, newdata, nsims=1, verbose = 1, 
	vtype = "none", nback = 15)
{

	#The process goes:
	#				1.store the fit objects of each model
	#				2.generate nsims predictions for every point
	#				3.return the predictions
	
	# verbose = 0 No output
	# verbose = 1 info on prgerss of long computations
    suppressMessages(require(locfit))
    npredpts <- if(vtype == "retro") nback else nrow(newdata)
    
    gcvs <- obj$info[['gcv']]
    locfitcalib <- obj$fitobj
    psets <- obj$psets   # Grabs only predictors information
	
	simdata <- matrix(0,nrow=npredpts,ncol=nsims)
	
	W <- 1/gcvs	        # Least GCV combination gets more weight
	W <- cumsum(W/sum(W))  #W is the weight function
	
	if(verbose > 0){
	    cat('Making Predictions...\n')
	    flush.console()
	    pb <- txtProgressBar(1,npredpts,style=3)
	}
	for(j in 1:npredpts){
	    if(verbose > 0) setTxtProgressBar(pb, j)
				#for every point in prediction range, make a prediction
			
		
		    # For these verification types we need to re-fit the model 
		    # but using the same parameters. The assumption here is that
		    # although the data is changeing, the patterns we identified
		    # using all the data should hold with less of the data. 
		    # This assumption is probably very good for drop one and the 
		    # later years in the retroactive years but not so good in the
		    # first couple years of retroactive where significantly less 
		    # data is used. 
		if(vtype == "retro" || vtype == "drop-one"){
		    
		    locfitcalib <- list()
		    for(m in 1:nrow(obj$info)){
		        this.alpha <- obj$info$alpha[m]
			    this.deg <- obj$info$deg[m]
			    if(vtype == "retro"){
		        
			        rr <- nrow(obj$rawpredictors) - npredpts + j - 1
			        retrorange <- 1:rr
			        this.predictorset <- unclass(obj$rawpredictors)[retrorange,obj$psets[m,]]
    			    this.response <- unclass(obj$response[retrorange])
		        
			    }else{
		        
			        this.predictorset <- unclass(obj$rawpredictors)[-j,obj$psets[m,]]
    			    this.response <- unclass(obj$response[-j])
		        
			    }
		    #browser()
			    locfitcalib[[m]] <- locfit.raw(x=this.predictorset, 
			        y=this.response, alpha=this.alpha, deg=this.deg, 
			        kern="bisq", maxk=200, scale = T)
		    }
		}

		for(i in 1:nsims){
				#choose model
				#draw a U(0,1) RV
			randnum <- runif(1,0,1)
			    #attaches randomnumber to the weight function
			xyzran <- c(randnum,W)	
			    #the first number of the ranked vector is the 
			    #index of the randnum in the ordered vecor 
			modelnumber <- rank(xyzran)[1]	
						
	             #pull out combination for this iteration
			selectedpset <- as.logical(psets[modelnumber,])   
			    #pull out predictors corresponding to this combo
			modeldata <- newdata[,selectedpset]	
			
			    #select the model to use
			this.model <- locfitcalib[[modelnumber]]
			    
			if(sum(psets[modelnumber,])==1){
				    #then there is only one predictor
				    #The current state of the system
				predictat <- modeldata[j]	
			}else{
				    #there is more than one predictor
				    #so that the data is as.matrix
				predictat <- rbind(modeldata[j,])	
			}
			
			
			if(vtype == "retro")
    			predictat <- 
    		    if(sum(psets[modelnumber,])==1)
    				modeldata[rr+1]	
    			else
    			    rbind(modeldata[rr+1,])

			
			locfitpredicted <- predict.locfit(this.model, predictat, 
			    se.fit=T, band="global")
	
			resid <- rnorm(1,0,locfitpredicted$se.fit[1])
			simdata[j,i] <- locfitpredicted$fit[1]+resid
	
		}
	}
	if(verbose > 0) close(pb)
	#cat('\n')
	obj$newdata <- newdata
	obj$pred <- t(simdata)
	return(obj)
}
