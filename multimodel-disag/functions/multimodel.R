multimodel <- function(obj, trainingmodel, trainingresponse,
    allpredictors, npredpts, nsims=1, outputfile='simdata.out')
{

	#The process goes:
	#				1.store the fit objects of each model
	#				2.generate nsims predictions for every point
	#				3.return the predictions
	
    require(locfit)
    require(lattice)
    require(akima)
    
    gcvs <- obj$info[['gcv']]
    locfitcalib <- obj$fitobj
    psets <- obj$p   # Grabs only predictors information 
	
	simdata=matrix(0,nrow=npredpts,ncol=nsims)
	
	W=1/gcvs	        # Least GCV combination gets more weight
	W=cumsum(W/sum(W))  #W is the weight function
	
	cat('Making Predicitons...\n\n')
	pb <- txtProgressBar(1,npredpts,style=3)
	for(j in 1:npredpts){
	    setTxtProgressBar(pb, j)
				#for every point in prediction range, make a prediction
		for(i in 1:nsims){
				#choose model
				#draw a U(0,1) RV
			randnum=runif(1,0,1)
			    #attaches randomnumber to the weight function
			xyzran=c(randnum,W)	
			    #the first number of the ranked vector is the 
			    #index of the randnum in the ordered vecor 
			modelnumber=rank(xyzran)[1]	
						
	             #pull out combination for this iteration
			selectedpset=as.logical(psets[modelnumber,])   
			    #pull out predictors corresponding to this combo
			modeldata=allpredictors[,selectedpset]	
			
			if(sum(psets[modelnumber,])==1){
				    #then there is only one predictor
				    #The current state of the system
				predictat=modeldata[j]	
			}else{
				    #there is more than one predictor
				    #so that the data is as.matrix
				predictat=modeldata[j,]
				predictat=rbind(predictat)		
			}

			
			locfitpredicted <- predict.locfit(locfitcalib[[modelnumber]],
			    predictat,se.fit=T,band="global")
	
			#if(j%%1==0){print(paste('model',modelnumber,'on point',j,
			#    'out of',npredpts),quote=F)}
	
			resid=rnorm(1,0,locfitpredicted$se.fit[1])
			simdata[j,i]=locfitpredicted$fit[1]+resid
		
			if(is.nan(simdata[j,i])){simdata[j,i]=0}
			if(simdata[j,i]<0){simdata[j,i]=0}
	
		}
	}
	close(pb)
	return(simdata)
}
