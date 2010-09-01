blindFC <- function(hts, models, I, fc = TRUE, blind = TRUE, tol = 0.2, 
	div.tol = 1, n.ahead = 50){
	

	fc.step.length <- 1

	require(locfit)

		#select only models within (100*tol)% of best model
	models <- models[(1:nrow(models))[models$gcv < min(models$gcv)*(1+tol)],]
	models <- models[order(models$gcv),]
	
		#the "forecast" matrix will hold the forecast of each model 
	forecast <- matrix(NA,nrow=n.ahead,ncol=nrow(models))  #nrow(models) is the number of models

	for(m in 1:nrow(models)) {

		DIM <- models$d[m]			#embedding dimension
		T <- models$T[m]			#lag
		alpha <- models$alpha[m]	#neighborhood size
		deg <- models$deg[m]		#local polynomial degree
		gcvsel <- models$gcv[m]		#computed gcv of corresponding combination
	
		#cat('Model',m,"dimension",DIM,"lag",T,"alpha",alpha,'degree',deg,'\n')

		N <- (I-1-(DIM-1)*T-1)				#N is the number of possible embedded timeseries 

		embeddedts <- matrix(ncol=DIM,nrow=N)
		response <- numeric(N)

		xx <- 1:DIM	   
			#Reconstruct phase space 
		for(i in 1:N){
			embeddedts[i,] <- hts[(xx - 1) * T + i]
			response[i] <- hts[(DIM - 1)*T + 1 + i]
		}

			#fit the local polynomial model 
		fit <- locfit.raw(x=embeddedts,y=response,alpha=alpha,deg=deg,scale=T,kern="bisq",maxk=5000)

	   this.step.forecast <- rep(NA,n.ahead)
	   fc.step <- unstable <- 0
	   #cat('.')
	   hts.fc <- c(hts[1:(I-1)],rep(NA,n.ahead))
	   initial <- I						   #where to start forecast
	   final <- initial + n.ahead - 1	   #where to end forecast
	   this.forecast <- rep(NA,n.ahead)
   
			#predict at every point with the current model
		for(i in initial:final){
			#cat('>')
			fc.step <- fc.step + 1 
			predict.at <- hts.fc[i - (rev(xx) - 1) * T - 1] #current state of the system 
			#print(i - (rev(xx) - 1) * T - 1)
			#print(predict.at)
			predict.at <- rbind(predict.at,predict.at) #the new data must be a matrix with more that one row
		   
			preds <- predict(fit,predict.at)
			
			hts.fc[i] <- this.forecast[fc.step] <- preds[1]	 #the first value is the actual prediction
		}
		
		#print(this.step.forecast)
	
		forecast[,m] <- this.forecast
		#cat('\n')
	}
	forecast
}