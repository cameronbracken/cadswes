dts <- function(hts,I,dim=3:5,tau=5:10,nalpha=25){
    

    #this function fits all possible models to the embeded time series with 
    #the parmeters dim and tau
    library(locfit)
    
    
    alpha <- rep(seq(0.1,1,length.out=nalpha),2)
    gcvs <- numeric(2*nalpha)
    imodel <- 0
    models <- numeric(0)
    cat('Fitting', length(dim)*length(tau)*nalpha*2, 'models.\n')

    for(idim in dim){
        DIM <- idim
    
        for(itau in tau){
            imodel <- imodel + 1
        
            if(itau>tau[1]){rm(predictors,response)}
                
            TAU <- itau
            cat('Fitting models for embedding dimension',DIM,'and lag',TAU)
    
            n <- (I-1-(DIM-1)*TAU-1)  #N is the number of embedded time series
    
            # "predictors" and "response" store the phase space co-ordinates 
            #  and corresponding output values for the simulation purpose
    
            predictors <- matrix(ncol = DIM, nrow = n)
            response <- integer(n)
    
            xx <- 1:DIM
    
            for(i in 1:n){
                #assemble all the series with delay time TAU
                predictors[i,] <- hts[((xx-1)*TAU)+i]
                response[i] <- hts[((DIM-1)*TAU)+1+i]
            }
    
            for(j in 1:(2*nalpha)){
                if(j > nalpha/2) deg = 2 else deg = 1
                
                fit <- locfit.raw(x=predictors,y=response,alpha=alpha[j],deg=deg,scale=T,kern="bisq",maxk=2500)
    
                z <- fit$dp[c("lk", "df1", "df2")]
                n <- fit$mi["n"]
                gcvs[j] = (-2 * n * z[1])/(n - z[2])^2
    
                cat('.')
            }
            cat('done.\n')
    
    
            models <- rbind(models,
                            cbind(rep(DIM,2*nalpha),
                                  rep(TAU,2*nalpha),
                                  c(rep(1,nalpha),rep(2,nalpha)),
                                  alpha,
                                  gcvs))

        } #END TAU LOOP
    
    } #END DIM LOOP
   
    models <- as.data.frame(models) 
    names(models) <- c('d','T','deg','alpha','gcv')
    models
    
} #END FUNCTION 
