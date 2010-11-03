RPSS <- function(histobs, ensemble, alt=F, altobs=0){
    # This function calculates the Rank Probability Skill Score(RPSS)
    # for each prediction year and gives the RPSS for 'wet', 'dry' and all years
    # 'wet' is in upper 67th percentile, dry is in lower 33rd percentile
    #
    # Variable list(in order of appearance): 
    # histobs         =column vector of historical flow values
    # ensemble        =all predictions of all models
    # alt             =if alt=T altobs must be given to calculate an alternate quantile
    #                  for back data
    # subt            =subtitle that you want to write on the graphs
    # rpss            =all the values of rpss 
    # isim            =vector of all simulated values(from all models) for one year
    # obsval          =observed value in any given year
    # calibdata       =historical flows with one year removed 
    # lq              =upper limit for the lower category (quantile)
    # uq              =lower limit for the upper category (quantile
    # ncat            =number of categories (we can set it to 3 for now)
    #                     3 i.e., Total, Wet and Dry periods
    # probs           =cumulative probability of 
    # pclim           =cumulative probability of falling in each category based on climatology (always 1/3 in each)
    # obscat          =observational vector (1 if forcast flow falls in category, 0 otherwise)
    # simcat          =the categories that each of the forecasts falls in 
    # nsim            =number of simulations

    nyr <- length(histobs) #Number of Years Record
    rpss <- 1:nyr                #Equivalent size to number of years

    for(j in 1:nyr) { 
        isim <- ensemble[,j]       # Ensemble of a particular year
        obsval <- histobs[j]   #Observed value of that particular year
 
        #remove the year we are on to get calibrated data
        calibdata <- histobs[-j]
    
        # Find 33rd and 67th percentile of calibrated data
        lq <- ifelse(alt, quantile(altobs,1/3), quantile(calibdata,1/3))
        uq <- ifelse(alt, quantile(altobs,2/3), quantile(calibdata,2/3))
             
        ncat <- 3      #Number of categories
        probs <- pclim <- obscat <- numeric(ncat)

        # assigns which category each of the simulations 
        # falls in for whole ensemble       
        nsim <- length(isim)       #Number of Simulations
        simcat <- numeric(nsim)
        simcat[isim >= uq] <- 3
        simcat[isim <= lq] <- 1
        simcat[isim > lq & isim <uq] <- 2

        # probability of simulations falling into each category        
        for(i in 1:ncat)       
            probs[i] <- length(simcat[simcat == i])/nsim 
        probs <- cumsum(probs)

        # raw probability of falling into each category   
        # 33% chance to fall in each one of 3 categories             
        pclim <- cumsum(rep(1,ncat)/ncat)
                        
        # where the obsevation actually falls        
        obscat <- rep(0,ncat)
        if(obsval >= uq) #in third category(quantile)
            obscat[3]=1
        if(obsval > lq & obsval < uq)   #obs in second quantile
            obscat[2]=1
        if(obsval <= lq) #obs in first quanitle
            obscat[1]=1
        
        obscat <- cumsum(obscat)

        rpss[j]=1-(sum((obscat-probs)^2))/(sum((obscat-pclim)^2))
    #print(obscat-probs)
  }
  
  #quartz()
    #hist(rpss,main=sitename)
            

    quw=quantile(histobs,2/3)  
    zzw=rpss[histobs>quw]           #Calculate the RPSS Scores for wet and dry periods
    #boxplot(zzw,main="Wet", ylim=c(-2,1))

    qld=quantile(histobs,1/3)      
    zzd=rpss[histobs<qld]  
    #browser()     
    #boxplot(zzd,main="Dry",sub=subt, ylim=c(-2,1))
    
    #print("Total Wet Dry")
    #print(c(length(rpss),length(zzw),length(zzd)))

    #Prints site name and statistics of the corresponding of the station on the screen
    #cat("Median:     ",median(rpss),'\n')
    #cat("Median Wet: ",median(zzw),'\n')
    #cat("Median Dry: ",median(zzd),"\n")
  #print(rpss)
  RPSS=median(rpss)
  return(rpss)
}
