library(flexmix) 
source('lib.R')

datafile <- "data/lf-annual-cy-af.txt" # "data/Meko.txt" or "data/lf-annual-cy-af.txt"
x <- read.table(datafile)[,2]/10^6 #read in data
family <- "gaussian"
#x <- meko
ndist <- 4

layout(rbind(1:2,3:4))
mm <- list()

plot.mixture <- function(x,fl){
    
    xx <- extendrange(x,f=.1)
    xx <- seq(xx[1],xx[2],,200)
    hist(x,freq=F,main=paste(fl@k,"Mixture model"))
    actual.pdf <- numeric(length(xx))
    for(j in 1:fl@k){
        ratio <- summary(fl)@comptab$ratio[j]
        mean <- parameters(fl,comp=j)[1]
        sd <- parameters(fl,comp=j)[2]
        lines(xx,ratio*dnorm(xx,mean=mean,sd=sd))
        actual.pdf <- ratio*dnorm(xx,mean=mean,sd=sd) + actual.pdf
    }
    lines(xx,actual.pdf,col='red')
    
}

plot.mixture.pois <- function(x,fl){
    
    xx <- extendrange(x,f=.1)
    xx <- seq(xx[1],xx[2],,200)
    hist(x,freq=F,main=paste(fl@k,"Mixture model"))
    actual.pdf <- numeric(length(xx))
    for(j in 1:fl@k){
        ratio <- summary(fl)@comptab$ratio[j]
        lambda <- parameters(fl,comp=j)
        lines(xx,ratio*dpois(xx,lambda=lambda))
        actual.pdf <- ratio*dpois(xx,lambda=lambda) + actual.pdf
    }
    lines(xx,actual.pdf,col='red')
    
}

for(i in 1:ndist){

    cat('Fitting', i, 'dist mixture model...\n')
        # fit a mixture once then continue to fit until we find a combination 
        # that has converged and has means that are different between components
    mm[[i]] <- flexmix(x ~ 1, k=i,model=FLXMRglm(family=family)) 
    mm[[i]] <- flexmix(x ~ 1, cluster=ntile.ts(x,i)+1,model=FLXMRglm(family=family)) 
    
    notfound <- TRUE
    while(notfound){
        if(mm[[i]]@converged && !any(diff(parameters(mm[[i]])[1,]) < .01))
             notfound <- FALSE
        else
            mm[[i]] <- flexmix(x ~ 1, cluster=ntile.ts(x,i)+1,model=FLXMRglm(family=family)) 
    }
        
    #print(summary(mm[[i]]))
    plot.mixture(x,mm[[i]])
}