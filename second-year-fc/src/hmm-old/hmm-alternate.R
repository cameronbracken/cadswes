library('mhsmm')
source('lib.R')

datafile <- "data/lf-annual-cy-af.txt"#"data/LeesFerry.txt" # "data/Meko.txt" or "data/lf-annual-cy-af.txt"
x <- read.table(datafile)
sy <- x[1,1]
x <- ts(x[,2]/10^6,start=sy,freq=1)

m <- 2
fd.name <- 'gamma'

delta <- rep(1/m,m)
P <- matrix(1/m,nrow=m,ncol=m)

dgamma.hsmm <- function (x, m, model) {
    d <-dgamma(x, shape = model$parms.emission$shape[m], rate = model$parms.emission$rate[m]))
    d[is.na(d)] <- 1
    d
}

mstep.gamma <- function (x, wt) {
    k = ncol(wt)
    mu = numeric(k)
    sigma = numeric(k)
    for (i in 1:k) {
        tmp = cov.wt(data.frame(x[!is.na(x)]), wt[!is.na(x), i])
        mu[i] = tmp$center
        sigma[i] = tmp$cov
    }
    list(mu = mu, sigma = sigma)
}

pars <- get.named.parlist(x, m, fd.name, lower=.01)
startval <- hmmspec(init=delta, trans=P, parms.emission=b0,dens.emission=dnorm.hsmm) 
h1 = hmmfit(train,startval,mstep=mstep.norm)