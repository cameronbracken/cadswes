library("HiddenMarkov")
library("sm")
source('lib.R')

datafile <- "data/lf-annual-cy-af.txt" # "data/Meko.txt" or "data/lf-annual-cy-af.txt"
x <- as.matrix(read.table(datafile))
sy <- x[1,1]
x <- ts(x[,2]/10^6,start=sy,freq=1)
#x <- (x - min(x)+.1)/max(x)

family <- "norm"  # underlying distribution for hmm
discrete <- FALSE
m <- 3        # max order to fit
stationary <- F   # use a stationary distribution of mixtures
do.ar <- TRUE     # generate forecasts using ar and 
nsims <- 3000
verif.start <- 2009
fcdist.len <- 500  # resolution of the output probability distribution
    # different initial condition types when family == "norm"
ic <- "both.diff"#c("same.sd","same.both","both.diff")

verif.range <- verif.start:end(x)[1]
verif.length <- length(verif.range)
tf <- tempfile()
fd.name <- ifelse(family == "norm", "normal", family)


Pi <- matrix(rep(1/m,m^2),m)
#Pi <- diag(rep(1-.05*(m-1),m))
#Pi[Pi == 0] <- .05
# random guess for the transition matrix
#Pi <- matrix(runif(m^2),m)
#Pi <- Pi/rowSums(Pi)
delta <- rep(1/m,m)

this.x <- window(x,start(x),verif.start-1)
pars <- get.named.parlist(this.x, m, fd.name, lower=.01, ic)#,start=list(shape1=2,shape2=2))

hmm <- dthmm(this.x, Pi=Pi, delta=delta, family, pars, nonstat=!stationary, discrete = discrete)


# estimate the parameters of the HMM model
sink(tempfile())
hmm <- BaumWelch(hmm)
sink()

# get the hidden states from the fittet model 
decoding <- ts(Viterbi(hmm),start=sy)

hmm.fc <- hmm.forecast.dist(hmm,len=fcdist.len)

n <- length(this.x)
ss <- sum(residuals(hmm)^2)
se <- sd(residuals(hmm))
np <- (2*m+1+m*(m-1))
LL <- hmm$LL
gcv <- (ss/n)/(1-np/n)^2
bic <- n*log(se)+np*log(n)
aic <- 2*np-2*log(LL+1000)

rpss <- RPSS(window(x,verif.start,verif.start),list(a=hmm.fc),altobs=x)

cat("RPSS: ",rpss,'\n')
print(integrate(approxfun(hmm.fc),0,max(hmm.fc$x)))

plot(hmm.fc,t='l',col='blue')
sm.density(x,add=T)
abline(v=window(x,verif.start,verif.start))
