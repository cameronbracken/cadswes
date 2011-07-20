source('lib.R')
require('msm')

  # number of states
m <- 3
sim.len <- 100
nsims <- 1200

paleo <- read.table('data/103.txt',skip=1,header=TRUE)
paleo <- paleo$RECON

otowi <- read.table('data/otowi-flow-vol-kaf-natural.txt',header=TRUE)
x <- ts(otowi$flow,otowi$year[1])

pools.to <- vector('list',m)
for(i in 1:m)
  pools.to[[i]] <- which(ntile.ts(x,m)==(i-1))

sims <- matrix(NA,nrow=nsims,ncol=sim.len)

cat('Simulating...\n');flush.console()
pb <- txtProgressBar(1,nsims,style=3)
for(i in 1:nsims){
  setTxtProgressBar(pb,i)
    #using weight = equal produces a better simulation of sd but worse acf
  sims[i,] <- simulate.mc.paleo(x, tpm(paleo,m), pools.to, sim.len, weight='equal')
}
close(pb)

cat('Calculating Statistics...\n');flush.console()
stats <- sim.stats.basic(sims,x)
boxplot.stats.list(stats,x)



