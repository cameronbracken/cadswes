source('lib.R')
require('msm')
require('tikzDevice')

m <- 3  # number of states
sim.len <- 50
nsims <- 1200
output.rdata <- paste('output/otowi-',m,'-state.RData',sep='')
plot.dir <- 'plots'

  # Read the paleo data
paleo <- read.table('data/103.txt',skip=1,header=TRUE)
paleo <- paleo$RECON

  # Read the flow data
otowi <- read.table('data/otowi-flow-vol-kaf-natural.txt',header=TRUE)
x <- ts(otowi$flow,otowi$year[1])

  # these are all the values in a particular state 
pools.to <- vector('list',m)
for(i in 1:m)
  pools.to[[i]] <- which(ntile.ts(x,m)==(i-1))

  # initilaize simulations
sims.nh <- sims <- sims.yr <- sims.nh.yr <- matrix(NA,nrow=nsims,ncol=sim.len)

  # only run the simulations if the data is not cached (saves time)
if(!file.exists(output.rdata)){
  
  message('Simulating...')
  pb <- txtProgressBar(1,nsims,style=3)
  
  for(i in 1:nsims){
    setTxtProgressBar(pb,i)
      
      # Homogeneous simulation
      # return the indexes so we can get both the years and the values
      # using weight = equal produces a better simulation of sd
    ind <- simulate.mc.paleo(x, tpm(paleo,m), pools.to, sim.len, 
      weight='equal', index = TRUE)
    sims[i,] <- x[ind]
    sims.yr[i,] <- time(x)[ind]
    
      # Nonhomogeneous simulation, Use a TPM from a random 50 year window
    window <- sample(1:(length(paleo)- sim.len),1)
    window <- window:(window+sim.len-1)
    ind <- simulate.mc.paleo(x, tpm(paleo[window],m), pools.to, sim.len, 
      weight='equal', index = TRUE)
    sims.nh[i,] <- x[ind]
    sims.nh.yr[i,] <- time(x)[ind]
  }
  
  close(pb)
  save(sims,sims.nh,sims.yr,sims.nh.yr,file=output.rdata)
  
}else{
  
  load(output.rdata)
  
}
write.table(sims, file=paste('output/SimFlow',m,'.txt',sep=''), 
  row.names = F, col.names = F)
write.table(sims.yr, file=paste('output/SimYear',m,'.txt',sep=''), 
  row.names = F, col.names = F)
write.table(sims.nh, file=paste('output/SimFlowNH',m,'.txt',sep=''), 
  row.names = F, col.names = F)
write.table(sims.nh.yr, file=paste('output/SimYearNH',m,'.txt',sep=''), 
  row.names = F, col.names = F)


if(!file.exists(plot.dir)) dir.create(plot.dir)
##################################
#
# Simulation Transition Prob boxplots
#
##################################
pdf(file.path(plot.dir,paste('tp-sim-',m,'.pdf',sep='')), height=4, width = 6)

  sim.tpms <- lapply(as.data.frame(t(sims)),tpm,m)
  sim.trans <- vector('list',m^2)
  for(i in 1:m^2)
    sim.trans[[i]] <- as.vector(sapply(sim.tpms,'[',i))
  
  boxplot(sim.trans,cex=.5)
  points(as.vector(tpm(paleo,m)),col=2,pch=2)
  
dev.off()

##################################
#
# Paleo Transition Prob boxplots
#
##################################
pdf(file.path(plot.dir,paste('tp-paleo',m,'.pdf',sep='')), height=4, width = 6)

  paleo.tpms <- vector('list',length(paleo)-sim.len)
  for(i in 1:(length(paleo)-sim.len))
    paleo.tpms[[i]] <- tpm(paleo[i:(i+sim.len-1)],m)
  
  paleo.trans <- vector('list',m^2)
  for(i in 1:m^2)
    paleo.trans[[i]] <- as.vector(sapply(paleo.tpms,'[',i))

  boxplot(paleo.trans,cex=.5)
  points(as.vector(tpm(paleo,m)),col=2,pch=2)

dev.off()


##################################
#
# Paleo Transition Prob time series
#
##################################
pdf(file.path(plot.dir,paste('tp-ts-',m,'.pdf',sep='')))

  grid.newpage() 
  pushViewport(viewport(layout = grid.layout(m, 1))) 
  vplayout <- function(x, y) 
      viewport(layout.pos.row = x, layout.pos.col = y)

  for(i in 1:m){
    paleo.trans.df <- as.data.frame(paleo.trans[1:m+((i-1)*m)])
    rownames(paleo.trans.df) <- -1:(end(ts(paleo,-1))[1] - sim.len)
    names(paleo.trans.df) <- paste(i,1:m,sep='')
    paleo.df <- melt(as.matrix(paleo.trans.df))
    paleo.df$X2 <- factor(paleo.df$X2)
    names(paleo.df) <- c('Year', 'Transition', 'Transition Probability')
    p <- qplot(Year,`Transition Probability`,data=paleo.df,color=Transition,geom='line')
    p <- p + theme_bw() +scale_color_manual(values= gray.colors(9))
    print(p, vp = vplayout(i, 1 ))
  }
  
dev.off()

##################################
#
# Basic Statistics
#
##################################
tikz(file.path(plot.dir,paste('basic-stats-',m,'.tikz',sep='')), height=4, width = 6)

  stats <- sim.stats.basic(sims,x)
  boxplot.stats.basic(stats,x)
  
dev.off()

##################################
#
# Drought Statistics
#
##################################

tikz(file.path(plot.dir,paste('drought-stats-',m,'.tikz',sep='')), height=4, width = 6)

  stats.dr <- sim.stats.drought(sims)
  boxplot.stats.drought(stats.dr,x)
  
dev.off()

##################################
#
# Basic Statistics NH
#
##################################
tikz(file.path(plot.dir,paste('basic-stats-nh-',m,'.tikz',sep='')), height=4, width = 6)

  stats.nh <- sim.stats.basic(sims.nh,x)
  boxplot.stats.basic(stats.nh,x)
  
dev.off()

##################################
#
# Drought Statistics NH
#
##################################

tikz(file.path(plot.dir,paste('drought-stats-nh-',m,'.tikz',sep='')), height=4, width = 6)

  stats.nh.dr <- sim.stats.drought(sims.nh)
  boxplot.stats.drought(stats.nh.dr,x)
  
dev.off()

