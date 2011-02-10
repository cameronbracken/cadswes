fullpool <- read.table('data/limited-pool-recons.txt',header=T,fill=T)
sy <- fullpool[1,1]
fullpool <- fullpool[,-1]
filter <- apply(!t(apply(fullpool,1,is.na)),1,all)
fullpool <- ts(fullpool[filter,],start=sy,freq=1)

sites <- c('Green.GRUt','Colo.Cisco','SJnBluff')

layout(cbind(1:length(sites)))

for(i in 1:length(sites))
    plot(ntile.ts(fullpool[,sites[i]],3),type='h')

paleo.bqr <- laglead(ntile.ts(fullpool[,sites[1]],3),1,1)
tr.paleo <- trprob( paleo.bqr$lag, paleo.bqr$lead )
tr.paleo$p