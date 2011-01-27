
	# parameter search region
d.min <- 3
d.max <- 5
T.min <- 3
T.max <- 5
nalpha <- 25

	# other parameters
n.ahead <- 100
ts.start <- 1906

	#Load functions
source('dts.R')
source('blindFC.R')

	#load data
#x <- read.table('data/lf-annual.txt')[,2]
x <- sin(seq(0,10*pi,,1000))
x <- ts(x,start=ts.start,frequency=1)

	#forecast from end of timeseries
I <- 99#length(x) + 1
fitted.models <- paste('data/fitted-models-',I,'.Rdata',sep='')
plot.file <- paste('plots/blindfc-',I,'.pdf',sep='')

	#fit all model combinations or load the results
if(!file.exists(fitted.models)){
	models <- dts(x,I,dim=d.min:d.max,tau=T.min:T.max,nalpha=nalpha)
	save(models,file=fitted.models)
}else{
	load(fitted.models)
}

	# do the blind forecast and calculate meian, 10 and 90 of ensembles
fc <- blindFC(x,models,I,n.ahead=n.ahead)
fc.med <- ts(apply(fc,1,median), start = (time(x)[1]-1+I), frequency=1)
fc.90 <- ts(apply(fc,1,quantile,.9), start = (time(x)[1]-1+I), frequency=1)
fc.10 <- ts(apply(fc,1,quantile,.1), start = (time(x)[1]-1+I), frequency=1)

pdf(plot.file)
	plot(x,type='l',xlim=c(start(x)[1],n.ahead+end(x)[1]),ylab='Lees')
	lines(fc.med,col='red')
	lines(fc.90,col='blue',lty=2)
	lines(fc.10,col='blue',lty=2)
dev.off()