
	# parameter search region
d.min <- 3
d.max <- 5
T.min <- 3
T.max <- 15
nalpha <- 25

	# other parameters
n.ahead <- 100
ts.start <- 1490

	#Load functions
source('dts.R')
source('blindFC.R')

	#load data
x <- scan('data/SAWP_decade.txt',quiet=T)
x <- ts(x,start=ts.start,frequency=1)

	#forecast from end of timeseries
I <- length(x) + 1
fitted.models <- paste('data/fitted-models-',I,'.Rdata')

	#fit all model combinations or load the results
if(!file.exists(fitted.models)){
	models <- dts(x,I,dim=d.min:d.max,tau=T.min:T.max,nalpha=nalpha)
	save(models,file=fitted.models)
}else{
	load(fitted.models)
}

	# do the blind forecast and calculate meian, 10 and 90 of ensembles
fc <- blindFC(x,models,I,n.ahead=n.ahead)
fc.med <- ts(apply(fc,1,median), start = end(x)[1]+1, frequency=1)
fc.90 <- ts(apply(fc,1,quantile,.9), start = end(x)[1]+1, frequency=1)
fc.10 <- ts(apply(fc,1,quantile,.1), start = end(x)[1]+1, frequency=1)

plot(x,type='l',xlim=c(1900,n.ahead+end(x)[1]))
lines(fc.med,col='red')
lines(fc.90,col='blue',lty=2)
lines(fc.10,col='blue',lty=2)
