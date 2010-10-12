    #number of simulations to perform
nsims <- 1000
    #lead time, could be nov, jan, feb, apr
predmonth <- "apr"
    # Should forecasts be done only for apr-july
seasonal <- FALSE
    # minimum proportion of data to use in local regression
min.alpha  <- .3
    # maximum number of variables to include in a single model 
nvmax <- 3
    # should intervening flows be used?  If FALSE, total flows are used. 
intervening <- T

    # load libraries, source code, and data
source('setup/setup.R')
calibration <- selectpredictors(predictors,response,min.alpha = min.alpha, 
    nvmax = nvmax)
pred <- multimodel(calibration, predictors, response, predictors, nsims)

pd <- pdisag(historical, sims=pred, nsim=nsims)
d <- disag(pd,plot=F)

diagnostics(d,main='Apr 1',time.names=month.name[4:7],
    site.names = site.names,cn = year.s:year.e,
    ylab='Flow Volume [MAF/month]',density=F)