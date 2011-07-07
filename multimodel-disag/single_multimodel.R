#!/usr/bin/env Rscript

    # This is the control script for the multimode ensemble forecast framework
    # the main parameters are set here, this script needs to be run for each
    # different lead time

    # load libraries, source code, and data, 
    # depends on the values of the parameters above
    # most importantly, get the "predictors" and "response" variables. 
    # also loads historical, n.sites, n.years, n.months, year.s, 
    # year.e, site.names, time.names
source('setup/setup.R')
training <- verificationsetup(predictors, response, historical.tot, vtype, 
    nback = nback)
training.intv <- verificationsetup(predictors, response, historical.intv, vtype, 
    nback = nback)

    # Run the model a bunch of times to get 
    # an estimate of the best parameters
selectpredictors_c <- cmpfun(selectpredictors)
fit <- selectpredictors(training$predictors,training$response, 
	min.alpha = min.alpha, nvmax = nvmax, verbose=0, ratio=1.5)


    # using the best set of predictors, run the multimodel 
    # and generate ensembles using new data
multimodel_c <- cmpfun(multimodel)
fit <- multimodel(fit, training$newdata, nsims, vtype = vtype, nback = nback)

    # set up the proportion disag object
pdisag_c <- cmpfun(pdisag)
pd <- pdisag_c(fit, training$historical, nsim=nsims, simname="pred")

    # do the proportion disag
disag.pdisag_c <- cmpfun(disag.pdisag)
d <- disag.pdisag_c(pd, plot=F)

    # convert back to intervening and replace the 
    # historical data with the intervening
disagSimsToIntervening_c <- cmpfun(disagSimsToIntervening)
d <- disagSimsToIntervening_c(d,t2i)
#d$hist <- training.intv$historical
d$disag <- d$disag.tot

main <- paste(CapFirst(predmonth),'1')
years <- time(training$newdata)
cn <- if(vtype=="retro") years[-(1:(length(years)-nback))] else years
  
# browser()  
    # save a bunch of plots and return stats
d <- diagnostics(d, main=main,time.names=time.names,
    site.names = site.names, cn = cn,
    ylab='Flow Volume [MAF/month]',vtype=vtype, nback=nback ,density=F)

source('cbrfc_plots.R')
    
save(predictors, response, historical.tot, historical.intv, vtype, predmonth, 
  nback, training, fit, d, main, years, cn,
  file=paste('diagnostics/output_data/',predmonth,'_',vtype,'.RData',sep=''))
 
