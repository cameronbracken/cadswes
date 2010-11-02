#!/usr/bin/env Rscript

# This is the control sctip for the multimode ensemble forecast framework
# the main parameters are set here, this script needs to be run for each
# different lead time

    #number of simulations to perform
nsims <- 1000
    #lead time, could be nov, jan, feb, apr
predmonth <- "nov"
    # Should forecasts be done only for apr-july
seasonal <- TRUE
    # minimum proportion of data to use in local regression
min.alpha  <- .3
    # maximum number of variables to include in a single model 
nvmax <- 3
    # should intervening flows be used?  If FALSE, total flows are used. 
intervening <- FALSE
    # Verification type, "drop-one", "retro", "all"
vtype <- "all"
    # in retroactive verification mode, the number of years back to simulate
nback <- 15



    # load libraries, source code, and data, 
    # depends on the values of the parameters above
    # most importantly, get the "predictors" and "response" variables. 
    # also loads historical, n.sites, n.years, n.months, year.s, 
    # year.e, site.names, time.names
source('setup/setup.R')

training <- verificationsetup(predictors, response, historical, vtype, 
    nback = nback)

    # Run the model a bunch of times to get 
    # an estimate of the best parameters
fit <- selectpredictors(training$predictors,training$response, 
	min.alpha = min.alpha, nvmax = nvmax, verbose=1)
    
    # using the best set of predictors, run the multimodel 
    # and generate ensembles using new data
fit <- multimodel(fit, training$newdata, nsims, vtype = vtype, nback = nback)

    # set up the proportion disag object
pd <- pdisag(fit, training$historical, nsim=nsims, simname="pred")

    # do the proportion disag
d <- disag(pd, plot=F)

main <- paste(CapFirst(predmonth),'1')
years <- time(training$newdata)
cn <- if(vtype=="retro") years[-(1:(length(years)-nback))] else years
    
    # save a bunch of plots and return stats
d <- diagnostics(d, main=main,time.names=time.names,
    site.names = site.names, cn = cn,
    ylab='Flow Volume [MAF/month]',vtype=vtype, nback=nback ,density=F)
 