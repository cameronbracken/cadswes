#!/usr/bin/env Rscript

# This is the control script for the multimodel ensemble forecast framework
# it will run all the lead times and verification modes, takes a long time

    #Load all the model options/parameters
source('settings.R')

for(predmonth in c("nov", "jan", "feb", "mar", "apr")){
    for(vtype in c("drop-one", "retro", "all")){
        cat('Predicting for:',predmonth,'\n')
        cat('Verification mode:',vtype,'\n')

            # load libraries, source code, and data, 
            # depends on the values of the parameters above
            # most importantly, get the "predictors" and "response" variables. 
            # also loads historical, n.sites, n.years, n.months, year.s, 
            # year.e, site.names, time.names
        source('setup/setup.R')
            # Run the model with the given parameters
        source('single_multimodel.R')
    }
}
