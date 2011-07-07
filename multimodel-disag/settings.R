    #number of simulations to perform
nsims <- 1000
    #lead time, could be nov, jan, feb, apr
predmonth <- "apr"
    # Should forecasts be done only for apr-july
seasonal <- TRUE
    # minimum proportion of data to use in local regression
min.alpha  <- .3
    # maximum number of variables to include in a single model 
nvmax <- 3
    # should intervening flows be used internally?  If FALSE, total flows are used. 
    # DONT CHANGE THIS! Forecasts should alway be made in intervening space 
    #                   but will get converted to total flow internally 
intervening <- FALSE
    # Verification type, "drop-one", "retro", "all"
vtype <- "all"
    # in retroactive verification mode, the number of years back to simulate
nback <- 15
