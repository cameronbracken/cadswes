verificationsetup <- function(predictors, response, historical, type){
    
    if(type == "all"){
        
        predictors <- window(predictors,start=start(response),end=end(response))
          # could use newdata as the entire predictor range but then we cant disag
        newdata <- predictors
        response <- response
        historical <- historical
    }
    if(type == "back"){
        
        ti <- time(response)
        newdata <- window(predictors,start=c(1991,1),end=c(2009,1))
        predictors <- window(predictors,start=start(response),end=c(1990,1))
        response <- window(response,start=start(response),end=c(1990,1))
        s <- which(ti==time(training$newdata)[1])
        e <- which(ti==time(training$newdata)[length(time(training$newdata))])
        historical <- historical[s:e,,]
    }
    return(list(predictors=predictors,newdata=newdata,
        response=as.vector(response), historical=historical))
}