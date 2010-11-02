verificationsetup <- function(predictors, response, historical, type, nback){
    
    if(type == "back"){
        
        ti <- time(response)
        newdata <- window(predictors,start=c(1991,1),end=c(2007,1))
        predictors <- window(predictors,start=start(response),end=c(1990,1))
        response <- window(response,start=start(response),end=c(1990,1))
        s <- which(ti==time(training$newdata)[1])
        e <- which(ti==time(training$newdata)[length(time(training$newdata))])
        historical <- historical[s:e,,]
    }else if(type == "retro"){
        
        predictors <- window(predictors,start=start(response),end=end(response))
        newdata <- predictors
        response <- response
        s <- nrow(historical) - nback + 1
        e <- nrow(historical)
        historical <- historical[s:e,,]
    
    }else{

        predictors <- window(predictors,start=start(response),end=end(response))
        newdata <- predictors
        response <- response
        historical <- historical
    }
    
    return(list(predictors=predictors,newdata=newdata,
        response=as.vector(response), historical=historical))
}