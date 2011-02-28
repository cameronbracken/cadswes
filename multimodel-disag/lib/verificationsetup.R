verificationsetup <- function(predictors, response, historical, type, nback){
    
    if(type == "all"){
        
        newdata <- predictors
        predictors <- window(predictors,start=start(response),end=end(response))
          # could use newdata as the entire predictor range but then we cant disag
        response <- response
        historical <- historical

      }else if(type == "retro"){
        
        newdata <- predictors
        predictors <- window(predictors,start=start(response),end=end(response))
        response <- response
        s <- nrow(historical) - nback + 1
        e <- nrow(historical)
        historical <- historical[s:e,,]
    
    }else{

        newdata <- predictors
        predictors <- window(predictors,start=start(response),end=end(response))
        response <- response
        historical <- historical
    }
    
    return(list(predictors=predictors,newdata=newdata,
        response=as.vector(response), historical=historical))
}