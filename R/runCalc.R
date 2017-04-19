runCalc<-function(base64Json, keep_backtest=F){
  if(exists("res",envir = globalenv())){rm(res)}
  res<-tryCatch(expr={
    print("running calculation")

    #parse the JSON
    theData <- parseJSON(base64Json)

    #subset all time series to before the base date
    theData <- subsetToBaseDate(theData)

    #extract the model_name
    model <- theData$model_name

    print(model)

    #get the model function file
    modelFunctionFile <- paste("R/",model,".RData",sep="")
    load(modelFunctionFile)

    #this runs and backtests the model
    out <- runModel(theData)

    if(model!="Average"){
      out<-getScore(out)
    }

    #this creates two versions of the ultimate projections, one weighted one unweighted
    out$predicted_values_weighted <- weightProjections(theData$employment,out$predicted_values_unweighted,theData$long_term_growth_rate)

    #this prepares the output JSON and makes sure the fields are in the right order
    prepareJSON(out,keep_data=keep_backtest)
  },
  error = function(err){
    toJSON(list(error=as.character(err)))
  }
  )
  res
}
