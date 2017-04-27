runCalc<-function(inputJSON, keep_backtest=F){
  if(exists("res")){rm(res)}
  res<-tryCatch(expr={

    #parse the JSON
    theData <- parseJSON(inputJSON)

    #subset all time series to before the base date
    theData <- subsetToBaseDate(theData)

    #extract the model_name
    model <- theData$model_name

    #get the model function file
    modelFunctionFile <- system.file("calc_engine", paste0(model,".RData"), package = "soiCalcEngine")
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
