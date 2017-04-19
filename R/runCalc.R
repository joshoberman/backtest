runCalc<-function(base64Json, keep_backtest=F){
  if(exists("res",envir = globalenv())){rm(res)}
  res<-tryCatch(expr={
    print("running calculation")
    #parse the JSON
    jsonParserFile <- "R/parseJSON.RData"
    load(jsonParserFile,envir=globalenv())
    theData <- parseJSON(base64Json)

    #subset all time series to before the base date
    subsetFile <- "R/subsetToBaseDate.RData"
    load(subsetFile, envir = globalenv())
    theData <- subsetToBaseDate(theData)

    #extract the model_name
    model <- theData$model_name

    print(model)

    #get the model function file
    modelFunctionFile <- paste("R/",model,".RData",sep="")
    load(modelFunctionFile, envir= globalenv())

    #this runs and backtests the model
    out <- runModel(theData)

    if(model!="Average"){
      load("R/getScore.RData", envir=globalenv())
      out<-getScore(out)
    }

    #this creates two versions of the ultimate projections, one weighted one unweighted
    load("R/weightProjections.RData", envir=globalenv())
    out$predicted_values_weighted <- weightProjections(theData$employment,out$predicted_values_unweighted,theData$long_term_growth_rate)

    #this prepares the output JSON and makes sure the fields are in the right order
    load("R/prepareJSON.RData",envir=globalenv())
    prepareJSON(out,keep_data=keep_backtest)
  },
  error = function(err){
    toJSON(list(error=as.character(err)))
  },
  finally={
    print(paste("ran the calculation"))
  }
  )
  res
}
