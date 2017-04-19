backTestMARS<-function(employment_series,empl_monthCounter,baseDate,naicsCode){
  #unlike the other backtesting functions, this function both does the backtesting and makes the mars model going forward
    
  #baseDates for backtesting    
  backTestBaseDates<-c(baseDate-23,baseDate-29,baseDate-41)
  
  #put it in a data frame for MARS (time  is the predictor, employment outcome)
  forMARS<-data.frame(employment=employment_series,monthCounter=empl_monthCounter)
  
  errorScores<-list(rmse=numeric(),rmspe=numeric(),mape=numeric(), nrmse=numeric())
  predictedValues<-list()
  actualValues<-list()
  theMonthCounters<-list()
  
  for(i in 1:length(backTestBaseDates)){
    #get the monthCounter of the back test date
    month<-backTestBaseDates[i]
    timeSeriesBack<-forMARS[forMARS$monthCounter<month,]
    timeSeriesActual<-forMARS[forMARS$monthCounter>=month,]
    
    employment.actual<-timeSeriesActual$employment
    employment.actual<-as.integer(unlist(employment.actual))
    #only comparing to 24 values for error metrics
    employment.actual<-employment.actual[1:24]
    
    #make the MARS model and predict
    res<-makeMARSandPredict(employment_series=timeSeriesBack$employment,monthCounters=timeSeriesBack$monthCounter,nAhead = 24)
    
    #get the error scores
    error<-employment.actual-res$predicted_values
    error<-as.numeric(unlist(error))
    pctError<-(error/employment.actual)*100
    pctError<-as.numeric(unlist(pctError))
    
    rmse<-(sqrt(mean(error^2)))
    mape<-(mean(abs(pctError)))
    rmspe<-(sqrt(mean(pctError^2)))
    nrmse<-rmse/(max(employment.actual)-min(employment.actual))
    
    #gather all the backtesting stuff
    errorScores$mape<-c(errorScores$mape,mape);errorScores$rmspe<-c(errorScores$rmspe,rmspe)
    errorScores$rmse<-c(errorScores$rmse,rmse);errorScores$nrmse<-c(errorScores$nrmse,nrmse)
    predictedValues[[i]]<-round(res$predicted_values)
    actualValues[[i]]<-employment.actual
    theMonthCounters[[i]]<-res$predicted_monthCounters
  }
  
  #bryceScore takes in to account variance across back testing points and the mean error across backtesting (using root mean square percent error)
  bryceScore<-sd(errorScores$nrmse)*2 + mean(errorScores$nrmse)
  
  #norm it relative to what we think the "worst" score is
  normedScore<-1-(bryceScore)
  
  #this is the final score that is determined by the equation 0.05*Model Complexity Rank + 0.1 * world-relevance + 0.85 * normed bryce score
  joshScore<-0.05 * 0.66 + 0.1 * 0 + 0.85 * normedScore
  
  
  #all the stuff for output, including the 10 year ahead projections
  out<-makeMARSandPredict(employment_series,empl_monthCounter,nAhead = 120)
  out$diagnostics$rmse<-errorScores$rmse
  out$diagnostics$rmspe<-errorScores$rmspe
  out$diagnostics$nrmse<-errorScores$nrmse
  out$diagnostics$errorScore<-normedScore
  out$diagnostics$finalScore<-joshScore
  out$back_test_base_dates<-backTestBaseDates;out$back_test_predicted<-predictedValues
  out$back_test_actuals<-actualValues
  out$back_test_monthCounters<-theMonthCounters
  out$outcome_naics<-as.character(naicsCode)
  out
}