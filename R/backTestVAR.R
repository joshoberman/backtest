backTestVAR<-function(forVAR,baseDate){
  library(data.table);library(forecast);library(vars)
  #how many variables in our VAR data? The pre-determined format for this is that the first two columns are monthCounter and outcome,
    #remaining are N number of predictors
  nCols<-ncol(forVAR)
  
  #declare variables
  naicsCode<-colnames(forVAR)[2]
  
  #preset back test dates
  backTestBaseDates<-c(baseDate-23,baseDate-29,baseDate-41)
  
  #track what we're doing while backtesting
  errorScores<-list(rmse=numeric(),rmspe=numeric(),mape=numeric(),nrmse=numeric())
  predictedValues<-list()
  actualValues<-list()
  theMonthCounters<-list()
  
  for(i in 1:length(backTestBaseDates)){
    #our base date monthCounter for back testing
    month<-backTestBaseDates[i]
    timeSeriesBack<-forVAR[monthCounter<month]
    timeSeriesActual<-forVAR[monthCounter>=month]
    
    #the actual employment values
    employment.actual<-timeSeriesActual[,2,with=F]
    employment.actual<-as.integer(unlist(employment.actual))
    employment.actual<-employment.actual[1:24]
    
    res<-makeVARandPredict(timeSeriesBack)
    
    error<-employment.actual-res$predicted_values_unweighted
    error<-as.numeric(unlist(error))
    pctError<-(error/employment.actual)*100
    pctError<-as.numeric(unlist(pctError))
    
    rmse<-(sqrt(mean(error^2)))
    mape<-(mean(abs(pctError)))
    rmspe<-(sqrt(mean(pctError^2)))
    nrmse<-rmse/(max(employment.actual)-min(employment.actual))
    
    errorScores$mape<-c(errorScores$mape,mape);errorScores$rmspe<-c(errorScores$rmspe,rmspe)
    errorScores$rmse<-c(errorScores$rmse,rmse);errorScores$nrmse<-c(errorScores$nrmse,nrmse)
    predictedValues[[i]]<-round(res$predicted_values)
    actualValues[[i]]<-employment.actual
    theMonthCounters[[i]]<-res$predicted_monthCounters
  }
  
  bryceScore<-sd(errorScores$nrmse)*2 + mean(errorScores$nrmse)
  
  normedScore<-1-(bryceScore)
  
  joshScore<-0.05 * 1 + 0.1 * 1 + 0.85 * normedScore
  
  out<-makeVARandPredict(forVAR,120)
  #out$diagnostics$rmse_backtesting<-errorScores$rmse;out$diagnostics$mape_backtesting<-errorScores$mape;
  #out$diagnostics$rmspe_backtesting<-errorScores$rmspe;
  out$diagnostics$rmse<-errorScores$rmse
  out$diagnostics$rmspe<-errorScores$rmspe
  out$diagnostics$nrmse<-errorScores$nrmse
  out$diagnostics$errorScore<-normedScore
  out$diagnostics$finalScore<-joshScore
  out$back_test_base_dates<-backTestBaseDates;out$back_test_predicted<-predictedValues
  out$back_test_actuals<-actualValues
  out$back_test_monthCounters<-theMonthCounters
  out$predictors<-colnames(forVAR)[3:nCols]
  out$outcome_naics<-as.character(naicsCode)
  out
}