backTestARIMA<-function(employment_series,arima_model,naicsCode,monthCounters,baseDate){
    
    #two subtractions, one for the monthCounter and one for the employment series (this is weird but it allows us to keeep 
    #them seperate, so the employment series is of class ts and the monthCounters are just a vector)
    backTestBaseDates<-c(baseDate-23,baseDate-29,baseDate-41)
    
    #get the p,d,q terms for the arima_model generated in the main projection
    terms<-arimaorder(arima_model)
    
    #grab the highest index in the employment_series
    highest_index<-length(employment_series)
    
    #track what we're doing while backtesting
    errorScores<-list(rmse=numeric(),rmspe=numeric(),mape=numeric(),nrmse=numeric())
    predictedValues<-list()
    actualValues<-list()
    theMonthCounters<-list()
    
    for(i in 1:length(backTestBaseDates)){
        #get the backTestIndex: this is what we subtract from the highest_index to do the backtesting
        index<-which(monthCounters==backTestBaseDates[i])
        #this index becomes our backtesting "base date"
        past<-seq(1,index-1,1)
        actual<-seq(index,index+23)
        timeSeriesBack<-employment_series[past]
        timeSeriesActual<-round(unlist(employment_series[actual]))
        
        predicted_monthCounters<-unlist(monthCounters[actual])
        
        arima_back_test_model<-auto.arima(ts(timeSeriesBack,frequency=12),D=1,seasonal = T)
        #make the arima model with the previously determined terms, depending on whether there is a seasonal component
        #then we'll either include those or not
        ##if(length(terms)>3){
        #  arima_back_test_model<-arima(timeSeriesBack,order = terms[1:3], seasonal=list(order=terms[4:6],period=terms[7]))
        #}
        #else if(length(terms)==3){
         # arima_back_test_model<-arima(timeSeriesBack,order = terms[1:3], seasonal=list(order=terms[4:6],period=terms[7]))
        #}
        #forecast 24 steps ahead using this
        arima_back_test_preds<-forecast(arima_back_test_model,h=24)
        arima_back_test_preds<-summary(arima_back_test_preds)
        thePreds<-round(unlist(arima_back_test_preds$`Point Forecast`))
        
        #get all the error stuff
        error<-timeSeriesActual-thePreds
        pctError<-(error/timeSeriesActual)*100
        pctError<-as.numeric(unlist(pctError))
        
        rmse<-(sqrt(mean(error^2)))
        mape<-(mean(abs(pctError)))
        rmspe<-(sqrt(mean(pctError^2)))
        nrmse<-rmse/(max(timeSeriesActual)-min(timeSeriesActual))
        
        #gather that darn data: predicted, actuals, error scores, and the associated monthCounters
        errorScores$mape<-c(errorScores$mape,mape);errorScores$rmspe<-c(errorScores$rmspe,rmspe)
        errorScores$rmse<-c(errorScores$rmse,rmse);errorScores$nrmse<-c(errorScores$nrmse,nrmse)
        predictedValues[[i]]<-round(thePreds)
        actualValues[[i]]<-timeSeriesActual
        theMonthCounters[[i]]<-predicted_monthCounters
    }
    
    #bryceScore takes in to account variance across back testing points and the mean error across backtesting (using root mean square percent error)
    bryceScore<-sd(errorScores$nrmse)*2 + mean(errorScores$nrmse)
    
    #norm it relative to what we think the "worst" score is
    normedScore<-1-(bryceScore)
    
    #this is the final score that is determined by the equation 0.05*Model Complexity Rank + 0.1 * world-relevance + 0.85 * normed bryce score
    joshScore<-0.05 * 0.66 + 0.1 * 0 + 0.85 * normedScore
    
    #output has diagnostics + backTest value data for plotting
    out<-list()
    out$diagnostics$rmse<-errorScores$rmse
    out$diagnostics$rmspe<-errorScores$rmspe
    out$diagnostics$nrmse<-errorScores$nrmse
    out$diagnostics$errorScore<-normedScore
    out$diagnostics$finalScore<-joshScore
    out$back_test_base_dates<-backTestBaseDates;out$back_test_predicted<-predictedValues
    out$back_test_actuals<-actualValues
    out$back_test_monthCounters<-theMonthCounters
    out
    
}