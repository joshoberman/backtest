source('backTestARIMA.R');source('weightProjections.R')
runModel<-function(input){
  library(forecast)
  theSeries<-as.integer(input$employment)
  themonthCounters<-as.integer(input$empl_monthCounter)
  
  theSeries.ts<-ts(theSeries,frequency = 12)
  arima_model<-auto.arima(theSeries.ts,D = 1,seasonal=TRUE)
  arima_preds<-forecast(arima_model,h=120)
  backScore<-backTestARIMA(theSeries.ts,arima_model,"111",themonthCounters,input$base_date)
  arima_preds<-summary(arima_preds)
  thePreds<-as.numeric(unlist(arima_preds$`Point Forecast`))
  thePreds<-round(thePreds)
  acc<-accuracy(arima_model)
  monthsAhead<-seq((input$base_date)+1,(input$base_date)+120)
  #diagnostics<-list(AIC=arima_model$aic,rmse=acc[,"RMSE"],mae=acc[,"MAE"],mape=acc[,"MAPE"])
  output<-list(model_name="Auto-ARIMA",predicted_values_unweighted=thePreds,
               predicted_monthCounters=monthsAhead,outcome_naics=as.character(input$outcome))
  output<-c(output,backScore)
  output$predictors<-NA;output$predictor_id<-input$predictor_id
  
  output
}

save(runModel,backTestARIMA,file="Auto-ARIMA.RData")