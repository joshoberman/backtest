runModel<-function(input){
  library(forecast)
  theSeries<-as.numeric(input$employment)
  the_monthCounters<-input$empl_monthCounter
  theSeries.ts<-ts(theSeries,frequency = 12)
  ets_model<-ets(theSeries.ts,model=c("ZZA"))
  backTested<-backTestETS(theSeries.ts,ets_model,the_monthCounters,input$base_date)
  ets_preds<-forecast(ets_model,h=120)
  ets_preds<-summary(ets_preds)
  thePreds<-as.numeric(unlist(ets_preds$`Point Forecast`))
  thePreds<-round(thePreds)
  #acc<-accuracy(ets_model)
  monthsAhead<-seq((input$base_date)+1,(input$base_date)+120)
  #diagnostics<-list(AIC=ets_model$aic,rmse=acc[,"RMSE"],mae=acc[,"MAE"],mape=acc[,"MAPE"])
  output<-list(model_name="ETS",predicted_values_unweighted=thePreds,
               predicted_monthCounters=monthsAhead,outcome_naics=as.character(input$outcome))

  output<-c(output,backTested)

  output$predictors<-NA;output$predictor_id<-input$predictor_id

  output
}

save(runModel, file="R/ETS.RData")
