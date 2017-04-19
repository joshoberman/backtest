runModel<-function(input){
  
  theSeries<-as.numeric(input$employment)
  the_monthCounters<-input$empl_monthCounter
  theSeries.ts<-ts(theSeries,frequency = 12)
  st<-stl(theSeries.ts,s.window="periodic")
  mn_of_last_year<-round(mean(tail(round(st$time.series[,"trend"]),12)))
  season<-round(tail(st$time.series[,"seasonal"],12))
  thePreds<-rep(mn_of_last_year,120)
  thePreds<-thePreds+season
  monthsAhead<-seq((input$base_date)+1,(input$base_date)+120)
  #diagnostics<-list(AIC=ets_model$aic,rmse=acc[,"RMSE"],mae=acc[,"MAE"],mape=acc[,"MAPE"])
  output<-list(model_name="Average",predicted_values_unweighted=thePreds,
               predicted_monthCounters=monthsAhead,outcome=as.character(input$outcome))
  output$diagnostics<-NA;output$back_test_base_dates<-NA;output$back_test_predicted<-NA;output$back_test_actual<-NA
  output$back_test_monthCounters<-NA
  output$predictors<-NA;output$predictor_id<-input$predictor_id
  output
  
}

save(runModel,file="Average.RData")