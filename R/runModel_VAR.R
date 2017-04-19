source('backTestVAR.R');source('makeVAR.R');source('weightProjections.R')
runModel<-function(input){
  library(data.table)
  formatted<-data.frame(var_name=character(),monthCounter=numeric(),var_value=numeric())
  
  #input$empl_monthCounter<-subset(input$empl_monthCounter,input$empl_monthCounter<=input$baseDate)
  
  nPredictors<-length(input$predictors)
  
  foo<-data.frame(var_name=input$outcome,monthCounter=(input$empl_monthCounter),var_value=(input$employment))
  formatted<-rbind(formatted,foo)
  
  for(i in 1:nPredictors){
    foo<-data.frame(var_name=input$predictors[i],monthCounter=(input$predictor_monthCounters[[i]]), 
                    var_value=(input$predictor_values[[i]]))
    formatted<-rbind(formatted,foo)
  }
  
  formatted<-data.table(formatted)
  formatted<-data.table::dcast(formatted,monthCounter~var_name)
  formatted<-na.omit(formatted)
  
  output<-backTestVAR(formatted,input$base_date)
  output$predictor_id<-input$predictor_id
  output
}

save(makeVARandPredict,backTestVAR,runModel,file="VAR.RData")