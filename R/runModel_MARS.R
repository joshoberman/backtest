source('makeMARS.R');source('backTestMARS.R')
runModel<-function(input){
  output<-backTestMARS(input$employment,input$empl_monthCounter,input$base_date,input$outcome)
  output$predictors<-NA;output$predictor_id<-input$predictor_id
  output
}

save(runModel,makeMARSandPredict,backTestMARS,file="MARS.RData")