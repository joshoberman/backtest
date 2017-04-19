subsetToBaseDate<-function(input){
  binded<-data.frame(employment=input$employment,monthCounter=input$empl_monthCounter)
  binded<-binded[binded$monthCounter<=input$base_date,]
  input$employment<-binded$employment;input$empl_monthCounter<-binded$monthCounter
  input
}

save(subsetToBaseDate,file="subsetToBaseDate.RData")