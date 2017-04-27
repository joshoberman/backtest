makeMARSandPredict<-function(employment_series,monthCounters,nAhead=24){
  #Subset to remove month counter
  theEmployment.ts<-ts(employment_series,frequency=12)
  #do season + trend extraction using stl() function
  st<-stl(theEmployment.ts,s.window = "periodic")
  trnd<-st$time.series[,"trend"]
  theSeries<-as.data.frame(cbind(trnd=as.numeric(trnd),time = monthCounters))
  season<-tail(st$time.series[,"seasonal"],12)
  random<-tail(st$time.series[,"remainder"])
  #here's our MARS model...time predicting the trend
  modMARS<-earth(trnd~.,theSeries)

  #gerneate values for the monthCounters moving forward
  nextTimePoints<-max(monthCounters)+1
  j=2
  while(j<=nAhead){
    nextTimePoints[j]<-nextTimePoints[j-1]+1
    #nextPointLongTerm[j]<-nextPointLongTerm[j-1]+theNAICS.growth_rate
    j=j+1
  }

  #predict
  predsMARS<-predict(modMARS,newdata=nextTimePoints)
  predsMARS<-predsMARS+season
  diagnostics<-list(r_squared=modMARS$rsq,generalized_rsq=modMARS$grsq)
  output<-list(model_name="MARS",diagnostics=diagnostics)
  output$predicted_values_unweighted<-as.integer(round(unlist(predsMARS)));
  output$predicted_monthCounters<-nextTimePoints;
  output
}

