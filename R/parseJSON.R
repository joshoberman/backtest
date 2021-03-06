parseJSON<-function(jsonInput){

  theInput <- jsonlite::fromJSON( jsonInput )
  theInput$model_name<-as.character(theInput$model_name)
  theInput$outcome<-as.character(theInput$outcome)
  theInput$predictor_monthCounters<-lapply(theInput$predictor_monthCounters,as.integer)
  theInput$predictor_id<-as.integer(theInput$predictor_id)
  #theInput$predictor_year<-lapply(theInput$predictor_monthCounters,monthCounterToYear)
  #theInput$predictor_quarter<-lapply(theInput$predictor_monthCounters,monthCounterToQuarter)
  theInput$predictor_values<-lapply(theInput$predictor_values,as.numeric)
  theInput$employment<-as.integer(theInput$employment)
  theInput$empl_monthCounter<-as.integer(theInput$empl_monthCounter)
  #theInput$empl_year<-monthCounterToYear(theInput$empl_monthCounter)
  #theEmployment.ts<-ts(theInput$employment,frequency = 12)
  theInput$predictors<-sapply(theInput$predictors,str_replace_all,fixed(" "),"")
  lastMnthCnt<-max(theInput$empl_monthCounter)
  names(theInput$predictor_monthCounters)<-sapply(names(theInput$predictor_monthCounters),str_replace_all,fixed(" "), "")
  names(theInput$predictor_values)<-sapply(names(theInput$predictor_values),str_replace_all,fixed(" "), "")
  theInput$base_date<-as.integer(theInput$base_date)
  theInput$long_term_growth_rate<-as.numeric(theInput$long_term_growth_rate)
  nPredictors<<-length(theInput$predictors)
  #cache these values
  monthsAhead<<-seq(lastMnthCnt+1,lastMnthCnt+120,1)

  theInput
}
