prepareJSON<-function(outputList,keep_data=F){

  #order everything and do type checking
  if(keep_data==T){
    output<-list(model_name=as.character(outputList$model_name),diagnostics=outputList$diagnostics,
                 predicted_values_unweighted=as.integer(outputList$predicted_values_unweighted),
                 predicted_values_weighted=as.integer(outputList$predicted_values_weighted),
                 predicted_monthCounters=as.integer(outputList$predicted_monthCounters),
                 back_test_base_dates=as.integer(outputList$back_test_base_dates),
                 back_test_predicted=(outputList$back_test_predicted),
                 back_test_actual=(outputList$back_test_actual),
                 back_test_monthCounters=(outputList$back_test_monthCounters),
                 outcome=as.character(outputList$outcome), predictors=outputList$predictors,
                 predictor_id=as.integer(outputList$predictor_id))
  }

  else if(keep_data==F){
    output<-list(model_name=as.character(outputList$model_name),diagnostics=outputList$diagnostics,
                 predicted_values_unweighted=as.integer(outputList$predicted_values_unweighted),
                 predicted_values_weighted=as.integer(outputList$predicted_values_weighted),
                 predicted_monthCounters=as.integer(outputList$predicted_monthCounters[1]),
                 outcome=as.character(outputList$outcome),
                 predictors=outputList$predictors,
                 predictor_id=as.integer(outputList$predictor_id))
  }

  output<-jsonlite::toJSON(output,auto_unbox = T)

  output

}

