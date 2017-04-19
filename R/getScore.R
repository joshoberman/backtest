getScore<-function(backtested_model){
  
  nrmses<-backtested_model$diagnostics$nrmse 
  
  score_A<-sd(nrmses)*2 + mean(nrmses)
  
  normedScore<-1-(score_A)
  
  finalScore<-0.05 * 1 + 0.1 * 1 + 0.85 * normedScore
  
  
  backtested_model$diagnostics$errorScore<-normedScore
  backtested_model$diagnostics$finalScore<-finalScore
  backtested_model
}

save(getScore,file="getScore.RData")