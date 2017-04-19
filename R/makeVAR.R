##Function to make VAR model with a given number of predictors and an n-Ahead projection
makeVARandPredict<-function(x,nAhead=24){
  #Subset to remove month counter  
  x.noMnthCnt<-subset(x,select=-c(monthCounter))
  x.noMnthCnt.ts<-ts(x.noMnthCnt,frequency=12)
  #year on year difference
  x.noMnthCnt.ts.diff<-diff(x.noMnthCnt.ts,lag=12)
  #make a model w/ up to lag 6 coeffs
  model<-VAR(x.noMnthCnt.ts.diff,p=6)
  #get summary model statistics
  summMod<-summary(model)
  rsq<-summMod$varresult[[1]]$r.squared
  adjusted_rsq<-summMod$varresult[[1]]$adj.r.squared
  aic<-AIC(model)
  #make forecasts of differenced values then de-difference
  preds<-predict(model,n.ahead=nAhead)
  fcstVals<-preds$fcst[[1]][,1]
  fcstVals<-round(fcstVals)
  predicted<-integer(length(fcstVals))
  lastN<-tail(x.noMnthCnt.ts[,1],12)
  for(j in 1:length(fcstVals)){
    #for the first twelve forecast values, add to previous years values
    if(j<=12){
      p<-fcstVals[j]
      prev<-lastN[j]
      predicted[j]<-prev+p
    }
    else{
      # for the rest, add to the 12 values from previous forecasted year
      p<-fcstVals[j]
      prev<-predicted[j-12]
      predicted[j]<-prev+p
      }
    }
    # generate monthCounters moving forward
    timePointsForward<-seq(max(x$monthCounter)+1,max(x$monthCounter)+nAhead)
    diagnostics<-list(rsq=rsq,adjusted_rsq=adjusted_rsq,AIC=aic)
    #gather values in to output
    all<-list(model_name="VAR",diagnostics=diagnostics,predicted_values_unweighted = predicted, predicted_monthCounters = timePointsForward)
    all
}