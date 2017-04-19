weightProjections<-function(actual_employment,projections,growth_rate){

  growth_rate_sequence<-numeric()
  growth_rate_sequence[1]<-mean(tail(actual_employment,12))+(mean(tail(actual_employment,12))*growth_rate)
  j=2
  while(j<=120){
      growth_rate_sequence[j]<-growth_rate_sequence[j-1]+growth_rate_sequence[j-1]*growth_rate
      j=j+1
  }

  theWeightingSeq<-seq(0,1,length.out=length(projections))
  theWeightingSeq<-(2^theWeightingSeq*exp(theWeightingSeq))
  theWeightingSeq<-(theWeightingSeq-min(theWeightingSeq))/(max(theWeightingSeq)-min(theWeightingSeq))

  weighted_predictions<-(projections*(1-theWeightingSeq)+(growth_rate_sequence*theWeightingSeq))
  round(weighted_predictions)

}
