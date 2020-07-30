#Calculate Rt
Rt <- function(parK,day,t){
  
  #Mean time
  meanTime <- parK$pS*parK$Ts + (1-parK$pS)*parK$Ti
  Rt <- list()
  
  for(t in 1:7){
    
  ##Calculate exposed
  parK$obs$E[[as.character(t)]] <- parK$upI*(1/(parK$gammaI))*(parK$obs$iso[[as.character(t+1)]]*parK$obs$E[[as.character(t+1)]]+
                                                                 (parK$nuI + parK$gammaS - 1)*parK$obs$iso[[as.character(t)]]*parK$obs$E[[as.character(t)]])
  parK$obs$E[[as.character(t)]] <- ifelse(parK$obs$E[[as.character(t)]] < 0,0,parK$obs$E[[as.character(t)]])
  
  #Calculate susceptibles
  Sobs <- parK$obs$iso[[as.character(t)]]*(parK$pop - parK$obs$Is[[as.character(t)]] - parK$obs$D[[as.character(t)]] - parK$obs$R[[as.character(t)]]) - 
    parK$obs$iso[[as.character(t)]]*parK$obs$E[[as.character(t)]] - parK$obs$iso[[as.character(t)]]*parK$upI*parK$obs$Is[[as.character(t)]] -
    parK$obs$iso[[as.character(t)]]*parK$upI*parK$obs$R[[as.character(t)]]
  
  #Calculate deaths
  Dobs <- parK$obs$D[[as.character(t)]]
  
  #Infected
  In <- parK$betaMedian*Sobs/(par$pop - Dobs)*(1 + parK$s*((par$mob[[as.character(ymd(day) - 7 + t)]] - 
                                                              diag(diag(par$mob[[as.character(ymd(day) - 7 + t)]]))) %*%
                                                             cbind((1+parK$obs$iso[[as.character(t)]]*parK$upI)*parK$obs$Is[[as.character(t)]])) + 1 + 
                                                 (parK$obs$iso[[as.character(t)]]*parK$upI+1)*parK$obs$Is[[as.character(t)]])
  
  #Rt
  Rt[[t]] <- In * meanTime
  }
  
  Rt <- data.frame(do.call(rbind,Rt))
  Rt <- apply(Rt,2,mean)
  
  return(list("Rt" = as.vector(Rt),"meanTi" = meanTime))
}