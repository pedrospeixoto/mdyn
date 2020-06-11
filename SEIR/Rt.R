#Calculate Rt
Rt <- function(parK,day,t){
  
  #Mean time
  meanTime <- parK$pS*parK$Ts + (1-parK$pS)*parK$Ti
  
  ##Calculate exposed
  parK$obs$E[[as.character(t)]] <- parK$upI*(1/(parK$gammaI))*(parK$obs$E[[as.character(t+2)]]+
                                                                                  (parK$nuI + parK$gammaS - 1)*parK$obs$E[[as.character(t+1)]])
  parK$obs$E[[as.character(t)]] <- ifelse(parK$obs$E[[as.character(t)]] < 0,0,parK$obs$E[[as.character(t)]])
  
  #Calculate Susceptible
  Sobs <- par$pop - parK$obs$E[[as.character(t)]] - (1+parK$upI)*parK$obs$Is[[as.character(t)]] - (parK$upI+1)*parK$obs$R[[as.character(t)]] - 
    parK$obs$D[[as.character(t)]]
  
  #Calculate deaths
  Dobs <- parK$obs$D[[as.character(t)]]
  
  #Infected
  In <- parK$beta*Sobs/(par$pop - Dobs)*(1 + parK$s*((par$mob[[as.character(day)]] - diag(diag(par$mob[[as.character(day)]]))) %*%
                                        cbind((1 + parK$upI) * parK$obs$Is[[as.character(t)]]))/(1 + (1 + parK$upI) * parK$obs$Is[[as.character(t)]]))
  
  #Rt
  Rt <- In * meanTime
  
  return(list("Rt" = as.vector(Rt),"meanTi" = meanTime))
}