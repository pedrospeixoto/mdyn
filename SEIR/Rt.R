#Calculate Rt
Rt <- function(parK,day,t){
  
  #Mean time
  meanTime <- parK$pS*parK$Ts + (1-parK$pS)*parK$Ti
  
  ##Calculate exposed
  parK$obs_DRS$E[[as.character(t)]] <- parK$lift*(1/(parK$gammaS*parK$gammaI))*(parK$obs_DRS$E[[as.character(t+2)]]+
                                                                                  (parK$nuI + parK$gammaS - 1)*parK$obs_DRS$E[[as.character(t+1)]])
  parK$obs_DRS$E[[as.character(t)]] <- ifelse(parK$obs_DRS$E[[as.character(t)]] < 0,0,parK$obs_DRS$E[[as.character(t)]])
  
  #Calculate Susceptible
  Sobs <- drs$N - parK$obs_DRS$E[[as.character(t)]] - (1+parK$upI)*parK$obs_DRS$Is[[as.character(t)]] - (parK$upI+1)*parK$obs_DRS$R[[as.character(t)]] - 
    parK$obs_DRS$D[[as.character(t)]]
  
  #Calculate deaths
  Dobs <- parK$obs$D[[as.character(t)]]
  
  #Infected
  In <- parK$beta*Sobs/(par$pop - Dobs)*(1 + parK$s*((par$mob[[as.character(day)]] - diag(diag(par$mob[[as.character(day)]]))) %*%
                                        cbind((1 + parK$upI) * parK$obs$Is[[as.character(t)]]))/(1 + (1 + parK$upI) * parK$obs$Is[[as.character(t)]]))
  
  #Rt
  Rt <- In * meanTime
  
  return(list("Rt" = as.vector(Rt),"menaTi" = meanTime))
}