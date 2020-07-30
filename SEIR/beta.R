#Calculate beta
beta <- function(parK,t,lambda,drs,day,obs){
  
  #####Beta by DRS#####
  
  #Calculate exposed
  parK$obs_DRS$E[[as.character(t)]] <- parK$upI*(1/(parK$gammaI))*(parK$obs_DRS$E[[as.character(t+1)]] +
                                                                                  (parK$nuI + parK$gammaS - 1)*parK$obs_DRS$E[[as.character(t)]])
  parK$obs_DRS$E[[as.character(t)]] <- ifelse(parK$obs_DRS$E[[as.character(t)]] < 0,0,parK$obs_DRS$E[[as.character(t)]])
  parK$obs$E[[as.character(t)]] <- parK$upI*(1/(parK$gammaI))*(parK$obs$E[[as.character(t+1)]]+
                                                                              (parK$nuI + parK$gammaS - 1)*parK$obs$E[[as.character(t)]])
  parK$obs$E[[as.character(t)]] <- ifelse(parK$obs$E[[as.character(t)]] < 0,0,parK$obs$E[[as.character(t)]])
  
  #Calculate susceptibles
  Sobs <- parK$pop - parK$obs$E[[as.character(t)]] - (1+parK$upI)*parK$obs$Is[[as.character(t)]] - (parK$upI+1)*parK$obs$R[[as.character(t)]] - 
    parK$obs$D[[as.character(t)]]
  
  #Lambda Exposed
  lambdaE <- logP(lambda + parK$nuI + parK$gammaS) + logP(parK$upI*parK$obs_DRS$Is[[as.character(t+1)]]) -
    logP(parK$gammaI * (parK$obs_DRS$E[[as.character(t)]])) #Growth rate
  
  #Numerator
  num <- ((lambdaE + parK$gammaI) * parK$obs$E[[as.character(t)]] * (par$pop - parK$obs$D[[as.character(t)]]))
  
  #Denominator
  den <- Sobs * (parK$s*((parK$mob[[as.character(ymd(day) + t - 1)]] - diag(diag(parK$mob[[as.character(ymd(day) + t - 1)]]))) %*% 
                           cbind((1+parK$upI)*parK$obs$Is[[as.character(t)]])) + 1 + (parK$upI+1)*parK$obs$Is[[as.character(t)]]) #Denominator
  
  #Beta
  beta <- as.vector(num/den) #Beta
  
  ######Cities with 1000+ cases######
  
  #Lambda exposed
  lambdaE <- logP(lambda + parK$nuI + parK$gammaS) + logP(parK$upI*parK$obs$Is[[as.character(t+1)]]) -
    logP(parK$gammaI * (parK$obs$E[[as.character(t)]])) #Growth rate
  
  #Numerator
  num <- ((lambdaE + parK$gammaI) * parK$obs$E[[as.character(t)]] * (par$pop - parK$obs$D[[as.character(t)]])) 

  #Denominator
  den <- Sobs * (parK$s*((parK$mob[[as.character(ymd(day) + t - 1)]] - diag(diag(parK$mob[[as.character(ymd(day) + t - 1)]]))) %*% 
                           cbind((1+parK$upI)*parK$obs$Is[[as.character(t)]])) + 1 + (parK$upI+1)*parK$obs$Is[[as.character(t)]]) 
  
  #Beta
  b <- as.vector(num/den)
  
  #Only cities with 1000+ cases
  c <- obs %>% filter(date == ymd(day) & confirmed_corrected > 500) 
  beta[par$names %in% c$city] <- b[par$names %in% c$city]
  
  #beta[beta <= 0] <- min(beta[beta > 0])
  
  return(beta)
}