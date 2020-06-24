#####Model specification#####
#t = time
#Y = observed quantities at time t
#par = A named list of model parameters
derivatives <- function(t,Y,parK){
  
  #States at time t
  E <- Y[1:parK$sites] #Exposed
  I <- Y[(parK$sites + 1):(2*parK$sites)] #Infected
  Is <- Y[(2*parK$sites + 1):(3*parK$sites)] #Statistics
  R <- Y[(3*parK$sites + 1):(4*parK$sites)] #Recovered
  D <- Y[(4*parK$sites + 1):(5*parK$sites)] #Deaths
  S <- parK$pop - E - I - Is - R - D #Susceptibles
  
  #Parameters
  if(parK$val) #If in validation period, take mobility matrix of the day
    mob <- parK$mob[[as.character(parK$day[t])]] #Mobility pattern of day
  else{ #If not, take of the day if exists; otherwise take of the weekday
    if(as.character(parK$day[t]) %in% names(parK$mob))
      mob <- parK$mob[[as.character(parK$day[t])]]
    else
      mob <- parK$mob[[weekdays(parK$day[t])]]
  }
  N <- parK$pop #Population
  Te <- parK$Te #Time exposed
  Ti <- parK$Ti #Time infected before recovering
  Ts <- parK$Ts #Time infected until statistics
  Tsr <- parK$Tsr #Time in statistics until recovering
  Td <- parK$Td #Time in statistics until death
  pS <- parK$pS #Proportion in statistics
  gammaI <- parK$gammaI #Rate from Exposed to Infected
  gammaS <- parK$gammaS #Rate from Infected to Statistics
  nuI <- parK$nuI #Rate from Infected to Recovered
  nuS <- parK$nuI #Rate from Statistics to Recovered
  delta <- parK$deltaRate #Rate from Statistics to Death
  s <- parK$s #Intensity of mobility
  if(parK$val)
    beta <- parK$beta #[[t]] #beta
  else
    beta <- parK$betaMedian
  
  #Derivatives
  dY <- vector(length = 6*parK$sites) #Vector of derivatives
  dY[1:parK$sites] <- -gammaI*E + beta*(S/(N-D))*(s*((mob-diag(diag(mob))) %*% cbind(I)) + I) #E
  dY[(parK$sites + 1):(2*parK$sites)] <- gammaI*E - nuI*I - gammaS*I #I
  dY[(2*parK$sites + 1):(3*parK$sites)] <- -nuS*Is + gammaS*I - delta*Is #Is
  dY[(3*parK$sites + 1):(4*parK$sites)] <- nuI*I + nuS*Is #R
  dY[(4*parK$sites + 1):(5*parK$sites)] <- delta*Is #D
  dY[(5*parK$sites + 1):(6*parK$sites)] <- gammaS*I #Add new cases to total
  
  return(list(dY)) #Return
}