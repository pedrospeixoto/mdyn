#Initial condition corrected
initial_condition_corrected <- function(init,init1f,init2f,parK){
  initK <- init
  
  #Correct Infected
  initK[(parK$sites + 1):(2*parK$sites)] <- (parK$upI+1)*initK[(parK$sites + 1):(2*parK$sites)]
  
  #Correct recovered
  initK[(3*parK$sites + 1):(4*parK$sites)] <- (parK$upI+1)*initK[(3*parK$sites + 1):(4*parK$sites)]
  
  #Exposed
  initK[1:parK$sites] <- (parK$upI+1)*(1/(parK$gammaI))*(init2f[1:parK$sites] + (parK$nuI+parK$gammaS-1)*init1f[1:parK$sites]) #Correct E
  initK[1:parK$sites] <- ifelse(initK[1:parK$sites] < 0,0,initK[1:parK$sites])
  
  return(initK)
}

