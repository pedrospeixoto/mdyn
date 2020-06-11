#Initial condition corrected
initial_condition_corrected <- function(init,init1f,init2f,parK){
  initK <- init
  
  #Correct Infected
  initK[(par$sites + 1):(2*par$sites)] <- (parK$upI+1)*initK[(par$sites + 1):(2*par$sites)]
  
  #Correct recovered
  initK[(3*par$sites + 1):(4*par$sites)] <- (parK$upI+1)*initK[(3*par$sites + 1):(4*par$sites)]
  
  #Exposed
  initK[1:par$sites] <- par$lift*(1/(gammaS*gammaI))*(init2f[1:par$sites] - init1f[1:par$sites] + (nuI+gammaS)*init1f[1:par$sites]) #Correct E
  initK[1:par$sites] <- ifelse(initK[1:par$sites] < 0,0,initK[1:par$sites])
  
  
}