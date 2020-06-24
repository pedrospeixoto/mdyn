#Sample parameters for a model
sample_parameters <- function(par,day_validate,drs){
  
  parK <- list()
  
  #Parameters
  parK$day <- day_validate #Days of validation
  parK$val <- TRUE #Is validation
  parK$mob <- par$mob #Mobility matrix
  parK$pop <- par$pop #Population
  parK$Te <- sample(x = par$Te,size = 1) #Te
  parK$Ti <- sample(x = par$Ti,size = 1) #Ti
  parK$Ts <- sample(x = par$Ts,size = 1) #Ts
  parK$Tsr <- sample(x = par$Tsr,size = 1) #Tsr
  parK$Td <- sample(x = par$Td,size = 1) #Td
  parK$pS <- sample(x = par$pS,size = 1) #Td
  parK$delta <- par$delta #delta
  parK$sites <- par$sites #Number of sites
  parK$s <- sample(x = par$s,size = 1) #s
  parK$lift <- par$lift#vector() #lift
  #for(d in par$lift$DRS)
  #  parK$lift[match(drs$Municipio[drs$Regiao == d],par$names)] <- par$lift$lift[par$lift$DRS == d]
  #parK$lift[269] <- par$lift$lift[par$lift$DRS == "Grande São Paulo"]
  parK$upI <- ((par$lift)^(1))*(1-parK$pS)/parK$pS #Number of missed cases for each one in statistics
  
  #Parameters
  parK$gammaI <- 1/parK$Te #Rate from Exposed to Infected
  parK$gammaS <- parK$pS/parK$Ts #Rate from Infected to Statistics
  parK$nuI <- (1-parK$pS)/parK$Ti #Rate from Infected to Recovered
  parK$nuS <- (1-parK$delta)/parK$Tsr #Rate from Statistics to Recovered
  parK$deltaRate <- parK$delta/parK$Td #Rate from Statistics to Death
  parK$obs <- par$obs
  parK$obs_DRS <- par$obs_DRS
  parK$names <- par$names
  
  return(parK)
}

