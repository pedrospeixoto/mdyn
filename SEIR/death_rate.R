#Calculate death rate for DRSs and cities with 5+ deaths
death_rate <- function(teste_D,teste_I,obs,day,drs,par){
  delta <- vector()
  
  for(d in unique(drs$DRS)){ #For each DRS
    tmpD <- teste_D %>% filter(DRS == d) #Death in DRS d
    tmpI <- teste_I %>% filter(DRS == d) #Confirmed in DRS d
    dr <- tmpD$D_drs[tmpD$date == day]/tmpI$I_drs[tmpI$date == day] #Death rate in DRS d in day
    delta[match(drs$Municipio[drs$DRS == d],par$names)] <- dr #Attribute death rate of DRS to each city
  }

  #Calculate death rate for each city with 50+ deaths
  C_50 <- obs %>% filter(date == day & deaths_corrected >= 50) %>% unique() #Data of cities with 50+ deaths in day
  C_50 <- C_50$city #Get city name
  for(c in C_50){ #For each city with 5+ deaths by day
    tmp <- obs %>% filter(city == c & date == day) #Get data of city is last day of validation
    dr <- tmp$deaths_corrected/tmp$confirmed_corrected #Death rate
    delta[match(c,par$names)] <- dr #Attribute death rate
  }
  
  return(delta)
}

