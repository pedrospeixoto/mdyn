#Calculate death rate for DRSs and cities with 5+ deaths
death_rate <- function(teste_D,teste_I,obs,day,drs,par){
  delta <- vector()
  
  for(d in unique(drs$DRS)){ #For each DRS
    tmpD <- teste_D %>% filter(DRS == d) #Death in DRS d
    tmpI <- teste_I %>% filter(DRS == d) #Confirmed in DRS d
    dr <- (tmpD$D_drs[tmpD$date == day] - tmpD$D_drs[tmpD$date == ymd(day)-7])/(tmpI$I_drs[tmpI$date == day] - tmpI$I_drs[tmpI$date == ymd(day)-7])#DR around day
    delta[match(drs$Municipio[drs$DRS == d],par$names)] <- dr #Attribute death rate of DRS to each city
  }

  #Calculate death rate for each city with 10+ deaths
  C_5 <- obs %>% filter(date == day & deaths_corrected >= 5) %>% unique() #Data of cities with 5+ deaths in day
  C_5 <- C_5$city #Get city name
  for(c in C_5){ #For each city with 5+ deaths by day
    tmp <- obs %>% filter(city == c & date >= ymd(day) - 7) #Get data of city is last day of validation
    dr <- (tmp$deaths_corrected[tmp$date == day] - tmp$deaths_corrected[tmp$date == ymd(day)-7])/(tmp$confirmed_corrected[tmp$date == day] - tmp$confirmed_corrected[tmp$date == ymd(day)-7])  #Death rate
    if(dr > 0)
      delta[match(c,par$names)] <- dr #Attribute death rate
  }
  
  return(delta)
}

