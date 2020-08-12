#Calculate lift of the death rate
lift_death <- function(obs,end_validate,par){
  tmp <- obs %>% filter(date >= ymd(end_validate) - 30) #Only end_validate
  lift <- data.frame("city" = NA,"lift" = NA)
  for(c in unique(tmp$city)){
    tmp1 <- tmp %>% filter(city == c)
    deaths <- tmp1$deaths_corrected[tmp1$date == ymd(end_validate)] - tmp1$deaths_corrected[tmp1$date == ymd(end_validate) - 30]
    cases <- tmp1$confirmed_corrected[tmp1$date == ymd(end_validate)] - tmp1$confirmed_corrected[tmp1$date == ymd(end_validate) - 30]
    lift <- rbind.data.frame(lift,data.frame("city" = c,"lift" = deaths/cases))
  }
  rate <- sum(tmp$new_death_cor)/sum(tmp$new_infected_cor) #death rate state
  lift$lift <- lift$lift/rate
  lift <- lift[match(par$names,lift$city),] #order cities
  lift$lift[is.na(lift$lift)] <- 1 #fill NA with 1
  lift$lift[lift$lift == 0] <- 1
  lift$lift[lift$lift < 0.9] <- 0.9 #Truncate
  lift$lift[lift$lift > 1.1] <- 1.1 #Truncate
  
  return(lift$lift)
}
