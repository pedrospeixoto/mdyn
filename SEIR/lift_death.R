#Calculate lift of the death rate
lift_death <- function(obs,end_validate,par){
  lift <- obs %>% filter(date == ymd(end_validate)) #Only end_validate
  lift <- lift[match(par$names,lift$city),] #order cities
  lift$rate <- lift$deaths_corrected/lift$confirmed_corrected #death rate cities
  rate <- sum(lift$deaths_corrected)/sum(lift$confirmed_corrected) #death rate state
  lift$lift <- lift$rate/rate #lift
  lift$lift[is.na(lift$lift)] <- 1 #fill NA with 1
  lift$lift[lift$lift < 1] <- 1
  lift$lift[lift$deaths_corrected < 10] <- 1 #if not enough data, fill with one
  
  return(lift$lift)
}
