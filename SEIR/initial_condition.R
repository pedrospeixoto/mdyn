#Create vector with initial condition
initial_condition <- function(obs,day,par){
  init <- vector()
  tmp <- obs %>% filter(date == ymd(day))
  tmp <- tmp[match(x = par$names,table = tmp$city),]
  init[1:par$sites] <- tmp$new_infected_cor #E
  init[(par$sites + 1):(2*par$sites)] <- tmp$infected #I
  init[(2*par$sites + 1):(3*par$sites)] <- tmp$infected #Is
  init[(3*par$sites + 1):(4*par$sites)] <- tmp$recovered #R
  init[(4*par$sites + 1):(5*par$sites)] <- tmp$deaths_corrected #D
  init[(5*par$sites + 1):(6*par$sites)] <- tmp$confirmed_corrected #prevalence
  
  return(init)
}