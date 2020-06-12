#Observed data by city and DRS around the week of validation
obs_around_init <- function(obs,obs_drs,par,day_init,start,end){
  r <- list()
  r_DRS <- list()
  for(t in start:end){
    tmp <- obs %>% filter(date == ymd(day_init) + t - 1)
    tmp <- tmp[match(x = par$names,table = tmp$city),]
    tmp1 <- obs_drs %>% filter(date == ymd(day_init) + t - 1)
    tmp1 <- tmp1[match(x = par$names,table = tmp1$city),]
    r$E[[as.character(t)]] <- tmp$infected #E
    r$I[[as.character(t)]] <- tmp$infected #I
    r$Is[[as.character(t)]] <- tmp$infected #Is
    r$R[[as.character(t)]] <- tmp$recovered #R
    r$D[[as.character(t)]] <- tmp$deaths_corrected #D
    r_DRS$E[[as.character(t)]] <- tmp1$infected #E
    r_DRS$I[[as.character(t)]] <- tmp1$infected #I
    r_DRS$Is[[as.character(t)]] <- tmp1$infected #Is
    r_DRS$R[[as.character(t)]] <- tmp1$recovered #R
    r_DRS$D[[as.character(t)]] <- tmp1$deaths_corrected #D
  }
  
  return(list(r,r_DRS))
}