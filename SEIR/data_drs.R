#Data by drs
data_drs <- function(obs,drs){
  obs_drs <- merge(obs,drs,by.x = "city",by.y = "Municipio") #Find DRS of each city
  obs_drs$key <- paste(obs_drs$DRS,obs_drs$date) #key
  obs_drs <- data.table(obs_drs) #data table
  obs_drs <- obs_drs[,last_available_confirmed := sum(last_available_confirmed),by = key] #Sum by DRS anda date
  obs_drs <- obs_drs[,last_available_deaths := sum(last_available_deaths),by = key] #Sum by DRS anda date
  obs_drs <- obs_drs[,recovered := sum(recovered),by = key] #Sum by DRS anda date
  obs_drs <- obs_drs[,infected := sum(infected),by = key] #Sum by DRS anda date
  obs_drs <- obs_drs[,new_infected := sum(new_infected),by = key] #Sum by DRS anda date
  obs_drs <- obs_drs[,new_death := sum(new_death),by = key] #Sum by DRS anda date
  obs_drs <- obs_drs[,new_infected_cor := sum(new_infected_cor),by = key] #Sum by DRS anda date
  obs_drs <- obs_drs[,new_death_cor := sum(new_death_cor),by = key] #Sum by DRS anda date
  obs_drs <- obs_drs[,confirmed_corrected := sum(confirmed_corrected),by = key] #Sum by DRS anda date
  obs_drs <- obs_drs[,deaths_corrected := sum(deaths_corrected),by = key] #Sum by DRS anda date
  
  return(obs_drs)
}