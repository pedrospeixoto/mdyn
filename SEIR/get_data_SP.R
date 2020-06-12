#Get notification data state of SP from SEADE
get_data_SP <- function(){
  file <- gzcon(url("https://github.com/seade-R/dados-covid-sp/raw/master/data/dados_covid_sp.csv")) #Data path
  txt <- readLines(file) #Read lines
  obs <- read.csv(textConnection(txt),sep = ";") #Get data
  names(obs)[c(5,1,6,10)] <- c("date","city","last_available_confirmed","last_available_deaths")
  obs <- obs %>% 
    select(city,date,last_available_confirmed,last_available_deaths) %>% na.omit() %>% filter(city != "Ignorado") #Only confirmed cases and death
  obs$city <- toupper(gsub(pattern = "'",replacement = "",x = obs$city)) #Correct names
  obs$city[obs$city == "ITAÓCA"] <- "ITAOCA" #Correct names
  obs$city[obs$city == "BIRITIBA MIRIM"] <- "BIRITIBA-MIRIM" #Correct names
  obs$date <- ymd(obs$date) #Date
  for(d in unique(as.character(obs$date))){ #Add zero to cities where and when no cases/deaths were confirmed
    tmp <- par$names[!(par$names %in% obs$city[obs$date == d])]
    tmp <- data.frame("city" = tmp,"date" = ymd(d),"last_available_confirmed" = 0,"last_available_deaths" = 0)
    obs <- rbind.data.frame(obs,tmp)
  }
  obs$key <- paste(obs$city,obs$date) #Set key
  obs_new <- obs[1,] #Create new dataframe
  obs_new$recovered <- 0 #Initialize new variables
  obs_new$infected <- 0 #Initialize new variables
  obs_new$new_infected <- 0 #Initialize new variables
  obs_new$new_death <- 0 #Initialize new variables
  obs_new$new_infected_cor <- 0 #Initialize new variables
  obs_new$new_death_cor <- 0 #Initialize new variables
  obs_new$confirmed_corrected <- 0 #Initialize new variables
  obs_new$deaths_corrected <- 0 #Initialize new variables
  for(c in unique(obs$city)){ #For each city
    tmp <- data.frame(obs %>% filter(city == c)) #Get data from the city
    tmp <- tmp[order(tmp$date),] #Order by date
    tmp$recovered <- 0 #Initialize new variables
    tmp$infected <- 0 #Initialize new variables
    tmp$new_infected <- c(tmp$last_available_confirmed[1],diff(tmp$last_available_confirmed)) #Incidence of infection
    tmp$new_death <- c(tmp$last_available_deaths[1],diff(tmp$last_available_deaths)) #Incidence death
    tmp$new_infected_cor <- 0
    tmp$new_infected_cor[1:3] <- tmp$new_infected[1:3] #Initialize new variables
    tmp$new_death_cor <- 0
    tmp$new_death_cor[1:3] <- tmp$new_death[1:3] #Initialize new variables
    tmp$confirmed_corrected <- 0 #Initialize new variables
    tmp$deaths_corrected <- 0 #Initialize new variables
    for(i in 4:(nrow(tmp)-3)){
      tmp$new_infected_cor[i] <- mean(tmp$new_infected[(i-3):(i+3)]) #New cases corrected
      tmp$new_death_cor[i] <- mean(tmp$new_death[(i-3):(i+3)]) #New deaths corrected
      tmp$confirmed_corrected <- cumsum(tmp$new_infected_cor) #Confirmed cases corrected
      tmp$deaths_corrected <- cumsum(tmp$new_death_cor) #Confirmed deaths corrected
      if(i > 15){ #Estimating recovered
        tmp$recovered[i] <- tmp$recovered[i-1] + tmp$new_infected_cor[i-15] #New corrected confirmed cases 28 days ago are recovered
        tmp$infected[i] <- tmp$confirmed_corrected[i] - tmp$recovered[i] #Delete recovered from infected
      }
      else{ #Correct for first 28 days
        tmp$recovered[i] <- 0
        tmp$infected[i] <- tmp$confirmed_corrected[i]
      }
    }
    obs_new <- rbind.data.frame(obs_new,tmp[-c(nrow(tmp)-1,nrow(tmp)-2,nrow(tmp)),]) #Bind
  }
  obs <- obs_new[-1,] #Erase first extra row
  obs$recovered <- obs$recovered - obs$deaths_corrected #Delete deaths from recovered
  obs[obs < 0] <- 0 #Correct
  
  return(obs)
}

