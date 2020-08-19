##############################
#####Simulation Paper SEIR####
##############################

library(lubridate)

#Dates to simulate
t0 <- seq.Date(from = ymd("2020-04-01"),to = ymd("2020-08-18"),by = 7)

for(t in as.character(t0)){
  cat("\n")
  cat(t)
  cat("\n")
  
  #
}