##############################
#####Simulation Paper SEIR####
##############################

library(lubridate)

sink("simulation_paper.txt",split = T)
#Dates to simulate
t0 <- seq.Date(from = ymd("2020-04-01"),to = ymd("2020-08-23"),by = 7)
t0 <- t0[-11] #Pulei dia 10/06
errors <- data.frame("t0" = NA,"Min" = NA,"MinDeath" = NA,"MinInfected" = NA)

for(t in as.character(t0)){
  cat("\n")
  cat(as.character(t))
  cat("\n")
  
  ##Parameters
  cores <- 8 #Number of cores to use in parallel computation
  pos <- t #What to add at the end of all output files
  seed <- as.numeric(ymd(t)) #Seed
  par <- list() #Candidate values of model parameters
  d_max <- ymd(t) + 6  #t0 + 6
  simulate_length <- as.numeric(ymd("2020-12-31") - ymd(t)) #Number of days to simulate
  
  #Set mobility matrix
  par$mob <- list()
  day <- seq.Date(from = min(ymd(t),ymd("2020-08-05"))-30,to = min(ymd(t),ymd("2020-08-05")),1)
  for(d in as.character(day)){
    par$mob[[as.character(d)]] <- as.matrix(read.csv(paste("/storage/inloco/data/mobility_br_2020/date0=",d,"/move_mat_SÃO PAULO_Municip.csv",sep = ""),
                                                     sep = " ",
                                                     header = F))[1:645,1:645]
  }
  for(d in as.character(day)){
    w <- weekdays(ymd(d))
    if(is.null(par$mob[[as.character(w)]]))
      par$mob[[as.character(w)]] <- as.matrix(read.csv(paste("/storage/inloco/data/mobility_br_2020/date0=",d,"/move_mat_SÃO PAULO_Municip.csv",sep = ""),
                                                       sep = " ",
                                                       header = F))[1:645,1:645]
    else
      par$mob[[as.character(w)]] <- par$mob[[as.character(w)]] + as.matrix(read.csv(paste("/storage/inloco/data/mobility_br_2020/date0=",d,
                                                                                          "/move_mat_SÃO PAULO_Municip.csv",sep = ""),sep = " ",
                                                                                    header = F))[1:645,1:645]
  }
  par$names <- as.vector(read.table("mdyn/SEIR/dados/move_mat_SÃO PAULO_Municip_reg_names.txt",sep = ";")[1:645,1]) #Sites name
  par$sites <- length(par$names) #Number of sites
  
  #Population
  par$pop <- read.csv("mdyn/SEIR/dados/population_sp_mun.csv",sep = ";")
  par$pop$municipio <- gsub(pattern = "'",replacement = "",x = par$pop$municipio)
  par$pop <- par$pop[match(x = par$names,table = toupper(par$pop$municipio)),]
  par$pop <- as.vector(par$pop$populacao_estimada)
  par$names[par$names == "BIRITIBA MIRIM"] <- "BIRITIBA-MIRIM"
  for(i in 1:length(par$mob)){
    diag(par$mob[[i]]) <- par$pop - colSums(par$mob[[i]] - diag(diag(par$mob[[i]])))
    for(j in 1:ncol(par$mob[[i]]))
      par$mob[[i]][,j] <- par$mob[[i]][,j]/par$pop[j]
  }
  for(w in unique(weekdays(day)))
    par$mob[[as.character(w)]] <- 0.25*par$mob[[as.character(w)]]
  for(d in as.character(seq.Date(from = ymd(t)-30,to = d_max,1)))
    par$mob[[as.character(ymd(d))]] <- par$mob[[as.character(weekdays(ymd(d)))]]
  
  #Cadidate parameters
  par$pS <- 1/c(5:10,15,20,30,40,50,100) 
  par$Te <- c(3:6) 
  par$Ti <- c(5:21)
  par$Ts <- 7:21 
  par$Tsr <- 7:21 
  par$Td <- c(7:28)
  par$s <- c(0.25,0.5,1,1.5,2,2.5,3)
  
  #Calculate error
  sample_size <- 2500
  max_models <- 2500
  source("mdyn/SEIR/SEIR_COVID19_get_error.R")
  e <- get_error_SEIR_covid(cores,par,pos,seed+1,sample_size,simulate_length,d_max,max_models,0.1,0.1)
  
  error_I <- 1.1*e$MinInfected
  error_D <- 1.1*e$MinDeath
  errors <- na.omit(rbind.data.frame(errors,data.frame("t0" = t,"Min" = e$Min,"MinDeath" = e$MinDeath,"MinInfected" = e$MinInfected)))
  
  #Sample models
  sample_size <- 50000
  max_models <- 50000
  source("mdyn/SEIR/SEIR_COVID19.R")
  SEIR_covid(cores,par,paste(pos,"_paper",sep = ""),seed,sample_size,simulate_length,d_max,max_models,error_I,error_D,process = F)
  write.csv(x = errors,file = "/storage/SEIR/errors_simulation.csv")
}
sink()

