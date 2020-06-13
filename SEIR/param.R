###############################
#####SEIR for COVID-19    #####
#####Diego Marcondes      #####
#####dmarcondes@ime.usp.br#####
###############################

library(lubridate)

#Parameters
cores <- 24 #Number   of cores to use in parallel computation
pos <- "teste" #What to add at the end of all output files
seed <- as.numeric(Sys.Date()) #Seed
par <- list() #Candidate values of model parameters
d_max <- "2020-06-03"
simulate_length <- 90 #as.numeric(ymd("2020-12-31") - ymd(d_max)) #Number of days to simulate
error_I <- 0.06
error_D <- 0.06

#Set mobility matrix
par$mob <- list()
day <- seq.Date(from = ymd("2020-05-20"),to = ymd("2020-06-03"),1)
for(d in as.character(day)){
  cat(d)
  cat("\n")
  par$mob[[as.character(d)]] <- as.matrix(read.csv(paste("/storage/inloco/data/mobility_br_2020/date0=",d,"/move_mat_SÃO PAULO_Municip.csv",sep = ""),
                                                   sep = " ",
                                                   header = F))[1:645,1:645]
}
for(i in 0:6){
  w <- unique(weekdays(day))[length(unique(weekdays(day)))-i]
  d <- as.character(day[length(day)-i])
  par$mob[[as.character(w)]] <- as.matrix(read.csv(paste("/storage/inloco/data/mobility_br_2020/date0=",d,"/move_mat_SÃO PAULO_Municip.csv",sep = ""),
                                                   sep = " ",
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

#Cadidate parameters  
par$pS <- 1/c(5:10,15,20)
par$Te <- c(3:6)
par$Ti <- c(4:10)
par$Ts <- 7:14
par$Tsr <- 14:28
par$Td <- c(1:14)
par$s <- c(1,1.5,2,2.5,3)

sample_size <- 1000
max_models <- 200
source("mdyn/SEIR/SEIR_COVID19.R")
SEIR_covid(cores,par,pos,seed,sample_size,simulate_length,d_max,max_models,error_I,error_D)

#####Sample Size#####
# log_choose <- function(n,k){
#   lc <- vector()
#   for(i in 1:length(k))
#     lc[i] <- sum(log((n-k[i]+1):n)) - sum(log(1:k[i]))
#   return(lc)
# }
# 
# mc <- function(p,c,delta,cardinality){
#   m <- (1/c)*(log_choose(cardinality,p) - log(delta))
#   ifelse(m <= cardinality,m,NA)
# }
# n_independent <- as.numeric(length(par$gammaA))*as.numeric(length(par$Te))*as.numeric(length(par$Ta))*as.numeric(length(par$Ts))*as.numeric(length(par$Td))*as.numeric(length(par$s))
# #n_dependent <- as.numeric((length(cand_beta)^10)^4)
# n_models <- n_independent
# sample_size <- mc(p = 0.1*n_models,c = 0.1,delta = 0.99,cardinality = n_models)
