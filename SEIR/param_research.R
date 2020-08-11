###############################
#####SEIR for COVID-19    #####
#####Diego Marcondes      #####
#####dmarcondes@ime.usp.br#####
###############################

library(lubridate)

#Parameters
cores <- 24 #Number   of cores to use in parallel computation
pos <- paste(Sys.Date(),"_research",sep = "") #"teste" #What to add at the end of all output files
seed <- as.numeric(Sys.Date()) #Seed
par <- list() #Candidate values of model parameters
d_max <- Sys.Date() #"2020-06-14"
simulate_length <- as.numeric(ymd("2020-09-30") + 5 - ymd(d_max)) #Number of days to simulate
error_I <- 0.065
error_D <- 0.065

#Set mobility matrix
par$mob <- list()
day <- seq.Date(from = ymd("2020-07-06"),to = ymd("2020-08-02"),1)
for(d in as.character(day)){
  cat(d)
  cat("\n")
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
for(d in as.character(seq.Date(from = ymd("2020-06-24"),to = d_max,1)))
  par$mob[[as.character(ymd(d))]] <- par$mob[[as.character(weekdays(ymd(d)))]]

#Cadidate parameters
par$pS <- 1/c(5:10,15,20,30,40,50,100)
par$Te <- c(4:6)
par$Ti <- c(3:15)
par$Ts <- 3:15
par$Tsr <- 3:28
par$Td <- c(7:28)
par$s <- c(0.25,0.5,1,1.5,2,2.5,3)

sample_size <- 10e6
max_models <- 50
source("mdyn/SEIR/SEIR_COVID19_research.R")
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
