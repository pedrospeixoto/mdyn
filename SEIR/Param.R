###############################
#####SEIR for COVID-19    #####
#####Diego Marcondes      #####
#####dmarcondes@ime.usp.br#####
###############################

library(lubridate)

#Parameters
#wd <- "/home/diego/" #Working directory where the SEIR folder is
wd <- paste(getwd(),"/",sep = "")
cores <- 24 #Number of cores to use in parallel computation
pos <- "teste" #What to add at the end of all output files
seed <- rnorm(1,10,100000) #Seed
par <- list() #Candidate values of model parameters
error_good <- 0.05

#Dates
simulate_length <- 365 #Number of days to simulate

#Set mobility matrix
par$mob <- list()
day <- seq.Date(from = ymd("2020-05-01"),to = ymd("2020-05-20"),1)
d_max <- "2020-05-20"
for(d in as.character(day)){
  cat(d)
  cat("\n")
  par$mob[[as.character(d)]] <- as.matrix(read.csv(paste("/storage/inloco/data/data_br_full_2020_05_20/date0=",d,"/move_mat_SÃO PAULO_Municip.csv",sep = ""),
                                                   sep = " ",
                                                   header = F))[1:645,1:645]
}
for(i in 0:6){
  w <- unique(weekdays(day))[length(unique(weekdays(day)))-i]
  d <- as.character(day[length(day)-i])
  par$mob[[as.character(w)]] <- as.matrix(read.csv(paste("/storage/inloco/data/data_br_full_2020_05_20/date0=",d,"/move_mat_SÃO PAULO_Municip.csv",sep = ""),
                                                   sep = " ",
                                                   header = F))[1:645,1:645]
}
par$names <- as.vector(read.table(paste(wd,"SEIR//Dados/move_mat_SÃO PAULO_Municip_reg_names.txt",sep = ""),sep = ";")[1:645,1]) #Sites name
par$sites <- length(par$names) #Number of sites

#Population
par$pop <- read.csv(paste(wd,"/SEIR/Dados/population_sp_mun.csv",sep = ""),sep = ";")
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
par$gammaA <-0.9
par$Te <- seq(5,14,1)
par$Ta <- seq(7,28,1)
par$Ts <- seq(7,28,1)
par$Td <- seq(7,28,1)
par$s <- c(0.01,0.5,1,1.5,2,2.5,3)
#cand_beta <- c(0.05,0.2)

#####Sample Size#####
log_choose <- function(n,k){
  lc <- vector()
  for(i in 1:length(k))
    lc[i] <- sum(log((n-k[i]+1):n)) - sum(log(1:k[i]))
  return(lc)
}

mc <- function(p,c,delta,cardinality){
  m <- (1/c)*(log_choose(cardinality,p) - log(delta))
  ifelse(m <= cardinality,m,NA)
}
n_independent <- as.numeric(length(par$gammaA))*as.numeric(length(par$Te))*as.numeric(length(par$Ta))*as.numeric(length(par$Ts))*as.numeric(length(par$Td))*as.numeric(length(par$s))
#n_dependent <- as.numeric((length(cand_beta)^10)^4)
n_models <- n_independent
sample_size <- mc(p = 0.1*n_models,c = 0.1,delta = 0.99,cardinality = n_models)
sample_size <- 100000
max_models <- 100000

source(paste(wd,"SEIR/Codigos/SEIR_COVID19.R",sep = ""))
SEIR_covid(wd,cores,par,cand_beta,pos,seed,sample_size,simulate_length,d_max,max_models,error_good)

# cat("Converting pdfs...\n")
# setwd(wd)
# #mogrify -density 100 -format png /home/diego/SEIR/Workspace/Plots/Videos/teste_big/*.pdf
#mogrify -density 100 -format png *.pdf &
#ffmpeg -framerate 2 -i teste_big_%03d.png -c:v libx264 -profile:v high -crf 20 -pix_fmt yuv420p video.mp4
#              sep = ""
# system(paste("for x in 0 1 2 3 4 5 6 7 8 9; do mogrify -density 100 -format png ",wd,
#              "SEIR/Workspace/Plots/Videos/",pos,"/",pos,"_0$x*.pdf & done",
#              sep = ""))
# setwd(paste(wd,"SEIR/Workspace/Plots/Videos/",pos,"/",sep =""))
# system(paste("ffmpeg -framerate 2 -i ",pos,"_%03d.png -c:v libx264 -profile:v high -crf 20 -pix_fmt yuv420p video.mp4",
#              sep = ""))
# setwd(wd)
