#######################
#####Data Analysis ####
#####SEIR Paper    ####
#####D. Marcondes  ####
#####September 2020####
#######################

library(lubridate)
library(data.table)
library(ggplot2)
library(tidyverse)
library(plyr)
library(EpiEstim)
source("/home/diego/mdyn/SEIR/get_data_SP.R")
source("/home/diego/mdyn/SEIR/seir_solver.R")

titles <- theme(strip.text = element_text(size = 12), axis.text = element_text(size = 12),
                axis.title = element_text(size = 14), legend.text = element_text(size = 14),
                legend.title = element_text(size = 14),
                panel.border = element_rect(size=0.5, linetype="solid",color = "black"),
                panel.background = element_rect(fill="white",size=0.5, linetype="solid"),
                #legend.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                legend.position="bottom",legend.spacing.x = unit(0.5, 'cm'))

#wd
setwd("/storage/SEIR")

#Get parameters each day
t0 <- seq.Date(from = ymd("2020-04-01"),to = ymd("2020-08-18"),by = 7)
t0 <- t0[-11]
par <- list()
for(t in as.character(t0)){
  par[[t]] <- read.csv(paste("./",t,"_paper/parameters_",t,"_paper.csv",sep = ""))
  par[[t]]$date <- t
  par[[t]]$Model <- NULL
  par[[t]]$n <- nrow(par[[t]])
}
par <- data.frame(rbindlist(par))
par <- par[,-c(11:14)]
par <- par %>% gather("par","value",-date)
par$par <- factor(par$par,levels(factor(par$par))[c(5,2,3,4,6,1,7:11)])
par$par <- mapvalues(par$par,levels(par$par),c("n",expression(1-p[S]),expression(beta),expression(R[t]),"s","MeanInfectedTime",
                                                               expression(tau[D]),
                                                               expression(tau[E]),
                                                               expression(tau[R]),
                                                               expression(tau[S]),
                                                               expression(tau[RS])))
par$date <- ymd(par$date)
l4 <-  seq.Date(from = min(par$date),to = max(par$date),by = 21)
l4 <- paste(month(l4),"/",ifelse(day(l4) < 10,paste("0",day(l4),sep = ""),day(l4)),sep = "")
p <- ggplot(par,aes(x = date,y = value,group = date)) + theme_bw() + titles + ylab("Value") + xlab(expression(t[0])) +
  facet_wrap(.~par,scales = "free",labeller = label_parsed) + geom_boxplot() +
  scale_x_date(breaks = seq.Date(from = min(par$date),to = max(par$date),by = 21),labels = l4) 
pdf(file = "./output_paper/parameters.pdf",width = 12.5,height = 10)
p
dev.off()

#Observed data
obs <- get_data_SP()
obs$key <- paste(obs$date,obs$city)

#Cases
cases <- list()
for(t in as.character(t0)){
  cases[[t]] <- fread(paste("./",t,"_paper/cases_",t,"_paper.csv",sep = ""),data.table = F) 
  cases[[t]]$Date <- ymd(cases[[t]]$Date)
  cases[[t]] <- cases[[t]] %>% filter(Date <= min(cases[[t]]$Date) + 6)
}
cases <- data.frame(rbindlist(cases))
cases$key <- paste(cases$Date,cases$Municipio)
tmp <- obs %>% select(key,confirmed_corrected)
cases <- merge(cases,tmp)
tmp <- cases %>% filter(Municipio %in% unique(obs$city[obs$confirmed_corrected > 2000]))
tmp <- tmp %>% filter(confirmed_corrected > 1000)

p <- ggplot(tmp,aes(x = Date,y = Mediana + 1,group = Municipio)) + theme_bw() + titles + geom_point(color = "blue") +
  geom_line(aes(y = confirmed_corrected+1),alpha = 0.25) + scale_y_log10() +
  ggtitle("Confirmed Cases observed and predicted by 7 days SEIR Model") + ylab("Confirmed Cases")
pdf(file = paste("./output_paper/cases.pdf",sep = ""),width = 10,height = 5)
print(p)
dev.off()

for(c in unique(cases$Municipio)){
  p <- ggplot(cases %>% filter(Municipio == c),aes(x = Date,y = Mediana + 1,gorup = 1)) + theme_bw() + titles + geom_point(color = "blue") +
    geom_line(aes(y = confirmed_corrected+1),alpha = 0.25) + scale_y_log10() +
    ggtitle(paste("Confirmed Cases observed and predicted by 7 days SEIR Model for",c)) + ylab("Confirmed Cases")
  pdf(file = paste("./output_paper/",c,"_cases.pdf",sep = ""),width = 10,height = 5)
  print(p)
  dev.off()
}

#Deaths
deaths <- list()
for(t in as.character(t0)){
  deaths[[t]] <- fread(paste("./",t,"_paper/deaths_",t,"_paper.csv",sep = ""),data.table = F) 
  deaths[[t]]$Date <- ymd(deaths[[t]]$Date)
  deaths[[t]] <- deaths[[t]] %>% filter(Date <= min(deaths[[t]]$Date) + 6)
}
deaths <- data.frame(rbindlist(deaths))
deaths$key <- paste(deaths$Date,deaths$Municipio)
tmp <- obs %>% select(key,deaths_corrected)
deaths <- merge(deaths,tmp)
tmp <- deaths %>% filter(Municipio %in% unique(obs$city[obs$deaths_corrected > 100]))
tmp <- tmp %>% filter(deaths_corrected > 100)

p <- ggplot(tmp,aes(x = Date,y = Mediana + 1,group = Municipio)) + theme_bw() + titles + geom_point(color = "blue") +
  geom_line(aes(y = deaths_corrected+1),alpha = 0.25) + scale_y_log10() +
  ggtitle("Confirmed Deaths observed and predicted by 7 days SEIR Model") + ylab("Confirmed Deaths")
pdf(file = paste("./output_paper/deaths.pdf",sep = ""),width = 10,height = 5)
print(p)
dev.off()

for(c in unique(deaths$Municipio)){
  p <- ggplot(deaths %>% filter(Municipio == c),aes(x = Date,y = Mediana + 1,gorup = 1)) + theme_bw() + titles + geom_point(color = "blue") +
    geom_line(aes(y = deaths_corrected+1),alpha = 0.25) + scale_y_log10() +
    ggtitle(paste("Confirmed Deaths observed and predicted by 7 days SEIR Model for",c)) + ylab("Confirmed Deaths")
  pdf(file = paste("./output_paper/",c,"_deaths.pdf",sep = ""),width = 10,height = 5)
  print(p)
  dev.off()
}

#Peak
peak <- list()
for(t in as.character(t0)){
  peak[[t]] <- fread(paste("./",t,"_paper/peak_",t,"_paper.csv",sep = ""),data.table = F) 
  peak[[t]]$date <- t
}
peak <- data.frame(rbindlist(peak))
peak$peak <- NA

for(t in as.character(t0))
  for(c in unique(peak$Municipio)){
    tmp <- obs %>% filter(date >= ymd(t) & city == c)
    p <- min(tmp$date[tmp$new_death_cor == max(tmp$new_death_cor)])
    peak$peak[peak$Municipio == c & peak$date == t] <- as.character(p)
  }
for(i in c(2:4,8,9))
  peak[,i] <- ymd(peak[,i])
c <- unique(obs$city[obs$deaths_corrected > 100])
peak <- peak %>% filter(Municipio %in% c)

p <- ggplot(peak,aes(x = peak,y = TMediana,ymin = TMinimo,ymax = TMaximo)) + theme_bw() + titles + geom_point() +
  geom_errorbar() +
  facet_wrap(~ date) + xlab("Observed Peak after t0") + ylab("Predicted Peak") +
  geom_abline(slope = 1,intercept = 0)
pdf(file = "./output_paper/pico.pdf",width = 15,height = 12)
print(p)
dev.off()

#####Comparative Study with model without mobility#####
#####Model specification#####
#t = time
#Y = observed quantities at time t
#par = A named list of model parameters
derivatives <- function(t,Y,parK){
  
  #States at time t
  E <- Y[1] #Exposed
  I <- Y[2] #Infected
  Is <- Y[3] #Statistics
  R <- Y[4] #Recovered
  D <- Y[5] #Deaths
  S <- parK$pop - E - I - Is - R - D #Susceptibles
  
  #Parameters
  N <- parK$pop #Population
  gammaI <- parK$gammaI #Rate from Exposed to Infected
  gammaS <- parK$gammaS #Rate from Infected to Statistics
  nuI <- parK$nuI #Rate from Infected to Recovered
  nuS <- parK$nuS #Rate from Statistics to Recovered
  delta <- parK$deltaRate #Rate from Statistics to Death
  beta <- parK$beta
  
  #Derivatives
  dY <- vector(length = 6) #Vector of derivatives
  dY[1] <- -gammaI*E + beta*(S/(N-D))*I #E
  dY[2] <- gammaI*E - nuI*I - gammaS*I #I
  dY[3] <- -nuS*Is + gammaS*I - delta*Is #Is
  dY[4] <- nuI*I + nuS*Is #R
  dY[5] <- delta*Is #D
  dY[6] <- gammaS*I #Add new cases to total
  
  return(list(dY)) #Return
}

#São Paulo
t <- ymd("2020-04-29")

tmp <- obs %>% filter(city == "SÃO PAULO" & date >= ymd(t) - 30) #Get data of city is last day of validation
dr <- (tmp$deaths_corrected[tmp$date == t] - tmp$deaths_corrected[tmp$date == ymd(t)-30])/(tmp$confirmed_corrected[tmp$date == t] - tmp$confirmed_corrected[tmp$date == ymd(t)-30])

par <- list()
par$names <- as.vector(read.table("/home/diego/mdyn/SEIR/dados/move_mat_SÃO PAULO_Municip_reg_names.txt",sep = ";")[1:645,1]) #Sites name
par$sites <- length(par$names) #Number of sites

#Population
par$pop <- read.csv("/home/diego/mdyn/SEIR/dados/population_sp_mun.csv",sep = ";")
par$pop$municipio <- gsub(pattern = "'",replacement = "",x = par$pop$municipio)
par$pop <- par$pop[match(x = par$names,table = toupper(par$pop$municipio)),]
par$pop <- as.vector(par$pop$populacao_estimada)
match("SÃO PAULO",par$names)

pred <- readRDS(paste("/storage/SEIR/",t,"_paper/prediction_",t,"_paper.rds",sep = ""))
est_par <- read.csv(paste("/storage/SEIR/",t,"_paper/parameters_",t,"_paper.csv",sep = ""))
simulate <- list()
for(k in 1:nrow(est_par)){
  parK <- list()
  parK$pop <- par$pop[269] #Population
  parK$Te <- est_par$Te[k] #Time exposed
  parK$Ti <- est_par$Ti[k] #Time infected before recovering
  parK$Ts <- est_par$Ts[k] #Time infected until statistics
  parK$Tsr <- est_par$Tsr[k] #Time in statistics until recovering
  parK$Td <- est_par$Td[k] #Time in statistics until death
  parK$pS <- (1-est_par$MedianAssymptomatic[k]) #Proportion in statistics
  parK$gammaI <- 1/parK$Te #Rate from Exposed to Infected
  parK$gammaS <- parK$pS/parK$Ts #Rate from Infected to Statistics
  parK$nuI <- (1-parK$pS)/parK$Ti #Rate from Infected to Recovered
  parK$nuS <- (1-dr)/parK$Tsr #Rate from Statistics to Recovered
  parK$deltaRate <- dr #Rate from Statistics to Death
  parK$beta <- pred[[k]]$beta[269]
  
  #Init
  init <- vector()
  init[1] <- pred[[k]]$E[,269][1]
  init[2] <- pred[[k]]$I[,269][1]
  init[3] <- pred[[k]]$Is[,269][1]
  init[4] <- pred[[k]]$R[,269][1]
  init[5] <- pred[[k]]$D[,269][1]
  init[6] <- pred[[k]]$It[,269][7]
  
  #Simulate
  simulate[[k]] <- solve_seir(init,1:200,derivatives,parK)
  simulate[[k]]<- simulate[[k]][7:200,]
  colnames(simulate[[k]]) <- c("t","E","I","Is","R","D","It")
}

#Simulate with mobility
cases <- fread(paste("/storage/SEIR/",t,"_paper/cases_all_",t,"_paper.csv",sep = ""),data.table = F) 
cases <- cases %>% filter(Municipio == "SÃO PAULO")
plot <- data.frame("Model" = NA,"type" = NA,"t"= NA,"Is" = NA,"D" = NA)
for(k in 1:32){
  plot <- rbind.data.frame(plot,data.frame("Model" = k,"type" = "mob","t"= 1:length(cases$Casos[cases$Modelo == k]),
                                           "Is" = cases$Casos[cases$Modelo == k],"D" = cases$Casos[cases$Modelo == k]))
  plot <- rbind.data.frame(plot,data.frame("Model" = k,"type" = "simple","t" = 1:length(simulate[[k]]$It),
                                           "Is" = simulate[[k]]$It,"D" = simulate[[k]]$It))
}
plot <- na.omit(plot)
head(plot)
plot$ModelT <- paste(plot$Model,plot$type)

p <- ggplot(plot %>% filter(Model == 1),aes(x = t,y = Is,group = ModelT,colour = type)) + geom_line() + scale_y_log10()

pdf("teste.pdf",10,7.5)
p
dev.off()
