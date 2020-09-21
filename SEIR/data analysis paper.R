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
source("/home/diego/mdyn/SEIR/get_data_SP.R")

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
pdf(file = "parameters.pdf",width = 12.5,height = 10)
p
dev.off()
summary(par)

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
  ggtitle("Observed and Predict by 7 days SEIR Model confirmed cases") + ylab("Confirmed Cases")
pdf(file = paste("./output_paper/cases.pdf",sep = ""),width = 10,height = 5)
print(p)
dev.off()


for(c in unique(cases$Municipio)){
  p <- ggplot(cases %>% filter(Municipio == c),aes(x = Date,y = Mediana + 1,gorup = 1)) + theme_bw() + titles + geom_line(linetype = "dashed") +
    geom_line(aes(y = confirmed_corrected+1),color = "red") + scale_y_log10() +
    ggtitle(paste("Observed and Predict bt 7 days SEIR Model confirmed cases for",c)) + ylab("Confirmed Cases")
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
  ggtitle("Observed and Predict by 7 days SEIR Model confirmed deaths") + ylab("Confirmed Deaths")
pdf(file = paste("./output_paper/deaths.pdf",sep = ""),width = 10,height = 5)
print(p)
dev.off()

for(c in unique(deaths$Municipio)){
  p <- ggplot(deaths %>% filter(Municipio == c),aes(x = Date,y = Mediana + 1,gorup = 1)) + theme_bw() + titles + geom_point(color = "blue") +
    geom_line(aes(y = deaths_corrected+1),alpha = 0.25) + scale_y_log10() +
    ggtitle(paste("Observed and Predict by 7 days SEIR Model confirmed deaths for",c)) + ylab("Confirmed Deaths")
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
#c <- c("SÃO PAULO")
peak <- peak %>% filter(Municipio %in% c)
summary(peak)

p <- ggplot(peak,aes(x = peak,y = TMediana,ymin = TMinimo,ymax = TMaximo)) + theme_bw() + titles + geom_point() +
  geom_errorbar() +
  facet_wrap(~ date) + xlab("Observed Peak after t0") + ylab("Predicted Peak") +
  geom_abline(slope = 1,intercept = 0)
pdf(file = "pico.pdf",width = 15,height = 12)
print(p)
dev.off()

