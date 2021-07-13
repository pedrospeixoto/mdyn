#A snapshot of a pandemic: the interplay between social isolation and COVID-19 dynamics in Brazil

library(ggplot2)
library(tidyverse)
library(readxl)
library(lubridate)
library(data.table)
library(ggpubr)
library(xtable)

#Style of plots
titles <- theme(strip.text = element_text(size = 16,colour = "black"),
                axis.text = element_text(size = 12,color = "black"),
                axis.title = element_text(size = 14), legend.text = element_text(size = 14),
                legend.title = element_text(size = 14), panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(), panel.border = element_blank(),
                panel.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                legend.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                legend.position="bottom",legend.spacing.x = unit(0.5, 'cm'),
                strip.background =element_rect(fill="gray73",color = "gray73"))

#Data
idh <- data.frame(read_xlsx("dados_isolXinc.xlsx",sheet = 4))
data <- data.frame(read_xlsx("data_ready.xlsx"))
data$city <- factor(data$city)
data$date <- ymd(data$date)
data$state <- factor(data$state)
data$region <- factor(data$region)
data <- data %>% filter(city != "Brasília") %>% droplevels()
summary(data)
length(levels(data$city))

#####Analysis 3.1####
data31 <- data %>% filter(city %in% c("São Paulo","Rio de Janeiro","Belo Horizonte","Curitiba","Florianópolis","Fortaleza",
                                      "Goiânia","Manaus","Belém","Porto Alegre","Recife","Salvador","Vitória","Campinas"))

#Figure 2
tmp <- data31 %>% select(city,region) %>% unique()
tmp <- tmp[order(tmp$region),]
data31$city <- factor(data31$city,tmp$city)
p <- ggplot(data31,aes(x = date,y = inc7)) + facet_wrap(~city,ncol = 2) + theme_linedraw() + titles +
  geom_line(aes(y = inc7,colour = region,linetype = "Incidence")) +
  geom_line(data = data31,aes(x = date,y = (iso714/max(iso714,na.rm = T))*max(inc7,na.rm = T),colour = region,linetype = "Isolation Index")) +
  ylab("7-day avarage of nowcasted incidence") + xlab("Date") +
  scale_y_continuous(sec.axis = sec_axis(~.*(max(data31$iso714,na.rm = T)/max(data31$inc7,na.rm = T)),
                                         name = "Mean relative isolation index of one week ago",
                                         labels = function(x) x)) + 
  scale_x_date(breaks =  ymd("2020-03-15","2020-04-15","2020-05-15","2020-06-15","2020-07-15","2020-08-15","2020-09-15","2020-10-15"),
               labels = c("Mar 15","Apr 15","May 15","Jun 15","Jul 15","Aug 15","Sep 15","Oct 15")) + 
  scale_linetype_manual("",values = c("Incidence" = "solid", "Isolation Index" = "dashed")) + scale_colour_discrete("Region")
pdf("IsoIncMainCities.pdf",width = 15,height = 25)
p
dev.off()

cc <- data.table(data31)
cc <- cc[,.(Cor = cor(iso714,inc7,use = "complete.obs")),by = city]
cc$xpos <- c(-Inf)
cc$ypos <- c(Inf)
cc$Cor <- paste("R =",round(cc$Cor,2))
cc <- merge(cc,unique(data31[,c(1,7)]))
p <- ggplot(data31,aes(x = iso714,y = inc7,colour = region)) + theme_linedraw() + titles +
  geom_point(pch = 1) + 
  ylab("7-day avarage of nowcasted incidence") + xlab("Mean relative isolation index of one week ago") + facet_wrap(~city,ncol = 2,scales = "free") +
  geom_text(data = cc,aes(x = xpos,y = ypos,label = Cor),hjust = -0.1,vjust = 2,color = "black") + scale_colour_discrete("Region")
pdf("CorIsoxIncMainCities.pdf",width = 15,height = 25)
p
dev.off()  

tapply(data31$isol[data31$date >= ymd("2020-03-15")],month(data31$date[data31$date >= ymd("2020-03-15")]),
       function(x) paste(round(mean(x,na.rm = T)),"\\pm",round(sd(x,na.rm = T),2)))

#####Analysis with Rt####
data_lockdow <- data %>% filter(city %in% c("São Luís","Belém","Fortaleza","Recife"))
load("/home/dmarcondes/GDrive/mdyn/paper_isol/Covid19.RData")
data_lockdow$rt[data_lockdow$rt > 3] <- 3
data_lockdow <- merge(data_lockdow,cidadeStatus[,c(2,4,5)],by.x = "city",all.x = T,all.y = F,by.y = "Cidade")
data_lockdow$Início <- ymd(data_lockdow$Início)
data_lockdow$Término <- ymd(data_lockdow$Término)
p <- ggplot(data_lockdow %>% filter(date >= ymd("2020-03-15")),
            aes(x = date,y = inc7)) + facet_wrap(~city,ncol = 2) + theme_linedraw() + titles +
  geom_rect(aes(xmin=Início,xmax=Término,ymin = 0,ymax = Inf),fill = "red",alpha = 0.5) +
  geom_point(aes(y = inc7),color = "darkgreen") +
  geom_point(aes(x = date,y = (rt/max(rt,na.rm = T))*max(inc7,na.rm = T)),color = "blue") +
  ylab("7-day avarage of nowcasted incidence") + xlab("Date") +
  scale_y_continuous(sec.axis = sec_axis(~.*(max(data_lockdow$rt,na.rm = T)/max(data_lockdow$inc7,na.rm = T)),
                                         name = "Rt",
                                         labels = function(x) x)) + 
  scale_x_date(breaks =  ymd("2020-03-15","2020-04-15","2020-05-15","2020-06-15","2020-07-15","2020-08-15","2020-09-15","2020-10-15"),
               labels = c("Mar 15","Apr 15","May 15","Jun 15","Jul 15","Aug 15","Sep 15","Oct 15")) + scale_colour_discrete("Region") +
  geom_hline(yintercept = (1/max(data_lockdow$rt,na.rm = T))*max(data_lockdow$inc7,na.rm = T),linetype = "dashed")
pdf("LockdownNortheast.pdf",width = 15,height = 10)
p
dev.off()

#Table decrease/increase isolation and incidence lockdown northeast
tab <- data.frame("City" = NA,"Period" = NA,"Isol_var" = NA,"Inc_var" = NA)
for(c in unique(data_lockdow$city)){
  #Prior
  iso0 <- mean(data_lockdow$isol[data_lockdow$city == c & data_lockdow$date < data_lockdow$Início & data_lockdow$date > data_lockdow$Início - 7])
  inc0 <- mean(data_lockdow$inc7[data_lockdow$city == c & data_lockdow$date < data_lockdow$Início & data_lockdow$date > data_lockdow$Início - 7])
  
  #Lockdown
  isolock <- mean(data_lockdow$isol[data_lockdow$city == c & data_lockdow$date >= data_lockdow$Início & data_lockdow$date <= data_lockdow$Término])/iso0 - 1
  inclock <- mean(data_lockdow$inc7[data_lockdow$city == c & data_lockdow$date >= data_lockdow$Início & data_lockdow$date <= data_lockdow$Término])/inc0 - 1
  
  #Periods of weeks after the lockdown ended
  isoPeriod <- vector()
  incPeriod <- vector()
  for(i in 1:12){
    iso <- mean(data_lockdow$isol[data_lockdow$city == c & data_lockdow$date > data_lockdow$Término + (i-1)*7 & data_lockdow$date <= data_lockdow$Término+ 7*i])/iso0 - 1
    inc <- mean(data_lockdow$inc7[data_lockdow$city == c & data_lockdow$date > data_lockdow$Término + (i-1)*7 & data_lockdow$date <= data_lockdow$Término+ 7*i])/inc0 - 1
    isoPeriod <- c(isoPeriod,iso)
    incPeriod <- c(incPeriod,inc)
  }
  tab <- rbind.data.frame(tab,data.frame("City" = c,"Period" = c("Prior","Lockdown",1:12),
                                         "Isol_var" = c(iso0,isolock,isoPeriod),
                                         "Inc_var" = c(inc0,inclock,incPeriod)))
  
}
tab <- na.omit(tab)
autoAnalise::salvar_xlsx(x = tab,arquivo = "change_lock.xlsx",anexar = F,row.names = F,col.names = T,planilha = "tab",latex = F)

#São Paulo analysis
data_sp <- data %>% filter(city %in% c("São Paulo","Campinas","Votuporanga"))
lapply(as.list(c("São Paulo","Campinas","Votuporanga")),function(x) na.omit(data_sp$date[data_sp$nowcasting == max(data_sp$nowcasting[data_sp$city == x],na.rm = T) & data_sp$city == x]))
lapply(as.list(c("São Paulo","Campinas","Votuporanga")),function(x) max(data_sp$nowcasting[data_sp$city == x],na.rm = T))
lapply(as.list(c("São Paulo","Campinas","Votuporanga")),function(x) mean(((data_sp$estimated_population_2019/1e5)*data_sp$nowcasting)[data_sp$city == x],na.rm = T))
lapply(as.list(c("São Paulo","Campinas","Votuporanga")),function(x) min(((data_sp$estimated_population_2019/1e5)*data_sp$nowcasting)[data_sp$city == x],na.rm = T))
lapply(as.list(c("São Paulo","Campinas","Votuporanga")),function(x) max(((data_sp$estimated_population_2019/1e5)*data_sp$nowcasting)[data_sp$city == x],na.rm = T))
data_sp$rt[data_sp$rt > 3] <- 3
cidadeStatus <- rbind.data.frame(cidadeStatus,data.frame("Estado" = "SP","Cidade" = "Votuporanga","Status" = c("Laranja","Amarelo"),
                                                         "Início" = ymd(c("2020-06-28","2020-10-08")),"Término" = ymd(c("2020-10-07","2020-10-24")),
                                                         "TempoIni" = NA,"TempoFim" = NA))

tmp <- cidadeStatus[,c(2,3,4,5)]
tmp$Início <- ymd(tmp$Início)
tmp$Término <- ymd(tmp$Término)
names(tmp) <- c("city","status","init","end")
tmp <- data.frame(tmp %>% filter(city %in% c("São Paulo","Campinas","Votuporanga")))
data_sp <- merge(data_sp,tmp,all.x = T)
data_sp$status <- factor(data_sp$status)
data_sp$status <- plyr::mapvalues(data_sp$status,c("Amarelo","Laranja","Vermelho","Verde"),c("Yellow","Orange","Red","Green"))
data_sp$end[data_sp$end > max(data_sp$date,na.rm = T)] <- max(data_sp$date,na.rm = T)

p <- ggplot(data_sp %>% filter(date >= ymd("2020-03-15")),
            aes(x = date,y = inc7)) + facet_wrap(~city,ncol = 3) + theme_linedraw() + titles +
  geom_rect(aes(xmin=init,xmax=end,ymin = 0,ymax = Inf,fill = status,colour = status)) +
  geom_line(aes(y = inc7),color = "darkgreen") +
  geom_line(data = data_sp %>% filter(date >= ymd("2020-03-15") & !is.na(rt)),aes(x = date,y = (rt/max(rt,na.rm = T))*max(inc7,na.rm = T)),color = "blue") +
  ylab("7-day avarage of nowcasted incidence") + xlab("Date") +
  scale_y_continuous(sec.axis = sec_axis(~.*(max(data_sp$rt,na.rm = T)/max(data_sp$inc7,na.rm = T)),
                                         name = "Rt",
                                         labels = function(x) x)) + 
  scale_x_date(breaks =  ymd("2020-03-15","2020-04-15","2020-05-15","2020-06-15","2020-07-15","2020-08-15","2020-09-15","2020-10-15"),
               labels = c("Mar 15","Apr 15","May 15","Jun 15","Jul 15","Aug 15","Sep 15","Oct 15")) + scale_fill_manual("São Paulo Plan",
                                                                                                                        values = c("yellow","orange","green","red")) +
  scale_colour_manual("São Paulo Plan",values = c("yellow","orange","green","red")) +
  geom_hline(yintercept = (1/max(data_sp$rt,na.rm = T))*max(data_sp$inc7,na.rm = T),linetype = "dashed")
pdf("SPplanIncRt.pdf",width = 17.5,height = 10)
p
dev.off()

#Mean of values in each phase
data_sp2 <- data %>% filter(city %in% c("São Paulo","Campinas","Votuporanga"))
data_sp2$status <- "No"
for(c in unique(data_sp2$city)){
  tmp <- cidadeStatus %>% filter(Cidade == c)
  for(i in 1:nrow(tmp))
    data_sp2$status[data_sp2$city == c & data_sp2$date >= tmp$Início[i] & data_sp2$date <= tmp$Término[i]] <- tmp$Status[i]
}
mrt <- tapply(X = data_sp2$rt,INDEX = paste(data_sp2$city,data_sp2$status),FUN = function(x) median(x,na.rm = T))
misol <- tapply(X = data_sp2$isol,INDEX = factor(paste(data_sp2$city,data_sp2$status)),FUN = function(x) median(x,na.rm = T))
minc <- tapply(X = data_sp2$nowcasting,INDEX = factor(paste(data_sp2$city,data_sp2$status)),FUN = function(x) median(x,na.rm = T))
xtable(cbind(mrt,misol,minc))

#####Upward phase####
data_upward <- data
for(c in levels(data$city)){
  d <- max(data_upward$date[data_upward$city == c & data_upward$inc7 == max(data_upward$inc7[data_upward$city == c],na.rm = T)],
           na.rm = T)
  data_upward$date[data_upward$city == c & data_upward$date > d] <- NA
}
data_upward <- data_upward %>% filter(!is.na(date))
data_upward <- data.table(data_upward)
data_upward <- data_upward[order(data_upward$date,decreasing = F),]

get_stage <- function(x){
  stag <- rep(NA,length(x))
  stag[x == max(x)] <- 4
  stag[abs(x - max(x)/2) == min(abs(x - max(x)/2))] <- 3
  stag[abs(x - max(x)/4) == min(abs(x - max(x)/4))] <- 2
  #stag[abs(x - max(x)/8) == min(abs(x - max(x)/8))] <- 3
  #stag[abs(x - max(x)/16) == min(abs(x - max(x)/16))] <- 2
  #stag[abs(x - max(x)/32) == min(abs(x - max(x)/32))] <- 3
  #stag[abs(x - max(x)/64) == min(abs(x - max(x)/64))] <- 2
  i <- 1
  for(j in 1:length(stag))
    if(!is.na(stag[j])){
      if(stag[j] == i)
        stag[j] <- NA
      else
        i <- stag[j]
    }
  return(stag)
}
data_upward <- data_upward[,stage := get_stage(acc_inc),by = city]
data_upward <- data.frame(data_upward)

#Plot
data <- data.table(data)
data <- data[,pre_peak := as.numeric(date <= max(date[inc7 == max(inc7,na.rm = T)],na.rm = T)),by = city]
data$pre_peak <- factor(data$pre_peak)
data <- data.frame(data)
ann <- data_upward %>% dplyr::select(date,city,stage) %>% na.omit()
data$pre_peak <- factor(data$pre_peak,c("1","0"))
data$pre_peak <- plyr::mapvalues(data$pre_peak,c("0","1"),c("Incidence after peak","Incidence prior to peak"))
data$pre_peak <- factor(data$pre_peak,c("Incidence prior to peak","Incidence after peak"))
p <- ggplot(data %>% filter(date >= ymd("2020-03-15")),aes(x = date,y = inc7)) + facet_wrap(~city,ncol = 4) + theme_linedraw() + titles +
  geom_line(aes(y = inc7,colour = region,linetype = pre_peak)) +
  geom_line(data = data,aes(x = date,y = (iso714/max(iso714,na.rm = T))*max(inc7,na.rm = T),colour = region,linetype = "Isolation Index")) +
  ylab("7-day avarage of nowcasted incidence") + xlab("Date") +
  scale_y_continuous(sec.axis = sec_axis(~.*(max(data$iso714,na.rm = T)/max(data$inc7,na.rm = T)),
                                         name = "Mean relative isolation index of one week ago",
                                         labels = function(x) x)) + 
  scale_x_date(breaks =  ymd("2020-03-15","2020-04-15","2020-05-15","2020-06-15","2020-07-15","2020-08-15","2020-09-15","2020-10-15"),
               labels = c("Mar","Apr","May","Jun","Jul","Aug","Sep","Oct")) + scale_colour_discrete("Region") +
  scale_linetype_manual("",values = c("dotted","solid","dashed")) +
  geom_vline(data = ann,aes(xintercept = date),linetype = "solid",color = "black",alpha = 0.25) +
  geom_vline(xintercept = ymd("2020-03-15"),linetype = "solid",color = "black",alpha = 0.25)
pdf("Stages.pdf",width = 15,height = 25)
p
dev.off()

#Data with speed and isolation
data_speed <- data.frame("city" = NA,"stage" = NA,"iso" = NA,"time" = NA)
for(c in levels(data_upward$city)){
  #Stage 1
  t1 <- as.numeric(na.omit(data_upward$date[data_upward$city == c & data_upward$stage == 2]) - min(data_upward$date[data_upward$city == c & data_upward$nowcasting > 0],na.rm = T))
  i1 <- median(data_upward$isol[data_upward$city == c & data_upward$date >= min(data_upward$date[data_upward$city == c & data_upward$nowcasting > 0],na.rm = T) &
                                  data_upward$date <= na.omit(data_upward$date[data_upward$city == c & data_upward$stage == 2])],na.rm = T)
  
  #Stage 2
  t2 <- as.numeric(na.omit(data_upward$date[data_upward$city == c & data_upward$stage == 3]) - na.omit(data_upward$date[data_upward$city == c & data_upward$stage == 2]))
  i2 <- median(data_upward$isol[data_upward$city == c & data_upward$date >= na.omit(data_upward$date[data_upward$city == c & data_upward$stage == 2]) &
                                  data_upward$date <= na.omit(data_upward$date[data_upward$city == c & data_upward$stage == 3])],na.rm = T)
  
  #Stage 3
  t3 <- as.numeric(max(data_upward$date[data_upward$city == c & data_upward$nowcasting > 0],na.rm = T) - na.omit(data_upward$date[data_upward$city == c & data_upward$stage == 3])) 
  i3 <- median(data_upward$isol[data_upward$city == c & data_upward$date <= max(data_upward$date[data_upward$city == c & data_upward$nowcasting > 0],na.rm = T) &
                                  data_upward$date >= na.omit(data_upward$date[data_upward$city == c & data_upward$stage == 3])],na.rm = T)
  
  #Data
  data_speed <- rbind.data.frame(data_speed,data.frame("city" = c,"stage" = 1:4,"iso" = c(i1,i2,i3,NA),"time" = c(t1,t2,t3,t1+t2+t3)))
  
}
data_speed <- data_speed[-1,]
data_speed$stage_iso <- paste("Stage",data_speed$stage,"Isolation")
data_speed$stage_time <- paste("Stage",data_speed$stage,"Length")
data_speed$stage_time[data_speed$stage_time == "Stage 4 Length"] <- "Upward phase length"
data_speed$stage <- NULL
data_speed <- merge(data_speed[,c(1,2,4)],data_speed[,c(1,3,5)],by = "city",all = T)
data_speed <- na.omit(data_speed)

p <- ggplot(data_speed,aes(x = iso,y = time)) + theme_linedraw() + titles + geom_point() +
  facet_grid(stage_time~stage_iso,scales = "free_y") + stat_cor() + geom_smooth(se = F,method = "lm",color = "black") +
  xlab("Median relative isolation index") + ylab("Length of stage")


#Tables
data_speed_gather <- data_speed %>% spread(key = "stage_iso",value = "iso") %>% spread(key = "stage_time",value = "time") %>% unique()
q1 <- c("Q1",apply(data_speed_gather[,-1],2,function(x) round(quantile(x,0.25))))
q2 <- c("Median",apply(data_speed_gather[,-1],2,function(x) round(quantile(x,0.5))))
q3 <- c("Q3",apply(data_speed_gather[,-1],2,function(x) round(quantile(x,0.75))))
q <- data.frame(rbind(q1,q2,q3))
colnames(q) <- colnames(data_speed_gather)
data_speed_gather <- rbind.data.frame(data_speed_gather,q)
for(i in 2:ncol(data_speed_gather))
  data_speed_gather[,i] <- as.numeric(data_speed_gather[,i])
print(xtable(data_speed_gather,digits = 2),include.rownames = F)

tmp <- data_speed_gather[-c(33,34,35),]
for(j in 2:ncol(tmp)){
  tmp[,j] <- cut(tmp[,j],quantile(tmp[,j],c(0,0.25,0.5,0.75,1)),c("Q1","Q2","Q3","Q4"),include.lowest = T) 
}
tmp <- tmp %>% gather("stage_iso","Qiso",-city,-"Stage 1 Length",-"Stage 2 Length",-"Stage 3 Length",-"Upward phase length")
tmp <- tmp %>% gather("stage_time","Qtime",-city,-stage_iso,-Qiso)
tmp$key <- paste(tmp$stage_iso,tmp$Qiso,tmp$stage_time,tmp$Qtime)
tmp <- data.table(tmp)
tmp <- tmp[,label := paste(city,collapse = "\n"),by = key]
tmp <- tmp[,-1] %>% unique()
p <- ggplot(tmp,aes(x = Qiso,y = Qtime,label = label)) + theme_linedraw() + titles + geom_text(size = 3) +
  facet_grid(stage_time~stage_iso,scales = "free_y") + 
  xlab("Quartile of median relative isolation index") + ylab("Quartile of length of stage")+
  geom_hline(yintercept = c(1.5,2.5,3.5))

#Isolation x Incidence upward phase
model <- lapply(as.list(levels(data_upward$city)),function(x) loess(inc7 ~ iso714, 
                                                                    data = data_upward[data_upward$city == x,],
                                                                    span = 0.75))
names(model) <- levels(data_upward$city)
tab <- data.frame("city" = NA,"ass" = NA)
for(c in levels(data_upward$city)){
  f <- function(x){predict(model[[c]],x)}
  m <- min(model[[c]]$x)
  M <- max(model[[c]]$x)
  d <- integrate(f,m,M)
  f <- function(x){predict(model[[c]],x)/d$value}
  mean <- integrate(function(x) x*f(x),m,M)$value
  sigma <- sqrt(integrate(function(x) f(x)*(x-mean)^2,m,M)$value)
  skew <- (1/sigma^3)*integrate(function(x) f(x)*(x-mean)^3,m,M)$value
  tab <- rbind.data.frame(tab,data.frame("city" = c,"ass" = skew))
}
tab <- tab[-1,]
tab$label <- paste("S =",round(tab$ass,2))
tab$xpos <- c(-Inf)
tab$ypos <- c(Inf)
for(c in levels(data_upward$city)){
  data_upward$stage[data_upward$city == c & is.na(data_upward$stage) & data_upward$date <= na.omit(data_upward$date[data_upward$city == c & data_upward$stage == 2])] <- 1
  data_upward$stage[data_upward$city == c & is.na(data_upward$stage) & data_upward$date <= na.omit(data_upward$date[data_upward$city == c & data_upward$stage == 3])] <- 2
  data_upward$stage[data_upward$city == c & is.na(data_upward$stage) & data_upward$date <= na.omit(data_upward$date[data_upward$city == c & data_upward$stage == 4])] <- 3
}
data_upward$stage[data_upward$stage == 4] <- 3
data_upward$stage <- factor(data_upward$stage)
p <- ggplot(data_upward,aes(x = iso714,y = inc7,group = city,colour = stage)) + theme_linedraw() + titles + 
  geom_point(pch = 3) + 
  facet_wrap(~city,scales = "free",ncol = 3) +
  stat_smooth(geom = "line",alpha = 0.5,se = F,color= "black") +# geom_line(alpha = 0.5) + 
  geom_text(data = tab,aes(x = xpos,y = ypos,label = label),hjust = -0.1,vjust = 2,color = "black") +
  ylab("7-day avarage of nowcasted incidence") + xlab("Mean relative isolation index of one week ago") +
  scale_colour_discrete("Stage")
pdf("IsolXIncUpward.pdf",width = 15,height = 20)
p
dev.off()

#S x Length stages
data_speed <- merge(data_speed,tab[,1:2])
tmp <- data_speed[,c(1,4,5,6)] %>% unique()
p <- ggplot(tmp,aes(x = ass,y = time)) + theme_linedraw() + titles + geom_point() +
  facet_wrap(.~stage_time,scales = "free_y") + stat_cor() + geom_smooth(se = F,method = "lm",color = "black") +
  xlab("Skewness") + ylab("Length of stage")
pdf("CorStageSkewLength.pdf",width = 15,height = 5)
p
dev.off()

#Peak for each city
peak <- tapply(X = data_upward$nowcasting,INDEX = data_upward$city,FUN = function(x) max(x,na.rm = T))
peak <- data.frame("city" = names(peak),"peak" = peak)
peak <- merge(peak,tab[,1:2])
p <- ggplot(peak,aes(x = ass,y = peak)) + theme_linedraw() + titles + geom_point() +
  stat_cor() + geom_smooth(se = F,method = "lm",color = "black") +
  xlab("Skewness") + ylab("Incidence on peak")
pdf("CorPeakSkew.pdf",width = 15,height = 5)
p
dev.off()

#Model
data_speed_gather <- merge(data_speed_gather,peak)
data_speed_gather <- merge(data_speed_gather,idh)
tmp <- data_speed_gather
names(tmp) <- c("city","S1I","S2I","S3I","S1L","S2L","S3L","Uplength","peak","ass","idh")
tmp$ass <- cut(x = tmp$ass,breaks = c(-Inf,0,Inf),labels = c("Isolation as remedy","Isolation as prevention"),include.lowest = T)
mod <- lm(peak ~ ass,tmp)
summary(mod)
table(tmp$ass)

names(data_speed_gather)[c(9,11)] <- c("Incidence on peak","HDI")
tmp_gather <- data_speed_gather %>% gather("var","value",-city,-ass)
tmp_gather$ass <- cut(x = tmp_gather$ass,breaks = c(-Inf,0,Inf),labels = c("Isolation as remedy","Isolation as prevention"),include.lowest = T)

pdf("BoxPlots.pdf",width = 15,height = 25)
ggplot(tmp_gather,aes(x = ass,y = value)) + theme_linedraw() + titles + geom_boxplot() + 
  facet_wrap(~var,scale = "free",ncol = 2) + xlab("") + ylab("") + 
  stat_compare_means(label.x.npc = 0,label.y.npc = 1)
dev.off()
tab <- NULL
for(v in names(tmp)[c(2:9,11)]){
  if(is.null(tab))
    tab <- data.frame("Var" = v,"Cat" = c("Isolation as remedy","Isolation as prevention"),autoAnalise::auto_resumo(x = tmp$ass,y = tmp[[v]]))
  else
    tab <- rbind.data.frame(tab,data.frame("Var" = v,"Cat" = c("Isolation as remedy","Isolation as prevention"),autoAnalise::auto_resumo(x = tmp$ass,y = tmp[[v]])))
}
print(xtable(tab),include.rownames = F)
