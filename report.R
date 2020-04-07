#Build plots for report

library(ggplot2)
library(rgdal)
library(tidyverse)
library(dplyr)
require(stringi)
library(plyr)
library(rgeos)
library(geosphere)

titles <- theme(strip.text = element_text(size = 12), axis.text = element_text(size = 12,color = "black"), 
                axis.title = element_text(size = 14), legend.text = element_text(size = 14),
                legend.title = element_text(size = 14),
                panel.border = element_blank(),
                panel.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                legend.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                legend.position="bottom",legend.spacing.x = unit(0.5, 'cm'))
themes <- list(theme_linedraw())

######Descriptive Analysis####
#Number of users
data <- read_ods("./tabulatedData/user_per_month.ods")
names(data) <- c("day","São Paulo - 2019","São Paulo - 2020","Rio de Janeiro - 2019",
                 "Rio de Janeiro - 2020")
data <- data[-c(1:2,33),]
data <- data %>% gather("local_ano","Users",-day)
data$City <- unlist(lapply(strsplit(data$local_ano,split = "-"),function(x) x[1]))
data$Year <- unlist(lapply(strsplit(data$local_ano,split = "-"),function(x) x[2]))
data$Weekend <- ifelse(wday(ymd(paste(data$Year,"-03-",data$day,sep = "")),label = T) %in% c("sáb","dom"),"Weekend","Weekday")
for(i in c(2,4:6))
  data[,i] <- factor(data[,i])
data$day <- as.numeric(data$day)
data$Users <- as.numeric(data$Users)
data$City <- mapvalues(data$City,c("São Paulo ","Rio de Janeiro "),c("SP","RJ"))
data$half <- factor(ifelse(data$day >= 15,"Second","First"))
data$key <- factor(paste(data$City,data$Year,data$halfs,data$Weekend))
tapply(data$Users,data$key,summary)
summary(data)
scales_y <- list(
  SP = scale_y_continuous(breaks = seq(2e6,8e6,1e6),
                                   label = c("2,000,000","3,000,000","4,000,000","5,000,000",
                                             "6,000,000","7,000,000","8,000,000")),
  RJ = scale_y_continuous(breaks = seq(300000,1600000,200000),
                                        label = c("300,000","500,000","700,000","900,000",
                                                  "1,100,000","1,300,000","1,500,000"))
)

p <- ggplot(data,aes(x = day,y = Users,pch = Weekend,linetype = Year,group = Year)) + themes + titles +
  ylab("Total Number of Recordings") + xlab("Day of March") +
  geom_point(size = 3) + geom_line() + scale_shape_manual("",values = c(19,0)) + 
  facet_grid_sc(rows = vars(City),scales = list(y = scales_y)) + 
  scale_x_continuous(breaks = seq(1,30,2))
pdf(file = "total_use.pdf",width = 10,height = (2/3)*10)
p
dev.off()

data$key2 <- factor(paste(data$half,data$Weekend))
p <- ggplot(data,aes(x = key2,y = Users,fill = Year)) + themes + titles + geom_boxplot() +
  facet_grid_sc(rows = vars(City),scales = list(y = scales_y)) + 
  xlab("Half of March and Day of the Week") + ylab("Total Number of Recordings") +
  scale_fill_manual(values = c("white","gray")) +
  geom_vline(xintercept = 2.5,linetype = "dashed")
pdf(file = "total_use_boxplot.pdf",width = 10,height = (2/3)*10)
p
dev.off()

#Plot relation lambda x transition probabilities
# dados <- data.frame(x = c(0,1))
# f <- function(x){
#   (x < 0.25)*3 + (x >= 0.25)*(3/0.75)*(1 - x)
# }
# 
# p <- ggplot(dados,aes(x = x)) + themes + titles + stat_function(fun = f) + 
#   scale_x_continuous(breaks = c(0,0.25,1),labels = c("0",expression(hat(p)[ii]^{M}),"1")) +
#   xlab(expression(hat(p)[ii]^{n})) + ylab(expression(lambda~"(i,t)")) +
#   scale_y_continuous(breaks = c(0,3),labels = c("0",expression(lambda)),limits = c(0,3.5)) +
#   geom_segment(aes(x = 0.25,xend = 0.25,y = 0,yend = 3),linetype = "dashed")
# pdf(file = "relation_lambda.pdf",width = 10,height = (2/3)*10)
# p
# dev.off()

#######Population Dynamics#####

setwd("~/mobilidade/mdyn/output/")

#Params
states <- c("rio","sao_paulo")
f_mat_new <- list(rio = "move_mat_RIO DE JANEIRO_RJ-Municipios_model.csv",sao_paulo = "move_mat_SÃO PAULO_SP-Municipios_model.csv")
f_mat <- list(rio = "move_mat_RIO DE JANEIRO_RJ-Municipios.csv",sao_paulo = "move_mat_SÃO PAULO_SP-Municipios.csv")
shape_rj <- readOGR(dsn = "~/mobilidade/mdyn/maps/rj_municipios/33MUE250GC_SIR.shp",stringsAsFactors = F)
shape_sp <- readOGR(dsn = "~/mobilidade/mdyn/maps/sp_municipios/35MUE250GC_SIR_mdyn.shp",stringsAsFactors = F)
n_mat <- list(sao_paulo = c(shape_sp$NM_MUNICIP,"MINAS GERAIS","RIO DE JANEIRO","PARANÁ","MATO GROSSO DO SUL"),rio = c(shape_rj$NM_MUNICIP,"MINAS GERAIS","ESPIRITO SANTO","SÃO PAULO"))
pos_mat <- list(sao_paulo = 269,rio = 18)
pop_rj <- read.csv("~/mobilidade/mdyn/maps/population/population_rj.csv",sep = ";")
pop_rj$municipio <- toupper(pop_rj$municipio)
pop_sp <- read.csv("~/mobilidade/mdyn/maps/population/population_sp.csv",sep = ";")
pop_sp$municipio <- toupper(pop_sp$municipio)
pop <- list(rio = pop_rj,sao_paulo = pop_sp )

#Rank study by city
d <- list("2018" = 1:31,"2020" = 1:30)
tab <- list(sao_paulo = data.frame(),rio = data.frame())

for(city in states)
  for(year in c("2018","2020"))
    for(day in d[[year]]){
      cat("\n")
      cat(paste(city,"-",year,"-03-",day,sep = ""))
      if(as.numeric(day) < 10)
        day_mod <- paste("0",day,sep = "")
      r_year <- ifelse(year == "2018","2019",year)
      f <- paste("/storage/inloco/data/visit_journey_",city,"_",year,"/dt=",r_year,"-03-",day_mod,"/",f_mat[[city]],sep = "")
      mat <- fread(file = f,data.table = F)
      colnames(mat) <- n_mat[[city]]
      tmp <- data.frame(rbind(rank(mat[,pos_mat[[city]]],ties.method = "average")))
      tmp$day <- day
      tmp$year <- r_year
      tmp$city <- city
      tmp$month <- "March"
      colnames(tmp)[1:length(n_mat[[city]])] <- n_mat[[city]]
      if(nrow(tab[[city]]) > 0)
        tab[[city]] <- rbind.data.frame(tab[[city]],tmp)
      else
        tab[[city]] <- tmp
            
      #Matriz por model
      for(col in 1:ncol(mat)){
        mat[,col] <- mat[,col]/pop[[city]]$populacao_estimada[pop[[city]]$municipio == colnames(mat)[col]]
        mat[col,col] <- 1 - sum(mat[-col,col])
      }
      if(sum(is.na(mat)) > 0)
        cat("Problem at new matrix!!")
      colnames(mat) <- NULL
      rownames(mat) <- NULL
      f2 <- paste("/storage/inloco/data/visit_journey_",city,"_",year,"/dt=",r_year,"-03-",day_mod,"/",f_mat_new[[city]],sep = "")
      write.csv(mat,file = f2,row.names = F,col.names = F)
    }
write.csv(x = tab[["sao_paulo"]],file = "rank_sao_paulo_municipios_March_2019_2020.csv")
write.csv(x = tab[["rio"]],file = "rank_rio_municipios_March_2019_2020.csv")

#Statistics
tab <- list()

tab[["sao_paulo"]] <- read.csv(file = "rank_sao_paulo_municipios_March_2019_2020.csv")
tab[["sao_paulo"]]$X <- NULL
w <- vector()
for(i in 1:(ncol(tab[["sao_paulo"]])-4))
  w[i] <- mean(tab[["sao_paulo"]][tab[["sao_paulo"]]$year == "2020",i])
names(w) <- names(tab[["sao_paulo"]])[c(1:(ncol(tab[["sao_paulo"]])-4))]
w <- w[order(w,decreasing = T)]
tab[["sao_paulo"]] <- tab[["sao_paulo"]][,colnames(tab[["sao_paulo"]]) %in% c(names(w)[2:16],"day","year","city","month")]
tab[["sao_paulo"]] <- tab[["sao_paulo"]] %>% gather("City","Rank",-day,-year,-city,-month)
View(tab[["sao_paulo"]])

tab[["rio"]] <- read.csv(file = "rank_rio_municipios_March_2019_2020.csv")
tab[["rio"]]$X <- NULL
w <- vector()
for(i in 1:(ncol(tab[["rio"]])-4))
  w[i] <- mean(tab[["rio"]][tab[["rio"]]$year == "2020",i])
names(w) <- names(tab[["rio"]])[c(1:(ncol(tab[["rio"]])-4))]
w <- w[order(w,decreasing = T)]
tab[["rio"]] <- tab[["rio"]][,colnames(tab[["rio"]]) %in% c(names(w)[2:16],"day","year","city","month")]
tab[["rio"]] <- tab[["rio"]] %>% gather("City","Rank",-day,-year,-city,-month)
View(tab[["rio"]])

#####Statistical Analysis of Model Simulations#####

setwd("~/mobilidade/mdyn/output")

name <- list(SP = "SÃO PAULO_SP-Municipios",RJ = "RIO DE JANEIRO_RJ-Municipios")
shape_rj <- readOGR(dsn = "~/GDrive/github/mdyn/maps/rj_municipios/33MUE250GC_SIR.shp",stringsAsFactors = F)
shape_sp <- readOGR(dsn = "~/GDrive/github/mdyn/maps/sp_municipios/35MUE250GC_SIR_mdyn.shp",stringsAsFactors = F)
n_mat <- list(SP = c(shape_sp$NM_MUNICIP,"MINAS GERAIS","RIO DE JANEIRO","PARANÁ","MATO GROSSO DO SUL"),
              RJ = c(shape_rj$NM_MUNICIP,"MINAS GERAIS","ESPIRITO SANTO","SÃO PAULO"))
tab <- list(SP = data.frame(),RJ = data.frame())


for(city in c("SP","RJ"))
  for(s in c("001","005","01","02","04","05","06","08","10","12","14","16","18","20","25","30")){
    f <- paste("~/GDrive/github/mdyn/output/Model_time/Model_",name[[city]],"_2020-03-01_2020-03-30_r04_s",s,"_risk_index_time_list.csv",sep = "")
    tmp <- read.csv(f,header = T)
    n <- tmp$Region
    tmp$Region <- NULL
    tmp$Index <- NULL
    tmp <- data.frame(t(as.matrix(tmp)))
    colnames(tmp) <- n
    tmp$s <- s
    if(nrow(tab[[city]]) == 0)
      tab[[city]] <- tmp
    else
      tab[[city]] <- rbind.data.frame(tab[[city]],tmp)
  }
write.csv(file = "rank_model_sp.csv",x = tab[["SP"]])
write.csv(file = "rank_model_rj.csv",x = tab[["RJ"]])

# tab <- list()
# tab[["SP"]] <- read.csv("rank_model_sp.csv",sep = ",",header = F)
# tab[["RJ"]] <- read.csv("rank_model_rj.csv",sep = ",",header = F)
# tab[["SP"]]$V1 <- NULL
# tab[["RJ"]]$V1 <- NULL
# n_mat <- list(SP = as.vector(as.matrix(tab[["SP"]])[1,]),
#               RJ = as.vector(as.matrix(tab[["RJ"]])[1,]))
# tab[["SP"]] <- read.csv("rank_model_sp.csv",sep = ",")
# tab[["RJ"]] <- read.csv("rank_model_rj.csv",sep = ",")
# tab[["SP"]]$X <- NULL
# tab[["RJ"]]$X <- NULL
# colnames(tab[["SP"]]) <- n_mat[["SP"]]
# colnames(tab[["RJ"]]) <- n_mat[["RJ"]]

tab[["SP"]]$s <- factor(c("0.001","0.005","0.1","0.2","0.4","0.5","0.6","0.8","1.0",
                   "1.2","1.4","1.6","1.8","2.0","2.5","3.0"))
tab[["RJ"]]$s <- factor(c("0.001","0.005","0.1","0.2","0.4","0.5","0.6","0.8","1.0",
                   "1.2","1.4","1.6","1.8","2.0","2.5","3.0"))
shape_rj <- readOGR(dsn = "~/GDrive/github/mdyn/maps/rj_municipios/33MUE250GC_SIR.shp",stringsAsFactors = F)
shape_sp <- readOGR(dsn = "~/GDrive/github/mdyn/maps/sp_municipios/35MUE250GC_SIR_mdyn.shp",stringsAsFactors = F)

rank <- list()
rank[["SP"]] <- tab[["SP"]]
rank[["RJ"]] <- tab[["RJ"]]
 
for(c in c("SP","RJ"))
  for(i in 1:nrow(rank[[c]])){
    rank[[c]][i,-ncol(rank[[c]])] <- dense_rank(as.numeric(as.character(rank[[c]][i,-ncol(rank[[c]])])))
    rank[[c]][i,is.na(rank[[c]][i,])] <- max(rank[[c]][i,-ncol(rank[[c]])],na.rm = T) + 1
  }
rank[["SP"]]$State <- "SP"
rank[["RJ"]]$State <- "RJ"
 
rank_invert <- list()
rank_invert[["SP"]] <- rank[["SP"]] %>% gather("City","Rank",-s,-State)
rank_invert[["SP"]]$City[rank_invert[["SP"]]$City == "RIO DE JANEIRO"] <- "RIO DE JANEIRO (S)"
rank_invert[["RJ"]] <- rank[["RJ"]] %>% gather("City","Rank",-s,-State)
rank_invert[["RJ"]]$City[rank_invert[["RJ"]]$City == "SÃO PAULO"] <- "SÃO PAULO (S)"
rank_invert <- rbind.data.frame(rank_invert[["SP"]],rank_invert[["RJ"]])
rank_invert$key <- paste(rank_invert$s,rank_invert$State,rank_invert$City)

tab_invert <- list()
tab[["SP"]]$State <- "SP"
tab[["RJ"]]$State <- "RJ"
tab_invert[["SP"]] <- tab[["SP"]] %>% gather("City","Time",-s,-State)
tab_invert[["SP"]]$City[tab_invert[["SP"]]$City == "RIO DE JANEIRO"] <- "RIO DE JANEIRO (S)"
tab_invert[["RJ"]] <- tab[["RJ"]] %>% gather("City","Time",-s,-State)
tab_invert[["RJ"]]$City[tab_invert[["RJ"]]$City == "SÃO PAULO"] <- "SÃO PAULO (S)"
tab_invert <- rbind.data.frame(tab_invert[["SP"]],tab_invert[["RJ"]])
tab_invert$key <- paste(tab_invert$s,tab_invert$State,tab_invert$City)
tab_invert[,c(1,2,3)] <- NULL

data <- merge(tab_invert,rank_invert)
data$key <- NULL

#SP
data_SP <- data[,-1] %>% spread(key = "s",value = "Rank") %>% filter(State == "SP")
set.seed(1)
c_SP_menor <- kmeans(data_SP[,c(3:10)],centers = 3)
rowMeans(c_SP_menor$centers)
data_SP$Grupo_menor <- factor(c_SP_menor$cluster)
data_SP$Grupo_menor <- mapvalues(data_SP$Grupo_menor,from = c("1","2","3"),to = c("Medium Risk",
                                                                                  "Low Risk","High Risk"))

set.seed(1)
c_SP_maior <- kmeans(data_SP[,c(11:18)],centers = 3)
rowMeans(c_SP_maior$centers)
data_SP$Grupo_maior <- factor(c_SP_maior$cluster)
data_SP$Grupo_maior <- mapvalues(data_SP$Grupo_maior,from = c("1","2","3"),to = c("Medium Risk",
                                                                                  "Low Risk","High Risk"))
table(data_SP$Grupo_menor,data_SP$Grupo_maior) #49 cidades

set.seed(1)
c_SP <- kmeans(data_SP[,c(3:18)],centers = 3)
rowMeans(c_SP$centers)
data_SP$Grupo <- factor(c_SP$cluster)
data_SP$Grupo <- mapvalues(data_SP$Grupo,from = c("1","2","3"),to = c("Medium Risk","Low Risk","High Risk"))
data_SP$Grupo <- factor(data_SP$Grupo,c("Low Risk","Medium Risk","High Risk"))

#Map
f <- fortify(shape_sp,region = "NM_MUNICIP")
f <- merge(f,data_SP[,c(2,21)],by.x = "id",by.y = "City")
map_SP <- ggplot() + theme_bw() + ylab("Latitude") + xlab("Longitude") +
  geom_polygon(data = f,aes(long, lat, group = group,fill = Grupo), color = "black") +
  scale_fill_manual("Risk",values = c("darkolivegreen2","orange","red")) + titles
pdf(file = "map_risk_SP.pdf",width = 10,height = (2/3)*10)
map_SP
dev.off()

#Plot
population <- read.csv("~/GDrive/github/mdyn/maps/population/population_sp.csv",sep = ";")
population$municipio <- toupper(population$municipio)
dist <- read.csv("~/GDrive/github/mdyn/tabulatedData/dist_SP.csv",sep = ";")
dist$cidade <- toupper(dist$cidade)
data_tmp <- merge(data,population,by.x = "City",by.y = "municipio")
data_tmp <- merge(data_tmp,dist,by.x = "City",by.y = "cidade",all.x = T)
cases <- read.csv("~/GDrive/github/mdyn/tabulatedData/covid19_06ABR.csv")
cases <- cases %>% filter(state == "SP" & date == "2020-04-06")
cases$city <- toupper(cases$city)
cases <- cases[,c(3,5)]
data_tmp <- merge(data_tmp,cases,by.x = "City",by.y = "city",all.x = T)
cases$city[!(cases$city %in% data_tmp$City)]
unique(data_tmp$City[!is.na(data_tmp$confirmed)]) 
summary(data_tmp)

tmp <- data_tmp %>% filter(populacao_estimada > 100000) %>% droplevels()
tmp$City <- factor(tmp$City,unique(tmp$City)[order(tapply(tmp$Rank,tmp$City,mean))])

tmp <- merge(tmp,data_SP[,c(2,21)])
tmp$km <- as.numeric(as.character(tmp$km))
tmp <- tmp %>% filter(!(City %in% c("MINAS GERAIS","ESPIRITO SANTO")))

p <- ggplot(tmp,aes(x = City,y = Rank,colour = Grupo)) + 
  geom_point(aes(y = max(tmp$Rank,na.rm = T)-Rank)) + #facet_grid("State",scales = "free_x") +
  geom_smooth(data = unique(tmp[c(1,7,9)]),se = F,aes(x = City,y = (km/max(tmp$km,na.rm = T)) * max(tmp$Rank,na.rm = T),
                                                      colour = Grupo,group = 1),color = "gray") +
  geom_point(data = unique(tmp[c(1,7,9)]),aes(x = City,y = (km/max(tmp$km,na.rm = T)) * max(tmp$Rank,na.rm = T),colour = Grupo),pch = 2) + 
  themes + titles + theme(axis.text.x = element_text(angle = 90, hjust = 1),panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank()) + 
  scale_y_continuous(sec.axis = sec_axis(~.*(max(tmp$km,na.rm = T)/max(tmp$Rank,na.rm = T)), 
                                         name = "Distance to capital city (km)"),breaks = c(0,10,20,27),
                     labels = c("27","20","10","0")) +
  scale_colour_manual("Risk",values = c("orange","red")) 
pdf(file = "plot_risk_SP.pdf",width = 10,height = (2/3)*10)
p
dev.off()
write.csv(x = data_SP,file = "rank_risk_sp.csv",)

#RJ
data_RJ <- data[,-1] %>% spread(key = "s",value = "Rank") %>% filter(State == "RJ")
set.seed(1)
c_RJ_menor <- kmeans(data_RJ[,c(3:10)],centers = 3)
rowMeans(c_RJ_menor$centers)
data_RJ$Grupo_menor <- factor(c_RJ_menor$cluster)
data_RJ$Grupo_menor <- mapvalues(data_RJ$Grupo_menor,from = c("1","2","3"),to = c("Medium Risk",
                                                                                  "Low Risk","High Risk"))

set.seed(1)
c_RJ_maior <- kmeans(data_RJ[,c(11:18)],centers = 3)
rowMeans(c_RJ_maior$centers)
data_RJ$Grupo_maior <- factor(c_RJ_maior$cluster)
data_RJ$Grupo_maior <- mapvalues(data_RJ$Grupo_maior,from = c("1","2","3"),to = c("Medium Risk",
                                                                                  "Low Risk","High Risk"))
table(data_RJ$Grupo_menor,data_RJ$Grupo_maior) #29 cidades

set.seed(1)
c_RJ <- kmeans(data_RJ[,c(3:18)],centers = 3)
rowMeans(c_RJ$centers)
data_RJ$Grupo <- factor(c_RJ$cluster)
data_RJ$Grupo <- mapvalues(data_RJ$Grupo,from = c("1","2","3"),to = c("Medium Risk","Low Risk","High Risk"))
data_RJ$Grupo <- factor(data_RJ$Grupo,c("Low Risk","Medium Risk","High Risk"))

#Map
f <- fortify(shape_rj,region = "NM_MUNICIP")
f <- merge(f,data_RJ[,c(2,21)],by.x = "id",by.y = "City")
map_RJ <- ggplot() + theme_bw() + ylab("Latitude") + xlab("Longitude") +
  geom_polygon(data = f,aes(long, lat, group = group,fill = Grupo), color = "black") +
  scale_fill_manual("Risk",values = c("darkolivegreen2","orange","red")) + titles
pdf(file = "map_risk_RJ.pdf",width = 10,height = (2/3)*10)
map_RJ
dev.off()

#Plot
population <- read.csv("~/GDrive/github/mdyn/maps/population/population_rj.csv",sep = ";")
population$municipio <- toupper(population$municipio)
shape_rj$dist <- NA
for(i in 1:nrow(shape_rj))
  shape_rj$dist[i] <- distHaversine(p1 = gCentroid(shape_rj[i,]),
                                    p2 = gCentroid(shape_rj[shape_rj$NM_MUNICIP == "RIO DE JANEIRO",]))/1000
dist <- data.frame("cidade" = shape_rj$NM_MUNICIP,"km" = shape_rj$dist)
data_tmp <- merge(data,population,by.x = "City",by.y = "municipio")
data_tmp <- merge(data_tmp,dist,by.x = "City",by.y = "cidade",all.x = T)
cases <- read.csv("~/GDrive/github/mdyn/tabulatedData/covid19_06ABR.csv")
cases <- cases %>% filter(state == "RJ" & date == "2020-04-06")
cases$city <- toupper(cases$city)
cases <- cases[-44,c(3,5)]
data_tmp <- merge(data_tmp,cases,by.x = "City",by.y = "city",all.x = T)
cases$city[!(cases$city %in% data_tmp$City)]
unique(data_tmp$City[!is.na(data_tmp$confirmed)]) 
summary(data_tmp)

tmp <- data_tmp %>% filter(populacao_estimada > 75000) %>% droplevels()
tmp$City <- factor(tmp$City,unique(tmp$City)[order(tapply(tmp$Rank,tmp$City,mean))])

tmp <- merge(tmp,data_RJ[,c(2,21)])
tmp$km <- as.numeric(as.character(tmp$km))
tmp <- tmp %>% filter(!(City %in% c("MINAS GERAIS","ESPIRITO SANTO")))

p <- ggplot(tmp,aes(x = City,y = Rank,colour = Grupo)) + 
  geom_point(aes(y = max(tmp$Rank,na.rm = T)-Rank)) + #facet_grid("State",scales = "free_x") +
  geom_smooth(data = unique(tmp[c(1,7,9)]),se = F,aes(x = City,y = (km/max(tmp$km,na.rm = T)) * max(tmp$Rank,na.rm = T),
                                                      colour = Grupo,group = 1),color = "gray") +
  geom_point(data = unique(tmp[c(1,7,9)]),aes(x = City,y = (km/max(tmp$km,na.rm = T)) * max(tmp$Rank,na.rm = T),colour = Grupo),pch = 2) + 
  themes + titles + theme(axis.text.x = element_text(angle = 90, hjust = 1),panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank()) + 
  scale_y_continuous(sec.axis = sec_axis(~.*(max(tmp$km,na.rm = T)/max(tmp$Rank,na.rm = T)), 
                                         name = "Distance to capital city (km)"),breaks = c(0,5,10,15,20),
                     labels = c("20","15","10","5","0")) +
  scale_colour_manual("Risk",values = c("orange","red")) 
pdf(file = "plot_risk_RJ.pdf",width = 10,height = (2/3)*10)
p
dev.off()
write.csv(x = data_RJ,file = "rank_risk_rj.csv",)

