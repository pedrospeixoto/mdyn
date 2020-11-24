################################################
#####COVID-19 and Dengue Incidence 2019-2020####
#####Diego Marcondes                        ####
#####dmarcondes@ime.usp.br                  ####
#####November 2020                          ####
################################################

#libraries
library(ggplot2)
library(tidyverse)
library(plyr)
library(readxl)
library(lubridate)
library(data.table)
library(rgdal)
library(readODS)
library(ggpubr)
library(localift) #Install from source at localift_0.2.0.tar.gz
library(car)
library(gridExtra)
library(ggpmisc)
library(ggrepel)
library(grid)
library(RColorBrewer)
source("residuos.R") #Residuals plot

#Auxiliary objects
titles <- theme(strip.text = element_text(size = 20), axis.text = element_text(size = 14,
                                                                               color = "black"), 
                axis.title = element_text(size = 18), legend.text = element_text(size = 15),
                plot.title = element_text(size = 20,face = "bold"),
                legend.title = element_text(size = 18,face = "bold"), panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(), panel.border = element_blank(),
                panel.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                legend.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                legend.position="bottom",legend.spacing.x = unit(0.5, 'cm'))
titles2 <- theme(strip.text = element_text(size = 16), axis.text = element_text(size = 10,
                                                                               color = "black"), 
                axis.title = element_text(size = 14), legend.text = element_text(size = 11),
                plot.title = element_text(size = 16,face = "bold"),
                legend.title = element_text(size = 12,face = "bold"), panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(), panel.border = element_blank(),
                panel.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                legend.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                legend.position="bottom",legend.spacing.x = unit(0.5, 'cm'))
capitais <- c("Salvador","Aracaju","Recife","Maceió","Natal","João Pessoa","Fortaleza","São Luís","Teresina","Manaus","Belém","Palmas",
              "Porto Velho","Boa Vista","Rio Branco","Macapá","São Paulo","Rio de Janeiro","Belo Horizonte","Vitória","Brasília",
              "Goiânia","Campo Grande","Cuiabá","Porto Alegre","Florianópolis","Curitiba")
estados <- c("BA","SE","PE","AL","RN","PB","CE","MA","PI","AM","PA","TO","RO","RR","AC","AP","SP","RJ","MG","ES","DF","GO","MS","MT","RS","SC","PR")

#Read data
data <- readRDS("data_week.rds") #Cases per week
data_total <- readRDS("data_total.rds") #Cases total mid-19 to mid-20
name_state <- readRDS("name_state.rds") #Names of states
name_region <- readRDS("name_region.rds") #Names of regions
load("brasil.dengue.covid.final") #Division of citiesin Micro, Meso and Region
brasil <- brasil %>% dplyr::select(nome,City,State,MicroRegion,MesoRegion,Region) %>% unique() #Get division
brasil$City <- substr(x = brasil$City,1,6) #IBGE 6 digit
names(brasil)[2] <- "IBGE" #Name collumn cities
for(i in 1:6) #Factor
  brasil[,i] <- factor(brasil[,i])
world <- readRDS("world.rds") #Data of world
data_total$IBGE <- data_total$IBGE %>% filter(cases_100k > 0) #Only cities with COVID cases
lapply(data_total,nrow) #Length data each level of hierarch
iso <- read.csv("iso_index.csv") #Isolation index
iso$X <- NULL
iso$day <- ymd(iso$day)
iso <- iso %>% filter(day >= ymd("2020-03-20") & day <= ymd("2020-06-30"))
iso <- data.table(iso)
iso <- iso[,isoMean := mean(iso),by = ibge_cod]
iso <- iso %>% dplyr::select(ibge_cod,isoMean) %>% unique()
iso$ibge_cod <- substr(iso$ibge_cod,1,6)

#####Incidence overtime####
for(h in names(brasil)[c(2,3,6)]){ #For city, State, Region
  tmp <- data[[h]]
  if(h == "IBGE"){
    tmp <- merge(tmp,brasil %>% dplyr::select(IBGE,State,nome) %>% unique()) #Get state number
    tmp$State <- as.factor(tmp$State)
    tmp <- merge(tmp,name_state %>% unique() %>% na.omit(),by = "State") #Merge with state name
    tmp$key <- paste(tmp$nome,tmp$name) #Key is name + state
    tmp <- tmp %>% filter(key %in% paste(toupper(capitais),estados)) %>% droplevels() #Only capital cities
    names(tmp)[15] <- "wrap" #Variable to separate plots
  }
  else if(h == "State"){
    tmp$State <- as.factor(tmp$State)
    tmp <- merge(tmp,name_state %>% unique() %>% na.omit(),by = "State") #Get state name
    names(tmp)[9] <- "wrap" #Variable to separate plots
  }
  else{
    tmp$Region <- factor(tmp$Region)
    tmp <- merge(tmp,name_region) #Get region name 
    names(tmp)[9] <- "wrap" #Variable to separate plots
  }
  
  #Incidence at peak
  tmp2 <- data.table(tmp)
  tmp2 <- tmp2[,pointDengue := dengue_week_100k == max(dengue_week_100k,na.rm = T),by = wrap] #Get week of maximum dengue incidence
  tmp2$pointDengue <- round(ifelse(tmp2$pointDengue,tmp2$dengue_week_100k,0),2) #Where maximum, get the maximum
  tmp2 <- tmp2 %>% filter(pointDengue !=  0) %>% dplyr::select(wrap,week,pointDengue) %>% unique() #Only weeks where maximum dengue
  tmp2$week <- ifelse(tmp2$week %in% c(27:29),as.numeric(as.character(tmp2$week)) + 2,tmp2$week) #Correct boundaries for better vizualization
  tmp2$time <- factor(tmp2$week,c(27:52,1:26))
  pointDengue <- tmp2
  
  tmp2 <- data.table(tmp)
  tmp2 <- tmp2[,pointCovid := cases_week_100k == max(cases_week_100k,na.rm = T),by = wrap] #Get week of maximum covid incidence
  tmp2$pointCovid <- round(ifelse(tmp2$pointCovid,tmp2$cases_week_100k,0),0) #Where maximum, get the maximum
  tmp2 <- tmp2 %>% filter(pointCovid !=  0) %>% dplyr::select(wrap,week,pointCovid) %>% unique() #Only weeks where maximum covid
  tmp2$week <- ifelse(tmp2$week == 26,25,tmp2$week) #Correct boundaries for better vizualization
  tmp2$time <- factor(tmp2$week,c(27:52,1:26))
  pointCovid <- tmp2
  
  #Plot
  breaks <- c(27:52,1:26)[c(1,seq(5,52,5))] #Breaks of plot axis
  tmp$time <- factor(tmp$week,c(27:52,1:26)) #Time in epi weeks
  tmp$threshold <- as.character(tmp$week*ifelse(tmp$cases_week_100k >= 10,1,NA)) #weeks with 10+ covid cases
  for(c in unique(tmp$wrap)){ #For each location
    tmp$dengue_week_100k[tmp$wrap == c] <- tmp$dengue_week_100k[tmp$wrap == c]/max(tmp$dengue_week_100k[tmp$wrap == c]) #Normalize dengue
    tmp$cases_week_100k[tmp$wrap == c] <- tmp$cases_week_100k[tmp$wrap == c]/max(tmp$cases_week_100k[tmp$wrap == c],na.rm = T) #Normalize covid
    tmp$threshold[tmp$wrap == c & !is.na(tmp$threshold) & tmp$week != min(tmp$week[tmp$wrap == c & !is.na(tmp$threshold)])] <- NA #Keep only first week with 10+ covid cases
  }
  if(h != "Region"){ #If not region
    p <- ggplot(tmp,aes(x = time)) + theme_linedraw() + titles + facet_wrap(~ wrap,ncol = 3) + 
      geom_line(aes(y = dengue_week_100k,group = wrap,colour = "Dengue fever")) +
      geom_line(aes(y = cases_week_100k,group = wrap,colour = "COVID-19")) +
      scale_colour_manual("",values = c("red","green3")) + geom_vline(aes(xintercept = threshold),linetype = "dashed") +
      scale_x_discrete(breaks = breaks) + xlab("Epidemiological Weeks from mid-2019 to mid-2020") +
      ylab("Incidence/Maximum incidence on the period") + scale_y_continuous(breaks = c(0,0.5,1),limits = c(0,1.2)) +
      geom_text(data = pointDengue,mapping = aes(x = time,y = 1.1,label = pointDengue),color = "green3") +
      geom_text(data = pointCovid,mapping = aes(x = time,y = 1.1,label = pointCovid),color = "red")
      pdf(file = paste("incidence_",h,".pdf",sep = ""),width = 20,height = 20)
      print(p)
      dev.off()
  }
  else{
    p <- ggplot(tmp,aes(x = time)) + theme_linedraw() + titles + facet_wrap(~ wrap,ncol = 2) + 
      geom_line(aes(y = dengue_week_100k,group = wrap,colour = "Dengue fever")) +
      geom_line(aes(y = cases_week_100k,group = wrap,colour = "COVID-19")) +
      scale_colour_manual("",values = c("red","green3")) + geom_vline(aes(xintercept = threshold),linetype = "dashed") +
      scale_x_discrete(breaks = breaks) + xlab("Epidemiological Weeks from mid-2019 to mid-2020") +
      ylab("Incidence/Maximum incidence on the period") + scale_y_continuous(breaks = c(0,0.5,1),limits = c(0,1.2)) +
      geom_text(data = pointDengue,mapping = aes(x = time,y = 1.1,label = pointDengue),color = "green3") +
      geom_text(data = pointCovid,mapping = aes(x = time,y = 1.1,label = pointCovid),color = "red")
    pdf(paste("incidence_",h,".pdf",sep = ""),width = 10,height = 7.5)
    print(p)
    dev.off()
  }
}
rm(p,pointCovid,pointDengue,tmp,tmp2,breaks,c,h,i)

#####Prevalence Dengue x COVID: Cluster Analysis#####
p <- list()
set.seed(534)
for(h in names(brasil)[c(2,4,5)]) #For each level get cluster
    data_total[[h]]$cluster <- factor(kmeans(x = scale(data_total[[h]][,2:3]),centers = 5)$cluster)
names_cluster <- list() #Name cluster for each level
names_cluster[["IBGE"]] <- c("Denv, COVID+++","Denv, COVID++","Denv++, COVID","Denv+, COVID","Denv, COVID+")
names_cluster[["MicroRegion"]] <- c("Denv+, COVID","Denv, COVID+++","Denv, COVID+",
                                    "Denv, COVID++","Denv++, COVID")
names_cluster[["MesoRegion"]] <- c("Denv++, COVID","Denv, COVID+","Denv+, COVID",
                                   "Denv, COVID++","Denv, COVID+++")
breaksC <- list()
breaksC[["IBGE"]] <- seq(0,1e6,1500)
breaksC[["MicroRegion"]] <- seq(0,1e6,10)
breaksC[["MesoRegion"]] <- seq(0,1e6,10)
breaksC[["State"]] <- seq(0,1e6,500)
breaksC[["Region"]] <- seq(0,1e6,2.5)

breaksD <- list()
breaksD[["IBGE"]] <- seq(0,1e6,5000)
breaksD[["MicroRegion"]] <- seq(0,1e6,100)
breaksD[["MesoRegion"]] <- seq(0,1e6,25)
breaksD[["State"]] <- seq(0,1e6,500)
breaksD[["Region"]] <- seq(0,1e6,2.5)

#Shapefile of states
shpEstados <- readOGR(dsn = "~/GDrive/mdyn/maps/UFEBRASIL/UFEBRASIL.shp",stringsAsFactors = F,verbose = F)
shpEstados <- fortify(shpEstados)

for(h in names(brasil)[c(2,4,5,6)]){
  if(!(h %in% c("State","Region"))){
    data_total[[h]]$cluster <- mapvalues(data_total[[h]]$cluster,1:5,names_cluster[[h]]) #Cluster names
    data_total[[h]]$cluster <- factor(data_total[[h]]$cluster,
                                      c("Denv+, COVID","Denv, COVID+","Denv, COVID++",
                                        "Denv, COVID+++","Denv++, COVID")) #Cluster order
    col <- c("darkgreen","gold2","orange","red","green3") #Cluster color
    tapply(data_total[[h]]$dengue_total_100k,data_total[[h]]$cluster,summary)
    tapply(data_total[[h]]$cases_100k,data_total[[h]]$cluster,summary)
    
    #Plot raw
    p <- ggplot(data_total[[h]],aes(x = dengue_total_100k,y = cases_100k,colour = cluster)) + 
      theme_linedraw() +
      titles2 + geom_point(size = 1) +
      xlab("Dengue fever cases per 100k inhabitants from mid-2019 to mid-2020") +
      ylab("COVID-19 cases per 100k inhabitants until mid-2020") +
      scale_colour_manual("Cluster",values = col) + theme(legend.position = "none") +
      scale_y_continuous(breaks = breaksC[[h]]) + scale_x_continuous(breaks = breaksD[[h]])
    pdf(paste("prevalence_",h,".pdf",sep = ""),width = 7.5,height = 7.5)
    print(p)
    dev.off()
    
    tmp <- data_total[[h]]
    #Log-log model
    modHlog <- lm(log(cases_100k) ~ log(dengue_total_100k+1),tmp)
    fHlog <- function(x){ exp(modHlog$coefficients[1] + modHlog$coefficients[2] * log(x+1))}
    s <- summary(modHlog)
    r <- paste("r =",round(cor(log(tmp$cases_100k),log(tmp$dengue_total_100k+1)),3))
    pvalue <- ifelse(s$coefficients[2,4] > 0.001,paste("p =",round(s$coefficients[2,4],3)),"p < 0.001")
    lab <- paste(r,"\n",pvalue,sep = "")
    grobHlog <- grobTree(textGrob(lab, x=0.8,  y=0.95, hjust=0,
                                  gp=gpar(fontsize=13, fontface="italic")))
    
    #Exponencial model
    modHexp <- lm(log(cases_100k) ~ dengue_total_100k,tmp)
    fHexp <- function(x){ exp(modHexp$coefficients[1] + modHexp$coefficients[2] * x)}
    s <- summary(modHexp)
    r <- paste("r =",round(cor(log(tmp$cases_100k),tmp$dengue_total_100k),3))
    pvalue <- ifelse(s$coefficients[2,4] > 0.001,paste("p =",round(s$coefficients[2,4],3)),"p < 0.001")
    lab <- paste(r,"\n",pvalue,sep = "")
    grobHexp <- grobTree(textGrob(lab, x=0.8,  y=0.95, hjust=0,
                                  gp=gpar(fontsize=13, fontface="italic")))
    
    #Log-Log
    p <- ggplot(data_total[[h]],aes(x = dengue_total_100k,y = cases_100k,colour = cluster)) + 
      theme_linedraw() + annotation_custom(grobHlog) +
      stat_function(fun = fHlog,linetype = "dashed",color = "black",size = 1) +
      titles2 + geom_point(size = 1,alpha = 0.5) +
      xlab("Dengue fever cases per 100k inhabitants from mid-2019 to mid-2020") +
      ylab("COVID-19 cases per 100k inhabitants until mid-2020") +
      scale_colour_manual("Cluster",values = col) + theme(legend.position = "none") +
      scale_y_continuous(breaks = breaksC[[h]]) + scale_x_continuous(breaks = breaksD[[h]])
    pdf(paste("prevalence_",h,"_LogLog.pdf",sep = ""),width = 7.5,height = 7.5)
    print(p)
    dev.off()
    
    #Exponential
    p <- ggplot(data_total[[h]],aes(x = dengue_total_100k,y = cases_100k,colour = cluster)) + 
      theme_linedraw() + annotation_custom(grobHexp) +
      stat_function(fun = fHexp,linetype = "dashed",color = "black",size = 1) +
      titles2 + geom_point(size = 1,alpha = 0.5) +
      xlab("Dengue fever cases per 100k inhabitants from mid-2019 to mid-2020") +
      ylab("COVID-19 cases per 100k inhabitants until mid-2020") +
      scale_colour_manual("Cluster",values = col) + theme(legend.position = "none") +
      scale_y_continuous(breaks = breaksC[[h]]) + scale_x_continuous(breaks = breaksD[[h]])
    pdf(paste("prevalence_",h,"_Exp.pdf",sep = ""),width = 7.5,height = 7.5)
    print(p)
    dev.off()
    
    #Mapa cluster
    tmp <- data_total[[h]] %>% dplyr::select(h,cluster) #Only cluster of each location
    tmp[[h]] <- factor(tmp[[h]])
    if(h != "IBGE")
      tmp <- merge(tmp,brasil) %>% dplyr::select(IBGE,cluster) %>% unique() #Get location of each city
    shp <- readRDS("~/GDrive/mdyn/maps/shp_brazil.rds") #Shapefile cities brazil
    shp$CD_GEOCMU <- substr(shp$CD_GEOCMU,1,6) #IBGE code 6 digits
    shp <- merge(shp,tmp,by.x = "CD_GEOCMU",by.y = "IBGE") #Merge data with shapefile
    shp@data$id <- rownames(shp@data) #Id
    shpf <- fortify(shp,region = "id") #Shapefile to data frame
    shp <- join(shpf,shp@data,by="id") #Get cluster of each city
    shp <- shp %>% filter(!is.na(shp$cluster)) #Erase cities without cluster
    
    p <- ggplot(shp, aes(x=long, y=lat, group=group))+ titles +
      geom_polygon(aes(fill=cluster))+ geom_polygon(data = shpEstados,mapping = aes(x=long, y=lat, group=group),
                                                    color = "black",fill = NA) + theme_void() + xlab("") + ylab("") +
      coord_fixed() + scale_fill_manual("Cluster",values = col) + theme(legend.position = "none")
    pdf(paste("mapa_cluster_",h,".pdf",sep = ""),width = 0.75*15,height = 0.75*10)
    print(p)
    dev.off()
    }
  else{
    h <- "State"
    tmp <- data_total[[h]]
    tmp <- merge(tmp,name_state %>% unique() %>% na.omit(),by = "State") #Get state name
    brasil[[h]] <- factor(brasil[[h]])
    tmp <- merge(tmp,brasil %>% dplyr::select(State,Region) %>% unique()) #Get region of each satate
    tmp$Region <- mapvalues(factor(tmp$Region),name_region$Region,name_region$name) #Get name region
    
    #Log-log model
    modSlog <- lm(log(cases_100k) ~ log(dengue_total_100k),tmp)
    fSlog <- function(x){ exp(modSlog$coefficients[1] + modSlog$coefficients[2] * log(x))}
    s <- summary(modSlog)
    r <- paste("r =",round(cor(log(tmp$cases_100k),log(tmp$dengue_total_100k)),3))
    pvalue <- ifelse(s$coefficients[2,4] > 0.001,paste("p =",round(s$coefficients[2,4],3)),"p < 0.001")
    lab <- paste(r,"\n",pvalue,sep = "")
    grobSlog <- grobTree(textGrob(lab, x=0.8,  y=0.95, hjust=0,
                              gp=gpar(fontsize=13, fontface="italic")))
    
    #Exponencial model
    modSexp <- lm(log(cases_100k) ~ dengue_total_100k,tmp)
    fSexp <- function(x){ exp(modSexp$coefficients[1] + modSexp$coefficients[2] * x)}
    s <- summary(modSexp)
    r <- paste("r =",round(cor(log(tmp$cases_100k),tmp$dengue_total_100k),3))
    pvalue <- ifelse(s$coefficients[2,4] > 0.001,paste("p =",round(s$coefficients[2,4],3)),"p < 0.001")
    lab <- paste(r,"\n",pvalue,sep = "")
    grobSexp <- grobTree(textGrob(lab, x=0.8,  y=0.95, hjust=0,
                                  gp=gpar(fontsize=13, fontface="italic")))
    
    #Log-Log
    ps <- ggplot(tmp,aes(x = dengue_total_100k,y = cases_100k,colour = Region)) + theme_linedraw() +
      titles2 + geom_text_repel(aes(label = name)) + geom_point() + annotation_custom(grobSlog) +
      xlab("Dengue fever cases per 100k inhabitants from mid-2019 to mid-2020") +
      ylab("COVID-19 cases per 100k inhabitants until mid-2020") + stat_function(fun = fSlog,group = 1,color = "black",
                                                                                 linetype = "dashed") +
      scale_y_continuous(breaks = breaksC[[h]]) + scale_x_continuous(breaks = breaksD[[h]])
    pdf("prevalence_State_LogLog.pdf",width = 7.5,height = 7.5)
    print(ps)
    dev.off()
    
    #Exponential
    ps <- ggplot(tmp,aes(x = dengue_total_100k,y = cases_100k,colour = Region)) + theme_linedraw() +
      titles2 + geom_text_repel(aes(label = name)) + geom_point() + annotation_custom(grobSexp) +
      xlab("Dengue fever cases per 100k inhabitants from mid-2019 to mid-2020") +
      ylab("COVID-19 cases per 100k inhabitants until mid-2020") + stat_function(fun = fSexp,group = 1,color = "black",
                                                                                 linetype = "dashed") +
      scale_y_continuous(breaks = breaksC[[h]]) + scale_x_continuous(breaks = breaksD[[h]])
    pdf("prevalence_State_Exp.pdf",width = 7.5,height = 7.5)
    print(ps)
    dev.off()
    
    h <- "Region"
    tmp <- data_total[[h]]
    tmp <- merge(tmp,name_region %>% unique() %>% na.omit(),by = "Region") #Get region name
    tmp$Region <- mapvalues(factor(tmp$Region),name_region$Region,name_region$name)
    
    #Log-Log model
    modRlog <- lm(log(cases_100k) ~ log(dengue_total_100k),tmp)
    fRlog <- function(x){ exp(modRlog$coefficients[1] + modRlog$coefficients[2] * log(x))}
    s <- summary(modRlog)
    r <- paste("r =",round(cor(log(tmp$cases_100k),log(tmp$dengue_total_100k)),3))
    pvalue <- ifelse(s$coefficients[2,4] > 0.001,paste("p =",round(s$coefficients[2,4],3)),"p < 0.001")
    lab <- paste(r,"\n",pvalue,sep = "")
    grobRlog <- grobTree(textGrob(lab, x=0.8,  y=0.95, hjust=0,
                              gp=gpar(fontsize=13, fontface="italic")))
    
    #Exponential model
    modRexp <- lm(log(cases_100k) ~ dengue_total_100k,tmp)
    fRexp <- function(x){ exp(modRexp$coefficients[1] + modRexp$coefficients[2] * x)}
    s <- summary(modRexp)
    r <- paste("r =",round(cor(log(tmp$cases_100k),tmp$dengue_total_100k),3))
    pvalue <- ifelse(s$coefficients[2,4] > 0.001,paste("p =",round(s$coefficients[2,4],3)),"p < 0.001")
    lab <- paste(r,"\n",pvalue,sep = "")
    grobRexp <- grobTree(textGrob(lab, x=0.8,  y=0.95, hjust=0,
                                  gp=gpar(fontsize=13, fontface="italic")))
    
    #Log-Log
    pr <- ggplot(tmp,aes(x = dengue_total_100k,y = cases_100k,colour = Region)) + theme_linedraw() +
      titles2 + geom_text(aes(label = name)) + annotation_custom(grobRlog) +
      xlab("Dengue fever cases per 100k inhabitants from mid-2019 to mid-2020") +
      ylab("COVID-19 cases per 100k inhabitants until mid-2020") +
      theme(legend.position = "none") + stat_function(fun = fRlog,group = 1,color = "black",linetype = "dashed") +
      scale_y_continuous(breaks = breaksC[[h]]) + scale_x_continuous(breaks = breaksD[[h]])
    pdf("prevalence_Region_LogLog.pdf",width = 7.5,height = 7.5)
    print(pr)
    dev.off()
    
    #Exponential
    pr <- ggplot(tmp,aes(x = dengue_total_100k,y = cases_100k,colour = Region)) + theme_linedraw() +
      titles2 + geom_text(aes(label = name)) + annotation_custom(grobRexp) +
      xlab("Dengue fever cases per 100k inhabitants from mid-2019 to mid-2020") +
      ylab("COVID-19 cases per 100k inhabitants until mid-2020") +
      theme(legend.position = "none") + stat_function(fun = fRexp,group = 1,color = "black",linetype = "dashed") +
      scale_y_continuous(breaks = breaksC[[h]]) + scale_x_continuous(breaks = breaksD[[h]])
    pdf("prevalence_Region_Exp.pdf",width = 7.5,height = 7.5)
    print(pr)
    dev.off()
  }
}

#Table Cluster x Region
tab <- NULL
for(h in names(brasil)[c(2,4,5)]){ #For each level of hierarch
  tmp <- data_total[[h]]
  brasil[[h]] <- factor(brasil[[h]])
  tmp <- merge(tmp,brasil %>% dplyr::select(h,Region) %>% unique()) #Get region of location
  tmp$Region <- factor(tmp$Region)
  tmp$Region <- mapvalues(tmp$Region,name_region$Region,name_region$name)
  t <- table(tmp$Region,tmp$cluster) #Table CLuster x Region
  tp <- prop.table(x = t,margin = 2) #Proportion per collumn table
  t <- addmargins(t) #Add margins
  for(i in 1:(nrow(t) - 1)) #Insert percentage next to frequency
    for(j in 1:(ncol(t) - 1))
      t[i,j] <- paste(t[i,j]," (",round(100*tp[i,j],1),"%)",sep = "")
  t[nrow(t),-ncol(t)] <- paste(t[nrow(t),-ncol(t)]," (",round(100*prop.table(as.numeric(t[nrow(t),-ncol(t)])),1),"%)",sep = "")
  t[-nrow(t),ncol(t)] <- paste(t[-nrow(t),ncol(t)]," (",round(100*prop.table(as.numeric(t[-nrow(t),ncol(t)])),1),"%)",sep = "")
  t1 <- matrix(nrow = nrow(t),ncol = ncol(t)) #Save result in matrix
  for(i in 1:nrow(t))
    for(j in 1:ncol(t))
      t1[i,j] <- t[i,j]
  colnames(t1) <- colnames(t)
  t <- data.frame("Location" = h,"Region" = rownames(t),t1) #Data frame
  colnames(t)[-c(1,2)] <- colnames(t1)
  if(is.null(tab)) #Store
    tab <- t
  else
    tab <- rbind.data.frame(tab,t)
  
}
tab
rm(grobHexp,grobHlog,grobRexp,grobRlog,grobSexp,grobSlog,modHexp,modHlog,modRexp,
   modRlog,modSexp,modSlog,p,names_cluster,pr,ps,s,shp,shpf,t,t1,tab,
   tmp,h,i,j,lab,pvalue,r,tp,fHexp,fHlog,fRexp,fRlog,fSexp,fSlog)

#####Cluster of cities####
tmp <- data_total$IBGE
set.seed(442)
tmp$cluster <- factor(kmeans(x = scale(tmp[,5:8]),centers = 5)$cluster)
tapply(tmp$dengue_total_100k,tmp$cluster,summary)
tapply(tmp$cases_100k,tmp$cluster,summary)

#Mapa cluster
tmp <- tmp %>% dplyr::select(IBGE,cluster) #Cluster of each city
shp <- readRDS("~/GDrive/mdyn/maps/shp_brazil.rds") #Shapefile cities Brazil
shp$CD_GEOCMU <- substr(shp$CD_GEOCMU,1,6) #IBGE code 6 digits
shp <- merge(shp,tmp,by.x = "CD_GEOCMU",by.y = "IBGE") #Mege shapefile anda data
shp@data$id <- rownames(shp@data) #ID
shpf <- fortify(shp,region = "id") #Shapefile to data frame
shp <- join(shpf,shp@data,by="id") #Get cluster of each city
shp <- shp %>% filter(!is.na(cluster)) #Erase cities without cluster

p <- ggplot(shp, aes(x=long, y=lat, group=group))+ titles +
  geom_polygon(aes(fill = cluster))+ geom_polygon(data = shpEstados,mapping = aes(x=long, y=lat, group=group),
                                                color = "black",fill = NA) + 
  theme_void() + xlab("") + ylab("") + 
  coord_fixed() + scale_fill_manual("Cluster",values = brewer.pal(5,"Spectral")) 
pdf(paste("mapa_cluster_factors.pdf",sep = ""),width = 0.75*15,height = 0.75*10)
print(p)
dev.off()

#Lift per cluster
tmp <- data_total$IBGE
set.seed(442)
tmp$cluster <- factor(kmeans(x = scale(tmp[,5:8]),centers = 5)$cluster)
tmp <- data.table(tmp)
cortarD <- function(x){ #Cut of dengue incidence
  cut(x = x,breaks = c(min(x),quantile(x,c(0.25,0.5,0.75)),max(x)),
      labels = c("Q1 Dengue","Q2 Dengue","Q3 Dengue","Q4 Dengue"),include.lowest = T)
}
cortarc <- function(x){ #Cut of covid incidence
  cut(x = x,breaks = c(min(x),quantile(x,c(0.25,0.5,0.75)),max(x)),
      labels = c("Q1 COVID","Q2 COVID","Q3 COVID","Q4 COVID"),include.lowest = T)
}
tmp <- tmp[,catdengue := cortarD(dengue_total_100k),by = cluster] #Cut dengue by cluster
tmp <- tmp[,catcovid := cortarc(cases_100k),by = cluster] #Cut covid by cluster

#Table
t <- table(tmp$catdengue,tmp$catcovid)
tp <- prop.table(x = t,margin = 1)
for(i in 1:nrow(t))
  for(j in 1:ncol(t))
    t[i,j] <- paste(t[i,j]," (",round(100*tp[i,j],1),"%)",sep = "")
chisq.test(x = tmp$catdengue,y = tmp$catcovid)

library(localift)
p <- plot_LiftTable(x = tmp$catdengue,y = factor(tmp$catcovid,levels(tmp$catcovid)[4:1]),y.lab = "",x.lab = "")
pdf("Lift_Inside_Cluster.pdf",height = 10,width = 10)
print(p)
dev.off()

tmp$cat <- factor(paste(tmp$catcovid,tmp$catdengue))
tmp$cor_COVIDr[tmp$catcovid == "Q1 COVID"] <- 0
tmp$cor_COVIDr[tmp$catcovid == "Q2 COVID"] <- 0.5
tmp$cor_COVIDr[tmp$catcovid == "Q3 COVID"] <- 2/3
tmp$cor_COVIDr[tmp$catcovid == "Q4 COVID"] <- 1
tmp$cor_COVIDg[tmp$catcovid == "Q1 COVID"] <- 1
tmp$cor_COVIDg[tmp$catcovid == "Q2 COVID"] <- 0.5
tmp$cor_COVIDg[tmp$catcovid == "Q3 COVID"] <- 1/3
tmp$cor_COVIDg[tmp$catcovid == "Q4 COVID"] <- 0
tmp$cor_COVIDb[tmp$catcovid == "Q1 COVID"] <- 0
tmp$cor_COVIDb[tmp$catcovid == "Q2 COVID"] <- 0
tmp$cor_COVIDb[tmp$catcovid == "Q3 COVID"] <- 1
tmp$cor_COVIDb[tmp$catcovid == "Q4 COVID"] <- 0
tmp$cor_dengue[tmp$catdengue == "Q1 Dengue"] <- 0.25
tmp$cor_dengue[tmp$catdengue == "Q2 Dengue"] <- 0.5
tmp$cor_dengue[tmp$catdengue == "Q3 Dengue"] <- 0.75
tmp$cor_dengue[tmp$catdengue == "Q4 Dengue"] <- 1
cor <- tmp %>% dplyr::select(cat,cor_COVIDr,cor_COVIDg,cor_COVIDb,cor_dengue) %>% unique() %>% data.frame()
cor$cor <- rgb(cor$cor_COVIDr,cor$cor_COVIDg,cor$cor_COVIDb,cor$cor_dengue)
cor <- cor[order(cor$cat),]
tmp$cat <- factor(tmp$cat,cor$cat)
tmp$cat <- mapvalues(tmp$cat,levels(tmp$cat),c("Q1 COV Q1 Den","Q1 COV Q2 Den","Q1 COV Q3 Den",
                                               "Q1 COV Q4 Den","Q2 COV Q1 Den",
                                               "Q2 COV Q2 Den","Q2 COV Q3 Den","Q2 COV Q4 Den","Q3 COV Q1 Den",
                                               "Q3 COV Q2 Den","Q3 COV Q3 Den","Q3 COV Q4 Den","Q4 COV Q1 Den",
                                               "Q4 COV Q2 Den","Q4 COV Q3 Den","Q4 COV Q4 Den"))

#Mapa cluster
tmp <- tmp %>% dplyr::select(IBGE,cat)
shp <- readRDS("~/GDrive/mdyn/maps/shp_brazil.rds") #Shapefile cities Brazil
shp$CD_GEOCMU <- substr(shp$CD_GEOCMU,1,6) #IBGE code 6 digits
shp <- merge(shp,tmp,by.x = "CD_GEOCMU",by.y = "IBGE") #Merge shape with data
shp@data$id <- rownames(shp@data) #Id
shpf <- fortify(shp,region = "id") #Shape to data frame
shp <- join(shpf,shp@data,by="id") #Join shape with factor
shp <- shp %>% filter(!is.na(cat)) #Only cities with data

p <- ggplot(shp, aes(x=long, y=lat, group=group))+ titles +
  geom_polygon(aes(fill = cat))+ geom_polygon(data = shpEstados,mapping = aes(x=long, y=lat, group=group),
                                                  color = "black",fill = NA) + 
  theme_void() + xlab("") + ylab("") + theme(legend.position="bottom") +
  coord_fixed() + scale_fill_manual("",values = c(cor$cor))
pdf(paste("mapa_quartiles.pdf",sep = ""),width = 0.75*15,height = 0.75*10)
print(p)
dev.off()
rm(cor,p,shp,shpf,tmp,i,j,t,tp,cortarc,cortarD)

#####Regression#####

#Model
data_total$IBGE$trans_cases_100k <- log(data_total$IBGE$cases_100k) #Transforms incidence COVID
tmp <- merge(data_total$IBGE,iso,by.x = "IBGE",by.y = "ibge_cod",all = T)
formula <- trans_cases_100k ~ log(estimated_population) + log(density) + log(pib_per_capta) + idhm + dengue_total_100k + isoMean #Formula
mod <- lm(formula,data = tmp)#data_total$IBGE) #Model
s <- summary(mod) #Summary
s$coefficients <- data.frame(s$coefficients) #Coefficientes
s$coefficients$LI <- s$coefficients[,1] - qnorm(0.975)*s$coefficients[,2] #Inferior limit
s$coefficients$LS <- s$coefficients[,1] + qnorm(0.975)*s$coefficients[,2] #Superior limit
100*(exp(5000*mod$coefficients[6])-1)
100*(exp(5000*mod$coefficients[6] + 5000*qnorm(p = 0.975)*s$coefficients[6,2])-1)
100*(exp(5000*mod$coefficients[6] - 5000*qnorm(p = 0.975)*s$coefficients[6,2])-1)

#Residuals analysis
r <- resanalysis(mod)
pdf(file = "residuals.pdf",width = 15,height = 7.5)
r <- resanalysis(mod)
dev.off()
rm(mod,r,s,formula)

#World
for(g in na.omit(unique(world$Region))){
  if(g == "Africa"){
    v <- "Denv_igg"
    labX <- "Dengue fever igg (%)"
  }
  else{
    v <- "Denv_inc_2019_2020_rep"
    labX <- "Dengue fever incidence"
  }
  cat("\n")
  cat(g)
  cat("\n")
  if(!(g %in% c("Mexico"))){
    tmp <- world %>% filter(Region == g) %>% dplyr::select(v,covid_inc1,Subregion) %>% na.omit() #Only data of region
    
    #Log-Log model
    if(g == "Island"){
      modLog <- lm(paste("log(tmp$covid_inc1+1) ~ log(",v,"+1)",sep = ""),tmp)
      r <- paste("r =",round(cor(log(tmp$covid_inc1+1),log(tmp[[v]]+1)),3))
      fLog <- function(x){ exp(modLog$coefficients[1] + modLog$coefficients[2] * log(x+1))-1}
    }
    else{
      modLog <- lm(paste("log(tmp$covid_inc1) ~ log(",v,"+1)",sep = ""),tmp)
      fLog <- function(x){ exp(modLog$coefficients[1] + modLog$coefficients[2] * log(x+1))}
      r <- paste("r =",round(cor(log(tmp$covid_inc1),log(tmp[[v]]+1)),3))
    }
    s <- summary(modLog)
    pvalue <- ifelse(s$coefficients[2,4] > 0.001,paste("p =",round(s$coefficients[2,4],3)),"p < 0.001")
    lab <- paste(r,"\n",pvalue,sep = "")
    grobLog <- grobTree(textGrob(lab, x=0.8,  y=0.9, hjust=0,
                              gp=gpar(fontsize=13, fontface="italic")))
    
    #Exponential model
    if(g == "Island"){
      modExp <- lm(paste("log(tmp$covid_inc1+1) ~ ",v,sep = ""),tmp)
      fExp <- function(x){ exp(modExp$coefficients[1] + modExp$coefficients[2] * x)-1}
      r <- paste("r =",round(cor(log(tmp$covid_inc1+1),tmp[[v]]),3))
    }
    else{
      modExp <- lm(paste("log(tmp$covid_inc1) ~ ",v,sep = ""),tmp)
      fExp <- function(x){ exp(modExp$coefficients[1] + modExp$coefficients[2] * x)}
      r <- paste("r =",round(cor(log(tmp$covid_inc1),tmp[[v]]),3))
    }
    s <- summary(modExp)
    pvalue <- ifelse(s$coefficients[2,4] > 0.001,paste("p =",round(s$coefficients[2,4],3)),"p < 0.001")
    lab <- paste(r,"\n",pvalue,sep = "")
    grobExp <- grobTree(textGrob(lab, x=0.8,  y=0.9, hjust=0,
                                 gp=gpar(fontsize=13, fontface="italic")))
    
    #Log-Log
    p <- ggplot(tmp,aes_string(x = v,y = "covid_inc1",label = "Subregion")) + theme_linedraw() +
      titles2 + geom_text_repel(size = 3) + geom_point() +
      stat_function(fun = fLog,linetype = "dashed") + annotation_custom(grobLog) +
      xlab(labX) + ylab("COVID-19 incidence")
    pdf(paste("incidence_world_",g,"_LogLog.pdf",sep = ""),width = 7.5,height = 7.5)
    print(p)
    dev.off()
    
    #Exp
    p <- ggplot(tmp,aes_string(x = v,y = "covid_inc1",label = "Subregion")) + theme_linedraw() +
      titles2 + geom_text_repel(size = 3) + geom_point() +
      stat_function(fun = fExp,linetype = "dashed") + annotation_custom(grobExp) +
      xlab(labX) + ylab("COVID-19 incidence")
    pdf(paste("incidence_world_",g,"_Exp.pdf",sep = ""),width = 7.5,height = 7.5)
    print(p)
    dev.off()
  }
  else if(g == "Mexico"){
    tmp <- world %>% filter(Region == g) %>% dplyr::select(v,covid_inc1,Subregion,Group) %>% na.omit()
    for(a in unique(tmp$Group)){
      tmp1 <- tmp %>% filter(tmp$Group == a)
      
      #Log-Log
      modLog <- lm(paste("log(tmp1$covid_inc1) ~ log(",v,")",sep = ""),tmp1)
      fLog <- function(x){ exp(modLog$coefficients[1] + modLog$coefficients[2] * log(x))}
      s <- summary(modLog)
      r <- paste("r =",round(cor(log(tmp1$covid_inc1),log(tmp1[[v]])),3))
      pvalue <- ifelse(s$coefficients[2,4] > 0.001,paste("p =",round(s$coefficients[2,4],3)),"p < 0.001")
      lab <- paste(r,"\n",pvalue,sep = "")
      grobLog <- grobTree(textGrob(lab, x=0.8,  y=0.9, hjust=0,
                                gp=gpar(fontsize=13, fontface="italic")))
      
      #Exp
      modExp <- lm(paste("log(tmp1$covid_inc1) ~ ",v,sep = ""),tmp1)
      fExp <- function(x){ exp(modExp$coefficients[1] + modExp$coefficients[2] * x)}
      s <- summary(modExp)
      r <- paste("r =",round(cor(log(tmp1$covid_inc1),tmp1[[v]]),3))
      pvalue <- ifelse(s$coefficients[2,4] > 0.001,paste("p =",round(s$coefficients[2,4],3)),"p < 0.001")
      lab <- paste(r,"\n",pvalue,sep = "")
      grobExp <- grobTree(textGrob(lab, x=0.8,  y=0.9, hjust=0,
                                   gp=gpar(fontsize=13, fontface="italic")))
      
      #Log-Log 
      p <- ggplot(tmp1,aes_string(x = v,y = "covid_inc1",label = "Subregion")) + theme_linedraw() +
        titles2 + geom_text_repel(size = 3) + geom_point() + facet_grid(~Group) +
        stat_function(fun = fLog,linetype = "dashed") + annotation_custom(grobLog) +
        xlab("Dengue fever incidence") + ylab("COVID-19 incidence")
      pdf(paste("incidence_world_",g,gsub(" ","",a),"_LogLog.pdf",sep = ""),width = 7.5,height = 7.5)
      print(p)
      dev.off()
      
      #Exp
      p <- ggplot(tmp1,aes_string(x = v,y = "covid_inc1",label = "Subregion")) + theme_linedraw() +
        titles2 + geom_text_repel(size = 3) + geom_point() + facet_grid(~Group) +
        stat_function(fun = fExp,linetype = "dashed") + annotation_custom(grobExp) +
        xlab("Dengue fever incidence") + ylab("COVID-19 incidence")
      pdf(paste("incidence_world_",g,gsub(" ","",a),"_Exp.pdf",sep = ""),width = 7.5,height = 7.5)
      print(p)
      dev.off()
  }
  }
}
rm(grobExp,grobLog,modExp,modLog,p,s,tmp,tmp1,a,g,lab,labX,pvalue,r,v,fExp,fLog)

#Colombia
cat("\n")
cat("Colombia Double")
cat("\n")
v <- "Denv_inc_2019_2020_rep"
tmp <- world %>% filter(Region == "Colombia")
tmp$Population <- as.numeric(as.character(tmp$Population))
tmp$Group <- factor(ifelse(tmp$Population < 1000000,"Population lesser than 1,000,000","Population greater than 1,000,000"))

#> 1,000,000
tmp2 <- tmp %>% filter(Group == levels(tmp$Group)[1])
modGreat <- lm(log(covid_inc1) ~ log(Denv_inc_2019_2020_rep+1),tmp2)
fGreat <- function(x){ exp(modGreat$coefficients[1] + modGreat$coefficients[2] * log(x+1))}
s2 <- summary(modGreat)
r2 <- paste("r =",round(cor(log(tmp2$covid_inc1),log(tmp2[[v]]+1)),3))
pvalue <- ifelse(s2$coefficients[2,4] > 0.001,paste("p =",round(s2$coefficients[2,4],3)),"p < 0.001")
lab2 <- paste(r2,"\n",pvalue,sep = "")
grobGreat <- grobTree(textGrob(lab2, x=0.8,  y=0.8, hjust=0,
                          gp=gpar(fontsize=13, fontface="italic")))

p <- ggplot(tmp2,aes_string(x = v,y = "covid_inc1",label = "Subregion")) + theme_linedraw() +
    titles2 + geom_text_repel(size = 3) + geom_point() + #facet_grid(~Group) +
    annotation_custom(grobGreat) +
    stat_function(fun = fGreat,linetype = "dashed",color = "black") +
    xlab("Dengue fever incidence") + ylab("COVID-19 incidence")
pdf(paste("incidence_world_Colombia_GreatPop_LogLog.pdf",sep = ""),width = 7.5,height = 7.5)
print(p)
dev.off()
rm(grobGreat,modGreat,p,s2,tmp,tmp2,lab2,pvalue,r2,v,fGreat)

#Carebean
cat("\n")
cat("Carabean")
cat("\n")
v <- "Denv_inc_2019_2020_rep"
tmp <- world %>% filter(Region == "Carabean")
set.seed(10)
tmp$cluster <- factor(kmeans(scale(tmp[,c(6,7)]),3)$cluster)

#Log-Log
modLog <- lm(paste("log(tmp$covid_inc1) ~ log(",v,"+1)",sep = ""),tmp)
fLog <- function(x){ exp(modLog$coefficients[1] + modLog$coefficients[2] * log(x+1))}
s <- summary(modLog)
r <- paste("r =",round(cor(log(tmp$covid_inc1),log(tmp[[v]]+1)),3))
pvalue <- ifelse(s$coefficients[2,4] > 0.001,paste("p =",round(s$coefficients[2,4],3)),"p < 0.001")
lab <- paste(r,"\n",pvalue,sep = "")
grobLog <- grobTree(textGrob(lab, x=0.8,  y=0.9, hjust=0,
                             gp=gpar(fontsize=13, fontface="italic")))

#Exp
modExp <- lm(paste("log(tmp$covid_inc1) ~ ",v,sep = ""),tmp)
fExp <- function(x){ exp(modExp$coefficients[1] + modExp$coefficients[2] * x)}
s <- summary(modExp)
r <- paste("r =",round(cor(log(tmp$covid_inc1),tmp[[v]]),3))
pvalue <- ifelse(s$coefficients[2,4] > 0.001,paste("p =",round(s$coefficients[2,4],3)),"p < 0.001")
lab <- paste(r,"\n",pvalue,sep = "")
grobExp <- grobTree(textGrob(lab, x=0.8,  y=0.9, hjust=0,
                             gp=gpar(fontsize=13, fontface="italic")))

#LogDengue
modLogD <- lm(paste("tmp$covid_inc1 ~ log(",v,"+1)",sep = ""),tmp)
fLogD <- function(x){ modExp$coefficients[1] + modExp$coefficients[2] * log(x+1)}
s <- summary(modLogD)
r <- paste("r =",round(cor(tmp$covid_inc1,log(tmp[[v]]+1)),3))
pvalue <- ifelse(s$coefficients[2,4] > 0.001,paste("p =",round(s$coefficients[2,4],3)),"p < 0.001")
lab <- paste(r,"\n",pvalue,sep = "")
grobLogD <- grobTree(textGrob(lab, x=0.8,  y=0.9, hjust=0,
                             gp=gpar(fontsize=13, fontface="italic")))

#logLog
p <- ggplot(tmp,aes_string(x = v,y = "covid_inc1",label = "Subregion",color = "cluster")) + theme_linedraw() +
  titles2 + geom_text_repel(size = 3) + geom_point() + scale_color_manual("",values = col[c(4,5,3)]) +
  xlab("Dengue fever incidence") + ylab("COVID-19 incidence") + theme(legend.position = "none") +
  annotation_custom(grobLog) +
  stat_function(fun = fLog,linetype = "dashed",color = "black")
pdf(paste("incidence_world_Carabean_Cluster_LogLog.pdf",sep = ""),width = 7.5,height = 7.5)
print(p)
dev.off()

#Exp
p <- ggplot(tmp,aes_string(x = v,y = "covid_inc1",label = "Subregion",color = "cluster")) + theme_linedraw() +
  titles2 + geom_text_repel(size = 3) + geom_point() + scale_color_manual("",values = col[c(4,5,3)]) +
  xlab("Dengue fever incidence") + ylab("COVID-19 incidence") + theme(legend.position = "none") +
  annotation_custom(grobExp) +
  stat_function(fun = fExp,linetype = "dashed",color = "black")
pdf(paste("incidence_world_Carabean_Cluster_Exp.pdf",sep = ""),width = 7.5,height = 7.5)
print(p)
dev.off()

#Log Dengue
p <- ggplot(tmp,aes_string(x = v,y = "covid_inc1",label = "Subregion",color = "cluster")) + theme_linedraw() +
  titles2 + geom_text_repel(size = 3) + geom_point() + scale_color_manual("",values = col[c(4,5,3)]) +
  xlab("Dengue fever incidence") + ylab("COVID-19 incidence") + theme(legend.position = "none") +
  annotation_custom(grobLogD) +
  stat_function(fun = fLogD,linetype = "dashed",color = "black")
pdf(paste("incidence_world_Carabean_Cluster_LogDengue.pdf",sep = ""),width = 7.5,height = 7.5)
print(p)
dev.off()
rm(grobExp,grobLog,grobLogD,modExp,modLog,modLogD,p,s,tmp,lab,pvalue,r,v,world,fExp,fLog,fLogD)

######Isolation map####
shp <- readRDS("~/GDrive/mdyn/maps/shp_brazil.rds") #Shapefile cities Brazil
shp$CD_GEOCMU <- substr(shp$CD_GEOCMU,1,6) #IBGE code 6 digits
shp <- merge(shp,iso,by.x = "CD_GEOCMU",by.y = "ibge_cod") #Merge shape with data
shp@data$id <- rownames(shp@data) #Id
shpf <- fortify(shp,region = "id") #Shape to data frame
shp <- join(shpf,shp@data,by="id") #Join shape with factor
shp <- shp %>% filter(!is.na(isoMean)) #Only cities with data

shpEstados <- readOGR(dsn = "~/GDrive/mdyn/maps/UFEBRASIL/UFEBRASIL.shp",stringsAsFactors = F,verbose = F)
shpEstados <- fortify(shpEstados)

p <- ggplot(shp, aes(x=long, y=lat, group=group))+ titles +
  geom_polygon(aes(fill = isoMean))+ 
  geom_polygon(data = shpEstados,mapping = aes(x=long, y=lat, group=group),
                                              color = "black",fill = NA) + 
  theme_void() + xlab("") + ylab("") + 
  coord_fixed() + scale_fill_gradient("",low = "red", high = "green",na.value = NA)
pdf("mapa_iso.pdf",width = 0.75*15,height = 0.75*10)
print(p)
dev.off()

