#Util functions for fitting and simulating the SEIR metapopulation model for COVID-19

#Libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(doParallel)
library(foreach)
library(rgdal)
library(rgeos)
library(geosphere)
library(svMisc)
library(data.table)
library(ggplot2)
library(ggthemes)
library(readODS)
library(doSNOW)
library(progress)
library(gridExtra)

#Source scripts
source("mdyn/ShinyApps/preprocessing/preprocess_SEIR_output.R")
source("mdyn/SEIR/model.R")
source("mdyn/SEIR/EPI_curve.R")
source("mdyn/SEIR/get_data_SP.R")
source("mdyn/SEIR/lift_death_reasearch.R")
source("mdyn/SEIR/data_drs.R")
source("mdyn/SEIR/initial_condition.R")
source("mdyn/SEIR/initial_condition_corrected.R")
source("mdyn/SEIR/obs_around_init.R")
source("mdyn/SEIR/growth_rate.R")
source("mdyn/SEIR/death_rate_research.R")
source("mdyn/SEIR/set_progress_bar.R")
source("mdyn/SEIR/sample_parameters.R")
source("mdyn/SEIR/beta.R")
source("mdyn/SEIR/seir_solver.R")
source("mdyn/SEIR/test_model.R")
source("mdyn/SEIR/Rt.R")
source("mdyn/SEIR/plot_map_summary.R")
source("mdyn/SEIR/plot_validate.R")
source("mdyn/SEIR/store_simulation.R")
source("mdyn/SEIR/build_maps.R")
source("mdyn/SEIR/testagem.R")

#Plot themes
titles <- theme(strip.text = element_text(size = 12), axis.text = element_text(size = 12,color = "black"),
                axis.title = element_text(size = 14), legend.text = element_text(size = 14),
                legend.title = element_text(size = 14),
                panel.border = element_blank(),
                panel.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                legend.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                legend.position="bottom",legend.spacing.x = unit(0.5, 'cm'))
titles_Map <- theme(strip.text = element_text(size = 12), axis.text = element_text(size = 12,color = "black"),
                    axis.title = element_text(size = 14), legend.text = element_text(size = 14),
                    legend.title = element_text(size = 14,face = "bold"),plot.title = element_text(size = 16,face = "bold",hjust = 0.5),
                    panel.border = element_blank(),legend.key.width=unit(4,"cm"),
                    panel.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                    legend.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                    legend.position="bottom",legend.spacing.x = unit(0.5, 'cm'))

#Get legend from ggplot
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

#Function to remove accents
rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  pattern <- unique(pattern)
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  accentTypes <- c("´","`","^","~","¨","ç")
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  return(str)
}

#Get drs data
drs <- readRDS(file = "mdyn/SEIR/dados/drs.rds")
drs <- drs[match(par$names,drs$Municipio),]
drs$N[drs$Municipio == "SÃO PAULO"] <- par$pop[par$names == "SÃO PAULO"]
# drs <- read.csv("/home/dmarcondes/mdyn/SEIR/dados/DRS.csv",sep = ";") #Read drs table
# drs$Municipio <- gsub("'","",drs$Municipio) #Correct names 
# drs <- drs[match(par$names,drs$Municipio),] #Order cities
# tmp <- data.frame("Municipio" = par$names,"pop" = par$pop) #Get population of each city
# drs <- data.table(merge(drs,tmp)) #Merge to get population
# drs <- drs[,N := sum(pop),by = DRS] #Population by DRS
# drs <- drs %>% select(DRS,Regiao,Municipio,N) %>% data.frame() #Clean
# drs$DRS <- as.character(drs$DRS) #Character DRS
# drs$DRS[drs$Municipio == "SÃO PAULO"] <- "0" #Set city of SP as DRS
# drs$Regiao <- as.character(drs$Regiao) #Character Regiao
# drs$Regiao[drs$Municipio == "SÃO PAULO"] <- "Cidade de São Paulo" #Set city of SP as DRS
# drs$DRS <- factor(drs$DRS,c("0","I","II","III","IV","V","VI","VII","VIII","IX","X","XI","XII",
#                             "XIII","XIV","XV","XVI","XVII")) #DRS to factor
# drs$Regiao <- factor(drs$Regiao) #Regiao to factor
# saveRDS(object = drs,"drs.rds")

#Get data API
get_data_API <- function(){
  library(httr)
  dados <- GET("https://brasil.io/api/dataset/covid19/caso_full/data/")
  dados <- content(dados)
  n <- dados$'next'
  dados <- dados$results
  dados <- lapply(dados,function(x) data.frame(rbind(unlist(x))))
  dados <- bind_rows(dados)
  cat(n)
  cat("\n")
  while(!is.null(n)){
    tmp <- GET(n)
    tmp <- content(tmp)
    n <- tmp$'next'
    cat(n)
    cat("\n")
    tmp <- tmp$results
    tmp <- lapply(tmp,function(x) data.frame(rbind(unlist(x))))
    tmp <- bind_rows(tmp)
    dados <- rbind.data.frame(dados,tmp)
  }
  return(dados)
}

#Positive log
logP <- function(x){ifelse(x>0,log(x),0)}

#Normalize isolation
iso_norm <- function(obs,base){
  ifelse(obs > base,(1-obs)/(1-base),1)
}
