#################################################
######Process mobility matrix to Shiny app#######
######Diego Marcondes                     #######
######dmarcondes@ime.usp.br               #######
#################################################

#libraries
library(data.table)
library(rgdal)
library(tidyverse)
library(progress)
library(lubridate)
#library(doParallel)
#library(foreach)
#library(doSNOW)

#Folder with raw matrices
fm <- "/storage/inloco/data" #Root folder
dir_fm <- list.dirs(fm,recursive = FALSE) #List directories in root folder
fm <- dir_fm[grepl("data_br_full",dir_fm,fixed = TRUE)] #Find directory containing "data_br_full" in the name

#Folder to save processed files
fsave <- "/storage/Shiny/mobilidade/www"

#Find dates for which we have data
fdates <- list.dirs(fm,recursive = FALSE) #List folders of fm directory
fdates <- unlist(lapply(strsplit(fdates,split = "="),function(x) x[[2]])) #Get dates
fdates_done <- list.files(fsave) #See files already saved
fdates_done <- fdates_done[grepl("graph_",fdates_done,fixed = TRUE)] #Only files with prefix graph_
fdates_done <- unlist(lapply(strsplit(fdates_done,split = "_"),function(x) x[[2]])) #Get dates.rds
fdates_done <- unlist(strsplit(fdates_done,split = ".rds")) #Get dates
fdates <- fdates[!(fdates %in% fdates_done)] #Only dates for which there is no processed data

#Read shape file
shp <- readOGR(dsn = "/home/pedrosp/mdyn/maps/br_municipios/br_mun_with_uf_regionmdyn_subdom.shp",stringsAsFactors = F,
               verbose = F) #Shapefiles
dic_estados <- list('Acre' = 'AC','Alagoas' = 'AL','Amapá' = 'AP','Amazonas' = 'AM',
                     'Bahia' = 'BA','Ceará' = 'CE','Distrito Federal' = 'DF',
                     'Espírito Santo' = 'ES','Goiás' = 'GO','Maranhão' = 'MA',
                     'Mato Grosso' = 'MT','Mato Grosso do Sul' = 'MS',
                     'Minas Gerais' = 'MG','Pará' = 'PA','Paraíba' = 'PB','Paraná' = 'PR',
                     'Pernambuco' = 'PE','Piauí' = 'PI','Rio de Janeiro' = 'RJ',
                     'Rio Grande do Norte' = 'RN','Rio Grande do Sul' = 'RS','Rondônia' = 'RO',
                     'Roraima' = 'RR','Santa Catarina' = 'SC','São Paulo' = 'SP',
                     'Sergipe' = 'SE','Tocantins' = 'TO')
for(s in unique(shp$Nome_UF))
   shp$UF[shp$Nome_UF == s] <- dic_estados[[s]]
saveRDS(object = shp,file = paste(fsave,"/shp_brazil.rds",sep = "")) #Save shapefile
shp$key <- paste(shp$Nome_Munic,"-",shp$UF,sdp = "")
names <- shp$key #Name of collums of matrix
center_begin <- data.frame(shp) %>% select(key,lonc,latc,CD_GEOCMU) #Center of each city
names(center_begin) <- c("begin","lonc_begin","latc_begin","begin_code") #Change names to merge below
center_end <- data.frame(shp) %>% select(key,lonc,latc,CD_GEOCMU) #Center of each city
names(center_end) <- c("end","lonc_end","latc_end","end_code") #Change names to merge below

if(length(fdates) == 0)
  cat("Mobility matrix are all up to date!")
if(length(fdates) > 0){
  #Progress bar
  # pb <- progress_bar$new(
  #   format = ":letter [:bar] :elapsed | eta: :eta",
  #   total = length(fdates),
  #   width = 60)
  # progress_letter <- paste(round(100*c(1:length(fdates))/length(fdates),2),"%")
  # progress <- function(n){
  #   pb$tick(tokens = list(letter = progress_letter[n]))
  # } 
  # opts <- list(progress = progress)
  # k <- 1
  # #Register parallel
  # cl <- makeSOCKcluster(24)
  # registerDoSNOW(cl)
  # 
  for(d in fdates){#,.options.snow = opts,.packages = c("tidyverse","data.table")) %dopar% { #For each day
    #pb$tick(tokens = list(letter = progress_letter[k]))
    #k <- k + 1
    file_d <- paste(fm,"/date0=",d,"/move_mat_Brasil_Municip.csv",sep = "") #Path to matrix
    if("move_mat_Brasil_Municip.csv" %in% list.files(paste(fm,"/date0=",d,"/",sep = ""))){
      mat <- as.matrix(fread(file_d)) #Open matrix
      colnames(mat) <- names #Colnames
      rownames(mat) <- names #Rownames
      mat <- as.data.frame.table(t(mat)) #Gather
      colnames(mat) <- c("begin","end","w") #Colnames
      mat <- mat %>% filter(w > 4) #Only edges with more than 4 trips
      mat <- merge(mat,center_begin) #Find center of begin
      mat <- merge(mat,center_end) #Find center of end
      mat <- mat %>% filter(end != begin) #Erase end = begin
      mat$key <- paste(mat$begin,"-->",mat$end) #Set key as the path
      mat <- mat %>% select(key,end_code,begin_code,w,lonc_begin,latc_begin,lonc_end,latc_end) %>% 
        gather("side_lng","lng",-key,-w,-begin_code,-end_code,-latc_begin,-latc_end) %>%
        gather("side_lat","lat",-side_lng,-key,-w,-begin_code,-end_code,-lng) %>% unique() #One line for each pair lat,long
      mat$side_lat <- unlist(lapply(strsplit(mat$side_lat,"_"),function(x) x[[2]])) #Find if lat is a begin or end
      mat$side_lng <- unlist(lapply(strsplit(mat$side_lng,"_"),function(x) x[[2]])) #Find if lgn is a begin or end
      mat <- mat %>% filter(side_lng == side_lat) #Only sides which are equal
      mat$side <- mat$side_lat #Get side
      mat$side_lng <- NULL #Erase
      mat$side_lat <- NULL #Erase
      names(mat)[2:3] <- c("end","begin") #Rename
      saveRDS(object = mat,file = paste(fsave,"/graph_",d,".rds",sep = "")) #save
      rm(mat,file_d)
    }
  }
  fdates_done <- list.files(fsave) #See files already saved
  fdates_done <- fdates_done[grepl("graph_",fdates_done,fixed = TRUE)] #Only files with prefix graph_
  fdates_done <- unlist(lapply(strsplit(fdates_done,split = "_"),function(x) x[[2]])) #Get dates.rds
  fdates_done <- unlist(strsplit(fdates_done,split = ".rds")) #Get dates
  dates <- data.frame("min" = min(ymd(fdates_done)),"max" = max(ymd(fdates_done))) #Min and Max date
  saveRDS(dates,paste(fsave,"/dates.rds",sep = "")) #Save
  cat("Done!")
  #stopCluster(cl)
}



