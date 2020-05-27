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
library(doParallel)
library(foreach)

#Folder with raw matrices
fm <- "/storage/inloco/data" #Root folder
dir_fm <- list.dirs(fm,recursive = FALSE) #List directories in root folder
fm <- dir_fm[grepl("data_br_full",dir_fm,fixed = TRUE)] #Find directory containing "data_br_full" in the name

#Folder to save processed files
fsave <- "/storage/Shiny/Mobilidade/www"

#Find dates for which we have data
fdates <- list.dirs(fm,recursive = FALSE) #List folders of fm directory
fdates <- unlist(lapply(strsplit(fdates,split = "="),function(x) x[[2]])) #Get dates
fdates_done <- list.files(fsave) #See files already saved
fdates_done <- fdates_done[grepl("graph_",fdates_done,fixed = TRUE)] #Only files with prefix graph_
fdates_done <- unlist(lapply(strsplit(fdates_done,split = "_"),function(x) x[[2]])) #Get dates.rds
fdates_done <- unlist(strsplit(fdates_done,split = ".rds")) #Get dates
fdates <- fdates[!(fdates %in% fdates_done)] #Only dates for which there is no processed data
dates <- ymd(c(fdates_done,fdates)) #Get all dates with date
dates <- data.frame("min" = min(dates),"max" = max(dates)) #Min and Max date
saveRDS(dates,paste(fsave,"/dates.rds",sep = "")) #Save

#Read shape file
shp <- readOGR(dsn = "/home/pedrosp/mdyn/maps/br_municipios/br_mun_with_uf_regionmdyn_subdom.shp",stringsAsFactors = F,
               verbose = F) #Shapefiles
saveRDS(object = shp,file = "shp_brazil.rds") #Save shapefile
names <- shp$CD_GEOCMU #Name of collums of matrix
center_begin <- data.frame(shp) %>% select(CD_GEOCMU,lonc,latc) #Center of each city
names(center_begin) <- c("begin","lonc_begin","latc_begin") #Change names to merge below
center_end <- data.frame(shp) %>% select(CD_GEOCMU,lonc,latc) #Center of each city
names(center_end) <- c("end","lonc_end","latc_end") #Change names to merge below

if(length(fdates) == 0)
  cat("Mobility matrix are all up to date!")
else{
  #Progress bar
  pb <- progress_bar$new(
    format = ":letter [:bar] :elapsed | eta: :eta",
    total = length(fdates),
    width = 60)
  progress_letter <- paste(round(100*c(1:length(fdates))/length(fdates),2),"%")
  progress <- function(n){
    pb$tick(tokens = list(letter = progress_letter[n]))
  } 
  opts <- list(progress = progress)
  k <- 1
  #Register parallel
  cl <- makeSOCKcluster(24)
  registerDoSNOW(cl)
  
  foreach(d = fdates,.options.snow = opts,.packages = c("tidyverse","data.table")) %dopar% { #For each day
    pb$tick(tokens = list(letter = progress_letter[k]))
    k <- k + 1
    file_d <- paste(fm,"/date0=",d,"/move_mat_Brasil_Municip.csv",sep = "") #Path to matrix
    if("move_mat_Brasil_Municip.csv" %in% list.files(paste(fm,"/date0=",d,"/",sep = ""))){
      mat <- as.matrix(fread(file_d)) #Open matrix
      colnames(mat) <- names #Colnames
      rownames(mat) <- names #Rownames
      mat <- as.data.frame.table(t(mat)) #Gather
      colnames(mat) <- c("begin","end","w") #Colnames
      mat <- mat %>% filter(w > 4) #Only edges with more than 4 trips
      mat <- merge(mat,center_begin) #Find center of begin
      head <- merge(mat,center_end) #Find center of end
      saveRDS(object = mat,file = paste(fsave,"/graph_",d,".rds",sep = ""))
      rm(mat,file_d)
    }
  }
  cat("Done!")
  stopCluster(cl)
}



