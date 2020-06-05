#Preprocess the output of SEIR model

preprocess_SEIR_output <- function(drs,pos,obs,init_validate){
  library(data.table)
  library(lubridate)
  
  #wd
  wd <- paste("/storage/SEIR/",pos,sep = "")
    
  ######Convert to png and create video######
  system(paste("./mdyn/sh/preprocess_SEIR_convert.sh",pos,"&> log_teste1.txt"))# ./mdyn/sh/preprocess_SEIR_video.sh",pos,"teste &> log_teste2.txt &"))
  #system(paste("./mdyn/sh/preprocess_SEIR_video.sh",pos))
    
  #####Create rds files for shiny#####
  cat("Creating files for ShinyApp...\n")
  
  #Cases city
  cases_city <- fread(paste(wd,"/cases_",pos,".csv",sep = ""),sep = ",")
  c_100 <- obs %>% filter(date == ymd(init_validate) & confirmed_corrected >= 100)
  c_100 <- c_100$city
  cases_city <- cases_city %>% filter(Municipio %in% c_100)
  cases_city$Date <- ymd(cases_city$Date)
  cases_city$Municipio <- factor(cases_city$Municipio)
  saveRDS(cases_city,"/storage/ShinyApps/seircovid19/www/cases_city.rds")
  
  #Deaths cities
  deaths_city <- fread(paste(wd,"/deaths_",pos,".csv",sep = ""),sep = ",")
  deaths_city <- deaths_city %>% filter(Municipio %in% c_100)
  deaths_city$Date <- ymd(deaths_city$Date)
  deaths_city$Municipio <- factor(deaths_city$Municipio)
  saveRDS(deaths_city,"/storage/ShinyApps/seircovid19/www/deaths_city.rds")
  
  #Cases DRS
  cases_DRS <- data.frame(fread(paste(wd,"/cases_DRS_",pos,".csv",sep = ""),sep = ","))
  cases_DRS$DRS <- NULL
  cases_DRS <- cases_DRS[,c(1,5,2,3,4)]
  names(cases_DRS)[2] <- "DRS"
  cases_DRS$DRS <- factor(cases_DRS$DRS)
  cases_DRS$Date <- ymd(cases_DRS$Date)
  saveRDS(cases_DRS,"/storage/ShinyApps/seircovid19/www/cases_DRS.rds")
  
  #Deaths cities
  deaths_DRS <- fread(paste(wd,"/deaths_DRS_",pos,".csv",sep = ""),sep = ",")
  deaths_DRS$DRS <- NULL
  deaths_DRS <- deaths_DRS[,c(1,5,2,3,4)]
  names(deaths_DRS)[2] <- "DRS"
  deaths_DRS$Date <- ymd(deaths_DRS$Date)
  deaths_DRS$DRS <- factor(deaths_DRS$DRS)
  saveRDS(deaths_DRS,"/storage/ShinyApps/seircovid19/www/deaths_DRS.rds")
  
  #peak
  peak_city <- data.frame(fread(paste(wd,"/peak_",pos,".csv",sep = ""),sep = ","))
  peak_city$TMediana <- ymd(peak_city$TMediana)
  peak_city$TMinimo <- ymd(peak_city$TMinimo)
  peak_city$TMaximo <- ymd(peak_city$TMaximo)
  peak_city <- peak_city[order(peak_city$MMediana),]
  peak_city$Municipio <- factor(peak_city$Municipio)
  saveRDS(peak_city,"/storage/ShinyApps/seircovid19/www/peak_city.rds")
  
  #peak DRS
  peak_DRS <- data.frame(fread(paste(wd,"/peak_DRS_",pos,".csv",sep = ""),sep = ","))
  peak_DRS$DRS <- NULL
  peak_DRS <- peak_DRS[,c(7,1:6)]
  names(peak_DRS)[1] <- "DRS"
  peak_DRS$TMediana <- ymd(peak_DRS$TMediana)
  peak_DRS$TMinimo <- ymd(peak_DRS$TMinimo)
  peak_DRS$TMaximo <- ymd(peak_DRS$TMaximo)
  peak_DRS$DRS <- factor(peak_DRS$DRS)
  peak_DRS <- peak_DRS[order(peak_DRS$MMediana),]
  saveRDS(unique(peak_DRS),"/storage/ShinyApps/seircovid19/www/peak_DRS.rds")
  
  #Rt
  Rt <- data.frame(fread(paste(wd,"/SP_Rt_",pos,".csv",sep = ""),sep = ","))
  Rt$Municipio <- factor(Rt$Municipio)
  Rt$DRS <- factor(Rt$DRS)
  saveRDS(Rt,"/storage/ShinyApps/seircovid19/www/Rt.rds")
  
  #Assymptomatics
  ass <- data.frame(fread(paste(wd,"/SP_assymptomatics_",pos,".csv",sep = ""),sep = ","))
  ass$Municipio <- factor(ass$Municipio)
  ass$DRS <- factor(ass$DRS)
  saveRDS(ass,"/storage/ShinyApps/seircovid19/www/assymptomatics.rds")
  
  #pos
  saveRDS(pos,"/storage/ShinyApps/seircovid19/www/pos.rds")
  
  #nmodels
  nmodels <- readRDS(paste("/storage/SEIR/",pos,"/result_",pos,".rds",sep = ""))
  nmodels <- length(nmodels$models)
  saveRDS(nmodels,"/storage/ShinyApps/seircovid19/www/nmodels.rds")
  
  #drs
  saveRDS(drs,"/storage/ShinyApps/seircovid19/www/drs.rds")
}