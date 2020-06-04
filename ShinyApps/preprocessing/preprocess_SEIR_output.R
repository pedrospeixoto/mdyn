#Preprocess the output of SEIR model

preprocess_SEIR_output <- function(pos,obs,init_validate){
  library(data.table)
  library(lubridate)
  library(lubridate)
  
  #wd
  wd <- paste("/storage/SEIR/",pos,sep = "")
  
  ######Convert to png and create video######
  cat("Converting pdf to png and creating videos...\n")
  system(paste("./mdyn/sh/preprocess_SIR.sh",pos))
  
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
  cases_DRS <- fread(paste(wd,"/cases_DRS_",pos,".csv",sep = ""),sep = ",") %>% select(DRS,date,Ipred,IpredInf,IpredSup,Regiao)
  cases_DRS$Date <- ymd(cases_DRS$Date)
  cases_DRS$Regiao <- factor(cases_DRS$Regiao)
  cases_DRS$DRS <- factor(cases_DRS$DRS)
  names(cases_DRS)[3:5] <- c("Mediana","Sup","Infimo")
  saveRDS(cases_DRS,"/storage/ShinyApps/seircovid19/www/cases_DRS.rds")
  
  #Deaths cities
  deaths_DRS <- fread(paste(wd,"/deaths_DRS_",pos,".csv",sep = ""),sep = ",") %>% select(DRS,date,Dpred,DpredInf,DpredSup,Regiao)
  deaths_DRS$date <- ymd(deaths_DRS$date)
  deaths_DRS$Regiao <- factor(deaths_DRS$Regiao)
  deaths_DRS$DRS <- factor(deaths_DRS$DRS)
  names(deaths_DRS)[3:5] <- c("Mediana","Sup","Infimo")
  saveRDS(deaths_DRS,"/storage/ShinyApps/seircovid19/www/deaths_DRS.rds")
  
  #peak
  peak_city <- data.frame(fread(paste(wd,"/peak_",pos,".csv",sep = ""),sep = ","))
  peak_city$TMediana <- ymd(peak_city$TMediana)
  peak_city <- peak_city[order(peak_city$MMediana),]
  saveRDS(peak_city,"/storage/ShinyApps/seircovid19/www/peak_city.rds")
  
  #peak DRS
  peak_DRS <- data.frame(fread(paste(wd,"/peak_DRS_",pos,".csv",sep = ""),sep = ","))
  peak_DRS <- peak_city[order(peak_DRS$MMediana),]
  saveRDS(peak_DRS,"/storage/ShinyApps/seircovid19/www/peak_DRS.rds")
  
  #Rt
  Rt <- data.frame(fread(paste(wd,"/SP_Rt_",pos,".csv",sep = ""),sep = ","))
  saveRDS(Rt,"/storage/ShinyApps/seircovid19/www/Rt.rds")
  
  #Assymptomatics
  ass <- data.frame(fread(paste(wd,"/SP_assymptomatics_",pos,".csv",sep = ""),sep = ","))
  saveRDS(ass,"/storage/ShinyApps/seircovid19/www/assymptomatics.rds")
  
  #pos
  saveRDS(pos,"/storage/ShinyApps/seircovid19/www/pos.rds")
  
  #####Sync with ShinyApps####
  cat("Syncing with Shiny server...\n")
  system('rsync -u -avz -e "ssh -p 2223" dmarcondes@shiny.ime.usp.br: /storage/ShinyApps')
  system('rsync -u -avz -e "ssh -p 2223" /storage/ShinyApps dmarcondes@shiny.ime.usp.br:')
}