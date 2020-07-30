#Preprocess the output of SEIR model

preprocess_SEIR_output <- function(param,drs,pos,obs,end_validate){
  library(data.table)
  library(lubridate)
  
  #wd
  wd <- paste("/storage/SEIR/",pos,sep = "")
    
  #####Create rds files for shiny#####
  cat("Creating files for ShinyApp...\n")
  
  #Cases city
  cases_city <- fread(paste(wd,"/cases_",pos,".csv",sep = ""),sep = ",")
  c_1000 <- obs %>% filter(date == ymd(end_validate) & (confirmed_corrected > 500 | deaths_corrected >= 100))
  c_1000 <- c_1000$city
  cases_city$c_1000 <- cases_city$Municipio %in% c_1000
  cases_city$Date <- ymd(cases_city$Date)
  cases_city$Municipio <- factor(cases_city$Municipio)
  saveRDS(cases_city,"/storage/ShinyApps/seircovid19research/www/cases_city.rds")
  
  #Deaths cities
  deaths_city <- fread(paste(wd,"/deaths_",pos,".csv",sep = ""),sep = ",")
  deaths_city$c_1000 <- deaths_city$Municipio %in% c_1000
  deaths_city$Date <- ymd(deaths_city$Date)
  deaths_city$Municipio <- factor(deaths_city$Municipio)
  saveRDS(deaths_city,"/storage/ShinyApps/seircovid19research/www/deaths_city.rds")
  
  #Which DRS
  cases_DRS <- data.frame(fread(paste(wd,"/cases_DRS_",pos,".csv",sep = ""),sep = ","))
  deaths_DRS <- fread(paste(wd,"/deaths_DRS_",pos,".csv",sep = ""),sep = ",")
  DRS_1000_C <- cases_DRS %>% filter(Date == ymd(end_validate) & Mediana >= 500)
  DRS_100_D <- deaths_DRS %>% filter(Date == ymd(end_validate) & Mediana >= 100)
  DRS_which <- unique(c(DRS_1000_C$DRS,DRS_100_D$DRS))  
  DRS_which <- DRS_which[DRS_which != "0"]
  cases_DRS <- cases_DRS %>% filter(DRS %in% DRS_which)
  deaths_DRS <- deaths_DRS %>% filter(DRS %in% DRS_which)
  
  #Cases DRS
  cases_DRS$DRS <- NULL
  cases_DRS <- cases_DRS[,c(1,5,2,3,4)]
  names(cases_DRS)[2] <- "DRS"
  cases_DRS$DRS <- factor(cases_DRS$DRS)
  cases_DRS$Date <- ymd(cases_DRS$Date)
  saveRDS(cases_DRS,"/storage/ShinyApps/seircovid19research/www/cases_DRS.rds")
  
  #Deaths DRS
  deaths_DRS$DRS <- NULL
  deaths_DRS <- deaths_DRS[,c(1,5,2,3,4)]
  names(deaths_DRS)[2] <- "DRS"
  deaths_DRS$Date <- ymd(deaths_DRS$Date)
  deaths_DRS$DRS <- factor(deaths_DRS$DRS)
  saveRDS(deaths_DRS,"/storage/ShinyApps/seircovid19research/www/deaths_DRS.rds")
  
  #peak
  peak_city <- data.frame(fread(paste(wd,"/peak_",pos,".csv",sep = ""),sep = ","))
  peak_city$TMediana <- ymd(peak_city$TMediana)
  peak_city$TMinimo <- ymd(peak_city$TMinimo)
  peak_city$TMaximo <- ymd(peak_city$TMaximo)
  peak_city <- peak_city[order(peak_city$MMediana),]
  peak_city$Municipio <- factor(peak_city$Municipio)
  peak_city <- peak_city %>% filter(Municipio %in% c_1000)
  saveRDS(peak_city,"/storage/ShinyApps/seircovid19research/www/peak_city.rds")
  
  #peak DRS
  peak_DRS <- data.frame(fread(paste(wd,"/peak_DRS_",pos,".csv",sep = ""),sep = ","))
  peak_DRS <- peak_DRS %>% filter(DRS %in% DRS_which)
  peak_DRS$DRS <- NULL
  peak_DRS <- peak_DRS[,c(7,1:6)]
  names(peak_DRS)[1] <- "DRS"
  peak_DRS$TMediana <- ymd(peak_DRS$TMediana)
  peak_DRS$TMinimo <- ymd(peak_DRS$TMinimo)
  peak_DRS$TMaximo <- ymd(peak_DRS$TMaximo)
  peak_DRS$DRS <- factor(peak_DRS$DRS)
  peak_DRS <- peak_DRS[order(peak_DRS$MMediana),]
  saveRDS(unique(peak_DRS),"/storage/ShinyApps/seircovid19research/www/peak_DRS.rds")
  
  #Rt
  Rt <- data.frame(fread(paste(wd,"/SP_Rt_",pos,".csv",sep = ""),sep = ","))
  Rt$Municipio <- factor(Rt$Municipio)
  Rt$DRS <- factor(Rt$DRS)
  saveRDS(Rt,"/storage/ShinyApps/seircovid19research/www/Rt.rds")
  
  #Parameters
  saveRDS(param,"/storage/ShinyApps/seircovid19research/www/param.rds")
  
  #pos
  saveRDS(pos,"/storage/ShinyApps/seircovid19research/www/pos.rds")
  
  #nmodels
  nmodels <- readRDS(paste("/storage/SEIR/",pos,"/result_",pos,".rds",sep = ""))
  nmodels <- length(nmodels$models)
  saveRDS(nmodels,"/storage/ShinyApps/seircovid19research/www/nmodels.rds")
  
  #drs
  saveRDS(drs,"/storage/ShinyApps/seircovid19research/www/drs.rds")
  
  #Data state
  state <- data.frame(fread(paste("/storage/SEIR/",pos,"/simulation_state_",pos,".csv",sep = "")))
  saveRDS(state,"/storage/ShinyApps/seircovid19research/www/state.rds")
  
  ######Convert to png and create video######
  system(paste("./mdyn/sh/preprocess_SEIR_convert_research.sh",pos))
}