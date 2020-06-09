###############################
#####SEIR for COVID-19    #####
#####Diego Marcondes      #####
#####dmarcondes@ime.usp.br#####
###############################

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
source("mdyn/SEIR/utils.R")
source("mdyn/ShinyApps/preprocessing/preprocess_SEIR_output.R")

SEIR_covid <- function(cores,par,pos,seed,sample_size,simulate_length,d_max){
  
  cat("\n")
  cat("Welcome to Covid SEIR Mobility Model estimation!")
  cat("\n")
  cat("One moment and I will be right there with you...")
  cat("\n")
  
  #Seed
  set.seed(seed)
  
  #####Model specification#####
  #t = time
  #Y = observed quantities at time t
  #par = A named list of model parameters (mob,pop,gamma,rho,beta,nu,d,s,sites)
  derivatives <- function(t,Y,parK){
    
    #States at time t
    E <- Y[1:parK$sites] #Exposed
    Ia <- Y[(parK$sites + 1):(2*parK$sites)] #Asymptomatic
    Is <- Y[(2*parK$sites + 1):(3*parK$sites)] #Symptomatic
    R <- Y[(3*parK$sites + 1):(4*parK$sites)] #Recovered
    D <- Y[(4*parK$sites + 1):(5*parK$sites)] #Deaths
    S <- parK$pop - E - Ia - Is - R - D #Susceptibles
    
    #Parameters
    if(parK$val) #If in validation period, take mobility matrix of the day
      mob <- parK$mob[[as.character(parK$day[t])]] #Mobility pattern of day
    else{ #If not, take of the day if exists; otherwise take of the weekday
      if(as.character(parK$day[t]) %in% names(parK$mob))
        mob <- parK$mob[[as.character(parK$day[t])]]
      else
        mob <- parK$mob[[weekdays(parK$day[t])]]
    }
    N <- parK$pop #Population
    Te <- parK$Te #Time exposed
    Ta <- parK$Ta #Time assymptomatic
    Td <- parK$Td #Time symptomatic until death
    Ts <- parK$Ts #Time symptomatic until recover
    delta <- parK$delta #Death rate
    gammaA <- parK$gammaA #Rate from Exposed to Asymptomatic
    gammaS <- (1 - Te*gammaA)/Te #Rate of Exposed to Symptomatic
    nuA <- 1/Ta #Rate from Asymptomatic to Recovered
    nuS <- (1-delta*Td)/Ts #Rate from Symptomatic to Recovered
    s <- parK$s #Intensity of mobility
    beta <- parK$beta #beta
    
    #Derivatives
    dY <- vector(length = 6*parK$sites) #Vector of derivatives
    dY[1:parK$sites] <- -(gammaS + gammaA)*E + beta*(S/(N-D))*(s*((mob-diag(diag(mob))) %*% cbind(Is+Ia)) + Is + Ia) #E
    dY[(parK$sites + 1):(2*parK$sites)] <- gammaA*E - nuA*Ia #Ia
    dY[(2*parK$sites + 1):(3*parK$sites)] <- -nuS*Is + gammaS*E - delta*Is #Is
    dY[(3*parK$sites + 1):(4*parK$sites)] <- nuA*Ia + nuS*Is #R
    dY[(4*parK$sites + 1):(5*parK$sites)] <- delta*Is #D
    dY[(5*parK$sites + 1):(6*parK$sites)] <- gammaS*E #Add new cases to total
    
    return(list(dY)) #Return
  }
  
  #####DRS#####
  drs <- readRDS(file = "mdyn/SEIR/dados/drs.rds")
  drs <- drs[match(par$names,drs$Municipio),]
  
  #####mkdir#####
  system(paste("mkdir /storage/SEIR/",pos,sep = ""))
  
  #####Notifications#####
  cat("Downloading data about confirmed cases and deaths and plot epidemiological curve...\n")
  obs <- get_data_SP()
  if(nrow(obs)/par$sites-round(nrow(obs)/par$sites) > 0)
    stop("There is a problem with the notifications dataset. Please fix it.")
  
  #####Days of validation#####
  end_validate <- min(max(ymd(na.omit(obs)$date)),ymd(d_max))
  init_validate <- end_validate - 6
  end_fit <- init_validate
  init_simulate <- end_validate
  day_validate <- seq.Date(from = ymd(init_validate),to = ymd(end_validate),by = 1) #Days to validate
  #tmp <- obs %>% filter(city == "SÃO PAULO")
  #ggplot(tmp,aes(x = ymd(date),y = new_infected_cor+1)) + geom_line() + 
  #  geom_point(aes(y = c(1,diff(last_available_confirmed))),colour = "red") + scale_y_log10()
  
  #Epidemiolohical curve
  EPI_curve(obs,end_validate,pos)
  
  #Calculate lift
  lift <- obs %>% filter(date == ymd(end_validate)) #Only end_validate
  lift <- lift[match(par$names,lift$city),] #order cities
  lift$rate <- lift$last_available_deaths/lift$last_available_confirmed #death rate cities
  rate <- sum(lift$last_available_deaths)/sum(lift$last_available_confirmed) #death rate state
  lift$lift <- lift$rate/rate #lift
  lift$lift[is.na(lift$lift)] <- 1 #fill NA with 1
  lift$lift[lift$last_available_deaths < 10] <- 1 #if not enough data, fill with one
  par$lift <- lift$lift #attribute to par
  
  #Obs by DRS
  obs_drs <- merge(obs,drs,by.x = "city",by.y = "Municipio") #Find DRS of each city
  obs_drs$key <- paste(obs_drs$DRS,obs_drs$date) #key
  obs_drs <- data.table(obs_drs) #data table
  obs_drs <- obs_drs[,last_available_confirmed := sum(last_available_confirmed),by = key] #Sum by DRS anda date
  obs_drs <- obs_drs[,last_available_deaths := sum(last_available_deaths),by = key] #Sum by DRS anda date
  obs_drs <- obs_drs[,recovered := sum(recovered),by = key] #Sum by DRS anda date
  obs_drs <- obs_drs[,infected := sum(infected),by = key] #Sum by DRS anda date
  obs_drs <- obs_drs[,new_infected := sum(new_infected),by = key] #Sum by DRS anda date
  obs_drs <- obs_drs[,new_death := sum(new_death),by = key] #Sum by DRS anda date
  obs_drs <- obs_drs[,new_infected_cor := sum(new_infected_cor),by = key] #Sum by DRS anda date
  obs_drs <- obs_drs[,new_infected_mean := sum(new_infected_mean),by = key] #Sum by DRS anda date
  obs_drs <- obs_drs[,new_death_cor := sum(new_death_cor),by = key] #Sum by DRS anda date
  obs_drs <- obs_drs[,confirmed_corrected := sum(confirmed_corrected),by = key] #Sum by DRS anda date
  obs_drs <- obs_drs[,deaths_corrected := sum(deaths_corrected),by = key] #Sum by DRS anda date

  #####Model estimation#####
  cat("Calculate growth and death rate...\n")
    
  #Initial condition
  init <- vector()
  tmp <- obs %>% filter(date == ymd(init_validate))
  tmp <- tmp[match(x = par$names,table = tmp$city),]
  init[1:par$sites] <- tmp$new_infected_mean #E
  init[(par$sites + 1):(2*par$sites)] <- tmp$infected #Ia
  init[(2*par$sites + 1):(3*par$sites)] <- tmp$infected #Is
  init[(3*par$sites + 1):(4*par$sites)] <- tmp$recovered #R
  init[(4*par$sites + 1):(5*par$sites)] <- tmp$deaths_corrected #D
  init[(5*par$sites + 1):(6*par$sites)] <- tmp$confirmed_corrected #prevalence
  I <- tmp$infected
    
  #Obs each day around week of validation
  par$obs <- list()
  par$obs_DRS <- list()
  for(t in 0:8){
    tmp <- obs %>% filter(date == ymd(init_validate) + t - 1)
    tmp <- tmp[match(x = par$names,table = tmp$city),]
    tmp1 <- obs_drs %>% filter(date == ymd(init_validate) + t - 1)
    tmp1 <- tmp1[match(x = par$names,table = tmp1$city),]
    par$obs$E[[as.character(t)]] <- tmp$new_infected_mean #E
    par$obs$Ia[[as.character(t)]] <- tmp$infected #Ia
    par$obs$Is[[as.character(t)]] <- tmp$infected #Is
    par$obs$R[[as.character(t)]] <- tmp$recovered #R
    par$obs$D[[as.character(t)]] <- tmp$deaths_corrected #D
    par$obs_DRS$E[[as.character(t)]] <- tmp1$new_infected_mean #E
    par$obs_DRS$Ia[[as.character(t)]] <- tmp1$infected #Ia
    par$obs_DRS$Is[[as.character(t)]] <- tmp1$infected #Is
    par$obs_DRS$R[[as.character(t)]] <- tmp1$recovered #R
    par$obs_DRS$D[[as.character(t)]] <- tmp1$deaths_corrected #D
  }
      
  #Test data by DRS
  teste <- obs %>% filter(date %in% seq.Date(from = ymd(init_validate),to = ymd(end_validate),1)) #Get data in validation days
  teste_D <- teste %>% dplyr::select(date,city,deaths_corrected) %>% 
    spread(key = city,value = deaths_corrected) #Select only deaths corrected and spread
  teste_D <- teste_D[,c(1,match(par$names,names(teste_D)))] #Order cities
  teste_D <- teste_D %>% gather("Municipio","D",-date) #Gather
  teste_D <- merge(teste_D,drs) #Merge to find DRSs
  teste_D$key <- paste(teste_D$date,teste_D$DRS) #Key
  teste_D <- data.table(teste_D) #Data table
  teste_D <- teste_D[,D_drs := sum(D),by = key] #Death by DRS
  teste_D <- teste_D %>% select(date,DRS,D_drs,key) %>% unique() %>% data.frame() #Clean
  
  teste_I <- teste %>% dplyr::select(date,city,confirmed_corrected) %>% 
    spread(key = city,value = confirmed_corrected) #Select only confirmed corrected and spread
  teste_I <- teste_I[,c(1,match(par$names,names(teste_I)))] #Order cities
  teste_I <- teste_I %>% gather("Municipio","I",-date) #Gather
  teste_I <- merge(teste_I,drs) #Merge to find DRSs
  teste_I$key <- paste(teste_I$date,teste_I$DRS) #Key
  teste_I <- data.table(teste_I) #Data table
  teste_I <- teste_I[,I_drs := sum(I),by = key] #Death by DRS
  teste_I <- teste_I %>% select(date,DRS,I_drs,key) %>% unique() %>% data.frame() #Clean

  #Calculate growth rate
  system(paste("mkdir /storage/SEIR/",pos,"/AjusteRate/",sep = ""))
  par$lambda <- vector()
  
  #For each DRS
  for(d in unique(drs$DRS)){
    tmp <- teste_I %>% filter(DRS == d) #Data of DRS
    tmp <- data.frame("t" = 0:6,"y" = tmp$I_drs) #Data
    mod <- lm(log(y) ~ t,data = tmp) #lm
    par$lambda[par$names %in% drs$Municipio[drs$DRS == d]] <- mod$coefficients[2] #Lambda
    p <- ggplot(tmp,aes(x = t,y = y)) + geom_point(color = "white") + 
      stat_function(fun = function(t) exp(mod$coefficients[1])*exp(mod$coefficients[2]*t),color = "white") +
      theme_solarized(light = FALSE) +  
      theme(legend.title = element_text(face = "bold"),legend.position = "none") + ylab("Casos Confirmados") +
        xlab("Data") + scale_x_continuous(breaks = 0:6,labels = paste(day(day_validate),"/0",
                                                                    month(day_validate),sep = "")) +
      theme(plot.title = element_text(face = "bold",size = 25,color = "white",hjust = 0.5),
            axis.text.x = element_text(size = 15,face = "bold",color = "white"),
            axis.text.y = element_text(size = 15,face = "bold",color = "white"),
            legend.box.margin = unit(x=c(20,0,0,0),units="mm"),
            legend.key.width=unit(3.5,"cm"),panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.title = element_text(color = "white",size = 20),
            plot.caption = element_text(face = "bold",color = "white",hjust = 0,size = 15)) +
      theme(plot.margin = unit(c(1,1,1,1), "lines")) +
      theme(strip.background = element_blank(),
            strip.text = element_text(size = 20,face = "bold",color = "white")) +
      labs(caption = "©IME - USP. Design: Diego Marcondes. Para mais informações e conteúdo sobre a COVID-19 acesse www.ime.usp.br/~pedrosp/covid19/") +
      ggtitle(paste("Crescimento exponencial na semana de validação para a DRS",d,"-",unique(drs$Regiao[drs$DRS == d])))
    pdf(file = paste("/storage/SEIR/",pos,"/AjusteRate/DRS_",gsub(" ","",unique(drs$Regiao[drs$DRS == d])),"_rate_",pos,".pdf",sep = ""),
        width = 15,height = 10)
    suppressWarnings(suppressMessages(print(p))) #Save plot
    dev.off()
  }
  
  #For each city with 100+ cases in init_validate
  c_100 <- obs %>% filter(date == ymd(init_validate) & confirmed_corrected >= 100)
  c_100 <- c_100$city
  for(c in c_100){
    tmp <- obs %>% filter(city == c & date >= ymd(init_validate) & date <= ymd(end_validate)) #Data of DRS
    tmp <- data.frame("t" = 0:6,"y" = tmp$confirmed_corrected) #Data
    mod <- lm(log(y) ~ t,data = tmp) #lm
    par$lambda[par$names == c] <- mod$coefficients[2] #Lambda
    p <- ggplot(tmp,aes(x = t,y = y)) + geom_point(color = "white") + 
      stat_function(fun = function(t) exp(mod$coefficients[1])*exp(mod$coefficients[2]*t),color = "white") +
      theme_solarized(light = FALSE) +  
      theme(legend.title = element_text(face = "bold"),legend.position = "none") + ylab("Casos Confirmados") +
      xlab("Data") + scale_x_continuous(breaks = 0:6,labels = paste(day(day_validate),"/0",
                                                                    month(day_validate),sep = "")) +
      theme(plot.title = element_text(face = "bold",size = 25,color = "white",hjust = 0.5),
            axis.text.x = element_text(size = 15,face = "bold",color = "white"),
            axis.text.y = element_text(size = 15,face = "bold",color = "white"),
            legend.box.margin = unit(x=c(20,0,0,0),units="mm"),
            legend.key.width=unit(3.5,"cm"),panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.title = element_text(color = "white",size = 20),
            plot.caption = element_text(face = "bold",color = "white",hjust = 0,size = 15)) +
      theme(plot.margin = unit(c(1,1,1,1), "lines")) +
      theme(strip.background = element_blank(),
            strip.text = element_text(size = 20,face = "bold",color = "white")) +
      labs(caption = "©IME - USP. Design: Diego Marcondes. Para mais informações e conteúdo sobre a COVID-19 acesse www.ime.usp.br/~pedrosp/covid19/") +
      ggtitle(paste("Crescimento exponencial na semana de validação para",c,"- SP"))
    pdf(file = paste("/storage/SEIR/",pos,"/AjusteRate/",gsub(" ","",c),"_rate_",pos,".pdf",sep = ""),
        width = 15,height = 10)
    suppressWarnings(suppressMessages(print(p))) #Save plot
    dev.off()
  }
    
  #Calculate death rate for each DRS
  for(d in unique(drs$DRS)){ #For each DRS
    tmpD <- teste_D %>% filter(DRS == d) #Death in DRS d
    tmpI <- teste_I %>% filter(DRS == d) #Confirmed in DRS d
    dr <- tmpD$D_drs[tmpD$date == end_validate]/tmpI$I_drs[tmpI$date == end_validate] #Death rate in DRS d in last day of validation
    par$delta[match(drs$Municipio[drs$DRS == d],par$names)] <- dr #Attribute death rate of DRS to each city
  }
    
  #Calculate death rate for each city with 10+ deaths
  C_10 <- obs %>% filter(date == init_validate & deaths_corrected >= 10) %>% unique() #Data of cities with 10+ deaths in last day of validation
  C_10 <- C_10$city #Get city name
  for(c in C_10){ #For each city with 50+ deaths by last_validation
    tmp <- obs %>% filter(city == c & date == end_validate) #Get data of city is last day of validation
    dr <- tmp$deaths_corrected/tmp$confirmed_corrected #Death rate
    par$delta[match(c,par$names)] <- dr #Attribute death rate
  }
  
  cat("Estimatimating the model...\n")
  #Choosing models
  pred <- vector("list",sample_size) #Store predicted values
  
  #Set progress bar
  pb <- progress_bar$new(
    format = ":letter [:bar] :elapsed | eta: :eta",
    total = sample_size,    # 100 
    width = 60)
  progress_letter <- paste(round(100*c(1:sample_size)/sample_size,2),"%")
  progress <- function(n){
    pb$tick(tokens = list(letter = progress_letter[n]))
  } 
  opts <- list(progress = progress)
  
  #Objects to store results
  results <- list()
  results$models <- vector("list",sample_size) #Store parameters of models
  kgood <- 0 #Number of good models
  is.good <- vector() #Track good models
  error <- vector() #track error
  minI <- 1
  maxI <- 1
  minD <- 1
  maxD <- 1
  mI <- 1
  mD <- 1
  
  for(k in 1:sample_size){#For each sampled model
    pb$tick(tokens = list(letter = paste(progress_letter[k],kgood,"D =",round(mD,5),"I =",round(mI,5)))) #Update progress bar
    
    #Parameters of model k
    parK <- list()
    parK$day <- day_validate #Days of validation
    parK$val <- TRUE #Is validation
    parK$mob <- par$mob #Mobility matrix
    parK$pop <- par$pop #Population
    parK$Te <- sample(x = par$Te,size = 1) #Te
    parK$Ta <- sample(x = par$Ta,size = 1) #Ta
    parK$Ts <- sample(x = par$Ts,size = 1) #Ts
    parK$Td <- sample(x = par$Td,size = 1) #Td
    parK$delta <- par$delta/parK$Td #delta
    parK$sites <- par$sites #Number of sites
    parK$s <- sample(x = par$s,size = 1) #s
    parK$upI <- par$lift*sample(x = c(6:10),size = 1) #Asymptomatic initial condition
    parK$gammaA <- parK$upI/((1+parK$upI)*parK$Te) #GammaA
    gammaS <- (1 - parK$Te*parK$gammaA)/parK$Te #Rate of Exposed to Symptomatic
    nuA <- 1/parK$Ta #Rate from Asymptomatic to Recovered
    nuS <- (1-parK$delta*parK$Td)/parK$Ts #Rate from Symptomatic to Recovered
    parK$upE <- par$lift/gammaS #To multiply number of new infected to get exposed
    initK <- init #Initial condition
    initK[1:par$sites] <- parK$upE*init[1:par$sites] #Correct
    initK[(par$sites + 1):(2*par$sites)] <- parK$upI*initK[(par$sites + 1):(2*par$sites)] #Assymptomatics
    initK[(3*par$sites + 1):(4*par$sites)] <- (parK$upI+1)*initK[(3*par$sites + 1):(4*par$sites)] #Correct R
    
    #Beta by DRS
    Sobs <- drs$N - parK$upE*par$obs_DRS$E[[as.character(0)]] - (1+parK$upI)*par$obs_DRS$Is[[as.character(0)]] - par$obs_DRS$R[[as.character(0)]] - 
      par$obs_DRS$D[[as.character(0)]]
    lambdaE <- 0.5*(log(par$lambda + nuS + parK$delta) + log(1+par$obs_DRS$Is[[as.character(1)]]) -
                      log(gammaS * parK$upE * (1+par$obs_DRS$E[[as.character(0)]]))) + 0.5*(log(par$lambda + nuA) + 
                                                                                          log(1+parK$upI*par$obs_DRS$Is[[as.character(0+1)]]) -
                                                                                          log(parK$gammaA * parK$upE * 
                                                                                                (1+par$obs_DRS$E[[as.character(0)]]))) #Growth rate
    num <- ((lambdaE + gammaS + parK$gammaA) * parK$upE * par$obs_DRS$E[[as.character(0)]] * (par$pop - par$obs_DRS$D[[as.character(0)]])) #Numerator
    den <- Sobs * (parK$s*((parK$mob[[as.character(init_validate-1)]]-
                                                      diag(diag(parK$mob[[as.character(init_validate-1)]]))) %*% 
                                                     cbind(par$obs_DRS$Is[[as.character(0)]] + parK$upI*par$obs_DRS$Is[[as.character(0)]])) + 
                                               (parK$upI+1)*par$obs_DRS$Is[[as.character(0)]]) #Denominator
    if(min(den) == 0) #Correct zero denominator
      den[den == 0] <- min(den[den > 0])
    parK$beta <- as.vector(num/den) #Beta
    
    #Cities with 100+ cases
    Sobs <- par$pop - parK$upE*par$obs$E[[as.character(0)]] - (1+parK$upI)*par$obs$Is[[as.character(0)]] - par$obs$R[[as.character(0)]] - 
      par$obs$D[[as.character(0)]]
    lambdaE <- 0.5*(log(par$lambda + nuS + parK$delta) + log(1+par$obs$Is[[as.character(1)]]) -
                      log(gammaS * parK$upE * (1+par$obs$E[[as.character(0)]]))) + 0.5*(log(par$lambda + nuA) + 
                                                                                              log(1+parK$upI*par$obs$Is[[as.character(0+1)]]) -
                                                                                              log(parK$gammaA * parK$upE * 
                                                                                                    (1+par$obs$E[[as.character(0)]]))) #Growth rate
    num <- ((lambdaE + gammaS + parK$gammaA) * parK$upE * par$obs$E[[as.character(0)]] * (par$pop - par$obs$D[[as.character(0)]])) #Numerator
    den <- Sobs * (parK$s*((parK$mob[[as.character(init_validate-1)]]-
                              diag(diag(parK$mob[[as.character(init_validate-1)]]))) %*% 
                             cbind(par$obs$Is[[as.character(0)]] + parK$upI*par$obs$Is[[as.character(0)]])) + 
                     (parK$upI+1)*par$obs$Is[[as.character(0)]]) #Denominator
    if(min(den) == 0) #Correct zero denominator
      den[den == 0] <- min(den[den > 0])
    b <- num/den
    parK$beta[par$names %in% c_100] <- b[par$names %in% c_100]
    if(min(parK$beta) < 0)
      parK$beta[parK$beta < 0] <- min(parK$beta[parK$beta > 0])
    
    #Model
    mod <- solve_seir(y = initK,times = 1:7,derivatives = derivatives,parms = parK)[,-1] #Simulate model k
      
    #Result
    D <- mod[,(4*parK$sites + 1):(5*parK$sites)] #Predicted death for testing
    I <- mod[,(5*parK$sites + 1):(6*parK$sites)] #Predicted cases for testing
      
    #Test if model predicted well
    
    #Deaths
    colnames(D) <- par$names
    D$date <- seq.Date(ymd(init_validate),ymd(end_validate),1) 
    D <- D %>% gather("Municipio","D",-date)
    D <- merge(D,drs)
    D$key <- paste(D$date,D$DRS)
    D <- data.table(D)
    D <- D[,D_pred := sum(D),by = key]
    D <- D %>% select(D_pred,key) %>% unique() %>% data.frame()
    D <- merge(D,teste_D)
    D$dif <- (D$D_pred - D$D_drs)/D$D_drs
    dif_D <- max(abs(D$dif)[D$D_drs > 50])
    
    #Cases
    colnames(I) <- par$names
    I$date <- seq.Date(ymd(init_validate),ymd(end_validate),1) 
    I <- I %>% gather("Municipio","I",-date)
    I <- merge(I,drs)
    I$key <- paste(I$date,I$DRS)
    I <- data.table(I)
    I <- I[,I_pred := sum(I),by = key]
    I <- I %>% select(I_pred,key) %>% unique() %>% data.frame()
    I <- merge(I,teste_I)
    I$dif <- (I$I_pred - I$I_drs)/I$I_drs
    dif_I <- max(abs(I$dif)[I$I_drs > 1000])
      
    #Is good
    good <- as.numeric(dif_I <= 0.05 & dif_D <= 0.05)
    is.good[k] <- good
    error[k] <- dif_D
    if(dif_I < mI)
      mI <- dif_I
    if(dif_D < mD)
      mD <- dif_D
    
    #Result
    if(good == 1){#Store good models
      #Prediction
      pred[[k]]$E <- mod[,1:parK$sites] #Prediction of E
      pred[[k]]$Ia <- mod[,(parK$sites + 1):(2*parK$sites)] #Prediction of Ia
      pred[[k]]$Is <- mod[,(2*parK$sites + 1):(3*parK$sites)] #Prediction of Is
      pred[[k]]$R <- mod[,(3*parK$sites + 1):(4*parK$sites)] #Prediction of R
      pred[[k]]$D <- mod[,(4*parK$sites + 1):(5*parK$sites)] #Prediction of D
      pred[[k]]$I <- mod[,(5*parK$sites + 1):(6*parK$sites)] #Total cases
      
      #Prediction of beta t0-1
      Sobs <- drs$N - parK$upE*par$obs_DRS$E[[as.character(6)]] - (1+parK$upI)*par$obs_DRS$Is[[as.character(6)]] - par$obs_DRS$R[[as.character(6)]] - 
        par$obs_DRS$D[[as.character(6)]]
      lambdaE <- 0.5*(log(par$lambda + nuS + parK$delta) + log(1+par$obs_DRS$Is[[as.character(7)]]) -
                        log(gammaS * parK$upE * (1+par$obs_DRS$E[[as.character(6)]]))) + 0.5*(log(par$lambda + nuA) + 
                                                                                                log(1+parK$upI*par$obs_DRS$Is[[as.character(7)]]) -
                                                                                                log(parK$gammaA * parK$upE * 
                                                                                                      (1+par$obs_DRS$E[[as.character(6)]]))) #Growth rate
      num <- ((lambdaE + gammaS + parK$gammaA) * parK$upE * par$obs_DRS$E[[as.character(6)]] * (par$pop - par$obs_DRS$D[[as.character(6)]])) #Numerator
      den <- Sobs * (parK$s*((parK$mob[[as.character(end_validate-1)]]-
                                diag(diag(parK$mob[[as.character(end_validate-1)]]))) %*% 
                               cbind(par$obs_DRS$Is[[as.character(6)]] + parK$upI*par$obs_DRS$Is[[as.character(6)]])) + 
                       (parK$upI+1)*par$obs_DRS$Is[[as.character(6)]]) #Denominator
      if(min(den) == 0) #Correct zero denominator
        den[den == 0] <- min(den[den > 0])
      parK$beta <- as.vector(num/den) #Beta
      
      #Cities with 100+ cases
      Sobs <- par$pop - parK$upE*par$obs$E[[as.character(6)]] - (1+parK$upI)*par$obs$Is[[as.character(6)]] - par$obs$R[[as.character(6)]] - 
        par$obs$D[[as.character(6)]]
      lambdaE <- 0.5*(log(par$lambda + nuS + parK$delta) + log(1+par$obs$Is[[as.character(7)]]) -
                        log(gammaS * parK$upE * (1+par$obs$E[[as.character(6)]]))) + 0.5*(log(par$lambda + nuA) + 
                                                                                            log(1+parK$upI*par$obs$Is[[as.character(7)]]) -
                                                                                            log(parK$gammaA * parK$upE * 
                                                                                                  (1+par$obs$E[[as.character(6)]]))) #Growth rate
      num <- ((lambdaE + gammaS + parK$gammaA) * parK$upE * par$obs$E[[as.character(6)]] * (par$pop - par$obs$D[[as.character(6)]])) #Numerator
      den <- Sobs * (parK$s*((parK$mob[[as.character(end_validate-1)]]-
                                diag(diag(parK$mob[[as.character(end_validate-1)]]))) %*% 
                               cbind(par$obs$Is[[as.character(6)]] + parK$upI*par$obs$Is[[as.character(6)]])) + 
                       (parK$upI+1)*par$obs$Is[[as.character(6)]]) #Denominator
      if(min(den) == 0) #Correct zero denominator
        den[den == 0] <- min(den[den > 0])
      b <- num/den
      parK$beta[par$names %in% c_100] <- b[par$names %in% c_100]
      if(min(parK$beta) < 0)
        parK$beta[parK$beta < 0] <- min(parK$beta[parK$beta > 0])
      pred[[k]]$beta <- parK$beta #Prediction of beta
      
      #Mean infected time and Rt
      Sobs <- par$pop - parK$upE*par$obs$E[[as.character(7)]] - (1+parK$upI)*par$obs$Is[[as.character(7)]] - par$obs$R[[as.character(7)]] - 
        par$obs$D[[as.character(7)]]
      Dobs <- par$obs$D[[as.character(7)]]
      parK$meanTi <- (parK$upI/(parK$upI + 1)) * parK$Ta + (1/(parK$upI + 1)) * (1-par$delta) * parK$Ts + (1/(parK$upI + 1)) * par$delta * parK$Td 
      parK$Rt <- parK$beta*Sobs/(par$pop - Dobs) + t(par$mob[[as.character(end_validate)]] - diag(diag(par$mob[[as.character(end_validate)]])))%*%
        cbind(parK$beta*Sobs/(par$pop - Dobs))
      parK$Rt <- parK$Rt*parK$meanTi
      
      pred[[k]]$meanTi <- parK$meanTi #Prediction of mean infection time
      pred[[k]]$Rt <- parK$Rt #Prediction of Rt
      
      kgood <- kgood + 1
      minDK <- ifelse(min(1 + D$dif[D$D_drs > 50]) < 1,min(1 + D$dif[D$D_drs > 50]),1)
      maxDK <- ifelse(max(1 + D$dif[D$D_drs > 50]) > 1,max(1 + D$dif[D$D_drs > 50]),1)
      minIK <- ifelse(min(1 + I$dif[I$I_drs > 1000]) < 1,min(1 + I$dif[I$I_drs > 1000]),1)
      maxIK <- ifelse(max(1 + I$dif[I$I_drs > 1000]) > 1,max(1 + I$dif[I$I_drs > 1000]),1)
      parK$minDK <- minDK
      parK$minIK <- minIK
      parK$maxDK <- maxDK
      parK$maxIK <- maxIK
      
      #Delete unecessary parameters
      parK$day <- NULL #Days of validation
      parK$val <- NULL #Is validation
      parK$mob <- NULL #Mobility matrix
      parK$pop <- NULL #Population
      
      results$models[[kgood]] <- parK
      if(minDK < minD)
        minD <- minDK
      if(minIK < minI)
        minI <- minIK
      if(maxDK > maxD)
        maxD <- maxDK
      if(maxIK > maxI)
        maxI <- maxIK
    }
    rm(parK,D,I,dif_D,dif_I,mod,good,initK,gammaS,nuA,nuS)
  }
  cat("\n")
  cat(paste("Good models: ",kgood," (",round(100*kgood/sample_size,2),"%)\n",sep = ""))
  cat("\n")

  cat("\n")
  cat("Saving parameters of good models...\n")
  
  #Saving parameters
  results$models <- results$models[unlist(lapply(results$models,function(x) ifelse(is.null(x),F,T)))] #Clean
  results$Vgood <- results$Vgood[unlist(lapply(results$Vgood,function(x) ifelse(is.null(x),F,T)))] #Clean
  Te <- unlist(lapply(results$models,function(x) x$Te)) #Te
  Ta <- unlist(lapply(results$models,function(x) x$Ta)) #Ta
  Ts <- unlist(lapply(results$models,function(x) x$Ts)) #Ts
  Td <- unlist(lapply(results$models,function(x) x$Td)) #Td
  cinfD <- unlist(lapply(results$models,function(x) x$minDK)) #cinfD
  cinfI <- unlist(lapply(results$models,function(x) x$minIK)) #cinfI
  csupD <- unlist(lapply(results$models,function(x) x$maxDK)) #cinfD
  csupI <- unlist(lapply(results$models,function(x) x$maxIK)) #cinfI
  gammaA <- unlist(lapply(lapply(results$models,function(x) x$gammaA*x$Te),median)) #Median gammaA
  s <- unlist(lapply(results$models,function(x) x$s)) #s
  upI <- lapply(results$models,function(x) x$upI) #Mutiply symptomatics to get assymptomatics
  assymptomatic <- unlist(lapply(upI,median)) #Mutiply symptomatics to get assymptomatics
  assymptomatic <- assymptomatic/(assymptomatic+1) #Assymptomatic
  upE <- lapply(results$models,function(x) x$upE) #Proportion of symptomatic which to put on exposed
  beta <- lapply(results$models,function(x) x$beta) #Beta
  betasave <- unlist(lapply(beta,median)) #Beta
  Rt <- lapply(results$models,function(x) x$Rt) #Rt
  Rtsave <- unlist(lapply(Rt,median)) #Rt
  pred <- pred[is.good == 1] #Prediction of only good models
  saveRDS(object = results,file = paste("/storage/SEIR/",pos,"/result_",pos,".rds",sep = "")) #Save results
  saveRDS(object = pred,file = paste("/storage/SEIR/",pos,"/prediction_",pos,".rds",sep = "")) #Save predictions
  #results <- readRDS(paste("/storage/SEIR/",pos,"/result_",pos,".rds",sep = "")) #Save results
  #pred <- readRDS(paste("/storage/SEIR/",pos,"/prediction_",pos,".rds",sep = "")) #Save predictions
  
  param <- data.frame("Model" = 1:kgood,Te,Ta,Ts,Td,s,gammaA,"MedianBeta" = betasave,"MedianRt" = Rtsave,"MedianAssymptomatic" = assymptomatic,
                      cinfD,csupD,cinfI,csupI) #Parameters
  fwrite(param,paste("/storage/SEIR/",pos,"/parameters_",pos,".csv",sep = "")) #Write parameters of good models
  
  cat("Plotting maps of parameters which are city dependent...\n")
    
  #Mapas
  shp <- readOGR(dsn = "~/mdyn/maps/sp_municipios/35MUE250GC_SIR.shp",stringsAsFactors = F,verbose = F) #Shapefiles
  shp$NM_MUNICIP <- gsub("'","",shp$NM_MUNICIP) #Correct names
  shp$NM_MUNICIP[shp$NM_MUNICIP == "BIRITIBA MIRIM"] <- "BIRITIBA-MIRIM"  #Correct names
  shp <- fortify(shp,region = "NM_MUNICIP") #Fortify
  rc_cont <- colorRampPalette(colors = c("white","orange","red"))(100)
  
  #Rt
  pRt <- lapply(Rt,function(x) data.frame(rbind(as.vector(x))))
  pRt <- bind_rows(pRt)
  colnames(pRt) <- par$names
  pRt <- apply(pRt,2,median)
  pRt <- data.frame("id" = names(pRt),"Rt" = pRt)
  pRt <- merge(pRt,drs %>% select(Municipio,Regiao),by.y = "Municipio",by.x = "id")
  pRt$Regiao <- as.character(pRt$Regiao)
  pRt$Regiao[pRt$Regiao == "Cidade de São Paulo"] <- "Grande São Paulo"
  pRt <- merge(pRt,shp)
  pRt <- pRt[order(pRt$order),]
  
  p <- ggplot(pRt,aes(long, lat,group=group,fill = log(1+Rt,2))) + theme_bw() + geom_polygon(colour='gray30') +
    xlab("") + ylab("") + scale_fill_gradientn("",colours = rc_cont,breaks = c(seq(1.1*min(log(1+pRt$Rt,2)),0.95*max(log(1+pRt$Rt,2)),length.out = 4)),
                                               labels = round(2^(seq(1.1*min(log(1+pRt$Rt,2)),0.95*max(log(1+pRt$Rt,2)),length.out = 4))-1,2),
                                               limits = c(min(log(1+pRt$Rt,2)),max(log(1+pRt$Rt,2)))) + titles_Map +
    ggtitle("Mediana do Rt estimado")
  pdf(file = paste("/storage/SEIR/",pos,"/SP_Rt_",pos,".pdf",sep = ""),width = 15,height = 10)
  suppressWarnings(suppressMessages(print(p)))
  dev.off()
  
  for(d in unique(pRt$Regiao)){
    tmp <- pRt %>% filter(Regiao == d)
    p <- ggplot(tmp,aes(long, lat,group=group,fill = log(1+Rt,2))) + theme_bw() + geom_polygon(colour='gray30') +
      xlab("") + ylab("") + scale_fill_gradientn("",colours = rc_cont,breaks = c(seq(1.1*min(log(1+pRt$Rt,2)),0.95*max(log(1+pRt$Rt,2)),length.out = 4)),
                                                 labels = round(2^(seq(1.1*min(log(1+pRt$Rt,2)),0.95*max(log(1+pRt$Rt,2)),length.out = 4))-1,2),
                                                 limits = c(min(log(1+pRt$Rt,2)),max(log(1+pRt$Rt,2)))) + 
      titles_Map +
      ggtitle(paste("Mediana do Rt estimado na DRS -",d))
    pdf(file = paste("/storage/SEIR/",pos,"/",gsub(" ","",d),"_Rt_",pos,".pdf",sep = ""),width = 15,height = 10)
    suppressWarnings(suppressMessages(print(p)))
    dev.off()
  }
  
  #Save Rt
  pRt <- data.frame("Municipio" = par$names,"Minimo" = apply(bind_rows(lapply(Rt,function(x) data.frame(rbind(as.vector(x))))),2,min),
                   "Mediana" = apply(bind_rows(lapply(Rt,function(x) data.frame(rbind(as.vector(x))))),2,median),
                   "Máximo" = apply(bind_rows(lapply(Rt,function(x) data.frame(rbind(as.vector(x))))),2,max))
  pRt <- merge(drs %>% select(Municipio,Regiao),pRt)
  names(pRt)[2] <- "DRS"
  pRt <- pRt[order(pRt$Mediana,decreasing = T),]
  write.csv(pRt,file = paste("/storage/SEIR/",pos,"/SP_Rt_",pos,".csv",sep = ""),row.names = F)
  
  #Assintomáticos
  pupI <- lapply(upI,function(x) data.frame(rbind(x)))
  pupI <- bind_rows(pupI)
  for(j in 1:ncol(pupI))
    pupI[,j] <- pupI[,j]/(1+pupI[,j])
  colnames(pupI) <- par$names
  pupI <- apply(pupI,2,median)
  pupI <- data.frame("id" = names(pupI),"upI" = pupI)
  pupI <- merge(pupI,drs %>% select(Municipio,Regiao),by.y = "Municipio",by.x = "id")
  pupI$Regiao <- as.character(pupI$Regiao)
  pupI$Regiao[pupI$Regiao == "Cidade de São Paulo"] <- "Grande São Paulo"
  pupI <- merge(pupI,shp)
  pupI <- pupI[order(pupI$order),]
  rc_ass <- colorRampPalette(colors = c("orange","red"))(100)
  
  p <- ggplot(pupI,aes(long, lat,group=group,fill = log(1+upI,2))) + theme_bw() + geom_polygon(colour='gray30') +
    xlab("") + ylab("") + 
    scale_fill_gradientn("",colours = rc_ass,breaks = c(seq(1.01*min(log(1+pupI$upI,2)),0.99*max(log(1+pupI$upI,2)),length.out = 4)),
                                               labels = round(2^(seq(1.01*min(log(1+pupI$upI,2)),0.99*max(log(1+pupI$upI,2)),length.out = 4))-1,2),
                         limits = c(min(log(1+pupI$upI,2)),max(log(1+pupI$upI,2)))) + 
    titles_Map +
    ggtitle("Mediana da proporção estimada de assintomáticos")
   pdf(file = paste("/storage/SEIR/",pos,"/SP_assymptomatics_",pos,".pdf",sep = ""),width = 15,height = 10)
   suppressWarnings(suppressMessages(print(p)))
   dev.off()
    
  for(d in unique(pupI$Regiao)){
   tmp <- pupI %>% filter(Regiao == d)
   p <- ggplot(tmp,aes(long, lat,group=group,fill = log(1+upI,2))) + theme_bw() + geom_polygon(colour='gray30') +
     xlab("") + ylab("") + 
     scale_fill_gradientn("",colours = rc_ass,breaks = c(seq(1.01*min(log(1+pupI$upI,2)),0.99*max(log(1+pupI$upI,2)),length.out = 4)),
                          labels = round(2^(seq(1.01*min(log(1+pupI$upI,2)),0.99*max(log(1+pupI$upI,2)),length.out = 4))-1,2),
                          limits = c(min(log(1+pupI$upI,2)),max(log(1+pupI$upI,2)))) + 
     titles_Map +
     ggtitle("Mediana da proporção estimada de assintomáticos")
   pdf(file = paste("/storage/SEIR/",pos,"/",gsub(" ","",d),"_assymptomatics_",pos,".pdf",sep = ""),width = 15,height = 10)
   suppressWarnings(suppressMessages(print(p)))
   dev.off()
  }
   
  #Save assymptomatics
  pA <- data.frame("Municipio" = par$names,"Minimo" = apply(bind_rows(lapply(upI,function(x) data.frame(rbind(x)))),2,min),
                  "Mediana" = apply(bind_rows(lapply(upI,function(x) data.frame(rbind(x)))),2,median),
                  "Máximo" = apply(bind_rows(lapply(upI,function(x) data.frame(rbind(x)))),2,max))
  pA <- merge(drs %>% select(Municipio,Regiao),pA)
  names(pA)[2] <- "DRS"
  for(j in 3:5)
    pA[,j] <- 100*pA[,j]/(1+pA[,j])
  pA <- pA[order(pA$Mediana,decreasing = T),]
  write.csv(pA,file = paste("/storage/SEIR/",pos,"/SP_assymptomatics_",pos,".csv",sep = ""),row.names = F)
    
  #######Plot by DRS#####
  cat("Plot observed and predicted number of deaths for each DRS...\n")
  system(paste("mkdir /storage/SEIR/",pos,"/validate",sep = ""))
  
  #For each DRS
  D <- vector("list",length(levels(drs$DRS)))
  names(D) <- levels(drs$DRS)
  for(k in 1:length(pred)){
    D_mod <- pred[[k]]$D
    colnames(D_mod) <- par$names
    D_mod$date <- seq.Date(ymd(init_validate),ymd(end_validate),1) 
    D_mod <- D_mod %>% gather("Municipio","D",-date)
    D_mod <- merge(D_mod,drs)
    D_mod$key <- paste(D_mod$date,D_mod$DRS)
    D_mod <- data.table(D_mod)
    D_mod <- D_mod[,D_pred := sum(D),by = key]
    D_mod <- D_mod %>% select(DRS,date,D_pred,key) %>% unique() %>% data.frame()
    names(D_mod)[3] <- paste("M",k,sep = "")
    for(d in unique(drs$DRS)){
      tmp <- D_mod %>% filter(DRS == d) %>% select(-DRS)
      if(is.null(D[[d]]))
        D[[d]] <- tmp 
      else
        D[[d]] <- merge(D[[d]],tmp %>% select(-date))
    }
  }
    
  #For each DRS cases
  I <- vector("list",length(levels(drs$DRS)))
  names(I) <- levels(drs$DRS)
  for(k in 1:length(pred)){
    I_mod <- pred[[k]]$I
    colnames(I_mod) <- par$names
    I_mod$date <- seq.Date(ymd(init_validate),ymd(end_validate),1) 
    I_mod <- I_mod %>% gather("Municipio","I",-date)
    I_mod <- merge(I_mod,drs)
    I_mod$key <- paste(I_mod$date,I_mod$DRS)
    I_mod <- data.table(I_mod)
    I_mod <- I_mod[,I_pred := sum(I),by = key]
    I_mod <- I_mod %>% select(DRS,date,I_pred,key) %>% unique() %>% data.frame()
    names(I_mod)[3] <- paste("M",k,sep = "")
    for(d in unique(drs$DRS)){
      tmp <- I_mod %>% filter(DRS == d) %>% select(-DRS)
      if(is.null(I[[d]]))
        I[[d]] <- tmp 
      else
        I[[d]] <- merge(I[[d]],tmp %>% select(-date))
    }
  }
    
  for(d in unique(drs$DRS)){
    D[[d]]$Dpred <- apply(X = D[[d]] %>% select(-date,-key),MARGIN = 1,FUN = median)
    D[[d]]$DpredInf <- minD*apply(X = D[[d]] %>% select(-date,-key),MARGIN = 1,FUN = min)
    D[[d]]$DpredInf[1] <- D[[d]]$DpredInf[1]/minD
    D[[d]]$DpredSup <- maxD*apply(X = D[[d]] %>% select(-date,-key),MARGIN = 1,FUN = max)
    D[[d]]$DpredSup[1] <- D[[d]]$DpredSup[1]/maxD
    I[[d]]$Ipred <- apply(X = I[[d]] %>% select(-date,-key),MARGIN = 1,FUN = median)
    I[[d]]$IpredInf <- minI*apply(X = I[[d]] %>% select(-date,-key),MARGIN = 1,FUN = min)
    I[[d]]$IpredInf[1] <- I[[d]]$IpredInf[1]/minI
    I[[d]]$IpredSup <- maxI*apply(X = I[[d]] %>% select(-date,-key),MARGIN = 1,FUN = max)
    I[[d]]$IpredSup[1] <- I[[d]]$IpredSup[1]/maxI
    tmp <- obs_drs %>% filter(ymd(date) >= ymd(end_validate)-31 & ymd(date) <= ymd(end_validate))
    tmp$key <- paste(tmp$date,tmp$DRS)
    tmp <- tmp[tmp$DRS == d,c(2,13)]
    names(tmp)[2] <- "D"
    D[[d]] <- merge(D[[d]],tmp,all = T)
    tmp <- obs_drs %>% filter(ymd(date) >= ymd(end_validate)-31 & ymd(date) <= ymd(end_validate))
    tmp$key <- paste(tmp$date,tmp$DRS)
    tmp <- tmp[tmp$DRS == d,c(2,12)]
    names(tmp)[2] <- "I"
    I[[d]] <- merge(I[[d]],tmp,all = T)
    
    #Plot
    if(max(I[[d]]$I) > 1000 | max(D[[d]]$D) > 50){
      tmp <- D[[d]]
      pD <- ggplot(tmp,aes(x = date)) + theme_solarized(light = FALSE) + geom_line(aes(y = D),color = "red") + 
        geom_line(aes(y = Dpred),linetype = "dashed",color = "red") +
        geom_ribbon(aes(ymin = DpredInf,ymax = DpredSup),fill = "red",alpha = 0.5) + xlab("Data") + ylab("Mortes confirmadas") +
        scale_x_date(breaks = seq.Date(from = min(ymd(tmp$date),na.rm = T),to = max(ymd(tmp$date),na.rm = T),by = 3),
                     labels = paste(day(seq.Date(from = min(ymd(tmp$date),na.rm = T),to = max(ymd(tmp$date),na.rm = T),by = 3)),"/0",
                                    month(seq.Date(from = min(ymd(tmp$date),na.rm = T),to = max(ymd(tmp$date),na.rm = T),by = 3)),sep = "")) +
        theme(legend.title = element_text(face = "bold"),legend.position = "none") +
        theme(plot.title = element_text(face = "bold",size = 25,color = "white",hjust = 0.5),
              axis.text.x = element_text(size = 15,face = "bold",color = "white"),
              axis.text.y = element_text(size = 15,face = "bold",color = "white"),
              legend.box.margin = unit(x=c(20,0,0,0),units="mm"),
              legend.key.width=unit(3.5,"cm"),panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.title = element_text(color = "white",size = 20),
              plot.caption = element_text(face = "bold",color = "white",hjust = 0,size = 15)) +
        theme(plot.margin = unit(c(1,1,1,1), "lines")) +
        theme(strip.background = element_blank(),
              strip.text = element_text(size = 20,face = "bold",color = "white")) +
        labs(caption = "©IME - USP. Design: Diego Marcondes. Para mais informações e conteúdo sobre a COVID-19 acesse www.ime.usp.br/~pedrosp/covid19/") +
      ggtitle(paste("Mortes confirmadas COVID-19 na DRS",d,"-",unique(drs$Regiao[drs$DRS == d])))
      pdf(file = paste("/storage/SEIR/",pos,"/validate/",gsub(" ","",unique(drs$Regiao[drs$DRS == d])),"_mortes.pdf",sep = ""),width = 15,height = 10)
     suppressWarnings(suppressMessages(print(pD)))
      dev.off()
    
      tmp <- I[[d]]
      pI <- ggplot(tmp,aes(x = date)) + theme_solarized(light = FALSE) + geom_line(aes(y = I),color = "red") + 
        geom_line(aes(y = Ipred),linetype = "dashed",color = "red") +
        geom_ribbon(aes(ymin = IpredInf,ymax = IpredSup),fill = "red",alpha = 0.5) + xlab("Data") + ylab("Casos confirmados") +
        scale_x_date(breaks = seq.Date(from = min(ymd(tmp$date)),to = max(ymd(tmp$date)),by = 3),
                     labels = paste(day(seq.Date(from = min(ymd(tmp$date),na.rm = T),to = max(ymd(tmp$date),na.rm = T),by = 3)),"/0",
                                    month(seq.Date(from = min(ymd(tmp$date),na.rm = T),to = max(ymd(tmp$date),na.rm = T),by = 3)),sep = "")) +
        theme(legend.title = element_text(face = "bold"),legend.position = "none") +
        theme(plot.title = element_text(face = "bold",size = 25,color = "white",hjust = 0.5),
              axis.text.x = element_text(size = 15,face = "bold",color = "white"),
              axis.text.y = element_text(size = 15,face = "bold",color = "white"),
              legend.box.margin = unit(x=c(20,0,0,0),units="mm"),
              legend.key.width=unit(3.5,"cm"),panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.title = element_text(color = "white",size = 20),
              plot.caption = element_text(face = "bold",color = "white",hjust = 0,size = 15)) +
        theme(plot.margin = unit(c(1,1,1,1), "lines")) +
        theme(strip.background = element_blank(),
              strip.text = element_text(size = 20,face = "bold",color = "white")) +
        labs(caption = "©IME - USP. Design: Diego Marcondes. Para mais informações e conteúdo sobre a COVID-19 acesse www.ime.usp.br/~pedrosp/covid19/") +
        ggtitle(paste("Casos confirmados COVID-19 na DRS",d,"-",unique(drs$Regiao[drs$DRS == d])))
      pdf(file = paste("/storage/SEIR/",pos,"/validate/",gsub(" ","",unique(drs$Regiao[drs$DRS == d])),"_casos.pdf",sep = ""),width = 15,height = 10)
      suppressWarnings(suppressMessages(print(pI)))
      dev.off()
    }
  }
   
  #Plot state
  tmp <- data.table(obs %>% filter(ymd(date) >= ymd(end_validate)-31 & ymd(date) <= ymd(end_validate))) #Notifications in validation period
  tmp <- tmp[,TD := sum(deaths_corrected),by = date] #Sum of deaths each day
  tmp <- tmp[,TI := sum(confirmed_corrected),by = date] #Sum of deaths each day
  tmp <- tmp %>% select(date,TD,TI) %>% unique()
  
  #Agregate predicted data
  c_pred <- data.frame("date" = seq.Date(from = ymd(init_validate),to = ymd(end_validate),by = 1),
                     "Ipred" = apply(rbindlist(lapply(lapply(X = pred,FUN = function(x) x$I),function(x) data.frame(rbind(rowSums(x))))),2,median),
                     "IpredInf" = minI*apply(rbindlist(lapply(lapply(X = pred,FUN = function(x) x$I),function(x) data.frame(rbind(rowSums(x))))),2,min),
                     "IpredSup" = maxI*apply(rbindlist(lapply(lapply(X = pred,FUN = function(x) x$I),function(x) data.frame(rbind(rowSums(x))))),2,max),
                     "Dpred" = apply(rbindlist(lapply(lapply(X = pred,FUN = function(x) x$D),function(x) data.frame(rbind(rowSums(x))))),2,median),
                     "DpredInf" = minD*apply(rbindlist(lapply(lapply(X = pred,FUN = function(x) x$D),function(x) data.frame(rbind(rowSums(x))))),2,min),
                     "DpredSup" = maxD*apply(rbindlist(lapply(lapply(X = pred,FUN = function(x) x$D),function(x) data.frame(rbind(rowSums(x))))),2,max))
  c_pred$DpredInf[1] <- c_pred$DpredInf[1]/minD
  c_pred$DpredSup[1] <- c_pred$DpredSup[1]/maxD
  c_pred$IpredInf[1] <- c_pred$IpredInf[1]/minI
  c_pred$IpredSup[1] <- c_pred$IpredSup[1]/maxI
  tmp <- merge(tmp,c_pred,all = TRUE) #Merge with observed data

  #D
  pD <- ggplot(tmp,aes(x = date)) + theme_solarized(light = FALSE) + geom_line(aes(y = TD),color = "red") + geom_line(aes(y = Dpred),
                                                                                                                      linetype = "dashed",
                                                                                                                      color = "red") +
      geom_ribbon(aes(ymin = DpredInf,ymax = DpredSup),fill = "red",alpha = 0.5) + xlab("Data") + ylab("Mortes confirmadas") +
      scale_x_date(breaks = seq.Date(from = min(ymd(tmp$date)),to = max(ymd(tmp$date)),by = 3),
                   labels = paste(day(seq.Date(from = min(ymd(tmp$date)),to = max(ymd(tmp$date)),by = 3)),"/0",
                                  month(seq.Date(from = min(ymd(tmp$date)),to = max(ymd(tmp$date)),by = 3)),sep = "")) +
      theme(legend.title = element_text(face = "bold"),legend.position = "none") +
      theme(plot.title = element_text(face = "bold",size = 25,color = "white",hjust = 0.5),
            axis.text.x = element_text(size = 15,face = "bold",color = "white"),
            axis.text.y = element_text(size = 15,face = "bold",color = "white"),
            legend.box.margin = unit(x=c(20,0,0,0),units="mm"),
            legend.key.width=unit(3.5,"cm"),panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.title = element_text(color = "white",size = 20),
            plot.caption = element_text(face = "bold",color = "white",hjust = 0,size = 15)) +
      theme(plot.margin = unit(c(1,1,1,1), "lines")) +
      theme(strip.background = element_blank(),
            strip.text = element_text(size = 20,face = "bold",color = "white")) +
      labs(caption = "©IME - USP. Design: Diego Marcondes. Para mais informações e conteúdo sobre a COVID-19 acesse www.ime.usp.br/~pedrosp/covid19/") +
      ggtitle("Mortes confirmadas COVID-19 no Estado de São Paulo")
  pdf(file = paste("/storage/SEIR/",pos,"/SP_mortes_",pos,".pdf",sep = ""),width = 15,height = 10)
  suppressWarnings(suppressMessages(print(pD)))
  dev.off()
  
  #I
  pI <- ggplot(tmp,aes(x = date)) + theme_solarized(light = FALSE) + geom_line(aes(y = TI),color = "red") + geom_line(aes(y = Ipred),
                                                                                                                      linetype = "dashed",
                                                                                                                      color = "red") +
    geom_ribbon(aes(ymin = IpredInf,ymax = IpredSup),fill = "red",alpha = 0.5) + xlab("Data") + ylab("Casos confirmados") +
    scale_x_date(breaks = seq.Date(from = min(ymd(tmp$date)),to = max(ymd(tmp$date)),by = 3),
                 labels = paste(day(seq.Date(from = min(ymd(tmp$date)),to = max(ymd(tmp$date)),by = 3)),"/0",
                                month(seq.Date(from = min(ymd(tmp$date)),to = max(ymd(tmp$date)),by = 3)),sep = "")) +
    theme(legend.title = element_text(face = "bold"),legend.position = "none") +
    theme(plot.title = element_text(face = "bold",size = 25,color = "white",hjust = 0.5),
          axis.text.x = element_text(size = 15,face = "bold",color = "white"),
          axis.text.y = element_text(size = 15,face = "bold",color = "white"),
          legend.box.margin = unit(x=c(20,0,0,0),units="mm"),
          legend.key.width=unit(3.5,"cm"),panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.title = element_text(color = "white",size = 20),
          plot.caption = element_text(face = "bold",color = "white",hjust = 0,size = 15)) +
    theme(plot.margin = unit(c(1,1,1,1), "lines")) +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 20,face = "bold",color = "white")) +
    labs(caption = "©IME - USP. Design: Diego Marcondes. Para mais informações e conteúdo sobre a COVID-19 acesse www.ime.usp.br/~pedrosp/covid19/") +
    ggtitle("Casos confirmados COVID-19 no Estado de São Paulo")
  pdf(file = paste("/storage/SEIR/",pos,"/SP_casos_",pos,".pdf",sep = ""),width = 15,height = 10)
  suppressWarnings(suppressMessages(print(pI)))
  dev.off()

  #######Simulation of scenarios########
  cat("Simulating scenarios...\n")
  init <- vector()
  tmp <- obs %>% filter(date == ymd(end_validate))
  tmp <- tmp[match(x = par$names,table = tmp$city),]
  init[1:par$sites] <- tmp$new_infected_mean #E
  init[(par$sites + 1):(2*par$sites)] <- tmp$infected #Ia
  init[(2*par$sites + 1):(3*par$sites)] <- tmp$infected #Is
  init[(3*par$sites + 1):(4*par$sites)] <- tmp$recovered #R
  init[(4*par$sites + 1):(5*par$sites)] <- tmp$deaths_corrected #D
  init[(5*par$sites + 1):(6*par$sites)] <- tmp$confirmed_corrected #prevalence

  #Simulate scenario everything is as week after end validation
  good_models <- length(pred) #number of good models
  predSIM <- vector("list",good_models) #To store the predicted values of the simulation
  pb <- progress_bar$new(
    format = "Iterations = :letter [:bar] :elapsed | eta: :eta",
    total = good_models,    # 100 
    width = 60)
  progress_letter <- paste(round(100*c(1:good_models)/good_models,2),"%")
  progress <- function(n){
    pb$tick(tokens = list(letter = progress_letter[n]))
  } 
  opts <- list(progress = progress)

  for(k in 1:good_models){
    pb$tick(tokens = list(letter = progress_letter[k]))
    
    #Parameters of model k
    parK <- list()
    parK$day <- seq.Date(from = ymd(init_simulate),to = ymd(init_simulate) + simulate_length - 1,by = 1) #Days of validation
    parK$val <- F #Is validation
    parK$mob <- par$mob #Mobility matrix
    parK$pop <- par$pop #Population
    parK$Te <-  results$models[[k]]$Te#Te
    parK$Ta <- results$models[[k]]$Ta #Ta
    parK$Ts <- results$models[[k]]$Ts #Ts
    parK$Td <- results$models[[k]]$Ts #Td
    parK$delta <- results$models[[k]]$delta #delta
    parK$sites <- par$sites #Number of sites
    parK$s <- results$models[[k]]$s #s
    parK$upI <- results$models[[k]]$upI #Asymptomatic initial condition
    parK$gammaA <- results$models[[k]]$gammaA #GammaA
    parK$upE <- results$models[[k]]$upE #To multiply number of new infected to get exposed
    initK <- init #Initial condition
    initK[1:par$sites] <- parK$upE*init[1:par$sites] #Correct
    initK[(par$sites + 1):(2*par$sites)] <- parK$upI*initK[(par$sites + 1):(2*par$sites)] #Assymptomatics
    initK[(3*par$sites + 1):(4*par$sites)] <- (parK$upI+1)*initK[(3*par$sites + 1):(4*par$sites)] #Correct R
    parK$beta <- results$models[[k]]$beta #Beta
    
    #Model
    mod <- solve_seir(y = initK,times = 1:simulate_length,derivatives = derivatives,parms = parK)[,-1]
  
    #Result
    predSIM[[k]]$E <- mod[,1:parK$sites] #E
    predSIM[[k]]$Ia <- mod[,(parK$sites + 1):(2*parK$sites)] #Ia
    predSIM[[k]]$Is <- mod[,(2*parK$sites + 1):(3*parK$sites)] #Is
    predSIM[[k]]$R <- mod[,(3*parK$sites + 1):(4*parK$sites)] #R
    predSIM[[k]]$D <- mod[,(4*parK$sites + 1):(5*parK$sites)] #D
    predSIM[[k]]$I <- mod[,(5*parK$sites + 1):(6*parK$sites)] #Total Infected
    predSIM[[k]]$S <- par$pop - pred[[k]]$E - pred[[k]]$Ia - pred[[k]]$Is - pred[[k]]$R - pred[[k]]$D
  }
  
  cat("Storing results of simulation...\n")
  
  #####Results of simulation#####
  peak <- data.frame("Municipio" = NA,"TMinimo" = NA,"TMediana" = NA,"TMaximo" = NA,"MMinimo" = NA,"MMediana" = NA,"MMaximo" = NA)
  deaths <- list()
  deaths$inf <- matrix(nrow = simulate_length,ncol = par$sites)
  deaths$sup <- matrix(nrow = simulate_length,ncol = par$sites)
  deaths$median <- matrix(nrow = simulate_length,ncol = par$sites)
  deaths_all <- data.frame("Municipio" = NA,"Data" = NA,"Modelo" = NA,"Mortes" = NA)
  cases <- list()
  cases$inf <- matrix(nrow = simulate_length,ncol = par$sites)
  cases$sup <- matrix(nrow = simulate_length,ncol = par$sites)
  cases$median <- matrix(nrow = simulate_length,ncol = par$sites)
  cases_all <- data.frame("Municipio" = NA,"Data" = NA,"Modelo" = NA,"Casos" = NA)
  
  #mkdir
  system(paste("mkdir /storage/SEIR/",pos,"/EPCurve/",sep = ""))
  
  pb <- progress_bar$new(
    format = "Iterations = :letter [:bar] :elapsed | eta: :eta",
    total = par$sites,    # 100 
    width = 60)
  progress_letter <- paste(round(100*c(1:par$sites)/par$sites,2),"%")
  progress <- function(n){
    pb$tick(tokens = list(letter = progress_letter[n]))
  } 
  opts <- list(progress = progress)
  i <- 1
  for(c in par$names){
    pb$tick(tokens = list(letter = progress_letter[i]))
    i <- i + 1
    
    #data for plot
    position <- match(x = c,table = par$names)
    
    #Get simulated data
    c_pred <- data.frame("date" = seq.Date(from = ymd(end_validate),to = ymd(end_validate)+simulate_length-1,by = 1),
                         "Epred" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$E[,position])))),2,median),
                         "EpredInf" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$E[,position])))),2,min),
                         "EpredSup" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$E[,position])))),2,max),
                         "Ispred" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$Is[,position])))),2,median),
                         "IspredInf" = minI*apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$Is[,position])))),2,min),
                         "IspredSup" = maxI*apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$Is[,position])))),2,max),
                         "Iapred" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$Ia[,position])))),2,median),
                         "IapredInf" = minI*apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$Ia[,position])))),2,min),
                         "IapredSup" = maxI*apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$Ia[,position])))),2,max),
                         "Ipred" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$I[,position])))),2,median),
                         "IpredInf" = minI*apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$I[,position])))),2,min),
                         "IpredSup" = maxI*apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$I[,position])))),2,max),
                         "Rpred" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$R[,position])))),2,median),
                         "RpredInf" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$R[,position])))),2,min),
                         "RpredSup" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$R[,position])))),2,max),
                         "Dpred" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$D[,position])))),2,median),
                         "DpredInf" = minD*apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$D[,position])))),2,min),
                         "DpredSup" = maxD*apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$D[,position])))),2,max))
    c_pred$DpredInf[1] <- c_pred$DpredInf[1]/minD
    c_pred$DpredSup[1] <- c_pred$DpredSup[1]/maxD
    c_pred$IapredInf[1] <- c_pred$IapredInf[1]/minI
    c_pred$IapredSup[1] <- c_pred$IapredSup[1]/maxI
    c_pred$IspredInf[1] <- c_pred$IspredInf[1]/minI
    c_pred$IspredSup[1] <- c_pred$IspredSup[1]/maxI
    c_pred$IpredInf[1] <- c_pred$IpredInf[1]/minI
    c_pred$IpredSup[1] <- c_pred$IpredSup[1]/maxI
    deaths$inf[,position] <- c_pred$DpredInf #Dinf
    deaths$sup[,position] <- c_pred$DpredSup #Dsup
    deaths$median[,position] <- c_pred$Dpred #Dpred
    cases$inf[,position] <- c_pred$IpredInf #Iinf
    cases$sup[,position] <- c_pred$IpredSup #Isup
    cases$median[,position] <- c_pred$Ipred #Ipred
    pd <- unlist(lapply(predSIM,function(x) which(diff(x$D[,position]) == max(diff(x$D[,position]),na.rm = T)))) #Peak
    mpd <- unlist(lapply(predSIM,function(x) max(diff(x$D[,position])))) #Peak
    peak <- rbind.data.frame(peak,data.frame("Municipio" = c,"TMinimo" = as.character(ymd(end_validate)+min(pd)),
                                             "TMediana" = as.character(ymd(end_validate)+median(pd)),
                                             "TMaximo" = as.character(ymd(end_validate)+max(pd)),
                                             "MMinimo" = min(mpd),"MMediana" = median(mpd),"MMaximo" = max(mpd))) #Peak
    
    #Store all predicted deaths
    tmp <- unlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$D[,position]))))
    tmp <- data.frame("Municipio" = c,expand.grid(1:simulate_length,1:length(predSIM)),"Mortes" = tmp)
    names(tmp)[2:3] <- c("Data","Modelo")
    tmp$Data <- as.character(ymd(end_validate) + tmp$Data - 1)
    deaths_all <- rbind.data.frame(deaths_all,tmp)
    
    #Store all predicted cases
    tmp <- unlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$I[,position]))))
    tmp <- data.frame("Municipio" = c,expand.grid(1:simulate_length,1:length(predSIM)),"Casos" = tmp)
    names(tmp)[2:3] <- c("Data","Modelo")
    tmp$Data <- as.character(ymd(end_validate) + tmp$Data - 1)
    cases_all <- rbind.data.frame(cases_all,tmp)
    
    #Epidemiological curve
    if(c_pred$Ipred[1] > 100){
      tmp <- c_pred
      p <- ggplot(tmp,aes(x = ymd(date),group = 1)) + geom_vline(xintercept = ymd(as.matrix(rbind(peak[nrow(peak),2:4]))[1,]),color = "white",
                                                                    linetype = "dashed") + 
        geom_line(aes(y = Ispred, color = "a")) + geom_ribbon(aes(ymin = IspredInf,ymax = IspredSup,fill = "a"),alpha = 0.25) +
        geom_line(aes(y = Dpred, color = "c")) + geom_ribbon(aes(ymin = DpredInf,ymax = DpredSup,fill = "c"),alpha = 0.25) + 
        theme_solarized(light = FALSE) +  scale_x_date(breaks = seq.Date(ymd(end_validate),ymd(end_validate)+simulate_length,length.out = 12),
                                                       labels = strftime(seq.Date(ymd(end_validate),ymd(end_validate)+simulate_length,length.out = 12),
                                                                         format="%d/%m/%y")) + 
        scale_y_continuous(breaks = round(seq(min(c(tmp$DpredInf,tmp$IspredInf)),max(c(tmp$DpredSup,tmp$IspredSup)),length.out = 10))) +
        theme(legend.title = element_text(face = "bold"),legend.position = "bottom") + ylab("Indivíduos") +
        xlab("Data") + scale_colour_discrete("",labels = c("Infectados","Total de Óbitos")) +
        theme(plot.title = element_text(face = "bold",size = 25,color = "white",hjust = 0.5),
              axis.text.x = element_text(size = 15,face = "bold",color = "white"),
              legend.text = element_text(size = 15,face = "bold",color = "white"),
              axis.text.y = element_text(size = 15,face = "bold",color = "white"),
              legend.key.width=unit(3.5,"cm"),panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.title = element_text(color = "white",size = 20),
              plot.caption = element_text(face = "bold",color = "white",hjust = 0,size = 15)) +
        theme(plot.margin = unit(c(1,1,1,1), "lines")) + scale_fill_discrete(guide = FALSE) + 
        theme(strip.background = element_blank(),
              strip.text = element_text(size = 20,face = "bold",color = "white")) +
        labs(caption = "©IME - USP. Design: Diego Marcondes. Para mais informações e conteúdo sobre a COVID-19 acesse www.ime.usp.br/~pedrosp/covid19/") +
        ggtitle(paste("Curva epidemiológica prevista para a cidade de",c,"- SP"))
        pdf(file = paste("/storage/SEIR/",pos,"/EPCurve/",gsub(" ","",c),"_EPCurve_",pos,".pdf",sep = ""),width = 15,height = 10)
        suppressWarnings(suppressMessages(print(p))) #Save plot
        dev.off()
    }
  }
  
  #Saving deaths
  tmpI <- data.frame(deaths$inf)
  colnames(tmpI) <- par$names
  tmpI$Date <- seq.Date(ymd(end_validate),ymd(end_validate)+simulate_length-1,1)
  tmpI <- tmpI %>% gather("Municipio","Infimo",-Date)
  tmpI$key <- paste(tmpI$Municipio,tmpI$Date)
  
  tmpm <- data.frame(deaths$median)
  colnames(tmpm) <- par$names
  tmpm$Date <- seq.Date(ymd(end_validate),ymd(end_validate)+simulate_length-1,1)
  tmpm <- tmpm %>% gather("Municipio","Mediana",-Date)
  tmpm$key <- paste(tmpm$Municipio,tmpm$Date)
  tmpm$Municipio <- NULL
  tmpm$Date <- NULL
  
  tmpS <- data.frame(deaths$sup)
  colnames(tmpS) <- par$names
  tmpS$Date <- seq.Date(ymd(end_validate),ymd(end_validate)+simulate_length-1,1)
  tmpS <- tmpS %>% gather("Municipio","Sup",-Date)
  tmpS$key <- paste(tmpS$Municipio,tmpS$Date)
  tmpS$Municipio <- NULL
  tmpS$Date <- NULL
  
  tmp <- merge(tmpI,tmpm)
  tmp <- merge(tmp,tmpS)
  tmp$key <- NULL
  Dsim <- tmp
  fwrite(tmp,paste("/storage/SEIR/",pos,"/deaths_",pos,".csv",sep = ""))
  
  #Saving Cases
  tmpI <- data.frame(cases$inf)
  colnames(tmpI) <- par$names
  tmpI$Date <- seq.Date(ymd(end_validate),ymd(end_validate)+simulate_length-1,1)
  tmpI <- tmpI %>% gather("Municipio","Infimo",-Date)
  tmpI$key <- paste(tmpI$Municipio,tmpI$Date)
  
  tmpm <- data.frame(cases$median)
  colnames(tmpm) <- par$names
  tmpm$Date <- seq.Date(ymd(end_validate),ymd(end_validate)+simulate_length-1,1)
  tmpm <- tmpm %>% gather("Municipio","Mediana",-Date)
  tmpm$key <- paste(tmpm$Municipio,tmpm$Date)
  tmpm$Municipio <- NULL
  tmpm$Date <- NULL
  
  tmpS <- data.frame(cases$sup)
  colnames(tmpS) <- par$names
  tmpS$Date <- seq.Date(ymd(end_validate),ymd(end_validate)+simulate_length-1,1)
  tmpS <- tmpS %>% gather("Municipio","Sup",-Date)
  tmpS$key <- paste(tmpS$Municipio,tmpS$Date)
  tmpS$Municipio <- NULL
  tmpS$Date <- NULL
  
  tmp <- merge(tmpI,tmpm)
  tmp <- merge(tmp,tmpS)
  tmp$key <- NULL
  Isim <- tmp
  fwrite(tmp,paste("/storage/SEIR/",pos,"/cases_",pos,".csv",sep = ""))
  
  #Saving all predictions
  deaths_all <- na.omit(deaths_all)
  cases_all <- na.omit(cases_all)
  fwrite(deaths_all,paste("/storage/SEIR/",pos,"/deaths_all_",pos,".csv",sep = "")) #Save all predicted deaths
  fwrite(cases_all,paste("/storage/SEIR/",pos,"/cases_all_",pos,".csv",sep = "")) #Save all predicted cases
  
  #Peak
  peak <- na.omit(peak)
  for(i in 2:4)
    peak[,i] <- ymd(peak[,i])
  fwrite(peak,paste("/storage/SEIR/",pos,"/peak_",pos,".csv",sep = ""))
  
  cat("Just one more moment, while I calculate some thins for the DRSs...\n")
  
  #DRS
  peak <- data.frame("DRS" = NA,"TMinimo" = NA,"TMediana" = NA,"TMaximo" = NA,"MMinimo" = NA,"MMediana" = NA,"MMaximo" = NA)
  deaths <- list()
  deaths$inf <- matrix(nrow = simulate_length,ncol = length(unique(drs$DRS)))
  deaths$sup <- matrix(nrow = simulate_length,ncol = length(unique(drs$DRS)))
  deaths$median <- matrix(nrow = simulate_length,ncol = length(unique(drs$DRS)))
  cases <- list()
  cases$inf <- matrix(nrow = simulate_length,ncol = length(unique(drs$DRS)))
  cases$sup <- matrix(nrow = simulate_length,ncol = length(unique(drs$DRS)))
  cases$median <- matrix(nrow = simulate_length,ncol = length(unique(drs$DRS)))
  
  for(d in unique(drs$DRS)){
    position <- match(drs$Municipio[drs$DRS == d],par$names)
    c_pred <- list()
    for(v in c("E","Is","Ia","I","R","D")){
      tmp <- list()
      for(k in 1:length(position))
        tmp[[k]] <- lapply(X = predSIM,FUN = function(x) data.frame(rbind(x[[v]][,position[k]])))
      if(length(position) >= 2){
        for(i in 1:length(tmp[[1]]))
          for(k in 2:length(position))
            tmp[[1]][[i]] <- tmp[[1]][[i]] + tmp[[k]][[i]]
      }
      if(v == "D"){
        pd <- unlist(lapply(tmp[[1]],function(x) which(diff(as.vector(t(x[1,]))) == max(diff(as.vector(t(x[1,]))),na.rm = T)))) #Peak
        mpd <- unlist(lapply(tmp[[1]],function(x) max(diff(as.vector(t(x[1,])))))) #Peak
      }
      c_pred[[v]] <- rbindlist(tmp[[1]])
    }
    c_pred <-  data.frame("date" = seq.Date(from = ymd(end_validate),to = ymd(end_validate)+simulate_length-1,by = 1),
                           "Epred" = apply(c_pred$E,2,median),
                           "EpredInf" = apply(c_pred$E,2,min),
                           "EpredSup" = apply(c_pred$E,2,max),
                           "Ispred" = apply(c_pred$Is,2,median),
                           "IspredInf" = minI*apply(c_pred$Is,2,min),
                           "IspredSup" = maxI*apply(c_pred$Is,2,max),
                           "Iapred" = apply(c_pred$Ia,2,median),
                           "IapredInf" = minI*apply(c_pred$Ia,2,min),
                           "IapredSup" = maxI*apply(c_pred$Ia,2,max),
                           "Ipred" = apply(c_pred$I,2,median),
                           "IpredInf" = minI*apply(c_pred$I,2,min),
                           "IpredSup" = maxI*apply(c_pred$I,2,max),
                           "Rpred" = apply(c_pred$R,2,median),
                           "RpredInf" = apply(c_pred$R,2,min),
                           "RpredSup" = apply(c_pred$R,2,max),
                           "Dpred" = apply(c_pred$D,2,median),
                           "DpredInf" = minD*apply(c_pred$D,2,min),
                           "DpredSup" = maxD*apply(c_pred$D,2,max))
    c_pred$DpredInf[1] <- c_pred$DpredInf[1]/minD
    c_pred$DpredSup[1] <- c_pred$DpredSup[1]/maxD
    c_pred$IapredInf[1] <- c_pred$IapredInf[1]/minI
    c_pred$IapredSup[1] <- c_pred$IapredSup[1]/maxI
    c_pred$IspredInf[1] <- c_pred$IspredInf[1]/minI
    c_pred$IspredSup[1] <- c_pred$IspredSup[1]/maxI
    c_pred$IpredInf[1] <- c_pred$IpredInf[1]/minI
    c_pred$IpredSup[1] <- c_pred$IpredSup[1]/maxI
    
    #Save
    position <- match(d,unique(drs$DRS))
    deaths$inf[,position] <- c_pred$DpredInf #Dinf
    deaths$sup[,position] <- c_pred$DpredSup #Dsup
    deaths$median[,position] <- c_pred$Dpred #Dpred
    cases$inf[,position] <- c_pred$IpredInf #Iinf
    cases$sup[,position] <- c_pred$IpredSup #Isup
    cases$median[,position] <- c_pred$Ipred #Ipred
    peak <- rbind.data.frame(peak,data.frame("DRS" = d,"TMinimo" = as.character(ymd(end_validate)+min(pd)),
                                             "TMediana" = as.character(ymd(end_validate)+median(pd)),
                                             "TMaximo" = as.character(ymd(end_validate)+max(pd)),
                                             "MMinimo" = min(mpd),"MMediana" = median(mpd),"MMaximo" = max(mpd))) #Peak
    #Epidemiological curve
    tmp <- c_pred
    p <- ggplot(tmp,aes(x = ymd(date),group = 1)) + geom_vline(xintercept = ymd(as.matrix(rbind(peak[nrow(peak),2:4]))[1,]),color = "white",
                                                                    linetype = "dashed") +
        geom_line(aes(y = Ispred, color = "a")) + geom_ribbon(aes(ymin = IspredInf,ymax = IspredSup,fill = "a"),alpha = 0.25) +
        geom_line(aes(y = Dpred, color = "c")) + geom_ribbon(aes(ymin = DpredInf,ymax = DpredSup,fill = "c"),alpha = 0.25) + 
        theme_solarized(light = FALSE) +  scale_x_date(breaks = seq.Date(ymd(end_validate),ymd(end_validate)+simulate_length,length.out = 12),
                                                       labels = strftime(seq.Date(ymd(end_validate),ymd(end_validate)+simulate_length,length.out = 12),
                                                                         format="%d/%m/%y")) + 
      scale_y_continuous(breaks = round(seq(min(tmp$Dpred),max(tmp$IspredSup),length.out = 10))) +
        theme(legend.title = element_text(face = "bold"),legend.position = "bottom") + ylab("Indivíduos") +
        xlab("Data") + scale_colour_discrete("",labels = c("Infectados","Total de Óbitos")) +
        theme(plot.title = element_text(face = "bold",size = 25,color = "white",hjust = 0.5),
              axis.text.x = element_text(size = 15,face = "bold",color = "white"),
              legend.text = element_text(size = 15,face = "bold",color = "white"),
              axis.text.y = element_text(size = 15,face = "bold",color = "white"),
              legend.key.width=unit(3.5,"cm"),panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.title = element_text(color = "white",size = 20),
              plot.caption = element_text(face = "bold",color = "white",hjust = 0,size = 15)) +
        theme(plot.margin = unit(c(1,1,1,1), "lines")) + scale_fill_discrete(guide = FALSE) + 
        theme(strip.background = element_blank(),
              strip.text = element_text(size = 20,face = "bold",color = "white")) +
        labs(caption = "©IME - USP. Design: Diego Marcondes. Para mais informações e conteúdo sobre a COVID-19 acesse www.ime.usp.br/~pedrosp/covid19/") +
        ggtitle(paste("Curva epidemiológica prevista para DRS",unique(drs$Regiao[drs$DRS == d])))
        pdf(file = paste("/storage/SEIR/",pos,"/EPCurve/DRS_",gsub(" ","",unique(drs$Regiao[drs$DRS == d])),"_EPCurve_",pos,".pdf",sep = ""),
            width = 15,height = 10)
        suppressWarnings(suppressMessages(print(p))) #Save plot
        dev.off()
  } 
  
  #Saving deaths
  tmpI <- data.frame(deaths$inf)
  colnames(tmpI) <- unique(drs$DRS)
  tmpI$Date <- seq.Date(ymd(end_validate),ymd(end_validate)+simulate_length-1,1)
  tmpI <- tmpI %>% gather("DRS","Infimo",-Date)
  tmpI$key <- paste(tmpI$DRS,tmpI$Date)
  
  tmpm <- data.frame(deaths$median)
  colnames(tmpm) <- unique(drs$DRS)
  tmpm$Date <- seq.Date(ymd(end_validate),ymd(end_validate)+simulate_length-1,1)
  tmpm <- tmpm %>% gather("DRS","Mediana",-Date)
  tmpm$key <- paste(tmpm$DRS,tmpm$Date)
  tmpm$DRS <- NULL
  tmpm$Date <- NULL
  
  tmpS <- data.frame(deaths$sup)
  colnames(tmpS) <- unique(drs$DRS)
  tmpS$Date <- seq.Date(ymd(end_validate),ymd(end_validate)+simulate_length-1,1)
  tmpS <- tmpS %>% gather("DRS","Sup",-Date)
  tmpS$key <- paste(tmpS$DRS,tmpS$Date)
  tmpS$DRS <- NULL
  tmpS$Date <- NULL
  
  tmp <- merge(tmpI,tmpm)
  tmp <- merge(tmp,tmpS)
  tmp$key <- NULL
  Dsim_drs <- tmp
  tmp <- merge(tmp,unique(drs %>% select(DRS,Regiao)))
  fwrite(tmp,paste("/storage/SEIR/",pos,"/deaths_DRS_",pos,".csv",sep = ""))
  
  #Saving Cases
  tmpI <- data.frame(cases$inf)
  colnames(tmpI) <- unique(drs$DRS)
  tmpI$Date <- seq.Date(ymd(end_validate),ymd(end_validate)+simulate_length-1,1)
  tmpI <- tmpI %>% gather("DRS","Infimo",-Date)
  tmpI$key <- paste(tmpI$DRS,tmpI$Date)
  
  tmpm <- data.frame(cases$median)
  colnames(tmpm) <- unique(drs$DRS)
  tmpm$Date <- seq.Date(ymd(end_validate),ymd(end_validate)+simulate_length-1,1)
  tmpm <- tmpm %>% gather("DRS","Mediana",-Date)
  tmpm$key <- paste(tmpm$DRS,tmpm$Date)
  tmpm$DRS <- NULL
  tmpm$Date <- NULL
  
  tmpS <- data.frame(cases$sup)
  colnames(tmpS) <- unique(drs$DRS)
  tmpS$Date <- seq.Date(ymd(end_validate),ymd(end_validate)+simulate_length-1,1)
  tmpS <- tmpS %>% gather("DRS","Sup",-Date)
  tmpS$key <- paste(tmpS$DRS,tmpS$Date)
  tmpS$DRS <- NULL
  tmpS$Date <- NULL
  
  tmp <- merge(tmpI,tmpm)
  tmp <- merge(tmp,tmpS)
  tmp$key <- NULL
  Isim_drs <- tmp
  tmp <- merge(tmp,unique(drs %>% select(DRS,Regiao)))
  fwrite(tmp,paste("/storage/SEIR/",pos,"/cases_DRS_",pos,".csv",sep = ""))
  
  #Peak
  peak <- na.omit(peak)
  for(i in 2:4)
    peak[,i] <- ymd(peak[,i])
  peak <- merge(peak,unique(drs %>% select(DRS,Regiao)))
  fwrite(peak,paste("/storage/SEIR/",pos,"/peak_DRS_",pos,".csv",sep = ""))
  
  #Curve for state
  c_pred <- data.frame("date" = seq.Date(from = ymd(end_validate),to = ymd(end_validate)+simulate_length-1,by = 1),
                         "Epred" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$E))))),2,median),
                         "EpredInf" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$E))))),2,min),
                         "EpredSup" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$E))))),2,max),
                         "Ispred" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$Is))))),2,median),
                         "IspredInf" = minI*apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$Is))))),2,min),
                         "IspredSup" = maxI*apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$Is))))),2,max),
                         "Iapred" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$Ia))))),2,median),
                         "IapredInf" = minI*apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$Ia))))),2,min),
                         "IapredSup" = maxI*apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$Ia))))),2,max),
                         "Ipred" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$I))))),2,median),
                         "IpredInf" = minI*apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$I))))),2,min),
                         "IpredSup" = maxI*apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$Ia))))),2,max),
                         "Rpred" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$R))))),2,median),
                         "RpredInf" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$R))))),2,min),
                         "RpredSup" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$R))))),2,max),
                         "Dpred" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$D))))),2,median),
                         "DpredInf" = minD*apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$D))))),2,min),
                         "DpredSup" = maxD*apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$D))))),2,max))
  p <- ggplot(c_pred,aes(x = ymd(date),group = 1)) + geom_vline(xintercept = ymd(as.matrix(rbind(peak[nrow(peak),2:4]))[1,]),color = "white",
                                                                    linetype = "dashed") +
        geom_line(aes(y = Ispred, color = "a")) + geom_ribbon(aes(ymin = IspredInf,ymax = IspredSup,fill = "a"),alpha = 0.25) +
        geom_line(aes(y = Dpred, color = "c")) + geom_ribbon(aes(ymin = DpredInf,ymax = DpredSup,fill = "c"),alpha = 0.25) + 
        theme_solarized(light = FALSE) +  scale_x_date(breaks = seq.Date(ymd(end_validate),ymd(end_validate)+simulate_length,length.out = 12),
                                                       labels = strftime(seq.Date(ymd(end_validate),ymd(end_validate)+simulate_length,length.out = 12),
                                                                         format="%d/%m/%y")) + 
      scale_y_continuous(breaks = round(seq(min(c_pred$Dpred),max(c_pred$IspredSup),length.out = 10))) +
        theme(legend.title = element_text(face = "bold"),legend.position = "bottom") + ylab("Indivíduos") +
        xlab("Data") + scale_colour_discrete("",labels = c("Infectados","Total de Óbitos")) +
        theme(plot.title = element_text(face = "bold",size = 25,color = "white",hjust = 0.5),
              axis.text.x = element_text(size = 15,face = "bold",color = "white"),
              legend.text = element_text(size = 15,face = "bold",color = "white"),
              axis.text.y = element_text(size = 15,face = "bold",color = "white"),
              legend.key.width=unit(3.5,"cm"),panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.title = element_text(color = "white",size = 20),
              plot.caption = element_text(face = "bold",color = "white",hjust = 0,size = 15)) +
        theme(plot.margin = unit(c(1,1,1,1), "lines")) + scale_fill_discrete(guide = FALSE) + 
        theme(strip.background = element_blank(),
              strip.text = element_text(size = 20,face = "bold",color = "white")) +
        labs(caption = "©IME - USP. Design: Diego Marcondes. Para mais informações e conteúdo sobre a COVID-19 acesse www.ime.usp.br/~pedrosp/covid19/") +
        ggtitle(paste("Curva epidemiológica prevista para o Estado de São Paulo"))
  pdf(file = paste("/storage/SEIR/",pos,"/SP_EPcurve_predicted_",pos,".pdf",sep = ""),
      width = 15,height = 10)
  suppressWarnings(suppressMessages(print(p))) #Save plot
  dev.off()
  
  cat("Building maps...\n")
  drs$DRS <- as.character(drs$DRS)
  drs$DRS[drs$Municipio == "SÃO PAULO"] <- "I"
  drs$DRS <- factor(drs$DRS)
  drs$Regiao <- as.character(drs$Regiao)
  drs$Regiao[drs$Municipio == "SÃO PAULO"] <- "Grande São Paulo"
  drs$Regiao <- factor(drs$Regiao)
  
  #Mapas
  shp <- readOGR(dsn = "~/mdyn/maps/sp_municipios/35MUE250GC_SIR.shp",stringsAsFactors = F,verbose = F) #Shapefiles
  shp$NM_MUNICIP <- gsub("'","",shp$NM_MUNICIP) #Correct names
  shp$NM_MUNICIP[shp$NM_MUNICIP == "BIRITIBA MIRIM"] <- "BIRITIBA-MIRIM"  #Correct names
  shp <- fortify(shp,region = "NM_MUNICIP") #Fortify
  shp <- merge(shp,drs,by.x = "id",by.y = "Municipio")
  #shp <- readRDS("~/mdyn/SEIR/dados/shp.rds")
  shp$DRS <- as.character(shp$DRS)
  shp$DRS[shp$DRS == "0"] <- "I"
  shp$DRS <- as.factor(shp$DRS)
  Dsim <- merge(Dsim,data.frame("Municipio" = par$names,"pop" = par$pop,"DRS" = drs$DRS))
  Isim <- merge(Isim,data.frame("Municipio" = par$names,"pop" = par$pop,"DRS" = drs$DRS))
  mD <- max(1e5*Dsim$Sup/Dsim$pop) #Get maximum of death per 100k
  mI <- max(1e5*Isim$Sup/Isim$pop) #Get maximum of cases per 100k

  #Peak
  tmp <- peak
  tmp$tmedian <- as.numeric(ymd(tmp$TMediana) - ymd(end_validate)+1)
  tmp <- tmp %>% select(DRS,tmedian)
  tmp$DRS[tmp$DRS == "0"] <- "I"
  tmp$DRS <- factor(tmp$DRS)
  tmp <- merge(shp,tmp)
  tmp <- tmp[order(tmp$order),]
  rc_cont_inv <- colorRampPalette(colors = c("red","darkgoldenrod1","white"))(simulate_length+1)
  p <- ggplot(tmp,aes(long, lat, group=group,fill = log(1+tmedian))) + theme_bw() + geom_polygon(colour='gray30') +
    xlab("") + ylab("") + titles_Map + scale_fill_gradientn("",colours = rc_cont_inv,limits = log(1+c(0,simulate_length)),
                                                            breaks = c(1,log(1+simulate_length)),
                                                            labels = c("Pico próximo","Pico distante")) +
    theme(legend.background = element_blank()) + ggtitle("Distância até o pico por Município")
  pdf(file = paste("/storage/SEIR/",pos,"/risk_peak_",pos,".pdf",sep = ""),width = 15,height = 10)
  print(p)
  dev.off()

  #Generate map of death for each day
  pb <- progress_bar$new(
    format = "Iterations = :letter [:bar] :elapsed | eta: :eta",
    total = simulate_length,    # 100 
    width = 60)
  progress_letter <- paste(round(100*c(1:simulate_length)/simulate_length,2),"%")
  progress <- function(n){
    pb$tick(tokens = list(letter = progress_letter[n]))
  } 
  opts <- list(progress = progress)
  
  system(paste("mkdir",paste("/storage/SEIR/",pos,"/Videos/",sep = ""))) #mkdir
  system(paste("mkdir",paste("/storage/SEIR/",pos,"/Videos/Estado/",sep = "")))
  system(paste("mkdir",paste("/storage/SEIR/",pos,"/Videos/Estado/mortes/",sep = "")))
  system(paste("mkdir",paste("/storage/SEIR/",pos,"/Videos/Estado/casos/",sep = "")))
  for(d in unique(drs$Regiao)){
    system(paste("mkdir",paste("/storage/SEIR/",pos,"/Videos/",gsub(" ","",d),"/",sep = "")))
    system(paste("mkdir",paste("/storage/SEIR/",pos,"/Videos/",gsub(" ","",d),"/mortes/",sep = "")))
    system(paste("mkdir",paste("/storage/SEIR/",pos,"/Videos/",gsub(" ","",d),"/casos/",sep = "")))
  }
  
  #Parallel
  cl <- makeSOCKcluster(cores)
  registerDoSNOW(cl)
  a <- foreach(t = 1:simulate_length,.options.snow = opts,.packages = c("tidyverse","ggplot2","ggthemes","lubridate","data.table","gridExtra")) %dopar%{
    pb$tick(tokens = list(letter = progress_letter[t]))
    titles_Map <- theme(strip.text = element_text(size = 12), axis.text = element_text(size = 12,color = "black"),
                        axis.title = element_text(size = 14), legend.text = element_text(size = 14),
                        legend.title = element_text(size = 14,face = "bold"),plot.title = element_text(size = 16,face = "bold",hjust = 0.5),
                        panel.border = element_blank(),legend.key.width=unit(4,"cm"),
                        panel.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                        legend.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                        legend.position="bottom",legend.spacing.x = unit(0.5, 'cm'))
    
    rc_cont <- colorRampPalette(colors = c("white","darkgoldenrod1","red"))(200)
    
    #Deaths
    tmp <- Dsim %>% filter(Date == ymd(end_validate)+t-1) %>% select(Municipio,Mediana,pop)
    tmp$Dpred <- 1e5*tmp$Mediana/tmp$pop
    tmp <- merge(shp,tmp,by.x = "id",by.y = "Municipio")
    tmp <- tmp[order(tmp$order),]
    
    pD <- ggplot(tmp,aes(long, lat, group=group,fill = log(1+Dpred,2))) + theme_bw() + geom_polygon(colour='gray30') +
          xlab("") + ylab("") + scale_fill_gradientn("Mortes 100k",colours = rc_cont,limits = c(0,log(1+mD,2)+0.1),
                                                     breaks = round(seq(0,log(mD+1,2),log(mD+1,2)/5)),
                                                     labels = round(c(0,2^(round(seq(0,log(mD+1,2),log(mD+1,2)/5)))[-1]))) + titles_Map +
      ggtitle(paste("Mortes por 100k estimadas em",ymd(end_validate)+t-1))
    pdf(file = paste("/storage/SEIR/",pos,"/Videos/Estado/mortes/",sprintf("%03d", t),".pdf",sep = ""),width = 15,height = 10)
    print(pD)
    dev.off()
    
    for(d in unique(drs$DRS)){
      tmpd <- tmp %>% filter(DRS == d)
      tmpd <- tmpd[order(tmpd$order),]
       pD <- ggplot(tmpd,aes(long, lat, group=group,fill = log(1+Dpred,2))) + theme_bw() + geom_polygon(colour='gray30') +
          xlab("") + ylab("") + scale_fill_gradientn("Mortes 100k",colours = rc_cont,limits = c(0,log(1+mD,2)+0.1),
                                                     breaks = round(seq(0,log(mD+1,2),log(mD+1,2)/5)),
                                                     labels = round(c(0,2^(round(seq(0,log(mD+1,2),log(mD+1,2)/5)))[-1]))) + titles_Map +
      ggtitle(paste("Mortes por 100k estimadas em",ymd(end_validate)+t-1,"na DRS",unique(drs$Regiao[drs$DRS == d])))
      pdf(file = paste("/storage/SEIR/",pos,"/Videos/",gsub(" ","",unique(drs$Regiao[drs$DRS == d])),"/mortes/",sprintf("%03d", t),".pdf",sep = ""),
          width = 15,height = 10)
      print(pD)
      dev.off()
    }
    
    #Cases
    tmp <- Isim %>% filter(Date == ymd(end_validate)+t-1) %>% select(Municipio,Mediana,pop)
    tmp$Ipred <- 1e5*tmp$Mediana/tmp$pop
    tmp <- merge(shp,tmp,by.x = "id",by.y = "Municipio")
    tmp <- tmp[order(tmp$order),]
    
    pI <- ggplot(tmp,aes(long, lat, group=group,fill = log(1+Ipred,2))) + theme_bw() + geom_polygon(colour='gray30') +
          xlab("") + ylab("") + scale_fill_gradientn("Casos 100k",colours = rc_cont,limits = c(0,log(1+mI,2)+0.1),
                                                     breaks = round(seq(0,log(mI+1,2),log(mI+1,2)/5)),
                                                     labels = round(c(0,exp(round(seq(0,log(mD+1,2),log(mD+1,2)/5)))[-1]))) + titles_Map +
      ggtitle(paste("Casos por 100k estimados em",ymd(end_validate)+t-1))
    pdf(file = paste("/storage/SEIR/",pos,"/Videos/Estado/casos/",sprintf("%03d", t),".pdf",sep = ""),width = 15,height = 10)
    print(pI)
    dev.off()
    
    for(d in unique(drs$DRS)){
      tmpi <- tmp %>% filter(DRS == d)
      tmpi <- tmpi[order(tmpi$order),]
       pI <- ggplot(tmpi,aes(long, lat, group=group,fill = log(1+Ipred,2))) + theme_bw() + geom_polygon(colour='gray30') +
          xlab("") + ylab("") + scale_fill_gradientn("Casos 100k",colours = rc_cont,limits = c(0,log(1+mI,2)+0.1),
                                                     breaks = round(seq(0,log(mI+1,2),log(mI+1,2)/5)),
                                                     labels = round(c(0,2^(round(seq(0,log(mD+1,2),log(mD+1,2)/5)))[-1]))) + titles_Map +
      ggtitle(paste("Casos por 100k estimados em",ymd(end_validate)+t-1,"na DRS",unique(drs$Regiao[drs$DRS == d])))
      pdf(file = paste("/storage/SEIR/",pos,"/Videos/",gsub(" ","",unique(drs$Regiao[drs$DRS == d])),"/casos/",sprintf("%03d", t),".pdf",sep = ""),
          width = 15,height = 10)
      print(pI)
      dev.off()
    }
  }
  stopCluster(cl)
  
  cat("\n")
  cat("We are done fitting the model! I will starting preprocessing the data in a moment...\n")
  preprocess_SEIR_output(drs,pos,obs,init_validate)
  
  cat("\n")
  cat("And that is it! You can create the videos. Please come back more often.\n")
}
  
