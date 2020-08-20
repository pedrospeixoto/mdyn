###############################
#####SEIR for COVID-19    #####
#####Diego Marcondes      #####
#####dmarcondes@ime.usp.br#####
###############################

suppressMessages(source("mdyn/SEIR/utils.R"))

get_error_SEIR_covid <- function(cores,par,pos,seed,sample_size,simulate_length,d_max,max_models,error_I,error_D){
  
  #Seed
  set.seed(seed)
  
  #####mkdir#####
  pos <- paste(pos,"_error",sep = "")
  system(paste("mkdir /storage/SEIR/",pos,sep = ""))
  
  #####Notifications#####
  obs <- get_data_SP()
  if(nrow(obs)/par$sites-round(nrow(obs)/par$sites) > 0)
    stop("There is a problem with the notifications dataset. Please fix it.")
  
  #####Days of validation#####
  end_validate <- min(max(ymd(na.omit(obs)$date))-1,ymd(d_max))
  init_validate <- end_validate - 6
  init_simulate <- end_validate
  day_validate <- seq.Date(from = ymd(init_validate),to = ymd(end_validate),by = 1) #Days to validate
  
  #Calculate lift
  par$lift <- lift_death(obs,end_validate,par) #testagem()
  
  #Obs by DRS
  drs$DRS[drs$Municipio == "SÃO PAULO"] <- "I"
  drs$Regiao[drs$Municipio == "SÃO PAULO"] <- "Grande São Paulo"
  drs <- droplevels(drs)
  obs_drs <- data_drs(obs,drs)
  
  #####Model estimation#####
  #Initial condition
  init <- initial_condition(obs,init_validate,par) #Initial condition
  init1f <- initial_condition(obs,init_validate+1,par) #Data one day after initial
  init1p <- initial_condition(obs,init_validate-1,par) #Data one day before initial
  
  #Obs each day around week of validation
  par$obs <- obs_around_init(obs,obs_drs,par,init_validate,start = 0,end = 9)
  par$obs_DRS <- par$obs[[2]]
  par$obs <- par$obs[[1]]
  
  #Test data by DRS
  teste_D <- list()
  teste_D$DRS <- obs_drs %>% filter(date %in% seq.Date(from = ymd(end_validate)-60,to = ymd(end_validate),1)) %>%
    select(date,DRS,deaths_corrected) %>% unique()
  names(teste_D$DRS)[3] <- "D_drs"
  teste_D$DRS$key <- paste(teste_D$DRS$date,teste_D$DRS$DRS)
  
  teste_D$city <- obs %>% filter(date %in% seq.Date(from = ymd(init_validate),to = ymd(end_validate),1)) %>%
    select(date,city,deaths_corrected)
  teste_D$city <- spread(data = teste_D$city,key = "city",value = "deaths_corrected")
  teste_D$city <- teste_D$city[,c(1,match(par$names,colnames(teste_D$city)))]
  
  teste_I <- list()
  teste_I$DRS <- obs_drs %>% filter(date %in% seq.Date(from = ymd(end_validate)-60,to = ymd(end_validate),1)) %>%
    select(date,DRS,confirmed_corrected) %>% unique()
  names(teste_I$DRS)[3] <- "I_drs"
  teste_I$DRS$key <- paste(teste_I$DRS$date,teste_I$DRS$DRS)
  
  teste_I$city <- obs %>% filter(date %in% seq.Date(from = ymd(init_validate),to = ymd(end_validate),1)) %>%
    select(date,city,confirmed_corrected)
  teste_I$city <- spread(data = teste_I$city,key = "city",value = "confirmed_corrected")
  teste_I$city <- teste_I$city[,c(1,match(par$names,colnames(teste_I$city)))]
  
  #Calculate growth rate
  system(paste("mkdir /storage/SEIR/",pos,"/AjusteRate/",sep = ""))
  par$lambda <- growth_rate(obs,obs_drs,drs,par,pos,init_validate,end_validate,day_validate)
  
  #Calculate death rate for each DRS
  par$delta <- death_rate(teste_D$DRS,teste_I$DRS,obs,end_validate,drs,par)
  teste_D$DRS <- teste_D$DRS %>% filter(date >= ymd(init_validate)) 
  teste_I$DRS <- teste_I$DRS %>% filter(date >= ymd(init_validate))
  
  #Choosing models
  pred <- vector("list",sample_size) #Store predicted values
  pb <- set_progress_bar(sample_size)[[1]] 
  progress_letter <- set_progress_bar(sample_size)[[2]] 
  
  #Objects to store results
  results <- list()
  results$models <- vector("list",sample_size) #Store parameters of models
  kgood <- 0 #Number of good models
  is.good <- rep(0,sample_size) #Track good models
  
  #Track error
  minI <- 1
  maxI <- 1
  minD <- 1
  maxD <- 1
  mI <- 1
  mD <- 1
  mm <- Inf
  
  for(k in 1:sample_size){#For each sampled model
    pb$tick(tokens = list(letter = paste(progress_letter[k],kgood,round(mm,5),"D =",round(mD,5),"I =",round(mI,5)))) #Update progress bar
    
    #Parameters of model k
    parK <- sample_parameters(par,day_validate)
    
    #Initial condition
    initK <- initial_condition_corrected(init,init1f,parK)
    
    #Calculate beta
    parK$beta <- list()
    prox <- F
    for(t in 1:7)
      parK$beta[[t]] <- beta(parK,t = t,lambda = par$lambda,drs,day = init_validate,obs)
    parK$beta <- as.vector(apply(bind_rows(lapply(parK$beta,function(x) data.frame(rbind(x)))),2,median))
    if(min(parK$beta) < 0){
      is.good[k] <- 0
      prox <- T
    }
    if(prox){
      rm(initK,parK)
      next
    }
    parK$beta[parK$beta < 0.01] <- 0.01
    
    #Model
    mod <- solve_seir(y = initK,times = 1:7,derivatives = derivatives,parms = parK)[,-1] #Simulate model k
    
    #Result
    D <- mod[,(4*parK$sites + 1):(5*parK$sites)] #Predicted death for testing
    I <- mod[,(5*parK$sites + 1):(6*parK$sites)] #Predicted cases for testing
    
    #Test if model predicted well
    test <- test_model(D,I,teste_D,teste_I,drs,init_validate,end_validate)
    
    #Is good
    good <- as.numeric(test$dif_I <= error_I & test$dif_D <= error_D) #Test if is good
    is.good[k] <- good #Store
    if(test$dif_I < mI & test$dif_D < mD) #If is minimum error so far
      mI <- test$dif_I
    if(test$dif_I < mI & test$dif_D < mD) #If is minimum error so far
      mD <- test$dif_D
    if(max(test$dif_D,test$dif_I) < mm)
      mm <- max(test$dif_D,test$dif_I)
    
    #Result
    if(good == 1){#Store good models
      
      #Median of beta
      parK$betaMedian <- parK$beta #as.vector(apply(bind_rows(lapply(parK$beta,function(x) data.frame(rbind(x)))),2,median))
      pred[[k]]$beta <- parK$betaMedian
      names(parK$beta) <- weekdays(seq.Date(from = ymd(init_validate),to = ymd(end_validate),1))
      
      #Prediction
      pred[[k]]$E <- mod[,1:parK$sites] #Prediction of E
      pred[[k]]$I <- mod[,(parK$sites + 1):(2*parK$sites)] #Prediction of I
      pred[[k]]$Is <- mod[,(2*parK$sites + 1):(3*parK$sites)] #Prediction of Is
      pred[[k]]$R <- mod[,(3*parK$sites + 1):(4*parK$sites)] #Prediction of R
      pred[[k]]$D <- mod[,(4*parK$sites + 1):(5*parK$sites)] #Prediction of D
      pred[[k]]$It <- mod[,(5*parK$sites + 1):(6*parK$sites)] #Total cases
      
      
      #Mean infected time and Rt
      parK$Rt <- Rt(parK,end_validate,7)
      parK$meanTi <- parK$Rt$meanTi
      parK$Rt <- parK$Rt$Rt
      pred[[k]]$meanTi <- parK$meanTi #Prediction of mean infection time
      pred[[k]]$Rt <- parK$Rt #Prediction of Rt
      
      #Number of good models
      kgood <- kgood + 1
      
      #Error of this
      minDK <- ifelse(min(1 + test$error_D) < 1,min(1 + test$error_D),1)
      maxDK <- ifelse(max(1 + test$error_D) > 1,max(1 + test$error_D),1)
      minIK <- ifelse(min(1 + test$error_I) < 1,min(1 + test$error_I),1)
      maxIK <- ifelse(max(1 + test$error_I) > 1,max(1 + test$error_I),1)
      parK$minDK <- minDK
      parK$minIK <- minIK
      parK$maxDK <- maxDK
      parK$maxIK <- maxIK
      
      #Delete unecessary parameters
      parK$day <- NULL #Days of validation
      parK$val <- NULL #Is validation
      parK$mob <- NULL #Mobility matrix
      parK$pop <- NULL #Population
      parK$obs <- NULL #Obs
      parK$obs_DRS <- NULL #Obs_DRS
      parK$sites <- NULL #Sites
      
      results$models[[kgood]] <- parK
      if(minDK < minD)
        minD <- minDK
      if(minIK < minI)
        minI <- minIK
      if(maxDK > maxD)
        maxD <- maxDK
      if(maxIK > maxI)
        maxI <- maxIK
      if(kgood == max_models)
        break
    }
    rm(parK,D,I,test,mod,good,initK)
  }
  cat("\n")
  cat(paste("Min = " = round(mm,5)," MinD = ",round(mD,5)," MinI = ",round(mI,5),sep = ""))
  cat("\n")
  
  l <- list("MinDeath" = mD,"MinInfected" = mI,"Min" = mm)
  return(l)
}

