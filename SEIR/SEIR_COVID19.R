###############################
#####SEIR for COVID-19    #####
#####Diego Marcondes      #####
#####dmarcondes@ime.usp.br#####
###############################

source("mdyn/SEIR/utils.R")

SEIR_covid <- function(cores,par,pos,seed,sample_size,simulate_length,d_max,max_models,error_I,error_D){
  
  cat("\n")
  cat("Welcome to Covid SEIR Mobility Model estimation!")
  cat("\n")
  cat("One moment and I will be right there with you...")
  cat("\n")
  
  #Seed
  set.seed(seed)
  
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
  init_simulate <- end_validate
  day_validate <- seq.Date(from = ymd(init_validate),to = ymd(end_validate),by = 1) #Days to validate
  
  #Epidemiolohical curve
  EPI_curve(obs,end_validate,pos)
  
  #Calculate lift
  par$lift <- lift_death(obs,end_validate,par)

  #Obs by DRS
  obs_drs <- data_drs(obs,drs)

  #####Model estimation#####
  cat("Calculate growth and death rate...\n")
    
  #Initial condition
  init <- initial_condition(obs,init_validate,par) #Initial condition
  init1f <- initial_condition(obs,init_validate+1,par) #Data one day after initial
  init1p <- initial_condition(obs,init_validate-1,par) #Data one day before initial
  init2f <- initial_condition(obs,init_validate+2,par) #Data two days after initial
  
  #Obs each day around week of validation
  par$obs <- obs_around_init(obs,obs_drs,par,init_validate,start = 0,end = 9)
  par$obs_DRS <- par$obs[[2]]
  par$obs <- par$obs[[1]]
      
  #Test data by DRS
  teste_D <- list()
  teste_D$DRS <- obs_drs %>% filter(date %in% seq.Date(from = ymd(init_validate),to = ymd(end_validate),1)) %>%
    select(date,DRS,deaths_corrected) %>% unique()
  names(teste_D$DRS)[3] <- "D_drs"
  teste_D$DRS$key <- paste(teste_D$DRS$date,teste_D$DRS$DRS)
  
  teste_D$city <- obs %>% filter(date %in% seq.Date(from = ymd(init_validate),to = ymd(end_validate),1)) %>%
    select(date,city,deaths_corrected)
  teste_D$city <- spread(data = teste_D$city,key = "city",value = "deaths_corrected")
  teste_D$city <- teste_D$city[,c(1,match(par$names,colnames(teste_D$city)))]
  
  teste_I <- list()
  teste_I$DRS <- obs_drs %>% filter(date %in% seq.Date(from = ymd(init_validate),to = ymd(end_validate),1)) %>%
    select(date,DRS,confirmed_corrected) %>% unique()
  names(teste_I$DRS)[3] <- "I_drs"
  teste_I$DRS$key <- paste(teste_I$DRS$date,teste_I$DRS$DRS)
  
  teste_I$city <- obs %>% filter(date %in% seq.Date(from = ymd(init_validate),to = ymd(end_validate),1)) %>%
    select(date,city,confirmed_corrected)
  teste_I$city <- spread(data = teste_I$city,key = "city",value = "confirmed_corrected")
  teste_I$city <- teste_I$city[,c(1,match(par$names,colnames(teste_I$city)))]

  #Calculate growth rate
  system(paste("mkdir /storage/SEIR/",pos,"/AjusteRate/",sep = ""))
  par$lambda <- growth_rate(obs,obs_drs,drs,par,pos)

  #Calculate death rate for each DRS
  par$delta <- death_rate(teste_D$DRS,teste_I$DRS,obs,end_validate,drs,par)
  
  cat("Estimatimating the model...\n")
  
  #Choosing models
  pred <- vector("list",sample_size) #Store predicted values
  pb <- set_progress_bar(sample_size)[[1]] 
  progress_letter <- set_progress_bar(sample_size)[[2]] 
  
  #Objects to store results
  results <- list()
  results$models <- vector("list",sample_size) #Store parameters of models
  kgood <- 0 #Number of good models
  is.good <- vector() #Track good models
  
  #Track error
  minI <- 1
  maxI <- 1
  minD <- 1
  maxD <- 1
  mI <- 1
  mD <- 1
  
  for(k in 1:sample_size){#For each sampled model
    pb$tick(tokens = list(letter = paste(progress_letter[k],kgood,"D =",round(mD,5),"I =",round(mI,5)))) #Update progress bar
    
    #Parameters of model k
    parK <- sample_parameters(par,day_validate)
    
    #Initial condition
    initK <- initial_condition_corrected(init,init1f,init2f,parK)
    
    #Calculate beta
    parK$beta <- list()
    prox <- F
    for(t in 1:7){
      parK$beta[[t]] <- beta(parK,t = t,lambda = par$lambda,drs,day = init_validate,obs)
      if(min(parK$beta[[t]]) < 0){
        is.good[k] <- 0
        prox <- T
      }
    }
    if(prox){
      rm(initK,parK)
      next
    }
    
    #Model
    mod <- solve_seir(y = initK,times = 1:7,derivatives = derivatives,parms = parK)[,-1] #Simulate model k
      
    #Result
    D <- mod[,(4*parK$sites + 1):(5*parK$sites)] #Predicted death for testing
    I <- mod[,(5*parK$sites + 1):(6*parK$sites)] #Predicted cases for testing
      
    #Test if model predicted well
    test <- test_model(D,I,teste_D,teste_I,drs)

    #Is good
    good <- as.numeric(test$dif_I <= error_I & test$dif_D <= error_D) #Test if is good
    is.good[k] <- good #Store
    if(test$dif_I < mI) #If is minimum error so far
      mI <- test$dif_I
    if(test$dif_D < mD) #If is minimum error so far
      mD <- test$dif_D
    
    #Result
    if(good == 1){#Store good models
      
      #Prediction of beta t0-1
      #parK$beta <- beta(parK,t = 6,par$lambda,drs,end_validate,obs)
      #if(is.null(parK$beta)){
      #  is.good[k] <- 0
      #  rm(initK,parK)
      #  next
      #}
      parK$beta <- as.vector(apply(bind_rows(lapply(parK$beta,function(x) data.frame(rbind(x)))),2,median))
      pred[[k]]$beta <- parK$beta
      
      #Prediction
      pred[[k]]$E <- mod[,1:parK$sites] #Prediction of E
      pred[[k]]$I <- mod[,(parK$sites + 1):(2*parK$sites)] #Prediction of I
      pred[[k]]$Is <- mod[,(2*parK$sites + 1):(3*parK$sites)] #Prediction of Is
      pred[[k]]$R <- mod[,(3*parK$sites + 1):(4*parK$sites)] #Prediction of R
      pred[[k]]$D <- mod[,(4*parK$sites + 1):(5*parK$sites)] #Prediction of D
      pred[[k]]$I <- mod[,(5*parK$sites + 1):(6*parK$sites)] #Total cases
      
      
      #Mean infected time and Rt
      parK$Rt <- Rt(parK,end_validate,7)
      parK$meanTi <- parK$Rt$meanTi
      parK$Rt <- parK$Rt$Rt
      pred[[k]]$meanTi <- parK$menaTi #Prediction of mean infection time
      pred[[k]]$Rt <- parK$Rt #Prediction of Rt
      
      #Number of good models
      kgood <- kgood + 1
      
      #Error of this
      minDK <- ifelse(min(1 + test$error_D$dif[test$error_D$D_drs > 50]) < 1,
                      min(1 + test$error_D$dif[test$error_D$D_drs > 50]),1)
      maxDK <- ifelse(max(1 + test$error_D$dif[test$error_D$D_drs > 50]) > 1,
                      max(1 + test$error_D$dif[test$error_D$D_drs > 50]),1)
      minIK <- ifelse(min(1 + test$error_I$dif[test$error_I$I_drs > 1000]) < 1,
                      min(1 + test$error_I$dif[test$error_I$I_drs > 1000]),1)
      maxIK <- ifelse(max(1 + test$error_I$dif[test$error_I$I_drs > 1000]) > 1,
                      max(1 + test$error_I$dif[test$error_I$I_drs > 1000]),1)
      parK$minDK <- minDK
      parK$minIK <- minIK
      parK$maxDK <- maxDK
      parK$maxIK <- maxIK
      
      #Delete unecessary parameters
      parK$day <- NULL #Days of validation
      parK$val <- NULL #Is validation
      parK$mob <- NULL #Mobility matrix
      parK$pop <- NULL #Population
      parK$obs <- NULL
      parK$obs_DRS <- NULL
      parK$sites <- NULL
      parK$lift <- NULL
      
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
  cat(paste("Good models: ",kgood," (",round(100*kgood/sample_size,2),"%)\n",sep = ""))
  cat("\n")

  cat("\n")
  cat("Saving parameters of good models...\n")
  
  #Saving parameters
  results$models <- results$models[unlist(lapply(results$models,function(x) ifelse(is.null(x),F,T)))] #Clean
  results$Vgood <- results$Vgood[unlist(lapply(results$Vgood,function(x) ifelse(is.null(x),F,T)))] #Clean
  Te <- unlist(lapply(results$models,function(x) x$Te)) #Te
  Ti <- unlist(lapply(results$models,function(x) x$Ti)) #Ti
  Ts <- unlist(lapply(results$models,function(x) x$Ts)) #Ts
  Tsr <- unlist(lapply(results$models,function(x) x$Tsr)) #Tsr
  Td <- unlist(lapply(results$models,function(x) x$Td)) #Td
  cinfD <- unlist(lapply(results$models,function(x) x$minDK)) #cinfD
  cinfI <- unlist(lapply(results$models,function(x) x$minIK)) #cinfI
  csupD <- unlist(lapply(results$models,function(x) x$maxDK)) #cinfD
  csupI <- unlist(lapply(results$models,function(x) x$maxIK)) #cinfI
  s <- unlist(lapply(results$models,function(x) x$s)) #s
  pS <- lapply(results$models,function(x) x$pS) #Missed cases
  assymptomatic <- 1-unlist(lapply(pS,median)) #Missed cases
  beta <- lapply(results$models,function(x) x$beta) #Beta
  betasave <- unlist(lapply(beta,median)) #Beta
  Rt <- lapply(results$models,function(x) x$Rt) #Rt
  Rtsave <- unlist(lapply(Rt,median)) #Rt
  
  pred <- pred[is.good == 1] #Prediction of only good models
  saveRDS(object = results,file = paste("/storage/SEIR/",pos,"/result_",pos,".rds",sep = "")) #Save results
  saveRDS(object = pred,file = paste("/storage/SEIR/",pos,"/prediction_",pos,".rds",sep = "")) #Save predictions
  
  param <- data.frame("Model" = 1:kgood,Te,Ti,Ts,Tsr,Td,s,"MedianBeta" = betasave,"MedianRt" = Rtsave,"MedianAssymptomatic" = assymptomatic,
                      cinfD,csupD,cinfI,csupI) #Parameters
  fwrite(param,paste("/storage/SEIR/",pos,"/parameters_",pos,".csv",sep = "")) #Write parameters of good models
  
  cat("Plotting maps of parameters which are city dependent...\n")
  
  plot_maps_summary(Rt,par,drs,obs,end_validate)  
  
  #######Plot by DRS#####
  cat("Plot observed and predicted number of deaths for each DRS...\n")
  system(paste("mkdir /storage/SEIR/",pos,"/validate",sep = ""))
  
  plot_validate() #AQUI!!
  

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
  
