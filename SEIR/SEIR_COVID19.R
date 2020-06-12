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
      
      #Mediana of beta
      parK$betaMedian <- as.vector(apply(bind_rows(lapply(parK$beta,function(x) data.frame(rbind(x)))),2,median))
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
      pred[[k]]$meanTi <- parK$menaTi #Prediction of mean infection time
      pred[[k]]$Rt <- parK$Rt #Prediction of Rt
      
      #Number of good models
      kgood <- kgood + 1
      
      #Error of this
      minDK <- ifelse(min(1 + test$error_D) < 1,
                      min(1 + test$error_D),1)
      maxDK <- ifelse(max(1 + test$error_D) > 1,
                      max(1 + test$error_D),1)
      minIK <- ifelse(min(1 + test$error_I) < 1,
                      min(1 + test$error_I),1)
      maxIK <- ifelse(max(1 + test$error_I) > 1,
                      max(1 + test$error_I),1)
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
  meanTi <- unlist(lapply(results$models,function(x) x$meanTi)) #meanTi
  cinfD <- unlist(lapply(results$models,function(x) x$minDK)) #cinfD
  cinfI <- unlist(lapply(results$models,function(x) x$minIK)) #cinfI
  csupD <- unlist(lapply(results$models,function(x) x$maxDK)) #cinfD
  csupI <- unlist(lapply(results$models,function(x) x$maxIK)) #cinfI
  s <- unlist(lapply(results$models,function(x) x$s)) #s
  pS <- lapply(results$models,function(x) x$pS) #Missed cases
  assymptomatic <- 1-unlist(lapply(pS,median)) #Missed cases
  beta <- lapply(results$models,function(x) x$betaMedian) #Beta
  betasave <- unlist(lapply(beta,median)) #Beta
  Rt <- lapply(results$models,function(x) x$Rt) #Rt
  Rtsave <- unlist(lapply(Rt,median)) #Rt
  
  pred <- pred[is.good == 1] #Prediction of only good models
  saveRDS(object = results,file = paste("/storage/SEIR/",pos,"/result_",pos,".rds",sep = "")) #Save results
  saveRDS(object = pred,file = paste("/storage/SEIR/",pos,"/prediction_",pos,".rds",sep = "")) #Save predictions
  
  param <- data.frame("Model" = 1:kgood,Te,Ti,Ts,Tsr,Td,meanTi,s,"MedianBeta" = betasave,"MedianRt" = Rtsave,"MedianAssymptomatic" = assymptomatic,
                      cinfD,csupD,cinfI,csupI) #Parameters
  fwrite(param,paste("/storage/SEIR/",pos,"/parameters_",pos,".csv",sep = "")) #Write parameters of good models
  
  cat("Plotting maps of parameters which are city dependent...\n")
  
  plot_maps_summary(Rt,par,drs,obs,end_validate,pos)  
  
  #######Plot Validation#####
  cat("Plot observed and predicted number of deaths for each DRS...\n")
  system(paste("mkdir /storage/SEIR/",pos,"/validate",sep = ""))
  
  plot_validate(drs,par,pred,init_validate,end_validate,pos)

  #######Simulation of scenarios########
  cat("Simulating scenarios...\n")
  init <- initial_condition(obs,end_validate,par) #Initial condition
  init1f <- initial_condition(obs,end_validate+1,par) #Data one day after initial
  init1p <- initial_condition(obs,end_validate-1,par) #Data one day before initial
  init2f <- initial_condition(obs,end_validate+2,par) #Data two days after initial
  
  #Simulate scenario everything is as week after end validation
  good_models <- length(pred) #number of good models
  predSIM <- vector("list",good_models) #To store the predicted values of the simulation
  pb <- set_progress_bar(good_models)[[1]] 
  progress_letter <- set_progress_bar(good_models)[[2]] 

  for(k in 1:good_models){
    pb$tick(tokens = list(letter = progress_letter[k]))
    
    #Parameters of model k
    parK <- results$models[[k]]
    parK$day <- seq.Date(from = ymd(init_simulate),to = ymd(init_simulate) + simulate_length - 1,by = 1) #Days of validation
    parK$val <- F #Is validation
    parK$mob <- par$mob #Mobility matrix
    parK$pop <- par$pop #Population
    parK$sites <- par$sites #Sites
    
    #Initial condition
    initK <- initial_condition_corrected(init,init1f,init2f,parK)
    
    #Model
    mod <- solve_seir(y = initK,times = 1:simulate_length,derivatives = derivatives,parms = parK)[,-1]
  
    #Result
    predSIM[[k]]$E <- mod[,1:parK$sites] #E
    predSIM[[k]]$I <- mod[,(parK$sites + 1):(2*parK$sites)] #I
    predSIM[[k]]$Is <- mod[,(2*parK$sites + 1):(3*parK$sites)] #Is
    predSIM[[k]]$R <- mod[,(3*parK$sites + 1):(4*parK$sites)] #R
    predSIM[[k]]$D <- mod[,(4*parK$sites + 1):(5*parK$sites)] #D
    predSIM[[k]]$It <- mod[,(5*parK$sites + 1):(6*parK$sites)] #Total Infected
    #predSIM[[k]]$S <- par$pop - pred[[k]]$E - pred[[k]]$I - pred[[k]]$Is - pred[[k]]$R - pred[[k]]$D
  }
  
  cat("Storing results of simulation...\n")
  
  #####Results of simulation#####
  
  
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
  
