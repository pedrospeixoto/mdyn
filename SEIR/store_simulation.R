#Store the results of the simulation
store_simulation <- function(predSIM,par,simulate_length,pos,drs,minI,maxI,minD,maxD,end_validate,obs,obs_drs){
  
  #Create objects
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
                         "Ipred" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$I[,position])))),2,median),
                         "IpredInf" = minI*apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$I[,position])))),2,min),
                         "IpredSup" = maxI*apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$I[,position])))),2,max),
                         "Itpred" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$It[,position])))),2,median),
                         "ItpredInf" = minI*apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$It[,position])))),2,min),
                         "ItpredSup" = maxI*apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$It[,position])))),2,max),
                         "Rpred" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$R[,position])))),2,median),
                         "RpredInf" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$R[,position])))),2,min),
                         "RpredSup" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$R[,position])))),2,max),
                         "Dpred" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$D[,position])))),2,median),
                         "DpredInf" = minD*apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$D[,position])))),2,min),
                         "DpredSup" = maxD*apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$D[,position])))),2,max))
    c_pred$DpredInf[1] <- c_pred$DpredInf[1]/minD
    c_pred$DpredSup[1] <- c_pred$DpredSup[1]/maxD
    c_pred$IpredInf[1] <- c_pred$IpredInf[1]/minI
    c_pred$IpredSup[1] <- c_pred$IpredSup[1]/maxI
    c_pred$IspredInf[1] <- c_pred$IspredInf[1]/minI
    c_pred$IspredSup[1] <- c_pred$IspredSup[1]/maxI
    c_pred$ItpredInf[1] <- c_pred$ItpredInf[1]/minI
    c_pred$ItpredSup[1] <- c_pred$ItpredSup[1]/maxI
    deaths$inf[,position] <- c_pred$DpredInf #Dinf
    deaths$sup[,position] <- c_pred$DpredSup #Dsup
    deaths$median[,position] <- c_pred$Dpred #Dpred
    cases$inf[,position] <- c_pred$ItpredInf #Itinf
    cases$sup[,position] <- c_pred$ItpredSup #Itsup
    cases$median[,position] <- c_pred$Itpred #Itpred
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
    tmp_deaths <- tmp
    
    #Store all predicted cases
    tmp <- unlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$It[,position]))))
    tmp <- data.frame("Municipio" = c,expand.grid(1:simulate_length,1:length(predSIM)),"Casos" = tmp)
    names(tmp)[2:3] <- c("Data","Modelo")
    tmp$Data <- as.character(ymd(end_validate) + tmp$Data - 1)
    cases_all <- rbind.data.frame(cases_all,tmp)
    
    #Infected for plot
    tmp <- unlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(x$Is[,position]))))
    tmp <- data.frame("Municipio" = c,expand.grid(1:simulate_length,1:length(predSIM)),"Casos" = tmp)
    names(tmp)[2:3] <- c("Data","Modelo")
    tmp$Data <- as.character(ymd(end_validate) + tmp$Data - 1)
    tmp_cases <- tmp
        
    #Epidemiological curve
    if(c_pred$Itpred[1] > 500 | c_pred$Dpred[1] > 100){
      tmp2 <- obs %>% filter(city == c & confirmed_corrected >= 100 & date <= min(c_pred$date))
      tmp <- rbind.data.frame(data.frame("date" = tmp2$date,"Epred" = NA,"EpredInf" = NA,"EpredSup" = NA,"Ispred" = tmp2$infected,"IspredInf" = NA,
                                         "IspredSup" = NA,"Ipred" = NA,"IpredInf" = NA,"IpredSup" = NA,"Itpred" = tmp2$confirmed_corrected,
                                         "ItpredInf" = NA,"ItpredSup" = NA,"Rpred" = NA,"RpredInf" = NA,"RpredSup" = NA,"Dpred" = tmp2$deaths_corrected,
                                         "DpredInf" = NA,"DpredSup" = NA),c_pred)
      
      p <- ggplot(tmp,aes(x = ymd(date),group = 1)) + geom_vline(xintercept = ymd(as.matrix(rbind(peak[nrow(peak),2:4]))[1,]),color = "white",
                                                                 linetype = "dashed") + 
        geom_line(aes(y = Ispred, color = "a")) + geom_ribbon(aes(ymin = IspredInf,ymax = IspredSup,fill = "a"),alpha = 0.25) +
        geom_line(aes(y = Dpred, color = "c")) + geom_ribbon(aes(ymin = DpredInf,ymax = DpredSup,fill = "c"),alpha = 0.25) + 
        theme_solarized(light = FALSE) +  scale_x_date(breaks = seq.Date(ymd(min(ymd(tmp$date),na.rm = T)),ymd(end_validate)+simulate_length,length.out = 12),
                                                       labels = strftime(seq.Date(ymd(min(ymd(tmp$date))),ymd(end_validate)+simulate_length,length.out = 12),
                                                                         format="%d/%m/%y")) + 
        scale_y_continuous(breaks = round(seq(min(c(tmp$DpredInf,tmp$IspredInf),na.rm = T),
                                              max(c(tmp$DpredSup,tmp$IspredSup),na.rm = T),length.out = 10))) +
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
      pdf(file = paste("/storage/SEIR/",pos,"/EPCurve/",gsub(" ","",c),"_EPCurve_MEDIAN_",pos,".pdf",sep = ""),width = 15,height = 10)
      suppressWarnings(suppressMessages(print(p))) #Save plot
      dev.off()
      
      tmp_deaths$key <- paste(tmp_deaths$Municipio,tmp_deaths$Data,tmp_deaths$Modelo)
      tmp_cases$key <- paste(tmp_cases$Municipio,tmp_cases$Data,tmp_cases$Modelo)
      pl <- merge(tmp_cases,tmp_deaths %>% select(key,Mortes),by = "key")
      pl$key <- NULL
      pl <- rbind.data.frame(data.frame("Municipio" = c,"Data" = tmp2$date,"Modelo" = "0","Casos" = tmp2$infected,"Mortes" = tmp2$deaths_corrected),pl)
      
      p <- ggplot(pl %>% filter(Modelo != "0"),aes(x = ymd(Data),group = Modelo)) + 
        geom_vline(xintercept = ymd(as.matrix(rbind(peak[nrow(peak),2:4]))[1,]),color = "white",
                                                                 linetype = "dashed") + 
        geom_line(aes(y = Casos, color = "a"),alpha = 0.4) + 
        geom_line(data = pl %>% filter(Modelo == "0"),aes(x = ymd(Data), y = Casos, color = "a",group = "1")) +
        geom_line(data = tmp,aes(x = ymd(date), y = Ispred, color = "a",group = "1")) +
        geom_line(data = tmp,aes(x = ymd(date), y = Dpred, color = "c",group = "1")) +
        geom_ribbon(data = tmp,aes(x = ymd(date),ymin = IspredInf,ymax = IspredSup,fill = "a",group = "1"),alpha = 0.25) +
        geom_line(aes(y = Mortes, color = "c"),alpha = 0.4) + 
        geom_line(data = pl %>% filter(Modelo == "0"),aes(x = ymd(Data),y = Mortes, color = "c",group = "1")) + 
        geom_ribbon(data = tmp,aes(x = ymd(date),ymin = DpredInf,ymax = DpredSup,fill = "c",group = "1"),alpha = 0.25) + 
        theme_solarized(light = FALSE) +  scale_x_date(breaks = seq.Date(ymd(min(ymd(tmp$date),na.rm = T)),ymd(end_validate)+simulate_length,length.out = 12),
                                                       labels = strftime(seq.Date(ymd(min(ymd(tmp$date))),ymd(end_validate)+simulate_length,length.out = 12),
                                                                         format="%d/%m/%y")) + 
        scale_y_continuous(breaks = round(seq(min(c(tmp$DpredInf,tmp$IspredInf),na.rm = T),
                                              max(c(tmp$DpredSup,tmp$IspredSup),na.rm = T),length.out = 10))) +
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
  peakM <- peak
  
  cat("Just one more moment, while I calculate some things for the DRSs...\n")
  
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
    for(v in c("E","I","Is","It","R","D")){
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
    dados_DRS <- c_pred
    c_pred <-  data.frame("date" = seq.Date(from = ymd(end_validate),to = ymd(end_validate)+simulate_length-1,by = 1),
                          "Epred" = apply(c_pred$E,2,median),
                          "EpredInf" = apply(c_pred$E,2,min),
                          "EpredSup" = apply(c_pred$E,2,max),
                          "Ispred" = apply(c_pred$Is,2,median),
                          "IspredInf" = minI*apply(c_pred$Is,2,min),
                          "IspredSup" = maxI*apply(c_pred$Is,2,max),
                          "Ipred" = apply(c_pred$I,2,median),
                          "IpredInf" = minI*apply(c_pred$I,2,min),
                          "IpredSup" = maxI*apply(c_pred$I,2,max),
                          "Itpred" = apply(c_pred$It,2,median),
                          "ItpredInf" = minI*apply(c_pred$It,2,min),
                          "ItpredSup" = maxI*apply(c_pred$It,2,max),
                          "Rpred" = apply(c_pred$R,2,median),
                          "RpredInf" = apply(c_pred$R,2,min),
                          "RpredSup" = apply(c_pred$R,2,max),
                          "Dpred" = apply(c_pred$D,2,median),
                          "DpredInf" = minD*apply(c_pred$D,2,min),
                          "DpredSup" = maxD*apply(c_pred$D,2,max))
    c_pred$DpredInf[1] <- c_pred$DpredInf[1]/minD
    c_pred$DpredSup[1] <- c_pred$DpredSup[1]/maxD
    c_pred$IpredInf[1] <- c_pred$IpredInf[1]/minI
    c_pred$IpredSup[1] <- c_pred$IpredSup[1]/maxI
    c_pred$IspredInf[1] <- c_pred$IspredInf[1]/minI
    c_pred$IspredSup[1] <- c_pred$IspredSup[1]/maxI
    c_pred$ItpredInf[1] <- c_pred$ItpredInf[1]/minI
    c_pred$ItpredSup[1] <- c_pred$ItpredSup[1]/maxI
    
    #All deaths
    dados_DRS$D <- data.frame(t(dados_DRS$D))
    dados_DRS$D$Data <- as.character(ymd(end_validate) + 1:nrow(dados_DRS$D) - 1)
    dados_DRS$D <- dados_DRS$D %>% gather("Modelo","Mortes",-Data)
    tmp_deaths <- dados_DRS$D
    
    #Infected for plot
    dados_DRS$Is <- data.frame(t(dados_DRS$Is))
    dados_DRS$Is$Data <- as.character(ymd(end_validate) + 1:nrow(dados_DRS$Is) - 1)
    dados_DRS$Is <- dados_DRS$Is %>% gather("Modelo","Casos",-Data)
    tmp_cases <- dados_DRS$Is
    
    #Save
    position <- match(d,unique(drs$DRS))
    deaths$inf[,position] <- c_pred$DpredInf #Dinf
    deaths$sup[,position] <- c_pred$DpredSup #Dsup
    deaths$median[,position] <- c_pred$Dpred #Dpred
    cases$inf[,position] <- c_pred$ItpredInf #Iinf
    cases$sup[,position] <- c_pred$ItpredSup #Isup
    cases$median[,position] <- c_pred$Itpred #Ipred
    peak <- rbind.data.frame(peak,data.frame("DRS" = d,"TMinimo" = as.character(ymd(end_validate)+min(pd)),
                                             "TMediana" = as.character(ymd(end_validate)+median(pd)),
                                             "TMaximo" = as.character(ymd(end_validate)+max(pd)),
                                             "MMinimo" = min(mpd),"MMediana" = median(mpd),"MMaximo" = max(mpd))) #Peak
    #Epidemiological curve
    tmp2 <- obs_drs %>% select(-city) %>% filter(DRS == d & confirmed_corrected >= 100 & date <= min(c_pred$date)) %>% unique() %>% data.frame()
    tmp <- rbind.data.frame(data.frame("date" = tmp2$date,"Epred" = NA,"EpredInf" = NA,"EpredSup" = NA,"Ispred" = tmp2$infected,"IspredInf" = NA,
                                       "IspredSup" = NA,"Ipred" = NA,"IpredInf" = NA,"IpredSup" = NA,"Itpred" = tmp2$confirmed_corrected,
                                       "ItpredInf" = NA,"ItpredSup" = NA,"Rpred" = NA,"RpredInf" = NA,"RpredSup" = NA,"Dpred" = tmp2$deaths_corrected,
                                       "DpredInf" = NA,"DpredSup" = NA),c_pred)
    p <- ggplot(tmp,aes(x = ymd(date),group = 1)) + geom_vline(xintercept = ymd(as.matrix(rbind(peak[nrow(peak),2:4]))[1,]),color = "white",
                                                               linetype = "dashed") +
      geom_line(aes(y = Ispred, color = "a")) + geom_ribbon(aes(ymin = IspredInf,ymax = IspredSup,fill = "a"),alpha = 0.25) +
      geom_line(aes(y = Dpred, color = "c")) + geom_ribbon(aes(ymin = DpredInf,ymax = DpredSup,fill = "c"),alpha = 0.25) + 
      theme_solarized(light = FALSE) +  scale_x_date(breaks = seq.Date(ymd(min(ymd(tmp$date),na.rm = T)),ymd(end_validate)+simulate_length,length.out = 12),
                                                     labels = strftime(seq.Date(ymd(min(ymd(tmp$date),na.rm = T)),
                                                                                ymd(end_validate)+simulate_length,length.out = 12),
                                                                       format="%d/%m/%y")) + 
      scale_y_continuous(breaks = round(seq(min(tmp$Dpred,na.rm = T),max(tmp$IspredSup,na.rm = T),length.out = 10))) +
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
    pdf(file = paste("/storage/SEIR/",pos,"/EPCurve/DRS_",gsub(" ","",unique(drs$Regiao[drs$DRS == d])),"_EPCurve_MEDIAN_",pos,".pdf",sep = ""),
        width = 15,height = 10)
    suppressWarnings(suppressMessages(print(p))) #Save plot
    dev.off()
    
    tmp_deaths$key <- paste(tmp_deaths$Data,tmp_deaths$Modelo)
    tmp_cases$key <- paste(tmp_cases$Data,tmp_cases$Modelo)
    pl <- merge(tmp_cases,tmp_deaths %>% select(key,Mortes),by = "key")
    pl$key <- NULL
    pl <- rbind.data.frame(data.frame("Data" = tmp2$date,"Modelo" = "0","Casos" = tmp2$infected,"Mortes" = tmp2$deaths_corrected),pl)
    
    p <- ggplot(pl %>% filter(Modelo != "0"),aes(x = ymd(Data),group = Modelo)) + 
      geom_vline(xintercept = ymd(as.matrix(rbind(peak[nrow(peak),2:4]))[1,]),color = "white",
                 linetype = "dashed") + 
      geom_line(aes(y = Casos, color = "a"),alpha = 0.4) + 
      geom_line(data = pl %>% filter(Modelo == "0"),aes(x = ymd(Data), y = Casos, color = "a",group = "1")) +
      geom_line(data = tmp,aes(x = ymd(date), y = Ispred, color = "a",group = "1")) +
      geom_line(data = tmp,aes(x = ymd(date), y = Dpred, color = "c",group = "1")) +
      geom_ribbon(data = tmp,aes(x = ymd(date),ymin = IspredInf,ymax = IspredSup,fill = "a",group = "1"),alpha = 0.25) +
      geom_line(aes(y = Mortes, color = "c"),alpha = 0.4) + 
      geom_line(data = pl %>% filter(Modelo == "0"),aes(x = ymd(Data),y = Mortes, color = "c",group = "1")) + 
      geom_ribbon(data = tmp,aes(x = ymd(date),ymin = DpredInf,ymax = DpredSup,fill = "c",group = "1"),alpha = 0.25) + 
      theme_solarized(light = FALSE) +  scale_x_date(breaks = seq.Date(ymd(min(ymd(tmp$date),na.rm = T)),ymd(end_validate)+simulate_length,length.out = 12),
                                                     labels = strftime(seq.Date(ymd(min(ymd(tmp$date))),ymd(end_validate)+simulate_length,length.out = 12),
                                                                       format="%d/%m/%y")) + 
      scale_y_continuous(breaks = round(seq(min(c(tmp$DpredInf,tmp$IspredInf),na.rm = T),
                                            max(c(tmp$DpredSup,tmp$IspredSup),na.rm = T),length.out = 10))) +
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
                       "Ipred" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$I))))),2,median),
                       "IpredInf" = minI*apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$I))))),2,min),
                       "IpredSup" = maxI*apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$I))))),2,max),
                       "Itpred" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$It))))),2,median),
                       "ItpredInf" = minI*apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$It))))),2,min),
                       "ItpredSup" = maxI*apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$It))))),2,max),
                       "Rpred" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$R))))),2,median),
                       "RpredInf" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$R))))),2,min),
                       "RpredSup" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$R))))),2,max),
                       "Dpred" = apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$D))))),2,median),
                       "DpredInf" = minD*apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$D))))),2,min),
                       "DpredSup" = maxD*apply(rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$D))))),2,max))
  tmp2 <- obs %>% filter(date <= min(c_pred$date)) %>% select(date,infected,confirmed_corrected,deaths_corrected) %>% data.table()
  c_pred$DpredInf[1] <- c_pred$DpredInf[1]/minD
  c_pred$DpredSup[1] <- c_pred$DpredSup[1]/maxD
  c_pred$IpredInf[1] <- c_pred$IpredInf[1]/minI
  c_pred$IpredSup[1] <- c_pred$IpredSup[1]/maxI
  c_pred$IspredInf[1] <- c_pred$IspredInf[1]/minI
  c_pred$IspredSup[1] <- c_pred$IspredSup[1]/maxI
  c_pred$ItpredInf[1] <- c_pred$ItpredInf[1]/minI
  c_pred$ItpredSup[1] <- c_pred$ItpredSup[1]/maxI
  tmp2 <- tmp2[,infected := sum(infected),by = date]
  tmp2 <- tmp2[,confirmed_corrected := sum(confirmed_corrected),by = date]
  tmp2 <- tmp2[,deaths_corrected := sum(deaths_corrected),by = date]
  tmp2 <- unique(data.frame(tmp2))
  tmp2 <- tmp2 %>% filter(confirmed_corrected > 100)
  tmp <- rbind.data.frame(data.frame("date" = tmp2$date,"Epred" = NA,"EpredInf" = NA,"EpredSup" = NA,"Ispred" = tmp2$infected,"IspredInf" = NA,
                                     "IspredSup" = NA,"Ipred" = NA,"IpredInf" = NA,"IpredSup" = NA,"Itpred" = tmp2$confirmed_corrected,
                                     "ItpredInf" = NA,"ItpredSup" = NA,"Rpred" = NA,"RpredInf" = NA,"RpredSup" = NA,"Dpred" = tmp2$deaths_corrected,
                                     "DpredInf" = NA,"DpredSup" = NA),c_pred)
  p <- ggplot(tmp,aes(x = ymd(date),group = 1)) + geom_vline(xintercept = ymd(as.matrix(rbind(peak[nrow(peak),2:4]))[1,]),color = "white",
                                                                linetype = "dashed") +
    geom_line(aes(y = Ispred, color = "a")) + geom_ribbon(aes(ymin = IspredInf,ymax = IspredSup,fill = "a"),alpha = 0.25) +
    geom_line(aes(y = Dpred, color = "c")) + geom_ribbon(aes(ymin = DpredInf,ymax = DpredSup,fill = "c"),alpha = 0.25) + 
    theme_solarized(light = FALSE) +  scale_x_date(breaks = seq.Date(ymd(min(ymd(tmp$date),na.rm = T)),ymd(end_validate)+simulate_length,length.out = 12),
                                                   labels = strftime(seq.Date(ymd(min(ymd(tmp$date),na.rm = T)),
                                                                              ymd(end_validate)+simulate_length,length.out = 12),
                                                                     format="%d/%m/%y")) + 
    scale_y_continuous(breaks = round(seq(min(c_pred$Dpred,na.rm = T),max(c_pred$IspredSup,na.rm = T),length.out = 10))) +
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
  pdf(file = paste("/storage/SEIR/",pos,"/SP_EPcurve_predicted_MEDIAN_",pos,".pdf",sep = ""),
      width = 15,height = 10)
  suppressWarnings(suppressMessages(print(p))) #Save plot
  dev.off()
  
  #All deaths
  tmp_deaths <- rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$D)))))
  tmp_deaths <- data.frame(t(tmp_deaths))
  tmp_deaths$Data <- as.character(ymd(end_validate) + 1:nrow(tmp_deaths) - 1)
  tmp_deaths <- tmp_deaths %>% gather("Modelo","Mortes",-Data)
  
  #Infected for plot
  tmp_cases <- rbindlist(lapply(X = predSIM,FUN = function(x) data.frame(rbind(rowSums(x$Is)))))
  tmp_cases <- data.frame(t(tmp_cases))
  tmp_cases$Data <- as.character(ymd(end_validate) + 1:nrow(tmp_cases) - 1)
  tmp_cases <- tmp_cases %>% gather("Modelo","Casos",-Data)
  
  tmp_deaths$key <- paste(tmp_deaths$Data,tmp_deaths$Modelo)
  tmp_cases$key <- paste(tmp_cases$Data,tmp_cases$Modelo)
  pl <- merge(tmp_cases,tmp_deaths %>% select(key,Mortes),by = "key")
  pl$key <- NULL
  pl <- rbind.data.frame(data.frame("Data" = tmp2$date,"Modelo" = "0","Casos" = tmp2$infected,"Mortes" = tmp2$deaths_corrected),pl)
  
  p <- ggplot(pl %>% filter(Modelo != "0"),aes(x = ymd(Data),group = Modelo)) + 
    geom_vline(xintercept = ymd(as.matrix(rbind(peak[nrow(peak),2:4]))[1,]),color = "white",
               linetype = "dashed") + 
    geom_line(aes(y = Casos, color = "a"),alpha = 0.4) + 
    geom_line(data = pl %>% filter(Modelo == "0"),aes(x = ymd(Data), y = Casos, color = "a",group = "1")) +
    geom_line(data = tmp,aes(x = ymd(date), y = Ispred, color = "a",group = "1")) +
    geom_line(data = tmp,aes(x = ymd(date), y = Dpred, color = "c",group = "1")) +
    geom_ribbon(data = tmp,aes(x = ymd(date),ymin = IspredInf,ymax = IspredSup,fill = "a",group = "1"),alpha = 0.25) +
    geom_line(aes(y = Mortes, color = "c"),alpha = 0.4) + 
    geom_line(data = pl %>% filter(Modelo == "0"),aes(x = ymd(Data),y = Mortes, color = "c",group = "1")) + 
    geom_ribbon(data = tmp,aes(x = ymd(date),ymin = DpredInf,ymax = DpredSup,fill = "c",group = "1"),alpha = 0.25) + 
    theme_solarized(light = FALSE) +  scale_x_date(breaks = seq.Date(ymd(min(ymd(tmp$date),na.rm = T)),ymd(end_validate)+simulate_length,length.out = 12),
                                                   labels = strftime(seq.Date(ymd(min(ymd(tmp$date))),ymd(end_validate)+simulate_length,length.out = 12),
                                                                     format="%d/%m/%y")) + 
    scale_y_continuous(breaks = round(seq(min(c(tmp$DpredInf,tmp$IspredInf),na.rm = T),
                                          max(c(tmp$DpredSup,tmp$IspredSup),na.rm = T),length.out = 10))) +
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
  
  c_pred <- c_pred %>% select("date","Itpred","ItpredInf","ItpredSup","Dpred","DpredInf","DpredSup")
  names(c_pred) <- c("date","median_cases","min_cases","max_cases","median_deaths","min_deaths","max_deaths")
  fwrite(c_pred,paste("/storage/SEIR/",pos,"/simulation_state_",pos,".csv",sep = ""))
  
  return(list("Dsim" = Dsim,"Isim" = Isim,"peakM" = peakM))
}