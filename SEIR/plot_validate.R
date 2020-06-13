#Plot validate curves
plot_validate <- function(drs,obs_drs,par,pred,init_validate,end_validate,pos,minI,maxI,minD,maxD){
  
  #For each DRS deaths
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
    I_mod <- pred[[k]]$It
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
    if(max(I[[d]]$I) > 1000 | max(D[[d]]$D) > 100){
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
      pdf(file = paste("/storage/SEIR/",pos,"/validate/DRS_",gsub(" ","",unique(drs$Regiao[drs$DRS == d])),"_mortes.pdf",sep = ""),width = 15,height = 10)
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
      pdf(file = paste("/storage/SEIR/",pos,"/validate/DRS_",gsub(" ","",unique(drs$Regiao[drs$DRS == d])),"_casos.pdf",sep = ""),width = 15,height = 10)
      suppressWarnings(suppressMessages(print(pI)))
      dev.off()
    }
  }
  
  #Plot city
  for(c in par$names){
    tmp <- obs %>% filter(date == ymd(end_validate) & city == c)
    if(tmp$confirmed_corrected > 1000 | tmp$deaths_corrected > 100){
      position <- match(c,par$names)
      D <- data.frame()
      I <- data.frame()
      tmp <- data.frame("Dpred" = rep(NA,as.numeric(ymd(end_validate)-ymd(init_validate))+1),"DpredInf" = NA,"DpredSup" = NA,
                        "Ipred" = NA,"IpredInf" = NA,"IpredSup" = NA)
      for(k in 1:length(pred)){
        D <- bind_rows(D,data.frame(rbind(pred[[k]]$D[,position])))
        I <- bind_rows(I,data.frame(rbind(pred[[k]]$It[,position])))
      }
      tmp$Dpred <- apply(X = D,MARGIN = 2,FUN = median)
      tmp$DpredInf <- minD*apply(X = D,MARGIN = 2,FUN = min)
      tmp$DpredInf[1] <- tmp$DpredInf[1]/minD
      tmp$DpredSup <- maxD*apply(X = D,MARGIN = 2,FUN = max)
      tmp$DpredSup[1] <- tmp$DpredSup[1]/maxD
      tmp$Ipred <- apply(X = I,MARGIN = 2,FUN = median)
      tmp$IpredInf <- minI*apply(X = I,MARGIN = 2,FUN = min)
      tmp$IpredInf[1] <- tmp$IpredInf[1]/minI
      tmp$IpredSup <- maxI*apply(X = I,MARGIN = 2,FUN = max)
      tmp$IpredSup[1] <- tmp$IpredSup[1]/maxI
      tmp$date <- seq.Date(ymd(init_validate),ymd(end_validate),1) 
      tmp1 <- obs %>% filter(date >= ymd(end_validate)-31 & date <= ymd(end_validate) & city == c) %>% select(date,confirmed_corrected,deaths_corrected)
      names(tmp1)[2:3] <- c("I","D")
      tmp <- merge(tmp,tmp1,all = T)
      
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
        ggtitle(paste("Mortes confirmadas COVID-19 em",c,"- SP"))
      pdf(file = paste("/storage/SEIR/",pos,"/validate/",gsub(" ","",c),"_mortes.pdf",sep = ""),width = 15,height = 10)
      suppressWarnings(suppressMessages(print(pD)))
      dev.off()
      
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
        ggtitle(paste("Casos confirmados COVID-19 em",c,"- SP"))
      pdf(file = paste("/storage/SEIR/",pos,"/validate/",gsub(" ","",c),"_casos.pdf",sep = ""),width = 15,height = 10)
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
                       "Ipred" = apply(rbindlist(lapply(lapply(X = pred,FUN = function(x) x$It),function(x) data.frame(rbind(rowSums(x))))),2,median),
                       "IpredInf" = minI*apply(rbindlist(lapply(lapply(X = pred,FUN = function(x) x$It),function(x) data.frame(rbind(rowSums(x))))),2,min),
                       "IpredSup" = maxI*apply(rbindlist(lapply(lapply(X = pred,FUN = function(x) x$It),function(x) data.frame(rbind(rowSums(x))))),2,max),
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
}