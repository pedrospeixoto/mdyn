#build maps of simulation
build_maps <- function(dataSim,drs,par,end_validate){
  
  #Data
  Dsim <- dataSim$Dsim
  Isim <- dataSim$Isim
  peak <- dataSim$peakM
  
  #Correct drs
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
  tmp <- tmp %>% select(Municipio,tmedian)
  tmp <- merge(shp,tmp,by.x = "id",by.y = "Municipio")
  tmp <- tmp[order(tmp$order),]
  rc_cont_inv <- colorRampPalette(colors = c("red","darkgoldenrod1","white"))(simulate_length+1)
  p <- ggplot(tmp,aes(long, lat, group=group,fill = log(1+tmedian))) + theme_bw() + geom_polygon(colour='gray30') +
    xlab("") + ylab("") + titles_Map + scale_fill_gradientn("",colours = rc_cont_inv,limits = log(1+c(0,simulate_length)),
                                                            breaks = c(0.25,log(1+simulate_length)),
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
}