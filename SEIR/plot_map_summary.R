#Build maps with models parameters
plot_maps_summary <- function(Rt,par,drs,obs,day,pos){
  
  #Mapas
  shp <- readOGR(dsn = "~/mdyn/maps/sp_municipios/35MUE250GC_SIR.shp",stringsAsFactors = F,verbose = F) #Shapefiles
  shp$NM_MUNICIP <- gsub("'","",shp$NM_MUNICIP) #Correct names
  shp$NM_MUNICIP[shp$NM_MUNICIP == "BIRITIBA MIRIM"] <- "BIRITIBA-MIRIM"  #Correct names
  shp <- fortify(shp,region = "NM_MUNICIP") #Fortify
  rc_cont <- colorRampPalette(colors = c("white","orange","red"))(100)
  
  c_100 <- obs %>% filter(date == ymd(day) & confirmed_corrected >= 1000) %>% select(city)
  
  #Rt
  pRt <- lapply(Rt,function(x) data.frame(rbind(as.vector(x))))
  pRt <- bind_rows(pRt)
  colnames(pRt) <- par$names
  pRt <- apply(pRt,2,median)
  pRt <- data.frame("id" = names(pRt),"Rt" = pRt)
  pRt <- merge(pRt,drs %>% select(Municipio,Regiao),by.y = "Municipio",by.x = "id")
  pRt$Regiao <- as.character(pRt$Regiao)
  pRt$Regiao[pRt$Regiao == "Cidade de São Paulo"] <- "Grande São Paulo"
  pRt <- pRt %>% filter(id %in% c_100$city)
  pRt <- merge(pRt,shp,all = T)
  pRt$Rt[is.na(pRt$Rt)] <- 0
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
  pRt <- data.frame("Municipio" = par$names,"Minimo" = apply(bind_rows(lapply(Rt,function(x) data.frame(rbind(x)))),2,min),
                    "Mediana" = apply(bind_rows(lapply(Rt,function(x) data.frame(rbind(x)))),2,median),
                    "Máximo" = apply(bind_rows(lapply(Rt,function(x) data.frame(rbind(x)))),2,max))
  pRt <- merge(drs %>% select(Municipio,Regiao),pRt)
  names(pRt)[2] <- "DRS"
  pRt <- pRt[order(pRt$Mediana,decreasing = T),]
  pRt <- pRt %>% filter(Municipio %in% c_100$city)
  write.csv(pRt,file = paste("/storage/SEIR/",pos,"/SP_Rt_",pos,".csv",sep = ""),row.names = F)
} 

