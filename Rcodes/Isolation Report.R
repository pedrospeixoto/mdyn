###########################################
######Relatório Índice de Isolamento#######
######Abril de 2020                 #######
######Diego Marcondes               #######
######dmarcondes@ime.usp.br         #######
###########################################

#wd
setwd("~/mdyn")
library(ggplot2)
library(htmlwidgets)
library(tidyverse)
library(lubridate)
library(grid)
library(data.table)
library(DescTools)
library(plyr)
library(rgdal)
library(ggthemes)
library(raster)
library(rgeos)
library(mapview)
library(sf)
library(svMisc)
library(leaflet.extras)
library(leaflet.providers)
library(htmltools)
library(leafletCN)
library(leaflet)
source("./Rcodes/utils.R")
#options(encoding = "Latin1")
rc5 <- colorRampPalette(colors = c("red","darkgoldenrod1","green"), space = "Lab")(5)
rc3 <- colorRampPalette(colors = c("red","darkgoldenrod1","green"), space = "Lab")(3)
get_png <- function(filename) {
  grid::rasterGrob(image = png::readPNG(filename))
}
logo <- get_png("./logos/logos_juntos.png")

titles <- theme(strip.text = element_text(size = 12), 
                axis.text = element_text(size = 12,color = "black"), axis.title = element_text(size = 14), 
                legend.text = element_text(size = 14),
                legend.title = element_text(size = 14), panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(), panel.border = element_blank(),
                panel.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                legend.background = element_rect(fill="white",size=0.5, linetype="solid",color = "black"),
                legend.position="bottom",legend.spacing.x = unit(0.5, 'cm'))
themes <- list(theme_linedraw())

#Input
ini_quar <- "2020-03-01"
end_quar <- "2020-04-13"
dias_quar <- seq.Date(from = ymd(ini_quar),to = ymd(end_quar),by = 1)
dias_padrao_pan <- seq.Date(from = ymd("2020-03-29"),to = ymd(end_quar),by = 1)

ini_padrao <- "2019-06-01"
end_padrao <- "2020-02-29"
dias_padrao <- seq.Date(from = ymd(ini_padrao),to = ymd(end_padrao),by = 1)

dic_estados <- list('AC' = 'Acre','AL' = 'Alagoas','AP' = 'Amapá','AM' = 'Amazonas','BA' = 'Bahia',
                    'CE' = 'Ceará','DF' = 'Distrito Federal','ES' = 'Espírito Santo','GO' = 'Goiás',
                    'MA' = 'Maranhão','MT' = 'Mato Grosso','MS' = 'Mato Grosso do Sul',
                    'MG' = 'Minas Gerais','PA' = 'Pará','PB' = 'Paraíba','PR' = 'Paraná',
                    'PE' = 'Pernambuco','PI' = 'Piauí','RJ' = 'Rio de Janeiro',
                    'RN' = 'Rio Grande do Norte','RS' = 'Rio Grande do Sul','RO' = 'Rondônia',
                    'RR' = 'Roraima','SC' = 'Santa Catarina','SP' = 'São Paulo','SE' = 'Sergipe',
                    'TO' = 'Tocantins')
dic_shp <- list("AC" = "12MUE250GC_SIR","AL" = "27MUE250GC_SIR","AP" = "16MUE250GC_SIR",
                "AM" = "13MUE250GC_SIR","BA" = "29MUE250GC_SIR",
                "CE" = "23MUE250GC_SIR","DF" = "53MUE250GC_SIR","ES" = "32MUE250GC_SIR",
                "GO" = "52MUE250GC_SIR","MA" = "21MUE250GC_SIR","MT" = "51MUE250GC_SIR",
                "MS" = "50MUE250GC_SIR","MG" = "31MUE250GC_SIR","PA" = "15MUE250GC_SIR",
                "PB" = "25MUE250GC_SIR","PR" = "41MUE250GC_SIR","PE" = "26MUE250GC_SIR",
                "PI" = "22MUE250GC_SIR","RJ" = "33MUE250GC_SIR","RN" = "24MUE250GC_SIR",
                "RS" = "43MUE250GC_SIR","RO" = "11MUE250GC_SIR","RR" = "14MUE250GC_SIR",
                "SC" = "42MUE250GC_SIR","SP" = "35MUE250GC_SIR","SE" = "28MUE250GC_SIR",
                "TO" = "17MUE250GC_SIR")
dic_pronome <- list('AC' = 'do','AL' = 'de','AP' = 'do','AM' = 'do','BA' = 'da','CE' = 'do',
                    'DF' = 'do','ES' = 'do','GO' = 'de','MA' = 'do','MT' = 'do','MS' = 'do',
                    'MG' = 'de','PA' = 'do','PB' = 'da','PR' = 'do','PE' = 'de','PI' = 'do',
                    'RJ' = 'do','RN' = 'do','RS' = 'do','RO' = 'de','RR' = 'de','SC' = 'de',
                    'SP' = 'de','SE' = 'de','TO' = 'do')
estados <- names(dic_estados)
dir_data <- "/home/pedrosp/mdyn/dump/mdyn_params_mun_index/"
posfix_data <- "_Municipios_2019-06-01_2020-04-14_iso_index.csv"
city_names <- read.csv("./maps/population/population_mun_br_2019_namesfixed.csv")

#Organizando os dados de cada estado
for(s in estados){
  cat("Estado: ")
  cat(s)
  cat("\n")
  file <- paste(dir_data,toupper(dic_estados[s]),posfix_data,sep = "")
  dados <- data.frame(read.csv(file)) 
  dados$iso <- 1 - dados$left_home/dados$active_users_in_month
  saveRDS(object = dados,file = paste("./dataR/original_",acento(s),".rds",sep = ""))
  dados <- dados %>% select("reg_name","iso","day")
  dados$day <- ymd(dados$day)
  dados$weekday <- weekdays(dados$day)
  dados$key <- paste(dados$reg_name,dados$weekday)
  dados <- dados %>% filter(reg_name != "-1")
  
  #Calcular padrão pre-pandemia
  padrao_pre <- data.table(dados %>% filter(day %in% dias_padrao))
  padrao_pre <- padrao_pre[,mean_pre := mean_trim(iso),by = key]    
  padrao_pre <- data.frame(padrao_pre[,sd_pre := sd_trim(iso),by = key])  
  padrao_pre <- padrao_pre %>% select("key","mean_pre","sd_pre") %>% unique()
  
  #Calcular padrao durante pandemia
  padrao_pan <- data.table(dados %>% filter(day %in% dias_quar))
  padrao_pan <- padrao_pan[,mean_pan := mean_pan(iso,day),by = key]    
  padrao_pan <- padrao_pan[,sd_pan := sd_pan(iso,day),by = key]
  padrao_pan <- data.frame(padrao_pan[,last_week := last_pan(iso,day),by = key])
  padrao_pan$mean_pan[!(padrao_pan$day %in% dias_padrao_pan)] <- NA
  padrao_pan$sd_pan[!(padrao_pan$day %in% dias_padrao_pan)] <- NA
  padrao_pan$last_week[!(padrao_pan$day %in% dias_padrao_pan)] <- NA
  padrao_pan$key <- paste(padrao_pan$reg_name,padrao_pan$day)
  padrao_pan <- padrao_pan %>% select("key","mean_pan","sd_pan","last_week") %>% unique()
  
  #Dias de quarentena
  dados <- dados %>% filter(day %in% dias_quar)
  
  #Juntando e calculando indice
  dados <- merge(dados,padrao_pre)
  dados$key <- paste(dados$reg_name,dados$day)
  dados <- merge(dados,padrao_pan)
  dados$weekday <- factor(dados$weekday)
  dados$key <- NULL
  
  #Calculando Indices
  dados$indice_pre <- indice_pre(dados$iso,dados$mean_pre,dados$sd_pre)
  dados$indice_pan <- indice_pan(dados$iso,dados$mean_pan,dados$sd_pan)
  dados$indice_week <- indice_week(dados$iso,dados$last_week,dados$sd_pan)
  
  #Salvando
  saveRDS(object = dados,file = paste("./dataR/",s,".rds",sep = ""))
  
  #Apagando
  rm(padrao_pan,padrao_pre,dados)
}

#Mapa leflet
dadosBR <- data.frame()
for(s in estados){
  cat(paste("Estado:",s))
  cat("\n")
  dados <- readRDS(file = paste("./dataR/",s,".rds",sep = ""))
  dados$UF <- s
  if(nrow(dadosBR) == 0)
    dadosBR <- dados
  else
    dadosBR <- rbind.data.frame(dadosBR,dados)
}
dadosBR <- dadosBR %>% filter(day == max(dadosBR$day))
dadosBR <- dadosBR %>% filter(UF == "AL")
shp <- readOGR("./maps/br_municipios/BRMUE250GC_SIR.shp",verbose = F)
city_names$MUNICIPIO <- toupper(city_names$MUNICIPIO)
shp <- merge(x = shp,y = city_names,by.x = "NM_MUNICIP",by.y = "MUNICIPIO")
#shp$key <- paste(shp$NM_MUNICIP,shp$UF)
#dadosBR$key <- paste(dadosBR$reg_name,dadosBR$UF)
tmp <- merge(shp,dadosBR,by.x = "NM_MUNICIP",by.y = "reg_name",all.x = F,all.y = F)

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
"))

title <- tags$div(
  tag.map.title, HTML("Isolamento Social Comparativo IME - USP")
)  

ime <- tags$div(
  HTML('<a href="https://www.ime.usp.br/"> <img border="0" alt="ImageTitle" src="./logos/IME_simplificado.jpg" width="60" height="100"> </a>')
)  
usp <- tags$div(
  HTML('<a href="https://www.usp.br/"> <img border="0" alt="ImageTitle" src="./logos/logo_USP.jpeg" width="60" height="35"> </a>')
) 
fapesp <- tags$div(
  HTML('<a href="http://www.fapesp.br/en/"> <img border="0" alt="ImageTitle" src="./logos/FAPESP.png" width="60" height="35"> </a>')
) 

mypal <- colorFactor(palette = rc5, domain = tmp$indice_pre)

mapa <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron,options = providerTileOptions(minZoom = 4)) %>% 
  addEasyButton(easyButton(
    icon="fa-crosshairs", title="Locate Me",
    onClick=JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
  addControl(title, position = "topleft", className="map-title") %>%
  addControl(ime, position = "bottomleft") %>%
  addControl(usp, position = "bottomleft") %>%
  addControl(fapesp, position = "bottomleft") %>%
  addPolygons(data = tmp,weight = 1,fillColor = mypal(tmp$indice_pre),color = "grey",
              popup = paste('<img title="Teste" src = ./plots/isol_',tmp$NM_MUNICIP,'_',tmp$UF,
                            '.png width="750" height="500"/>',
                            sep = ""),options = popupOptions(opacity = 0,closeButton = FALSE),
              opacity = 0.5,fillOpacity = 0.5,label = paste(tmp$NM_MUNICIP,'-',tmp$UF))
  
  #addPolygons(data = tmp,weight = 1,fillColor = mypal(tmp$indice_pre),color = "grey",
  #            popup = paste('Cidade:',tmp$NM_MUNICIP,'-',tmp$UF),options = popupOptions(opacity = 0,closeButton = FALSE),
  #            opacity = 0,fillOpacity = 0)

  addLegend(position = "bottomright", pal = mypal, values = 100*tmp$iso,na.label = "S/D",
            title = "Isolamento (%)",opacity = 0.8)    
mapa
saveWidget(mapa, file="mapa_BR.html")

#Gráfico por cidade
for(s in estados){
  cat("Estado: ")
  cat(s)
  cat("\n")
  
  #Lendo os dados
  cat("Lendo os dados...")
  dados <- readRDS(file = paste("./dataR/",s,".rds",sep = ""))
  cat(" OK!\n")
  dados$indice_weekN <- as.numeric(as.character(mapvalues(dados$indice_week,levels(dados$indice_week),c(0.5,1,1.5))))
  dados$indice_panN <- as.numeric(as.character(mapvalues(dados$indice_pan,levels(dados$indice_pan),c(1.5,2,2.5)+0.25)))
  dados$indice_preN <- as.numeric(as.character(mapvalues(dados$indice_pre,levels(dados$indice_pre),c(2.5,2.5+0.375,2.5+2*0.375,2.5+3*0.375,2.5+4*0.375)+0.5)))
  
  cat("Construindo gráficos para as cidades...")
  for(c in unique(dados$reg_name)){
    tmp <- dados %>% filter(day %in% dias_quar & reg_name == c)
    tmp$y <- NA
    tmp$day <- as.numeric(tmp$day)
    
    p <- ggplot(tmp,aes(x = day,y = y)) + theme_solarized(light = FALSE) +
      geom_hline(yintercept = c(1.625,2.875)) +
      xlab("Data") + ylab("") + 
      scale_x_continuous(breaks = as.numeric(seq.Date(from = ymd(ini_quar),to = ymd(end_quar),by = 3)),
                    labels = paste(day(seq.Date(from = ymd(ini_quar),to = ymd(end_quar),by = 3)),"/0",
                                  month(seq.Date(from = ymd(ini_quar),to = ymd(end_quar),by = 3)),sep = "")) +
      theme(legend.title = element_text(face = "bold"),legend.position = "none") + 
      theme(plot.title = element_text(face = "bold",size = 20,color = "white"),
            axis.text.x = element_text(size = 15,color = "white"),
            axis.text.y = element_text(size = 20,color = "white"),
            legend.box.margin = unit(x=c(0,0,0,0),units="mm"),
            legend.key.width=unit(3.5,"cm"),panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(), 
            axis.title = element_text(color = "white",size = 20)) +
      scale_y_continuous(breaks = c(1,2.25,3),labels = c("Semana anterior","Pandemia\n(A partir de 16/03/2020)","Pré-pandemia\n(Julho/19-Fevereiro/20)"),
                         limits = c(0.45,4.6)) +
      ggtitle(paste("Isolamento Social Comparativo IME - USP\n",c," - ",s,sep = "")) +
      geom_segment(aes(x = as.numeric(ymd("2020-02-27")),y = 1,xend = as.numeric(ymd("2020-02-27")),yend = 0.5),color = rc3[1]) +
      annotate(geom = "text",label = "Queda",x = as.numeric(ymd("2020-02-27")),y = 0.45,color = rc3[1],size = 5) +
      geom_segment(aes(x = as.numeric(ymd("2020-02-27")),y = 1,xend = as.numeric(ymd("2020-02-27")),yend = 1.5),color = rc3[3]) +
      annotate(geom = "text",label = "Alta",x = as.numeric(ymd("2020-02-27")),y = 1.55,color = rc3[3],size = 5) +
      geom_segment(aes(x = as.numeric(ymd("2020-02-27")),y = 1,xend = as.numeric(ymd("2020-02-27")),yend = 1),color = rc3[2]) +
      annotate(geom = "text",label = "Estável",x = as.numeric(ymd("2020-02-27"))-1.5,y = 1.05,color = rc3[2],size = 5) +
      
      geom_segment(aes(x = as.numeric(ymd("2020-02-27")),y = 2.25,xend = as.numeric(ymd("2020-02-27")),yend = 1.75),color = rc3[1]) +
      annotate(geom = "text",label = "Abaixo Padrão",x = as.numeric(ymd("2020-02-27")),y = 1.7,color = rc3[1],size = 5) +
      geom_segment(aes(x = as.numeric(ymd("2020-02-27")),y = 2.25,xend = as.numeric(ymd("2020-02-27")),yend = 2.75),color = rc3[3]) +
      annotate(geom = "text",label = "Acima Padrão",x = as.numeric(ymd("2020-02-27")),y = 2.8,color = rc3[3],size = 5) +
      geom_segment(aes(x = as.numeric(ymd("2020-02-27")),y = 2.25,xend = as.numeric(ymd("2020-02-27")),yend = 2.25),color = rc3[2]) +
      annotate(geom = "text",label = "Padrão",x = as.numeric(ymd("2020-02-27"))-1.5,y = 2.3,color = rc3[2],size = 5) +
      
      scale_color_manual(values=c("Abaixo do padrão" = rc3[1],"Dentro do padrão" = rc3[2],"Acima do padrão" = rc3[3], "Padrão" = rc5[1],"Leve" = rc5[2],"Moderado" = rc5[3],"Alto" = rc5[4],"Intenso" = rc5[5],"Queda" = rc3[1],"Estável" = rc3[2],"Alta" = rc3[3])) +
      
      geom_segment(aes(x = as.numeric(ymd("2020-02-27")),y = 3,xend = as.numeric(ymd("2020-02-27")),yend = 3),
                   color = rc5[1]) +
      annotate(geom = "text",label = levels(tmp$indice_pre)[1],x = as.numeric(ymd("2020-02-27"))-1.5,y = 3.05,
               color = rc5[1],size = 5) +
      geom_segment(aes(x = as.numeric(ymd("2020-02-27")),y = 3,xend = as.numeric(ymd("2020-02-27")),yend = 3.375),
                   color = rc5[2]) +
      annotate(geom = "text",label = levels(tmp$indice_pre)[2],x = as.numeric(ymd("2020-02-27"))-1.5,y = 3.375,
               color = rc5[2],size = 5) +
      geom_segment(aes(x = as.numeric(ymd("2020-02-27")),y = 3.375,xend = as.numeric(ymd("2020-02-27")),yend = 3.75),
                   color = rc5[3]) +
      annotate(geom = "text",label = levels(tmp$indice_pre)[3],x = as.numeric(ymd("2020-02-27"))-2,y = 3.75,
               color = rc5[3],size = 5) +
      geom_segment(aes(x = as.numeric(ymd("2020-02-27")),y = 3.75,xend = as.numeric(ymd("2020-02-27")),yend = 4.125),
                   color = rc5[4]) +
      annotate(geom = "text",label = levels(tmp$indice_pre)[4],x = as.numeric(ymd("2020-02-27"))-1.5,y = 4.125,
               color = rc5[4],size = 5) +
      geom_segment(aes(x = as.numeric(ymd("2020-02-27")),y = 4.125,xend = as.numeric(ymd("2020-02-27")),yend = 4.5),
                   color = rc5[5]) +
      annotate(geom = "text",label = levels(tmp$indice_pre)[5],x = as.numeric(ymd("2020-02-27"))-1.5,y = 4.5,
               color = rc5[5],size = 5) +
      geom_point(aes(x = day,y = indice_preN,color = indice_pre)) +
      geom_point(aes(x = day,y = indice_panN,color = indice_pan)) +
      geom_point(aes(x = day,y = indice_weekN,color = indice_week)) +
      scale_fill_manual(values=c("Abaixo do padrão" = rc3[1],"Dentro do padrão" = rc3[2],"Acima do padrão" = rc3[3], "Padrão" = rc5[1],"Leve" = rc5[2],"Moderado" = rc5[3],"Alto" = rc5[4],"Intenso" = rc5[5],"Queda" = rc3[1],"Estável" = rc3[2],"Alta" = rc3[3])) + 
      annotation_custom(logo, xmin = 18303, xmax = 18311, ymin = 3.5, ymax = 5) +
      annotation_custom(textGrob("Desenvolvido por Diego Marcondes.",gp=gpar(col="white")), xmin = 18300, xmax = 18315, ymin = 0, ymax = 0) +
      coord_cartesian(clip = "off") +
      theme(plot.margin = unit(c(1, 1, 1, 1), "lines")) +
      theme(axis.text.y=element_text(hjust=0.5,vjust=0.2))
    
    if(sum(!is.na(tmp$indice_preN)) != 0){
      spline_pre <- data.frame(spline(x = tmp$day,y = tmp$indice_preN))
      p <- p + geom_line(data = spline_pre,aes(x = x,y = y),color = "grey",alpha = 1)
    }
    if(sum(!is.na(tmp$indice_panN)) != 0){
      spline_pre <- data.frame(spline(x = tmp$day,y = tmp$indice_panN))
      p <- p + geom_line(data = spline_pre,aes(x = x,y = y),color = "grey",alpha = 1)
    }
    if(sum(!is.na(tmp$indice_weekN)) != 0){
      spline_pre <- data.frame(spline(x = tmp$day,y = tmp$indice_weekN))
      p <- p + geom_line(data = spline_pre,aes(x = x,y = y),color = "grey",alpha = 1)
    }
    
    pdf(file = paste("./plots/isol_",acento(c),"_",s,".pdf",sep = ""),width = 15,height = 10)
    print(p)
    dev.off()
  }
}


