###########################################
######Relatório Índice de Isolamento#######
######Abril de 2020                 #######
######Diego Marcondes               #######
######dmarcondes@ime.usp.br         #######
######Utils                         #######
###########################################

#Add SOCIAL!!!!

#Calcula o indice
indice <- function(iso,media,desvio){
  v <- media + 2*desvio
  a <- ifelse(v >= 1,NA,as.numeric(iso > v)*(iso - v)/(1-v))
  return(a)
}

#Mean e sd cortando 5%
mean_trim <- function(x){
  return(mean(x = x,trim = 0.05))
}

sd_trim <- function(x){
  return(sd(x = Trim(x = x,trim = 0.05)))
}

#Generate map
generate_map <- function(d,dados,shp,s){
  dados_dia <- dados %>% filter(day == d) %>% select(reg_name,indice)
  names(dados_dia)[1] <- "id"
  dados_mapa <- inner_join(x = shp,y = dados_dia,by = c("id","id"))
  p <- ggplot(dados_mapa,aes(x=long, y = lat, group = group,fill = indice)) + 
    geom_polygon(color = "black") +
    theme_bw() + ylab("") + xlab("Índice de Isolamento") +
    scale_fill_gradientn("",colours = rc,na.value = "transparent",
                         limits = c(0,max(dados$indice,na.rm = T)),
                         breaks = c(min(dados$indice,na.rm = T),max(dados$indice,na.rm = T)),
                         labels = c("Dentro do Normal","Todos em Isolamento")) +
    ggtitle(paste("Índice de Isolamento\n",s,"-",format.Date(d,"%d/%m/%Y"),
                  "\nPara mais informações acesse www.ime.usp.br/~pedrosp/covid19/",
                  sep = "")) +
    theme(legend.title = element_text(face = "bold"),legend.position = "bottom") + 
    theme(plot.title = element_text(face = "bold"),
          legend.text = element_text(face = "bold",size = 15,color = c("red","green")),
          legend.box.margin = unit(x=c(0,0,0,0),units="mm"),
          legend.key.width=unit(3.5,"cm"),
          axis.title = element_text(face = "bold",size = 20))
  p
  dados_dia$d <- format.Date(d,"%d/%m/%Y")
  names(dados_dia) <- c("Cidade","Índice de Isolamento","Dia")
  return(list("p" = p,"tab" = dados_dia))
}

gerar_mapas <- function(dados,shp,s){
  return(function(d) generate_map(d = d,dados = dados,shp = shp,s = s))
}

#Tirar acento
acento <- function(x) iconv(x, to = "ASCII//TRANSLIT")

