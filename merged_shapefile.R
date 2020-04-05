#Merge shafiles of Municipio and of Setores Censitários
#Citys with less than 500.000 people are one region
#Citys with more than 500.000 people are divided in Setores Censitários

require(rgdal)
require(ggplot2)
library(raster)
library(maptools)
library(gridExtra)
library(raster)
library(geosphere)
library(mapview) 
library(rgeos)
library(sf)
library(svMisc)
library(progress)

#wd
setwd("~/GDrive/github/mdyn")

#São Paulo State

#Read shapefiles
# shape_state <- readOGR(dsn = "~/GDrive/github/mdyn/maps/sp_municipios/35MUE250GC_SIR_mdyn.shp",
#                        stringsAsFactors = F)
shape_setor <- readOGR(dsn = "~/GDrive/github/mdyn/maps/sp_setores_censitarios/setor_censitario_sao_paulo.shp",
                       stringsAsFactors = F,encoding = "latin1")

#Correct symbols
# shape_state$NM_MUNICIP[shape_state$NM_MUNICIP == "EMBU DAS ARTES"] <- "EMBU"
# shape_state$NM_MUNICIP[shape_state$NM_MUNICIP == "SÃO LUIZ DO PARAITINGA"] <- "SÃO LUÍS DO PARAITINGA"
# shape_state$NM_MUNICIP[shape_state$NM_MUNICIP == "BIRITIBA MIRIM"] <- "BIRITIBA-MIRIM"
# shape_state$NM_MUNICIP[shape_state$NM_MUNICIP == "ITAOCA"] <- "ITAÓCA"
# shape_setor$NM_MUNICIP[shape_setor$NM_MUNICIP == "MOJI MIRIM"] <- "MOGI MIRIM"
# shape_setor$NM_MUNICIP[shape_setor$NM_MUNICIP == "FLORÍNIA"] <- "FLORÍNEA"

#Merging the neighborhoods
shape_setor$key <- factor(paste(shape_setor$NM_MUNICIP,shape_setor$NM_BAIRRO)) 
shape_bairro <-  gUnaryUnion(shape_setor,shape_setor$key)
shape_bairro_dt <- as(shape_bairro, "SpatialPolygonsDataFrame")
shape_bairro_dt$ID <- names(shape_bairro)
shape_bairro_dt$dummy <- NULL
writeOGR(shape_bairro_dt, ".", "shape_sp_bairro", driver="ESRI Shapefile")

#Population
#population <- read.csv("~/GDrive/github/mdyn/maps/population/population_sp.csv",sep = ";")
#population$municipio <- toupper(population$municipio)
#population <- population[population$populacao_estimada >= 2000000,]

#Merging shapefiles
# shape_setor <- subset(shape_setor,NM_MUNICIP %in% population$municipio)
# shape_state <- subset(shape_state,!(NM_MUNICIP %in% population$municipio))
# shape <- bind(shape_setor,shape_state)
# shape$ID[is.na(shape$ID)] <- shape$NM_MUNICIP[is.na(shape$ID)]
# writeOGR(shape, ".", "shape_sp_censitario_capital", driver="ESRI Shapefile")

#Rio de Janeiro State

#Read shapefiles
#shape_state <- readOGR(dsn = "~/GDrive/github/mdyn/maps/rj_municipios/33MUE250GC_SIR.shp",
#                       stringsAsFactors = F)
shape_setor <- readOGR(dsn = "~/GDrive/github/mdyn/maps/rj_setores_censitarios/33SEE250GC_SIR.shp",
                       stringsAsFactors = F,encoding = "latin1")

#Population
# population <- read.csv("~/GDrive/github/mdyn/maps/population/population_rj.csv",sep = ";")
# population$municipio <- toupper(population$municipio)
# population <- population[population$populacao_estimada >= 2000000,]

#Merging the neighborhoods
shape_setor$key <- factor(paste(shape_setor$NM_MUNICIP,shape_setor$NM_BAIRRO)) 
shape_bairro <-  gUnaryUnion(shape_setor,shape_setor$key)
shape_bairro_dt <- as(shape_bairro, "SpatialPolygonsDataFrame")
shape_bairro_dt$ID <- names(shape_bairro)
shape_bairro_dt$dummy <- NULL
writeOGR(shape_bairro_dt, ".", "shape_rj_bairro", driver="ESRI Shapefile")

#Merging shapefiles
# shape_setor <- subset(shape_setor,NM_MUNICIP %in% population$municipio)
# shape_state <- subset(shape_state,!(NM_MUNICIP %in% population$municipio))
# shape <- bind(shape_setor,shape_state)
# shape$ID[is.na(shape$ID)] <- shape$NM_MUNICIP[is.na(shape$ID)]
# writeOGR(shape, ".", "shape_rj_censitario_capital", driver="ESRI Shapefile")
# salvar_plot(p = p,arquivo = "censitario_rio_capital.pdf",width = 150,height = 100)

#Find out city of each subdistrict
shape_rj <- readOGR(dsn = "~/GDrive/github/mdyn/maps/rj_municipios/33MUE250GC_SIR.shp",stringsAsFactors = F)
shape_sp <- readOGR(dsn = "~/GDrive/github/mdyn/maps/sp_municipios/35MUE250GC_SIR_mdyn.shp",stringsAsFactors = F)
shape_rj_sub <- readOGR(dsn = "~/GDrive/github/mdyn/maps/rj_subdistritos/shape_rj_subdistritos.shp",stringsAsFactors = F)
shape_sp_sub <- readOGR(dsn = "~/GDrive/github/mdyn/maps/sp_subdistritos/shape_sp_subdistritos.shp",stringsAsFactors = F)

domain <- shape_rj
sub_domain <- shape_rj_sub
name_sub <- "MUNICIPIO"
name_dom <- "NM_MUNICIP"

get_domain <- function(domain,sub_domain,name_sub,name_dom){
  shp <- sub_domain
  pb <- progress_bar$new(
    format = " finding cities [:bar] :percent eta: :eta",
    total = nrow(shp)*nrow(domain), clear = FALSE, width= 60)
  for(i in 1:nrow(shp)){
    dist <- data.frame("city" = rep(NA,nrow(domain)),"dist" = rep(NA,nrow(domain)))
    c <- 1
    for(dom in 1:nrow(domain)){
      pb$tick()
      int <- gIntersection(spgeom1 = gBuffer(domain[dom,], byid=TRUE, width=0),spgeom2 = gBuffer(shp[i,], byid=TRUE, width=0))
      if(!is.null(int)){
        dist$city[c] <- domain[[name_dom]][dom]
        dist$dist[c] <- gArea(int)
        c <- c + 1
      }
    }
    if(c == 1)
      cat(paste("Domain not found for sub_domain",i,"\n"))
    else{
      dist <- na.omit(dist)
      shp[[name_sub]][i] <- dist$city[dist$dist == max(dist$dist)]
    }
  }  
  return(shp)
}
shape_rj_sub_municipio <- get_domain(domain = shape_rj,sub_domain = shape_rj_sub,name_sub = "MUNICIPIO",
                                     name_dom = "NM_MUNICIP")
writeOGR(shape_rj_sub_municipio, ".", "shape_rj_subdistritos_municipio", driver="ESRI Shapefile")
shape_sp_sub_municipio <- get_domain(domain = shape_sp,sub_domain = shape_sp_sub,name_sub = "MUNICIPIO",
                                     name_dom = "NM_MUNICIP")
writeOGR(shape_sp_sub_municipio, ".", "shape_sp_subdistritos_municipio", driver="ESRI Shapefile")

shp <- readOGR(dsn = "~/GDrive/github/mdyn/maps/sp_subdistritos/shape_sp_subdistritos_municipio.shp",stringsAsFactors = F)


