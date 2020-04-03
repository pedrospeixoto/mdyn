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

#wd
setwd("~/GDrive/github/mdyn")

#São Paulo State

#Read shapefiles
shape_state <- readOGR(dsn = "~/GDrive/github/mdyn/maps/sp_municipios/35MUE250GC_SIR_mdyn.shp",
                       stringsAsFactors = F)
#shape_setor <- readOGR(dsn = "~/GDrive/github/mdyn/maps/sp_setores_censitarios/setor_censitario_sao_paulo.shp",
#                       stringsAsFactors = F,encoding = "latin1")

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
writeOGR(as(shape_bairro, "SpatialPolygonsDataFrame"), ".", "shape_sp_bairro", driver="ESRI Shapefile")

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
writeOGR(as(shape_bairro, "SpatialPolygonsDataFrame"), ".", "shape_rj_bairro", driver="ESRI Shapefile")

#Merging shapefiles
# shape_setor <- subset(shape_setor,NM_MUNICIP %in% population$municipio)
# shape_state <- subset(shape_state,!(NM_MUNICIP %in% population$municipio))
# shape <- bind(shape_setor,shape_state)
# shape$ID[is.na(shape$ID)] <- shape$NM_MUNICIP[is.na(shape$ID)]
# writeOGR(shape, ".", "shape_rj_censitario_capital", driver="ESRI Shapefile")
# salvar_plot(p = p,arquivo = "censitario_rio_capital.pdf",width = 150,height = 100)
