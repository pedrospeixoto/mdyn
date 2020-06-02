shp <- readRDS(file = "./www/shp_brazil.rds")
shp_estados <- readRDS(file = "./www/shape_estados.rds")

shp.simplify <- gSimplify(shp_estados,0.1,topologyPreserve = T)
merge.df <- data.frame(shp_estados@data)
shp_s <- SpatialPolygonsDataFrame(shp.simplify, merge.df)
saveRDS(shp_s,"./www/shape_estados.rds")
