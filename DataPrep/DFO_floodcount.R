library(raster); library(rgdal); library(sf); library(magrittr)

mainDir <- "Z:/2.active_projects/Xander/! GIS_files/"

GridArea <- raster(paste(mainDir, "R_gis_exports/", "WGS84_cellArea_0d05res", ".tif", sep="")) # Grid area (km2) for WGS84 projection
DFO_set <- readOGR(dsn=paste(mainDir, "DartmouthFloodingObservatory", sep=""), layer="FloodsArchived_shape")

# select only floodings occuring during or before 2015
DFO_set$Ended %<>% as.Date()
DFO_2015clip <- DFO_set[DFO_set$Ended < "2016-01-01",]

# create raster at 0.05 resolutoin counting number of floods on record
DFO_2015clip$Counter <- 1
DFO_ras <- raster::rasterize(DFO_2015clip, GridArea, field = "Counter", fun = "count")
DFO_ras[is.na(DFO_ras)] <- 0

# write raster
writeRaster(DFO_ras, 
            filename="Z:/2.active_projects/Xander/! GIS_files/DartmouthFloodingObservatory/FloodCount_0d05_1985to2015.tif", 
            format="GTiff", overwrite=TRUE)
