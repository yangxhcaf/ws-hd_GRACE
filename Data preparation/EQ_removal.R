## Create raster to identify which regions to remove from analysis due to earthquake interference in TWS trend derivation

library(ggplot2); library(raster); library(dplyr); library(magrittr) 
library(reshape2); library(spatstat); library(e1071); library(rgdal); library(gdalUtils)
library(tmap); library(tmaptools)
if(!is.null(dev.list())) dev.off(); cat("\014");rm(list=ls())
setwd("Z:/2.active_projects/Xander/! GIS_files")

# load shapefile of earthquake regions to remove
## image of these regions can be found at: <https://github.com/XanderHuggins/ws-hd_GRACE/blob/master/AdditionalResources/EQ_removed_regions.png>
EQ_regions <- readOGR(dsn=paste("./Earthquakes", sep=""), layer="Tohoku+SumAnd_GADM1_exposed") # earthquake admin regions to remove

# load a raster w/ 0.05d resolution for reference
GridArea <- raster("./R_gis_exports/WGS84_cellArea_0d05res.tif") # Grid area (km2) for WGS84 projection

# convert EQ regions to binary raster at 0.05 resolution
EQ_regions$ID <- 1
EQ_regions_RAS <- raster::rasterize(EQ_regions, GridArea, 'ID', fun = 'max')

# write raster
writeRaster(EQ_regions_RAS, filename = "./Earthquakes/EQ_rm_0d05.tif",
            format = "GTiff", overwrite = T)
