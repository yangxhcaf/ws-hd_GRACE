library(ggplot2); library(raster); library(dplyr); library(magrittr)
library(reshape2); library(spatstat); library(e1071); library(rgdal); library(sf)
library(tmap); library(tmaptools)
if(!is.null(dev.list())) dev.off(); cat("\014");rm(list=ls())

mainDir <- "Z:/2.active_projects/Xander/! GIS_files/"

# import grid area raster already in 0.05 resolution
GridArea <- raster(paste(mainDir, "R_gis_exports/", "WGS84_cellArea_0d05res", ".tif", sep="")) # Grid area estimate for WGS84 reference elipsoid

# import raw feed, food, and nonfood rasters at 0.083333 resolution
FEED <- raster(paste(mainDir, "CropAllocationFoodFeedFuel_Geotiff/Cassidy_etal/", "GlbFeedkcal", ".tif", sep=""))
FOOD <- raster(paste(mainDir, "CropAllocationFoodFeedFuel_Geotiff/Cassidy_etal/", "GlbFoodkcal", ".tif", sep=""))
NONFOOD <- raster(paste(mainDir, "CropAllocationFoodFeedFuel_Geotiff/Cassidy_etal/", "GlbNonFoodkcal", ".tif", sep=""))

# aggregate each Cassidy dataset to 0.25 resolution (to preserve global sum); lowest aggregation factor that is also multiple of 0.05
FEED_0d25agg <- raster::aggregate(FEED, fact = (0.25/res(FEED)), fun = sum)
FOOD_0d25agg <- raster::aggregate(FOOD, fact = (0.25/res(FEED)), fun = sum)
NONFOOD_0d25agg <- raster::aggregate(NONFOOD, fact = (0.25/res(FEED)), fun = sum)

# resample each aggregated dataset to operating resolution of 0.05 and then divide by 25 to preserve total
FEED_0d05 <- raster::resample(FEED_0d25agg, GridArea, method = 'ngb')
FOOD_0d05 <- raster::resample(FOOD_0d25agg, GridArea, method = 'ngb')
NONFOOD_0d05 <- raster::resample(NONFOOD_0d25agg, GridArea, method = 'ngb')

FEED_0d05_gs <- FEED_0d05/25
FOOD_0d05_gs <- FOOD_0d05/25
NONFOOD_0d05_gs <- NONFOOD_0d05/25

writeRaster(NONFOOD_0d05_gs, filename=paste(mainDir, "CropAllocationFoodFeedFuel_Geotiff/Cassidy_gs/", "NONFOOD_0d05_gs", ".tif", sep=""),
            format="GTiff", overwrite=TRUE)
