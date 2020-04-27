library(raster); library(dplyr); library(magrittr); library(rgdal); library(gdalUtils)

# clear all
if(!is.null(dev.list())) dev.off(); cat("\014");rm(list=ls())

setwd("Z:/2.active_projects/Xander/! GIS_files")

# load core data
TWS_trend <- raster("./Rodell_SourceData/Rodell_etal_0d05.tif") # Rodell et al. (2018) TWS trends
GridArea <- raster("./R_gis_exports/WGS84_cellArea_0d05res.tif") # Grid area (km2) for WGS84 projection
FPU <- raster("./WaterScarcityAtlas/fpu30_hyde_compatible_v20d05.tif")
FPU[FPU < 0] <- NA; FPU[FPU>573] <- NA

######.
# calculate average of weighted product in each FPU
WeightedProduct_avg <- raster::zonal(TWS_trend*GridArea, FPU, fun = "mean", na.rm = T)
WeightedProduct_avg %<>% as.data.frame()

# note all FPU ID's aren't assigned values ... so assign NA to all of these regions
id_df <- c(seq(0, 573, by = 1)) %>% as.data.frame(); colnames(id_df) = "FPU.id"
id_df$setna <- NA

match_df <- merge(id_df, WeightedProduct_avg, by.x = "FPU.id", by.y = "zone", all.x = T)

# assign shortage (measured in cubic meters per year per capita)
match_df$l <- match_df$FPU.id - 0.5
match_df$h <- match_df$FPU.id + 0.5
FPU_assign1 <- data.frame(low = match_df$l, high = match_df$h, ReCLASS = match_df$mean)
FPU_product <- reclassify(FPU, FPU_assign1)

#####.
# Now repeat for grid areas
Area_avg <- raster::zonal(GridArea, FPU, fun = "mean", na.rm = T)
Area_avg %<>% as.data.frame()

# note all FPU ID's aren't assigned values ... so assign NA to all of these regions
id_df <- c(seq(0, 573, by = 1)) %>% as.data.frame(); colnames(id_df) = "FPU.id"
id_df$setna <- NA

match_df <- merge(id_df, Area_avg, by.x = "FPU.id", by.y = "zone", all.x = T)

# assign shortage (measured in cubic meters per year per capita)
match_df$l <- match_df$FPU.id - 0.5
match_df$h <- match_df$FPU.id + 0.5
FPU_assign2 <- data.frame(low = match_df$l, high = match_df$h, ReCLASS = match_df$mean)
FPU_Area <- reclassify(FPU, FPU_assign2)

#######.
# Remove ocean areas from analysis
GADM_lvl0 <- raster("./GADM/GADM_level0_0d05.tif") # GADM national dataset rasterized to 0.05

FPU_product[is.na(GADM_lvl0[])] <- NA
FPU_Area[is.na(GADM_lvl0[])] <- NA

FPU_meanTWS <- FPU_product/FPU_Area

writeRaster(FPU_meanTWS, 
            filename="./Rodell_SourceData/FPU_meanTWS", 
            format="GTiff", overwrite=TRUE)
