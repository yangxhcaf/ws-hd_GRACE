library(ggplot2)
library(dplyr)
library(raster)

mainDir <- "Z:/2.active_projects/Xander/"

# import all data
TWS_trend <- raster(paste(mainDir, "! GIS_files/GRACE/", "GRACE_coredata", ".tif", sep="")) # Rodell et al. (2018) TWS trends, resampled to 0.05 res
GridArea <- raster(paste(mainDir, "! GIS_files/R_gis_exports/", "WGS84_cellArea_0d05res", ".tif", sep="")) # Grid area (km2) for WGS84 projection
CropMask <- raster(paste(mainDir, "! GIS_files/Cropland/GFSAD/!Mask/", "GFSAD1KCM.2010.001__0d05", ".tif", sep="")) # Grid area (km2) for WGS84 projection

CropMask %>% as.data.frame() %>% set_names("ID") 
# 1 - irrigation, major 
# 2 - irrigation, minor
# 3 - rainfed

CropMask_stats <- cbind(as.data.frame(TWS_trend), as.data.frame(GridArea), as.data.frame(CropMask)) %>% 
  set_colnames(c("TWS", "GA", "Type"))

CropMask_stats$Trend <- ifelse(CropMask_stats$TWS < -0.5, "Drying",
                         ifelse(CropMask_stats$TWS > -0.5 & CropMask_stats$TWS < 0.5, "Static", "Wetting")) %>% as.factor()

Summ <- CropMask_stats %>% 
  group_by(Type, Trend) %>%
  summarise(Area = sum(GA)) %>% as.data.frame()
