library(raster)
library(dplyr)
library(magrittr)
library(rgdal)
library(tigris)
library(spatstat)

mainDir <- "Z:/2.active_projects/Xander/"

# import data  
# WaterW <- raster(paste(mainDir, "VSI/rasters/", "SensAETW", ".tif", sep="")) # EVI sensitivity to water (water component of VSI) - weighted dataset
GridArea <- raster(paste(mainDir, "! GIS_files/R_gis_exports/", "WGS84_cellArea_0d05res", ".tif", sep="")) # Grid area (km2) for WGS84 projection
AdaptCap <- raster(paste(mainDir, "! GIS_files/Varis_AdaptiveCapacity/rasters/", "AdaptiveCapacity_2015", ".tif", sep="")) # VAris et al. AC for 2015 
BasinID <- raster(paste(mainDir, "! GIS_files/Aqueduct/2019_release/", "Aqueduct2019_ID", ".tif", sep="")) # basin delineations
Aqd_Stress <- raster(paste(mainDir, "! GIS_files/Aqueduct/2019_release/", "BWS_score", ".tif", sep="")) # water stress
Aqd_Flood <- raster(paste(mainDir, "! GIS_files/Aqueduct/2019_release/", "RFR_score", ".tif", sep="")) # flooding risk
TWS_trend <- raster(paste(mainDir, "! GIS_files/GRACE/", "GRACE_coredata", ".tif", sep="")) # Rodell et al. (2018) TWS trends, resampled to 0.05 res

# identify reigons with both low water use and arid climate (concomitatnt) from analysis
rmRAS <- raster(GridArea)
rmRAS[] <- 0
rmRAS[Aqd_Stress == 5] <- 1

# also remove Antarctica (too large of influence on area-based analysis)
AntA <- raster(paste(mainDir, "! GIS_files/NaturalEarth/", "Antarctica", ".tif", sep="")); rmRAS[AntA == 1] <- 1

# remove these regions
Aqd_Stress[rmRAS == 1] <- NA
Aqd_Flood[rmRAS == 1] <- NA
AdaptCap[rmRAS == 1] <- NA

# Determine average TWS trend per basin
TWS_by_Grid <- (TWS_trend*GridArea) # weight by area
a <- zonal(TWS_by_Grid, BasinID, fun = "sum", na.rm = TRUE) # sum per basin
colnames(a) <- c("ID", "TWS_by_Grid_sum")
a %<>% as.data.frame()
b <- zonal(GridArea, BasinID, fun = "sum", na.rm = TRUE) # sum area per basin
colnames(b) <- c("ID", "Area")
b %<>% as.data.frame()
c <- merge(a, b, by = "ID")
c$TWS_mean_Basin <- c$TWS_by_Grid_sum/c$Area # get area weighted mean (aggregate)

## pair with basin shapefile (uncomment first three lines below if first run through)
# Aqd_2019 <- readOGR(dsn = "Z:/2.active_projects/Xander/! GIS_files/Aqueduct/2019_release", layer = "Aqueduct_2019_basinID")
# Aqd_2019_j <- geo_join(Aqd_2019, c, "aq30_id", "ID", by = NULL, how = "left")
# writeOGR(Aqd_2019_j, dsn = "Z:/2.active_projects/Xander/! GIS_files/Aqueduct/2019_release", "MeanTWS_aq30", driver = "ESRI Shapefile")
TWS_t_basin <- raster(paste(mainDir, "! GIS_files/Aqueduct/2019_release/", "TWS_mean", ".tif", sep="")) # reimport as raster (convert in QGIS)

# Derive TWS scaling function for GRACE modification of Aqueduct stress and flooding risk
TWS_FLOOD_scale <- raster(GridArea)
TWS_STRESS_scale <- raster(GridArea)
TWS_FLOOD_scale[TWS_t_basin <= -2] <- -2
TWS_FLOOD_scale[TWS_t_basin > -2 & TWS_t_basin <= -0.5] <- (((TWS_t_basin[TWS_t_basin > -2 & TWS_t_basin <= -0.5]+0.5)/1.5)*2)
TWS_FLOOD_scale[TWS_t_basin > -0.5 & TWS_t_basin <= 0.5] <- 0
TWS_FLOOD_scale[TWS_t_basin > 0.5 & TWS_t_basin <= 2] <- (((TWS_t_basin[TWS_t_basin > 0.5 & TWS_t_basin <= 2]-0.5)/1.5)*2)
TWS_FLOOD_scale[TWS_t_basin > 2] <- 2
TWS_STRESS_scale[] <-(-1)*TWS_FLOOD_scale[]

# Modify basin stress and flooding risk index by derived scalers
Stress_mod <- raster(GridArea)
Flood_mod <- raster(GridArea)

Stress_mod[] <- Aqd_Stress[] + TWS_STRESS_scale[] + 2 #plus 2 to have minimum set to 0
Flood_mod[] <- Aqd_Flood[] + TWS_FLOOD_scale[] + 2 #plus 2 to have minimum set to 0

# Scale modified stress and flood datasets by min and max, and normalize from 0 to 1 using area based 95th percentile
df_sm <- Stress_mod %>% as.data.frame() %>% set_colnames(c("SM"))
df_fm <- Flood_mod %>% as.data.frame() %>% set_colnames(c("FM"))
df_ga <- GridArea %>% as.data.frame() %>% set_colnames(c("GA"))
df <- cbind(df_sm, df_fm, df_ga)
p95.s <- weighted.quantile(df$SM, df$GA, 0.95, na.rm=TRUE) # determine max value at 95th percentile
p95.f <- weighted.quantile(df$FM, df$GA, 0.95, na.rm=TRUE) # determine max value at 95th percentile

Stress_mod <- Stress_mod/p95.s; Stress_mod[Stress_mod > 1] <- 1
Flood_mod <- Flood_mod/p95.f; Flood_mod[Flood_mod > 1] <- 1

# Normalize adaptive capacity (Varis et al. 2019) dataset to range [0,1]
AdaptCap_norm <- raster(GridArea)
AdaptCap_norm <-AdaptCap/maxValue(AdaptCap)

# Determine water security vulnerability by: WSV = Water Risk (flood and stress) - Adaptive capacity
WSV_flood  <- Flood_mod - AdaptCap_norm; writeRaster(WSV_flood, filename=paste(mainDir, "! GIS_files/R_gis_exports/", "WSV_flood-Varis", ".tif", sep=""),
                                                      format="GTiff", overwrite=TRUE)
WSV_stress <- Stress_mod - AdaptCap_norm; writeRaster(WSV_stress, filename=paste(mainDir, "! GIS_files/R_gis_exports/", "WSV_stress-Varis", ".tif", sep=""),
                                                      format="GTiff", overwrite=TRUE)

writeRaster(AdaptCap_norm, filename=paste(mainDir, "! GIS_files/R_gis_exports/", "Varis_AC_norm", ".tif", sep=""),
            format="GTiff", overwrite=TRUE)
