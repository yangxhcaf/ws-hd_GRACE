library(raster)
library(dplyr)
library(magrittr)
library(spatstat)

mainDir <- "Z:/2.active_projects/Xander/"

# import data
VSI_water_weighted <- raster(paste(mainDir, "VSI/rasters/", "SensAETW", ".tif", sep="")) # Vegetation sensitity index, water factor, weighted
TWS_trend <- raster(paste(mainDir, "! GIS_files/GRACE/", "GRACE_coredata", ".tif", sep="")) # Rodell et al. (2018) TWS trends, resampled to 0.05 res
GridArea <- raster(paste(mainDir, "! GIS_files/R_gis_exports/", "WGS84_cellArea_0d05res", ".tif", sep="")) # Grid area (km2) for WGS84 projection
BioHotspots <- raster(paste(mainDir, "! GIS_files/Biodiversity/Hotspots/", "Hotspots_NoBorder_0d05", ".tif", sep="")) # Biodiversity hotspots

TWS_trend %<>% crop(VSI_water_weighted)
GridArea %<>% crop(VSI_water_weighted)
BioHotspots %<>% crop(VSI_water_weighted)

# Scale water factor to 95th percentile 
df_vsi_w <- VSI_water_weighted %>% as.data.frame() %>% set_colnames(c("VSI_wat"))
df_ga <- GridArea %>% as.data.frame() %>% set_colnames(c("GA"))
df <- cbind(df_vsi_w, df_ga)
p95.vsi_w <- weighted.quantile(df$VSI_wat, df$GA, 0.95, na.rm=TRUE) # determine max value at 95th percentile
VSI_water_scale <- VSI_water_weighted/p95.vsi_w; VSI_water_scale[VSI_water_scale > 1] <- 1

writeRaster(VSI_water_scale, filename=paste(mainDir, "! GIS_files/R_gis_exports/", "VSI_water_weight_scale", ".tif", sep=""),
              format="GTiff", overwrite=TRUE)

# cap TWS trends at normalized scores in range of [-1, 1]
TWS_scale <- TWS_trend/2; TWS_scale[TWS_scale > 1] <- 1; TWS_scale[TWS_scale < -1] <- -1

# pass GRACE TWS trend dataset through water sensitivity filter
EcoImpact <- TWS_scale*VSI_water_scale; writeRaster(EcoImpact, filename=paste(mainDir, "! GIS_files/R_gis_exports/", "GRACE_ecoImpact", ".tif", sep=""),
                                                      format="GTiff", overwrite=TRUE)

Sum_stats <- cbind(as.data.frame(EcoImpact), as.data.frame(GridArea), as.data.frame(BioHotspots)) %>% set_colnames(c("EcoImp", "GA", "Hotspot"))
Sum_stats$Hotspot %<>% as.factor()
Sum_stats <- Sum_stats[complete.cases(Sum_stats), ]

Median_summ <- Sum_stats %>% 
  group_by(Hotspot) %>%
  summarise(median = weighted.quantile(EcoImp, GA, probs = 0.50, na.rm = TRUE)) %>%  as.data.frame() 
Ordered <- Median_summ[order(Median_summ$median),]