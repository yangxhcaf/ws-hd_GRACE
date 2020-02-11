library(raster); library(dplyr); library(magrittr); library(spatstat); library(rgdal); library(sf)
library(RColorBrewer); library(ggsci); library(tmap); library(tmaptools)

mainDir <- "Z:/2.active_projects/Xander/"

# import data
VSI_water_weighted <- raster(paste(mainDir, "VSI/rasters/", "SensAETW", ".tif", sep="")) # Vegetation sensitity index, water factor, weighted
TWS_trend <- raster(paste(mainDir, "! GIS_files/GRACE/", "GRACE_coredata", ".tif", sep="")) # Rodell et al. (2018) TWS trends, resampled to 0.05 res
GridArea <- raster(paste(mainDir, "! GIS_files/R_gis_exports/", "WGS84_cellArea_0d05res", ".tif", sep="")) # Grid area (km2) for WGS84 projection
Lakes <- raster(paste(mainDir, "! GIS_files/Lakes/", "Lakes_0d05", ".tif", sep="")) # Lakes 
BioHotspots_RAS <- raster(paste(mainDir, "! GIS_files/Biodiversity/Hotspots/", "Hotspots_NoBorder_0d05", ".tif", sep="")) # Biodiversity hotspots
BioHotspots_VEC <- readOGR(dsn=paste(mainDir, "! GIS_files/Biodiversity/Hotspots", sep=""), 
                           layer="Areas_noOuterLimit")
AntA <- raster(paste(mainDir, "! GIS_files/NaturalEarth/", "Antarctica", ".tif", sep="")) # Antarctica
GADM_lvl0 <- raster(paste(mainDir, "! GIS_files/GADM/", "GADM_level0_0d05", ".tif", sep="")) # GADM national dataset

# Remove lake and ocean areas
rmRAS <- raster(GridArea)
rmRAS[] <- 0
rmRAS[Lakes == 1 | AntA == 1] <- 1; rmRAS[is.na(GADM_lvl0)] <- 1

GridArea[rmRAS == 1] <- NA
TWS_trend[rmRAS == 1] <- NA
BioHotspots_RAS[rmRAS == 1] <- NA
VSI_water_weighted[rmRAS == 1] <- NA

s <- raster(GridArea)
res(s) <- 0.05
VSI_worldextent <- raster::resample(VSI_water_weighted, s, method = "ngb")

# Scale water factor to 95th percentile 
df <- cbind(as.data.frame(VSI_worldextent), as.data.frame(GridArea)) %>%  set_colnames(c("VSI_wat", "GA"))
p95.vsi_w <- weighted.quantile(df$VSI_wat, df$GA, 0.95, na.rm=TRUE) # determine max value at 95th percentile
VSI_water_scale <- VSI_worldextent/p95.vsi_w; VSI_water_scale[VSI_water_scale > 1] <- 1

# cap TWS trends at normalized scores in range of [-1, 1]
TWS_scale <- TWS_trend/2; TWS_scale[TWS_scale > 1] <- 1; TWS_scale[TWS_scale < -1] <- -1

# pass GRACE TWS trend dataset through water sensitivity filter
EcoImpact <- TWS_scale*VSI_water_scale

# make map plot
data("World"); tmap_options(max.raster = c(plot = 25920000, view = 25920000))
Lakes.narm <- Lakes; Lakes.narm[Lakes.narm == 0] <- NA

map <-    tm_shape(EcoImpact) + 
  tm_raster(style = "cont", midpoint = 0, palette = "RdBu") +
    tm_legend(show = FALSE, legend.position = c("left", "bottom"))+
    tm_shape(Lakes.narm) +
  tm_raster(style = "cat", palette = colorRampPalette(c("grey"))(1), n = 1, colorNA = NULL) +
    tm_legend(show = FALSE, legend.position = c("left", "bottom")) +
  tm_shape(World) +
    tm_borders("black", lwd = .5) +
  tm_shape(BioHotspots_VEC)+
    tm_borders("blue", lwd = .75) +
  tm_graticules(lwd = 0.15, labels.show = FALSE, labels.size = 0) 
# save map plot
tmap_save(map, "C:/Users/Tom/Desktop/Fig1d.png", dpi = 500, outer.margins = 0.01, width = 3.376, units = "in")



########################
## Summary statistics ##
########################

Sum_stats <- cbind(as.data.frame(EcoImpact), as.data.frame(GridArea), as.data.frame(BioHotspots)) %>% set_colnames(c("EcoImp", "GA", "Hotspot"))
Sum_stats$Hotspot %<>% as.factor()
Sum_stats <- Sum_stats[complete.cases(Sum_stats), ]

Median_summ <- Sum_stats %>% 
  group_by(Hotspot) %>%
  summarise(median = weighted.quantile(EcoImp, GA, probs = 0.50, na.rm = TRUE)) %>%  as.data.frame() 
Ordered <- Median_summ[order(Median_summ$median),]


writeRaster(AdaptCap, filename=paste(mainDir, "! GIS_files/R_gis_exports/", "AdaptCap_2015", ".tif", sep=""),
             format="GTiff", overwrite=TRUE)
# 
# writeRaster(EcoImpact, filename=paste(mainDir, "! GIS_files/R_gis_exports/wshdGRACE_Fig1/", "EcoImpact", ".tif", sep=""),
#             format="GTiff", overwrite=TRUE)
