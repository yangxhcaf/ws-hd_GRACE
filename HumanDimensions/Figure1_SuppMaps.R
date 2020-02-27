library(raster); library(dplyr); library(magrittr); library(rgdal); library(R.utils)
library(tigris); library(spatstat); library(ggplot2); library(RColorBrewer); library(ggsci);
library(tmap); library(tmaptools)

####################
## General set-up ##
####################

mainDir <- "Z:/2.active_projects/Xander/"

# import common data 
GridArea <- raster(paste(mainDir, "! GIS_files/R_gis_exports/", "WGS84_cellArea_0d05res", ".tif", sep="")) # Grid area (km2) for WGS84 projection
TWS_trend <- raster(paste(mainDir, "! GIS_files/Rodell_SourceData/", "Rodell_etal_0d05", ".tif", sep="")) # Rodell et al. (2018) TWS trends
GADM_lvl0 <- raster(paste(mainDir, "! GIS_files/GADM/", "GADM_level0_0d05", ".tif", sep="")) # GADM national dataset
Lakes <- raster(paste(mainDir, "! GIS_files/Lakes/", "Lakes_0d05", ".tif", sep="")) # Lakes 
AntA <- raster(paste(mainDir, "! GIS_files/NaturalEarth/", "Antarctica", ".tif", sep="")) # Antarctica

# Remove lake and ocean areas
rmRAS <- raster(GridArea)
rmRAS[] <- 0
rmRAS[Lakes == 1 | AntA == 1] <- 1; rmRAS[is.na(GADM_lvl0)] <- 1
GridArea[rmRAS == 1] <- NA
TWS_trend[rmRAS == 1] <- NA

# develop TWS scaler
TWS_scaler <- TWS_trend/2; TWS_scaler[TWS_scaler > 1] <- 1; TWS_scaler[TWS_scaler < -1] <- -1

# create function to reclassify a raster into weighted percentiles
PercentileAssignment <- function(PercentileRaster, RawRaster, WeightRaster){
  PercentileRaster <- raster(RawRaster)
  PercentileRaster[] <- 0
  Ref.DF <- cbind(as.data.frame(RawRaster), as.data.frame(WeightRaster)) %>%  set_colnames(c("RAW", "WEIGHT"))
  pb <- txtProgressBar(min = 0, max = 99, style = 3)
  
  for(i in 0:99){
    j = 1 - (i*0.01)
    k = 0.99 - (i*0.01)
    PercentileRaster[RawRaster <= as.numeric(unname(weighted.quantile(Ref.DF[,1], Ref.DF[,2], j, na.rm = TRUE))) & 
                       RawRaster > as.numeric(unname(weighted.quantile(Ref.DF[,1], Ref.DF[,2], k, na.rm = TRUE)))] <- j
    setTxtProgressBar(pb, i)
  }
  return(PercentileRaster)
}

########################
## Fig1a - Population ##
########################
Pop <- raster(paste(mainDir, "! GIS_files/R_gis_exports/", "POP_2015_0d05_UNWPP", ".tif", sep="")) # Population data from GWPv4 UNWPP 2015
Pop[rmRAS == 1] <- NA

# Determine population density raster and then create area-weighted percentile version
PopDens <- Pop/GridArea
PopDens_percentile <- PercentileAssignment(PopDens_percentile, PopDens, GridArea)
PopDens_percentile[rmRAS == 1] <- NA

# Scale by TWS trend
Pop_map <- PopDens_percentile*TWS_scaler

# make map plot
data("World"); tmap_options(max.raster = c(plot = 25920000, view = 25920000))
Lakes.narm <- Lakes; Lakes.narm[Lakes.narm == 0] <- NA
e <- extent(c(-180, 180, -60, 88))
map <-  
  tm_shape(Pop_map, projection="robin") + tm_raster(style = "cont", palette = "RdBu", midpoint = 0) +
  tm_shape(Lakes.narm) + tm_raster(style = "cat", palette = colorRampPalette(c("grey"))(1), n = 1, colorNA = NULL) +
  tm_shape(World) +  tm_borders("black", lwd = .5) +
  tm_style("white", legend.show = F,
           frame = F, bg.color = "white", earth.boundary = e, earth.boundary.color = "white", earth.boudary.lwd = 2,
           space.color="white", legend.frame = T, legend.bg.color="white")
# save map plot
tmap_save(map, "C:/Users/Tom/Desktop/Fig1a_sup.png", dpi = 500, outer.margins = 0.01, height = 2, units = "in")

#########################
## Fig1b - Agriculture ##
#########################
# import data
FEED <- raster(paste(mainDir, "! GIS_files/CropAllocationFoodFeedFuel_Geotiff/", "GlbFeedkcal_Resample_0d05", ".tif", sep="")) 
FOOD <- raster(paste(mainDir, "! GIS_files/CropAllocationFoodFeedFuel_Geotiff/", "GlbFoodkcal_Resample_0d05", ".tif", sep="")) 
NONFOOD <- raster(paste(mainDir, "! GIS_files/CropAllocationFoodFeedFuel_Geotiff/", "GlbNonFoodkcal_Resample_0d05", ".tif", sep="")) 

# sum for all production and get density/intensity
AgProduction <- FEED + FOOD + NONFOOD
AgProdDens <- AgProduction/GridArea

# Determine population density raster and then create area-weighted percentile version
AgProdDens_percentile <- PercentileAssignment(AgProdDens_percentile, AgProdDens, GridArea)
AgProdDens_percentile[rmRAS == 1] <- NA

# Scale by TWS trend
Ag_map <- AgProdDens_percentile*TWS_scaler

# make map plot
data("World"); tmap_options(max.raster = c(plot = 25920000, view = 25920000))
Lakes.narm <- Lakes; Lakes.narm[Lakes.narm == 0] <- NA
e <- extent(c(-180, 180, -60, 88))
map <-  
  tm_shape(Ag_map, projection="robin") + tm_raster(style = "cont", palette = "RdBu", midpoint = 0) +
  tm_shape(Lakes.narm) + tm_raster(style = "cat", palette = colorRampPalette(c("grey"))(1), n = 1, colorNA = NULL) +
  tm_shape(World) +  tm_borders("black", lwd = .5) +
  tm_style("white", legend.show = F,
           frame = F, bg.color = "white", earth.boundary = e, earth.boundary.color = "white", earth.boudary.lwd = 2,
           space.color="white", legend.frame = T, legend.bg.color="white")
# save map plot
tmap_save(map, "C:/Users/Tom/Desktop/Fig1b_sup.png", dpi = 500, outer.margins = 0.01, height = 2, units = "in")

#################
## Fig1c - GDP ##
#################
# import data
GDP <- raster(paste(mainDir, "! GIS_files/GDP/Kummu/", "GDP_PPP_2015_0d05", ".tif", sep="")) # Kummu et al. (2019) GDP (PPP) dataset

# sum for all production and get density/intensity
GDPDens <- GDP/GridArea

# Determine population density raster and then create area-weighted percentile version
GDPDens_percentile <- PercentileAssignment(GDPDens_percentile, GDPDens, GridArea)
GDPDens_percentile[rmRAS == 1] <- NA

# Scale by TWS trend
GDP_map <- GDPDens_percentile*TWS_scaler

# make map plot
data("World"); tmap_options(max.raster = c(plot = 25920000, view = 25920000))
Lakes.narm <- Lakes; Lakes.narm[Lakes.narm == 0] <- NA
e <- extent(c(-180, 180, -60, 88))
map <-  
  tm_shape(GDP_map, projection="robin") + tm_raster(style = "cont", palette = "RdBu", midpoint = 0) +
  tm_shape(Lakes.narm) + tm_raster(style = "cat", palette = colorRampPalette(c("grey"))(1), n = 1, colorNA = NULL) +
  tm_shape(World) +  tm_borders("black", lwd = .5) +
  tm_style("white", legend.show = F,
           frame = F, bg.color = "white", earth.boundary = e, earth.boundary.color = "white", earth.boudary.lwd = 2,
           space.color="white", legend.frame = T, legend.bg.color="white")
# save map plot
tmap_save(map, "C:/Users/Tom/Desktop/Fig1c_sup.png", dpi = 500, outer.margins = 0.01, height = 2, units = "in")

######################
## Fig1e - Combined ##
######################

####### Run below code if first time through
# PopImp <- Pop_map
# AgImp  <- Ag_map
# GDPImp <- GDP_map
# EcoImp <- raster(paste(mainDir, "! GIS_files/R_gis_exports/wshdGRACE_Fig1/", "EcoImpact", ".tif", sep="")) 

# import data if running seperately 
PopImp <- raster(paste(mainDir, "! GIS_files/R_gis_exports/wshdGRACE_Fig1/", "PopImpact", ".tif", sep="")) 
AgImp  <- raster(paste(mainDir, "! GIS_files/R_gis_exports/wshdGRACE_Fig1/", "AgImpact", ".tif", sep="")) 
GDPImp <- raster(paste(mainDir, "! GIS_files/R_gis_exports/wshdGRACE_Fig1/", "GDPImpact", ".tif", sep="")) 
EcoImp <- raster(paste(mainDir, "! GIS_files/R_gis_exports/wshdGRACE_Fig1/", "EcoImpact", ".tif", sep="")) 

# Extend EcoImp from -60 to -90 lat
ext <- extent(-180, 180, -90, 90)
EcoImp.ex <- raster::extend(EcoImp, ext)

# Load biodiversity hotspots to weight eco impact half as much if not inside of one
BioHotspots_RAS <- raster(paste(mainDir, "! GIS_files/Biodiversity/Hotspots/", "Hotspots_NoBorder_0d05", ".tif", sep="")) # Biodiversity hotspots
BioHotspots_RAS[BioHotspots_RAS >= 1] <- 1
EcoImp.ex_damp <- EcoImp.ex
EcoImp.ex_damp[BioHotspots_RAS == 1] <- EcoImp.ex[BioHotspots_RAS == 1]*2
EcoImp.ex_damp <- EcoImp.ex_damp/2

# Remove NAs so cumulative addition can be performed
PopImp[is.na(PopImp)] <- 0; AgImp[is.na(AgImp)] <- 0; GDPImp[is.na(GDPImp)] <- 0; EcoImp.ex_damp[is.na(EcoImp.ex_damp)] <- 0

# sum for all production and get density/intensity
CumulativeImp <- (PopImp + AgImp + GDPImp + EcoImp.ex_damp)/4
CumulativeImp[rmRAS == 1] <- NA # remove ocean and lake areas

# make map plot
data("World"); tmap_options(max.raster = c(plot = 25920000, view = 25920000))
Lakes.narm <- Lakes; Lakes.narm[Lakes.narm == 0] <- NA
e <- extent(c(-180, 180, -60, 88))
map <-  
  tm_shape(CumulativeImp, projection="robin") + tm_raster(style = "cont", palette = "RdBu", midpoint = 0, breaks = c(-1,1)) +
  tm_shape(Lakes.narm) + tm_raster(style = "cat", palette = colorRampPalette(c("grey"))(1), n = 1, colorNA = NULL) +
  tm_shape(World) +  tm_borders("black", lwd = .5) +
  tm_style("white", legend.show = F,
           frame = F, bg.color = "white", earth.boundary = e, earth.boundary.color = "white", earth.boudary.lwd = 2,
           space.color="white", legend.frame = T, legend.bg.color="white")
# save map plot
tmap_save(map, "C:/Users/Tom/Desktop/Fig1e_sup.png", dpi = 500, outer.margins = 0.01, height = 2, units = "in")

# template for saving creatd rasters
########################################################################################################
writeRaster(GDP_map, filename=paste(mainDir, "! GIS_files/R_gis_exports/wshdGRACE_Fig1/", "GDPImpact", ".tif", sep=""),
            format="GTiff", overwrite=TRUE)
