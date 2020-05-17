library(ggplot2); library(raster); library(dplyr); library(magrittr) 
library(reshape2); library(spatstat); library(e1071); library(rgdal); library(gdalUtils)
library(tmap); library(tmaptools)
if(!is.null(dev.list())) dev.off(); cat("\014");rm(list=ls())
setwd("Z:/2.active_projects/Xander/! GIS_files")

# import data
PopImp <- raster("C:/Users/Tom/Desktop/FinalRun/GTiff_populationimpact.tif") 
AgImp <-  raster("C:/Users/Tom/Desktop/FinalRun/GTiff_agricultureimpact.tif") 
GDPImp <- raster("C:/Users/Tom/Desktop/FinalRun/GTiff_economicimpact.tif") 
EcoImp <- raster("C:/Users/Tom/Desktop/FinalRun/GTiff_ecoimpact_pumping_g200_binary.tif") 

# Remove NAs so cumulative addition can be performed
PopImp[is.na(PopImp)] <- 0; AgImp[is.na(AgImp)] <- 0; GDPImp[is.na(GDPImp)] <- 0; EcoImp[is.na(EcoImp)] <- 0

# Equally weight all
CumImp <- stack(PopImp, AgImp, GDPImp, EcoImp)
CumImp_i <- calc(CumImp, fun = mean)

##### load rasters IDing regions to remove
EQ_regions <- raster("./Earthquakes/EQ_rm_0d05.tif") # earthquake regions to remove
GADM_lvl0 <- raster("./GADM/GADM_level0_0d05.tif") # GADM national dataset to remove oceans
Caspian <- raster("./Lakes/CaspianSea_0d05.tif")

CumImp_i[EQ_regions[] == 1 | is.na(GADM_lvl0[]) | Caspian[] == 1] <- NA

# make map plot
data(World)
Lakes_ply <- readOGR(dsn=paste("./Lakes", sep=""), layer="ne_10m_lakes")
Lakes_ply$scalerank %<>% as.numeric()
Lakes_large <- subset(Lakes_ply, scalerank < 4)

map <- 
  tm_shape(CumImp_i, projection = "robin") +  tm_raster(style = "cont", palette = "RdBu", midpoint = 0, breaks = c(-1, 1)) +
  tm_shape(World) +  tm_borders(lwd = 0.7) +
  tm_shape(Lakes_large) + tm_borders("black", lwd = 0.5) +
  tm_style("white", legend.show = F, earth.boundary = c(-180, -60, 180, 88), earth.boundary.color = "white",
           space.color = "white", legend.frame = F, frame = F)
map

# save map plot
tmap_save(map, "C:/Users/Tom/Desktop/Hotspots_Eco_w_g200binary.png", dpi = 500, 
          outer.margins = 0.01, height = 3, units = "in")

####################################################################### .
### Now for hotspot mapping

GridArea <- raster("./R_gis_exports/WGS84_cellArea_0d05res.tif") # Grid area (km2) for WGS84 projection
AdaptCap <- raster("./Varis_AdaptiveCapacity/AC_2015_0d05.tif") # VAris et al. AC for 2015 
TWS_trend <- raster("./Rodell_SourceData/Rodell_etal_0d05.tif") # Rodell et al. (2018) TWS trends
Pop <- raster("./R_gis_exports/POP_2015_0d05_UNWPP.tif") # Population data from GWPv4 UNWPP 2015
Greenland <- raster("./NaturalEarth/Greenland.tif")
Sub60S <- raster("./R_gis_exports/Sub60_rm_0d05.tif")
Sub60S <- raster::resample(Sub60S, GridArea, method = "ngb")

TotalImpact <- CumImp_i
TotalImpact[Greenland[] == 1 | Sub60S[] == 1] <- NA

# Remove EQ regions and oceans from input data
GridArea[is.na(TotalImpact)] <- NA
Pop[is.na(TotalImpact)] <- NA
AdaptCap[is.na(TotalImpact)] <- NA

thrsh_imp <- 0.05
# Determine population weighted threshold for total impact (wetting or drying)
TotalImp_ABS <- TotalImpact
TotalImp_ABS[TotalImpact[] < 0] <- TotalImpact[TotalImpact[] < 0]*(-1)

df <- cbind(as.data.frame(TotalImp_ABS), as.data.frame(Pop), as.data.frame(GridArea)) %>% 
  set_colnames(c("TWSImpAbs", "Pop", "GA"))
TWSImpAbs_high <- weighted.quantile(df$TWSImpAbs, df$GA, 1-thrsh_imp, na.rm=TRUE) %>% as.numeric()

thrsh_ac <- 0.20
# Determine thresholds of with low, moderate, and high AC 
df <- cbind(as.data.frame(AdaptCap), as.data.frame(Pop), as.data.frame(GridArea)) %>% 
  set_colnames(c("AC", "Pop", "GA"))
AC_low <- weighted.quantile(df$AC, df$Pop, thrsh_ac, na.rm=TRUE) %>% as.numeric()
AC_high <- weighted.quantile(df$AC, df$Pop, 1-thrsh_ac, na.rm=TRUE) %>% as.numeric()

# Create categorical raster:
SummaryRAS <- raster(GridArea)
SummaryRAS[TotalImp_ABS[] < TWSImpAbs_high] <- 0 # Low TWS impact
SummaryRAS[TotalImp_ABS[] >= TWSImpAbs_high & AdaptCap[] <= AC_low] <- 3 # High TWS impact and low AC
SummaryRAS[TotalImp_ABS[] >= TWSImpAbs_high & AdaptCap[] >  AC_low] <- 2 # High TWS impact and moderate AC
SummaryRAS[TotalImp_ABS[] >= TWSImpAbs_high & AdaptCap[] >= AC_high] <- 1 # High TWS impact and high AC
# writeRaster(SummaryRAS, filename=paste(mainDir, "! GIS_files/R_gis_exports/", "TWSImpact_Summary", ".tif", sep=""),
#             format="GTiff", overwrite=TRUE)

# make map plot
data(World)
Lakes_ply <- readOGR(dsn=paste("./Lakes", sep=""), layer="ne_10m_lakes")
Lakes_ply$scalerank %<>% as.numeric()
Lakes_large <- subset(Lakes_ply, scalerank < 4)

my_palette <- c("#c0c0c0", "#6f63bb", "#ffbd4c", "#e75726")
map <- 
  tm_shape(World, projection = "robin") +  tm_polygons(fill = "#c0c0c0") +
  tm_shape(SummaryRAS, projection = "robin") +  tm_raster(style = "cont", palette = my_palette) +
  tm_shape(World) +  tm_borders(lwd = 0.7) +
  tm_shape(Lakes_large) + tm_borders("black", lwd = 0.5) +
  tm_style("white", legend.show = F, earth.boundary = c(-180, -60, 180, 88), earth.boundary.color = "white",
           space.color = "white", legend.frame = F, frame = F)
map

# save map plot
tmap_save(map, "C:/Users/Tom/Desktop/Fig3d_5percentAreaImp_20percentPopAC.png", dpi = 500, 
          outer.margins = 0.01, height = 3, units = "in")
