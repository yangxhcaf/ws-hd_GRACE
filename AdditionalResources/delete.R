library(raster); library(dplyr); library(magrittr); library(rgdal); library(R.utils)
library(tigris); library(spatstat); library(ggplot2); library(RColorBrewer); library(ggsci);
library(tmap); library(tmaptools); library(reshape2)

mainDir <- "Z:/2.active_projects/Xander/! GIS_files/"

# import data
GridArea <- raster(paste(mainDir, "R_gis_exports/", "WGS84_cellArea_0d05res", ".tif", sep="")) # Grid area (km2) for WGS84 projection
AdaptCap <- raster(paste(mainDir, "Varis_AdaptiveCapacity/", "AC_2015_0d05", ".tif", sep="")) # VAris et al. AC for 2015 
TWS_trend <- raster(paste(mainDir, "Rodell_SourceData/", "Rodell_etal_0d05", ".tif", sep="")) # Rodell et al. (2018) TWS trends
Pop <- raster(paste(mainDir, "R_gis_exports/", "POP_2015_0d05_UNWPP", ".tif", sep="")) # Population data from GWPv4 UNWPP 2015
TotalImpact <- raster(paste(mainDir, "R_gis_exports/wshdGRACE_Fig1/", "TotalImpact", ".tif", sep="")) # Total SES impact

# import removal raster data
GADM_lvl0 <- raster(paste(mainDir, "GADM/", "GADM_level0_0d05", ".tif", sep="")) # GADM national dataset
Lakes <- raster(paste(mainDir, "Lakes/", "Lakes_0d05", ".tif", sep="")) # Lakes 
AntA <- raster(paste(mainDir, "NaturalEarth/", "Antarctica", ".tif", sep="")) # Antarctica
rmRAS <- raster(GridArea)
rmRAS[] <- 0; rmRAS[Lakes == 1 | AntA == 1] <- 1; rmRAS[is.na(GADM_lvl0)] <- 1
TotalImpact[rmRAS == 1] <- NA; AdaptCap[rmRAS == 1] <- NA

# Determine area weighted threshold for total impact (wetting or drying)
TotalImp_ABS <- TotalImpact
TotalImp_ABS[TotalImpact[] < 0] <- TotalImpact[TotalImpact[] < 0]*(-1)
df <- cbind(as.data.frame(TotalImp_ABS), as.data.frame(Pop), as.data.frame(GridArea)) %>% set_colnames(c("TWSImpAbs", "Pop", "GA"))
df <- df[complete.cases(df), ]
TWSImpAbs_high <- weighted.quantile(df$TWSImpAbs, df$Pop, 0.75, na.rm=TRUE) %>% as.numeric()
# p90 = 0.534, p85 = 0.434

# Determine regions with low, moderate, and high AC 
df <- cbind(as.data.frame(AdaptCap), as.data.frame(Pop)) %>% set_colnames(c("AC", "Pop"))
df <- df[complete.cases(df), ]
AC_low <- weighted.quantile(df$AC, df$Pop, 0.25, na.rm=TRUE) %>% as.numeric()
AC_high <- weighted.quantile(df$AC, df$Pop, 0.75, na.rm=TRUE) %>% as.numeric()

# Create categorical summary:
SummaryRAS <- raster(GridArea)
SummaryRAS[] <- 0 # Low TWS impact
SummaryRAS[TotalImp_ABS[] >= TWSImpAbs_high & AdaptCap[] <= AC_low] <- 3 # High TWS impact and low AC
SummaryRAS[TotalImp_ABS[] >= TWSImpAbs_high & AdaptCap[] >  AC_low] <- 2 # High TWS impact and moderate AC
SummaryRAS[TotalImp_ABS[] >= TWSImpAbs_high & AdaptCap[] >= AC_high] <- 1 # High TWS impact and high AC
SummaryRAS[rmRAS[] == 1] <- NA
plot(SummaryRAS)
# writeRaster(SummaryRAS, filename=paste(mainDir, "! GIS_files/R_gis_exports/", "TWSImpact_Summary", ".tif", sep=""),
#             format="GTiff", overwrite=TRUE)

# make map plot
data(World)
e <- extent(c(-180, 180, -60, 88))
#c("#C0C0C0", rev(pal_locuszoom("default")(4)))
my_palette <- c("#C0C0C0", "#EEA236FF", "#8B008B", "#D43F3AFF") #"#46B8DAFF",  

LakesRAS <- raster(paste(mainDir, "Lakes/", "Lakes_0d05", ".tif", sep=""))
Lakes.narm <- LakesRAS; Lakes.narm[Lakes.narm == 0] <- NA

Lakes <- readOGR(dsn=paste(mainDir, "Lakes", sep=""), layer="ne_10m_lakes")
Lakes$scalerank %<>% as.numeric()
Lakes_large <- subset(Lakes, scalerank < 3)

aaa = 0.7
bbb = 0.25

map <- tm_shape(SummaryRAS, projection = "robin") +  tm_raster(style = "cont", palette = my_palette) +
  tm_shape(Lakes.narm) + tm_raster(style = "cat", palette = colorRampPalette(c("grey"))(1), n = 1, colorNA = NULL) +
  tm_shape(World) +  tm_borders(lwd = aaa) +
  tm_shape(Lakes_large) + tm_borders("black", lwd = aaa) +
  tm_style("white", legend.show = F, earth.boundary = c(-180, -60, 180, 88), earth.boundary.color = "white",
           space.color = "white", legend.frame = F, frame = F)

# save map plot
tmap_save(map, "C:/Users/Tom/Desktop/CombinedImpact_hotspots.png", dpi = 500, 
          outer.margins = 0.01, height = 3, units = "in")
