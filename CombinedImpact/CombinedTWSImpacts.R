library(raster); library(dplyr); library(magrittr); library(rgdal); library(R.utils)
library(tigris); library(spatstat); library(ggplot2); library(RColorBrewer); library(ggsci);
library(tmap); library(tmaptools); library(reshape2)

mainDir <- "Z:/2.active_projects/Xander/"

# import data
GridArea <- raster(paste(mainDir, "! GIS_files/R_gis_exports/", "WGS84_cellArea_0d05res", ".tif", sep="")) # Grid area (km2) for WGS84 projection
AdaptCap <- raster(paste(mainDir, "! GIS_files/Varis_AdaptiveCapacity/", "AC_2015_0d05", ".tif", sep="")) # VAris et al. AC for 2015 
TWS_trend <- raster(paste(mainDir, "! GIS_files/Rodell_SourceData/", "Rodell_etal_0d05", ".tif", sep="")) # Rodell et al. (2018) TWS trends
Pop <- raster(paste(mainDir, "! GIS_files/R_gis_exports/", "POP_2015_0d05_UNWPP", ".tif", sep="")) # Population data from GWPv4 UNWPP 2015
TotalImpact <- raster(paste(mainDir, "! GIS_files/R_gis_exports/wshdGRACE_Fig1/", "TotalImpact", ".tif", sep="")) # Total SES impact

# import removal raster data
GADM_lvl0 <- raster(paste(mainDir, "! GIS_files/GADM/", "GADM_level0_0d05", ".tif", sep="")) # GADM national dataset
Lakes <- raster(paste(mainDir, "! GIS_files/Lakes/", "Lakes_0d05", ".tif", sep="")) # Lakes 
AntA <- raster(paste(mainDir, "! GIS_files/NaturalEarth/", "Antarctica", ".tif", sep="")) # Antarctica
rmRAS <- raster(GridArea)
rmRAS[] <- 0; rmRAS[Lakes == 1 | AntA == 1] <- 1; rmRAS[is.na(GADM_lvl0)] <- 1
TotalImpact[rmRAS == 1] <- NA; AdaptCap[rmRAS == 1] <- NA

# Determine area weighted threshold for total impact (wetting or drying)
TotalImp_ABS <- TotalImpact
TotalImp_ABS[TotalImpact[] < 0] <- TotalImpact[TotalImpact[] < 0]*(-1)
df <- cbind(as.data.frame(TotalImp_ABS), as.data.frame(Pop)) %>% set_colnames(c("TWSImpAbs", "Pop"))
df <- df[complete.cases(df), ]
TWSImpAbs_pXX <- weighted.quantile(df$TWSImpAbs, df$Pop, 0.75, na.rm=TRUE) %>% as.numeric()

## Determine regions with low AC (pop weighted p25)
# df <- cbind(as.data.frame(AdaptCap), as.data.frame(Pop)) %>% set_colnames(c("AC", "Pop"))
# df <- df[complete.cases(df), ]
# AC_p25 <- weighted.quantile(df$AC, df$Pop, 0.25, na.rm=TRUE) %>% as.numeric()
# AC_p50 <- weighted.quantile(df$AC, df$Pop, 0.50, na.rm=TRUE) %>% as.numeric()
# AC_p75 <- weighted.quantile(df$AC, df$Pop, 0.75, na.rm=TRUE) %>% as.numeric()

# Create categorical summary:
SummaryRAS <- raster(GridArea)
SummaryRAS[] <- 0 # Low TWS impact
SummaryRAS[TotalImp_ABS[] >= TWSImpAbs_pXX & AdaptCap[] <= 0.25] <- 3 # High TWS impact and low AC
SummaryRAS[TotalImp_ABS[] >= TWSImpAbs_pXX & AdaptCap[]  > 0.25] <- 2 # High TWS impact and moderate AC
SummaryRAS[TotalImp_ABS[] >= TWSImpAbs_pXX & AdaptCap[] >= 0.75] <- 1 # High TWS impact and high AC
SummaryRAS[rmRAS[] == 1] <- NA

writeRaster(SummaryRAS, filename=paste(mainDir, "! GIS_files/R_gis_exports/", "TWSImpact_Summary", ".tif", sep=""),
            format="GTiff", overwrite=TRUE)
