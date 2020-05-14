library(raster); library(dplyr); library(magrittr); library(spatstat)
if(!is.null(dev.list())) dev.off(); cat("\014");rm(list=ls())
setwd("Z:/2.active_projects/Xander/! GIS_files")

# import all data
TWS_trend <- raster("./Rodell_SourceData/Rodell_etal_0d05.tif") # Rodell et al. (2018) TWS trends, resampled to 0.05 res
GridArea <- raster("./R_gis_exports/WGS84_cellArea_0d05res.tif") # Grid area estimate for WGS84 reference elipsoid
FEED <- raster("./CropAllocationFoodFeedFuel_Geotiff/Cassidy_gs/FEED_0d05_gs.tif")
FOOD <- raster("./CropAllocationFoodFeedFuel_Geotiff/Cassidy_gs/FOOD_0d05_gs.tif")
NONFOOD <- raster("./CropAllocationFoodFeedFuel_Geotiff/Cassidy_gs/NONFOOD_0d05_gs.tif")
All_kcal <- FEED + FOOD + NONFOOD # sum of all kcal sets
All_kcal_r <- All_kcal
WaterShortage <- raster("./WaterScarcityAtlas/Shortage_2001t2010_Kummu_0d05.tif", sep="") # Kummu et al water crowding data
EQ_regions <- raster("./Earthquakes/EQ_rm_0d05.tif") # earthquake regions to remove

# Remove EQ regions from input data
TWS_trend[EQ_regions[] == 1] <- NA
FEED[EQ_regions[] == 1] <- NA
FOOD[EQ_regions[] == 1] <- NA
NONFOOD[EQ_regions[] == 1] <- NA
All_kcal[EQ_regions[] == 1] <- NA
WaterShortage[EQ_regions[] == 1] <- NA

# Set NAs to -99 for zonal statistics
WaterShortage_id <- raster(TWS_trend)
WaterShortage_id[is.na(WaterShortage[])] <- -1 # index for no data
WaterShortage_id[WaterShortage <= 1e6/2000] <- 5 # Falkenmark stress level 5
WaterShortage_id[WaterShortage > 1e6/2000 & WaterShortage <= 1e6/1000] <- 4 # Falkenmark stress level 4
WaterShortage_id[WaterShortage > 1e6/1000 & WaterShortage <= 1e6/600] <- 3 # Falkenmark stress level 3
WaterShortage_id[WaterShortage > 1e6/600 & WaterShortage <= 1e6/100] <- 2 # Falkenmark stress level 2
WaterShortage_id[WaterShortage > 1e6/100] <- 1 # Falkenmark stress level 1

TWS_trend_id <- raster(TWS_trend)
TWS_trend_id[TWS_trend <= -2] <- 1
TWS_trend_id[TWS_trend >-2 & TWS_trend <= -0.5] <- 2
TWS_trend_id[TWS_trend >-0.5 & TWS_trend < 0.5] <- 3
TWS_trend_id[TWS_trend >= 0.5 & TWS_trend < 2] <- 4
TWS_trend_id[TWS_trend >= 2] <- 5

df <- cbind(as.data.frame(TWS_trend_id), as.data.frame(FEED), as.data.frame(FOOD),
            as.data.frame(NONFOOD), as.data.frame(WaterShortage_id)) %>% 
  set_colnames(c("TWS.id", "FEED", "FOOD", "NONFOOD", "Shortage.id"))

df$TWS.id %<>% as.factor()
df$Shortage.id %<>% as.factor()
df_feed <- df[complete.cases(df$FEED), ]
df_food <- df[complete.cases(df$FOOD), ]
df_nonfd <- df[complete.cases(df$NONFOOD), ]

# summary table per TWS trend class (5 classes) and water stress class (7 classes) = 35 population counts (in millions)
FOOD_summary <- df_food %>% group_by(TWS.id, Shortage.id) %>% 
  summarise(FOOD_trillion = sum(FOOD, na.rm = T)/1e12) %>% as.data.frame()

FEED_summary <- df_feed %>% group_by(TWS.id, Shortage.id) %>% 
  summarise(FEED_trillion = sum(FEED, na.rm = T)/1e12) %>% as.data.frame()

NONFOOD_summary <- df_nonfd %>% group_by(TWS.id, Shortage.id) %>% 
  summarise(NONFOOD_trillion = sum(NONFOOD, na.rm = T)/1e12) %>% as.data.frame()

write.csv(FOOD_summary,"C:/Users/Tom/Desktop/FOOD_summary.csv", 
          row.names = FALSE)
write.csv(FEED_summary,"C:/Users/Tom/Desktop/FEED_summary.csv", 
          row.names = FALSE)
write.csv(NONFOOD_summary,"C:/Users/Tom/Desktop/NONFOOD_summary.csv", 
          row.names = FALSE)
