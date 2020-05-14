library(raster); library(dplyr); library(TAM); library(magrittr)
if(!is.null(dev.list())) dev.off(); cat("\014");rm(list=ls())
setwd("Z:/2.active_projects/Xander/! GIS_files")

TWS <- raster("./Rodell_SourceData/Rodell_etal_0d05.tif") # Rodell et al. (2018) TWS trends
Pop <- raster("./R_gis_exports/POP_2015_0d05_UNWPP.tif", sep="") # Population data from GWPv4 UNWPP 2015
FEED <- raster("./CropAllocationFoodFeedFuel_Geotiff/Cassidy_gs/FEED_0d05_gs.tif")
FOOD <- raster("./CropAllocationFoodFeedFuel_Geotiff/Cassidy_gs/FOOD_0d05_gs.tif")
NONFOOD <- raster("./CropAllocationFoodFeedFuel_Geotiff/Cassidy_gs/NONFOOD_0d05_gs.tif")
All_kcal <- FEED + FOOD + NONFOOD # sum of all kcal sets
GDP <- raster("./GDP/Kummu/GDP_PPP_2015_0d05.tif") # Kummu et al. (2019) GDP (PPP) dataset for 2015

EQ_regions <- raster("./Earthquakes/EQ_rm_0d05.tif") # earthquake regions to remove

TWS[EQ_regions[] == 1] <- NA
Pop[EQ_regions[] == 1] <- NA
All_kcal[EQ_regions[] == 1] <- NA
GDP[EQ_regions[] == 1] <- NA

df_p <- cbind(as.data.frame(TWS), as.data.frame(Pop)) %>% set_colnames(c("TWS", "Pop"))
df_p <- df_p[complete.cases(df_p), ]
weighted_skewness(x = df_p$TWS, w = df_p$Pop)
weighted_sd(x = df_p$TWS, w = df_p$Pop)
weighted_mean(x = df_p$TWS, w = df_p$Pop)
weighted_quantile(x = df_p$TWS, w = df_p$Pop, probs = 0.5)

df_k <- cbind(as.data.frame(TWS), as.data.frame(All_kcal)) %>% set_colnames(c("TWS", "kcal"))
df_k <- df_k[complete.cases(df_k), ]
weighted_skewness(x = df_k$TWS, w = df_k$kcal)
weighted_sd(x = df_k$TWS, w = df_k$kcal)
weighted_mean(x = df_k$TWS, w = df_k$kcal)
weighted_quantile(x = df_k$TWS, w = df_k$kcal, probs = 0.5)

df_g <- cbind(as.data.frame(TWS), as.data.frame(GDP)) %>% set_colnames(c("TWS", "GDP"))
df_g <- df_g[complete.cases(df_g), ]
weighted_skewness(x = df_g$TWS, w = df_g$GDP)
weighted_sd(x = df_g$TWS, w = df_g$GDP)
weighted_mean(x = df_g$TWS, w = df_g$GDP)
weighted_quantile(x = df_g$TWS, w = df_g$GDP, probs = 0.5)
