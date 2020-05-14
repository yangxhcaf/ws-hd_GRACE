# this script joins the .csv water shortage results for 2001-2010 (from Kummu et al, and the Water Scarcity Atlas)
# to their FPU identifier raster

library(raster); library(rgdal); library(gdalUtils); library(dplyr); library(magrittr); library(tmaptools)
rm(list = ls()); setwd("Z:/2.active_projects/Xander/! GIS_files")

# load Kummu et al. modified FPU raster at 0.5*
FPU <- raster("./WaterScarcityAtlas/fpu30_hyde_compatible_v2.tif")

# load Kummu et al. water shortage .csv dataset and select the decade 2001-2010
Shortage_df <- read.csv("./WaterScarcityAtlas/watch-watergap-nat-decadal-2020-04-27T14_40_51.066Z.csv")
Shortage_df.s <- Shortage_df %>% filter(ï..Time == "2001-2010") %>% select(RegionID, Shortage)

# note all FPU ID's aren't assigned Shortage values (548 FPUs, but IDs from 1-573)... so assign NA to these numbers
id_df <- c(seq(0, 573, by = 1)) %>% as.data.frame(); colnames(id_df) = "FPU.id"
id_df$setna <- NA
Shortage_df.sm <- merge(id_df, Shortage_df.s, by.x = "FPU.id", by.y = "RegionID", all.x = T)

# Reclassify the FPU raster data to their water shortage estimate by their FPU id
Shortage_df.sm$l <- Shortage_df.sm$FPU.id - 0.5
Shortage_df.sm$h <- Shortage_df.sm$FPU.id + 0.5
FPU_assign <- data.frame(low = Shortage_df.sm$l, high = Shortage_df.sm$h, ReCLASS = Shortage_df.sm$Shortage)
FPU_shortage <- reclassify(FPU, FPU_assign)

# write original data
writeRaster(FPU_shortage, filename = "./WaterScarcityAtlas/Shortage_2001t2010_Kummu.tif",
            format = "GTiff", overwrite = T)

# resample original data to 0.05 resolution
gdalwarp(srcfile = "./WaterScarcityAtlas/Shortage_2001t2010_Kummu.tif",
         dstfile = "./WaterScarcityAtlas/Shortage_2001t2010_Kummu_0d05.tif",
         t_srs = "+proj=longlat +datum=WGS84 +no_defs",
         te = c(-180, -90, 180, 90), 
         tr = c(0.05, 0.05),
         r = "near",
         output_Raster = TRUE,
         overwrite = TRUE,
         verbose = TRUE)
