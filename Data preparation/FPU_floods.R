# this script determines the number of flood events per 0.05d grid cell from 1985-2001
# and determines the maximum flood count per FPU

library(raster); library(rgdal); library(sf); library(magrittr)
setwd("Z:/2.active_projects/Xander/! GIS_files/")

#### Part 1: determine flood count per grid cell

# import grid area at 0.05d (for resolution reference) and the DFO flood archive shapefile
GridArea <- raster("./R_gis_exports/WGS84_cellArea_0d05res.tif") # Grid area (km2) for WGS84 projection
DFO_set <- readOGR(dsn="./DartmouthFloodingObservatory", layer="FloodsArchived_shape")

# select only flood events before 2002
DFO_set$Ended %<>% as.Date()
DFO_GRACEclip <- DFO_set[DFO_set$Ended < "2002-01-01",]

# create raster at 0.05 resolutoin counting number of floods on record
DFO_GRACEclip$Counter <- 1
DFO_ras <- raster::rasterize(DFO_GRACEclip, GridArea, field = "Counter", fun = "count")
DFO_ras[is.na(DFO_ras)] <- 0

# write raster
writeRaster(DFO_ras, 
            filename="Z:/2.active_projects/Xander/! GIS_files/DartmouthFloodingObservatory/FloodCount_0d05_1985to2002.tif", 
            format="GTiff", overwrite=TRUE)

#### Part 2: determine maximum flood count per FPU

# set all NA count values to 0
DFO_count <- DFO_ras # create copy as backup
DFO_count[is.na(DFO_count)] <- 0

# import FPU ID raster ('ngb' resampled to 0.05d)
FPU <- raster("./WaterScarcityAtlas/fpu30_hyde_compatible_v20d05.tif")
FPU[FPU < 0] <- NA; FPU[FPU>573] <- NA # set gdalwarp NA fill values to NA

# calculate maximum flood count per FPU
Flood_count_FPU <- raster::zonal(DFO_count, FPU, fun = max, na.rm = T)
Flood_count_FPU %<>% as.data.frame() # convert to dataframe

# note all IDs don't have corresponding FPUs, so set to NA
id_df <- c(seq(0, 573, by = 1)) %>% as.data.frame(); colnames(id_df) = "FPU.id"
id_df$setna <- NA

# merge the dataframes
flood_df <- merge(id_df, Flood_count_FPU, by.x = "FPU.id", by.y = "zone", all.x = T)

# assign flood counts based on FPU id by reclassifying FPU id raster
flood_df$l <- flood_df$FPU.id - 0.5
flood_df$h <- flood_df$FPU.id + 0.5
FPU_assign <- data.frame(low = flood_df$l, high = flood_df$h, ReCLASS = flood_df$value)
FPU_floods <- reclassify(FPU, FPU_assign)
FPU_floods[is.na(GADM_lvl0[])] <- NA

# write new raster
writeRaster(FPU_floods, filename = "./DartmouthFloodingObservatory/FPU_floodcount_GRACEonset.tif",
            format = "GTiff", overwrite = T)
