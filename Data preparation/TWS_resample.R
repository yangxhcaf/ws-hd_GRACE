# this script converts the Rodell et al. (2018) TWS trends provided at 
# 0.5* resolution in .csv format into a raster and resamples to 0.05* resolution

library(raster); library(dplyr); library(magrittr); library(tmap); library(tmaptools)
setwd("Z:/2.active_projects/Xander/! GIS_files/")

# import data
Rodell_csv <- read.csv("./Rodell_SourceData/41586_2018_123_MOESM1_ESM.csv", header = F) # downloaded source data

# convert to matrix while inverting rows (source data is vertically inverted)
Rodell_asGrid <- Rodell_csv %>% as.data.frame() %>% arrange(-row_number()) %>% as.matrix() 

#  convert to raster
Rodell_asRas <- raster(Rodell_asGrid) 
extent(Rodell_asRas) <- c(-180, 180, -90, 90)
crs(Rodell_asRas) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# resample to 0.05degree resolution
DummyRaster <- raster(Rodell_asRas) # initialize
res(DummyRaster) <- 0.05
Rodell_asRas_0d05 <- resample(Rodell_asRas, DummyRaster, method = "ngb") # use nearest neighbour, therefore no data manipulation

writeRaster(Rodell_asRas_0d05, 
            filename="Z:/2.active_projects/Xander/! GIS_files/Rodell_SourceData/Rodell_etal_0d05.tif", 
            format="GTiff", overwrite=TRUE)
