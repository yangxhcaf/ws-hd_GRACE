library(raster)
library(dplyr)
library(magrittr)

# this script creates a raster to correct for pixel area differences at 1 degree resolution

a <- 6378137.0 # major axis radius
b <- 6356752.3142 # minor axis radius
e2 <- ((a^2)-(b^2))/(a^2)
pi <- 3.14159265358979

LatDegrees <- c(seq(89.975, -89.975, by = -0.05)) %>% as.data.frame()
colnames(LatDegrees) <- "LAT"

AreaCorrection <- LatDegrees
AreaCorrection$LAT__rad <- pi*AreaCorrection$LAT/180
AreaCorrection$Width <- (0.05)*(pi*a*cos(AreaCorrection$LAT__rad))/(180*sqrt(1-(e2*(sin(AreaCorrection$LAT__rad)^2)))) # to convert from degree to 20th of degree
AreaCorrection$WidthKM <- AreaCorrection$Width/1e3
AreaCorrection$Height <- (0.05)*(pi*a*(1-e2))/(180*(1-(e2*sin(AreaCorrection$LAT__rad)^2))^(1.5))# to convert from degree to 20th of degree
AreaCorrection$HeightKM <- AreaCorrection$Height/1e3

AreaCorrection$Area__km2 <- AreaCorrection$HeightKM * AreaCorrection$WidthKM

WGS84area <- as.vector(AreaCorrection$Area__km2)

WGS84area.matrix <- matrix(WGS84area, nrow = 3600, ncol = 7200, byrow = FALSE, dimnames = NULL)

WGS84area_ras <- raster(WGS84area.matrix)
extent(WGS84area_ras) <- c(-180, 180, -90, 90)
crs(WGS84area_ras) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

writeRaster(WGS84area_ras, 
            filename="Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/WGS84_cellArea_0d05res.tif", 
            format="GTiff", overwrite=TRUE)