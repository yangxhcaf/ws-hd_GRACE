library(raster); library(dplyr); library(magrittr)

# this script creates a raster to correct for pixel area differences at specified degree resolution
a <- 6378137.0  # Semi-major Axis 
b <- 6356752.3142 # Semi-minor Axis 
e2 <- 0.00669437999014 # First Eccentricity Squared 
pi <- 3.141592653589793238462643383 

# state desired resolution below
RES <- 0.5

# LatDegrees <- c(seq(89.75, -89.75, by = -0.5)) %>% as.data.frame()
LatDegrees <- c(seq(90-(RES/2), -90+(RES/2), by = -RES)) %>% as.data.frame()
colnames(LatDegrees) <- "LAT"

WGSgridAreas <- LatDegrees
WGSgridAreas$LAT_in_rad <- pi*WGSgridAreas$LAT/180
WGSgridAreas$dLONG_m <- (RES)*((pi*a*cos(WGSgridAreas$LAT_in_rad))/(180*sqrt(1-(e2*(sin(WGSgridAreas$LAT_in_rad)^2)))))
WGSgridAreas$dLONG_km <- WGSgridAreas$dLONG_m/1e3
WGSgridAreas$dLAT_m <- (RES)*((pi*a*(1-e2))/(180*((1-(e2*sin(WGSgridAreas$LAT_in_rad)^2))^(1.5))))
WGSgridAreas$dLAT_km <- WGSgridAreas$dLAT_m/1e3
WGSgridAreas$Area_km2 <- WGSgridAreas$dLAT_km * WGSgridAreas$dLONG_km

WGS84area <- as.vector(WGSgridAreas$Area_km2)
WGS84area.matrix <- matrix(WGS84area, nrow = 180*(1/RES), ncol = 360*(1/RES), byrow = FALSE, dimnames = NULL)
WGS84area_ras <- raster(WGS84area.matrix)
extent(WGS84area_ras) <- c(-180, 180, -90, 90)
crs(WGS84area_ras) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

sum(WGS84area_ras[])

# Write raster - manually change filename
writeRaster(WGS84area_ras, 
            filename="Z:/2.active_projects/Xander/! GIS_files/R_gis_exports/WGS84_cellArea_0d5res.tif", 
            format="GTiff", overwrite=TRUE)
