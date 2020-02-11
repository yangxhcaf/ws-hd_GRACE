library(raster); library(ncdf4)

#############################
## Varis Adaptive Capacity ##
#############################

# set path and filename
ncpath <- "Z:/2.active_projects/Xander/! GIS_files/Varis_AdaptiveCapacity/"
ncname <- "adaptive_capacity"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "adaptive_capacity"  
rasbrick <- brick(ncfname)
AC_2015 <- raster::subset(rasbrick, "X2015", value = T)

# load existing raster with appropriate resolution and extent
GridArea <- raster(paste(mainDir, "! GIS_files/R_gis_exports/", "WGS84_cellArea_0d05res", ".tif", sep="")) # Grid area (km2) for WGS84 projection
s <- raster(GridArea)
AC_2015_0d05 <- raster::resample(AC_2015, s, method = "bilinear")

writeRaster(AC_2015_0d05, filename=paste(ncpath, "AC_2015_0d05", ".tif", sep=""),
            format="GTiff", overwrite=TRUE)

##########################
## Kummu GDP per capita ##
##########################

# set path and filename
ncpath <- "Z:/2.active_projects/Xander/! GIS_files/GDP/Kummu/"
ncname <- "GDP_per_capita_PPP_1990_2015_v2"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "GDP_per_capita_PPP"  
rasbrick <- brick(ncfname)
GDPpc_2015 <- raster::subset(rasbrick, "X2015", value = T)

# load existing raster with appropriate resolution and extent
mainDir <- "Z:/2.active_projects/Xander/"
GridArea <- raster(paste(mainDir, "! GIS_files/R_gis_exports/", "WGS84_cellArea_0d05res", ".tif", sep="")) # Grid area (km2) for WGS84 projection
s <- raster(GridArea)
GDPpc_2015_0d05 <- raster::resample(GDPpc_2015, s, method = "ngb")

writeRaster(GDPpc_2015_0d05, filename=paste(ncpath, "GDPpc_2015_0d05", ".tif", sep=""),
            format="GTiff", overwrite=TRUE)
