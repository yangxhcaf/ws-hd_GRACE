library(raster); library(ncdf4)

# set path and filename
ncpath <- "Z:/2.active_projects/Xander/! GIS_files/Varis_AdaptiveCapacity/"
ncname <- "adaptive_capacity"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "adaptive_capacity"  # note: tmp means temperature (not temporary)
rasbrick <- brick(ncfname)
AC_2015 <- raster::subset(rasbrick, "X2015", value = T)

# load existing raster with appropriate resolution and extent
GridArea <- raster(paste(mainDir, "! GIS_files/R_gis_exports/", "WGS84_cellArea_0d05res", ".tif", sep="")) # Grid area (km2) for WGS84 projection
s <- raster(GridArea)
AC_2015_0d05 <- raster::resample(AC_2015, s, method = "bilinear")

writeRaster(AC_2015_0d05, filename=paste(ncpath, "AC_2015_0d05", ".tif", sep=""),
            format="GTiff", overwrite=TRUE)
