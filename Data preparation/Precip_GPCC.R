# load GPCC monthly precipitation data, and determine annual precipitation over entire record

library(ncdf4); library(raster); library(magrittr); library(dplyr)
if(!is.null(dev.list())) dev.off(); cat("\014");rm(list=ls())
setwd("Z:/2.active_projects/Xander/! GIS_files")

# read netCDF
GPCC_all <- nc_open("./Precipitation/GPCC/full_data_monthly_v2018_05.nc")

# read lat and lon
lon <- ncvar_get(GPCC_all,"lon")
lat <- ncvar_get(GPCC_all,"lat")

# assign variable to parameter to extract
dname <- "precip"

# get metadeta
dlname <- ncatt_get(GPCC_all, dname,"long_name")
dunits <- ncatt_get(GPCC_all, dname,"units")
fillvalue <- ncatt_get(GPCC_all, dname,"_FillValue")

# extract precipitation data
GPCC_precip <- ncvar_get(GPCC_all, dname)
GPCC_precip[GPCC_precip==fillvalue$value] <- 0 # set fill values to 0
dim(GPCC_precip)

# run loop to calculate annual precipitation for entire record
for(i in 1:(1512/12)){
  j = 1 + ((i-1)*12)
  
  # extract each month in calendar year
  m1 <- raster(t(GPCC_precip[,,j]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  m2 <- raster(t(GPCC_precip[,,j+1]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  m3 <- raster(t(GPCC_precip[,,j+2]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  m4 <- raster(t(GPCC_precip[,,j+3]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  m5 <- raster(t(GPCC_precip[,,j+4]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  m6 <- raster(t(GPCC_precip[,,j+5]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  m7 <- raster(t(GPCC_precip[,,j+6]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  m8 <- raster(t(GPCC_precip[,,j+7]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  m9 <- raster(t(GPCC_precip[,,j+8]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  m10 <- raster(t(GPCC_precip[,,j+9]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  m11 <- raster(t(GPCC_precip[,,j+10]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  m12 <- raster(t(GPCC_precip[,,j+11]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

  # sum month sums to year
  yr <- m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11+m12 
  
  # ensure correct extent
  extent(yr) <- c(-180, 180, -90, 90)
  
  # write each year as .tif file
  writeRaster(yr, paste("./Precipitation/GPCC/AnnualSums/yr", 1890+i, ".tif", sep=""), 
              format = "GTiff",overwrite = T)
}
