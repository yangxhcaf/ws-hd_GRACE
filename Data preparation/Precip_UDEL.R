# load GPCC monthly precipitation data, and determine annual precipitation over entire record

library(ncdf4); library(raster); library(magrittr); library(dplyr)
if(!is.null(dev.list())) dev.off(); cat("\014");rm(list=ls())
setwd("Z:/2.active_projects/Xander/! GIS_files")

# read netCDF
UDEL_all <- nc_open("./Precipitation/UDEL/precip.mon.total.v401_z.nc")

# read lat and lon
lon <- ncvar_get(UDEL_all,"lon")
lat <- ncvar_get(UDEL_all,"lat")

# assign parameter to extract
dname <- "precip"

# get metadata
dlname <- ncatt_get(UDEL_all, dname,"long_name")
dunits <- ncatt_get(UDEL_all, dname,"units")
fillvalue <- ncatt_get(UDEL_all, dname,"_FillValue")

# extract precipitation
UDEL_get <- ncvar_get(UDEL_all, dname)
UDEL_all[UDEL_get==fillvalue$value] <- 0 # assign fill values to 0
dim(UDEL_get)

for(i in 1:(1380/12)){
  j = ((i-1)*12)+1
  
  # extract each month's total precipitation in each calendar year
  m1 <- raster(t(UDEL_get[,,j]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  m2 <- raster(t(UDEL_get[,,j+1]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  m3 <- raster(t(UDEL_get[,,j+2]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  m4 <- raster(t(UDEL_get[,,j+3]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  m5 <- raster(t(UDEL_get[,,j+4]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  m6 <- raster(t(UDEL_get[,,j+5]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  m7 <- raster(t(UDEL_get[,,j+6]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  m8 <- raster(t(UDEL_get[,,j+7]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  m9 <- raster(t(UDEL_get[,,j+8]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  m10 <- raster(t(UDEL_get[,,j+9]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  m11 <- raster(t(UDEL_get[,,j+10]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  m12 <- raster(t(UDEL_get[,,j+11]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  
  # sum month totals in each year
  yr <- m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11+m12
  
  # ensure correct extent
  extent(yr) <- c(-180, 180, -90, 90)
  
  # UDEL CS is 0-360, not -180-180, thus needs to be re-arranged
  Arrange <- raster(yr)
  Arrange[,1:360] <- yr[,361:720]
  Arrange[,361:720] <- yr[,1:360]
  Arrange <- Arrange*10  # convert from cm to mm
  
  # write each year to .tif
  writeRaster(Arrange, paste("./Precipitation/UDEL/AnnualSums/yr", 1899+i, ".tif", sep=""), 
              format = "GTiff",overwrite = T)
}
