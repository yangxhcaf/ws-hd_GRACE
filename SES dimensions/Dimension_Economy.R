library(ggplot2); library(raster); library(dplyr); library(magrittr) 
library(reshape2); library(spatstat); library(e1071); library(rgdal); library(gdalUtils)
library(tmap); library(tmaptools)
if(!is.null(dev.list())) dev.off(); cat("\014");rm(list=ls())
setwd("Z:/2.active_projects/Xander/! GIS_files")

# import all data
TWS_trend <- raster("./Rodell_SourceData/Rodell_etal_0d05.tif") # Rodell et al. (2018) TWS trends, resampled to 0.05 res
GDP_ppp <- raster("./GDP/Kummu/GDP_PPP_2015_0d05.tif") # Kummu et al. (2019) GDP (PPP) dataset for 2015
GDPpc <- raster("./GDP/Kummu/GDPpc_2015_0d05.tif") # Kummu et al. (2019) GDP (PPP) dataset for 2015
Pop <- raster("./R_gis_exports/POP_2015_0d05_UNWPP.tif") # GWPv4 UNWPP 2015 population dataset 
EQ_regions <- raster("./Earthquakes/EQ_rm_0d05.tif") # earthquake regions to remove

# Remove EQ regions from input data
Pop[EQ_regions[] == 1] <- NA
TWS_trend[EQ_regions[] == 1] <- NA
GDP_ppp[EQ_regions[] == 1] <- NA
GDPpc[EQ_regions[] == 1] <- NA

# classify GDPpc based on IMF default classes
ReclassRanges <- data.frame(low = c(0, 1000, 2500, 10000, 25000), 
                            high = c(1000, 2500, 10000, 25000, 1e12), 
                            ReCLASS = seq(1:5))
GDPpc_class <- reclassify(GDPpc, ReclassRanges)

# split GDP_PPP raster by IMF GDP_pc classes
# begin by initializing a raster for each class with same extent and resolution as rest of files
GDP.verylow  <- raster(TWS_trend); GDP.verylow[]  <- 0
GDP.low      <- raster(TWS_trend); GDP.low[]      <- 0
GDP.medium   <- raster(TWS_trend); GDP.medium[]   <- 0
GDP.high     <- raster(TWS_trend); GDP.high[]     <- 0
GDP.veryhigh <- raster(TWS_trend); GDP.veryhigh[] <- 0
GDP.NoDat    <- raster(TWS_trend); GDP.NoDat[]    <- 0

# populate population rasters per water stress class
GDP.verylow[GDPpc_class == 1]  <- GDP_ppp[GDPpc_class == 1]
GDP.low[GDPpc_class == 2]      <- GDP_ppp[GDPpc_class == 2]
GDP.medium[GDPpc_class == 3]   <- GDP_ppp[GDPpc_class == 3]
GDP.high[GDPpc_class == 4]     <- GDP_ppp[GDPpc_class == 4]
GDP.veryhigh[GDPpc_class == 5] <- GDP_ppp[GDPpc_class == 5]

# Reclassify TWS trend into 0.1 cm/yr increment bins
TWS_classes <- data.frame(low = seq(-40, 6.0, by = 0.1), 
                          high = seq(-39.9, 6.1, by = 0.1), 
                          ReCLASS = seq(-39.95, 6.05, by = 0.1))
TWS_binned <- raster::reclassify(TWS_trend, TWS_classes)

# Calculate GDP distribution for each water stress class
GDP.verylow.distr  <- zonal(GDP.verylow, TWS_binned, sum, na.rm = T, digits = 4)      %>% as.data.frame() %>% set_colnames(c("TWS_mv", "VeryLow"))
GDP.low.distr      <- zonal(GDP.low, TWS_binned, sum, na.rm = T, digits = 4)   %>% as.data.frame() %>% set_colnames(c("TWS_mv", "Low"))
GDP.medium.distr   <- zonal(GDP.medium, TWS_binned, sum, na.rm = T, digits = 4)  %>% as.data.frame() %>% set_colnames(c("TWS_mv", "Medium"))
GDP.high.distr     <- zonal(GDP.high, TWS_binned, sum, na.rm = T, digits = 4)     %>% as.data.frame() %>% set_colnames(c("TWS_mv", "High"))
GDP.veryhigh.distr <- zonal(GDP.veryhigh, TWS_binned, sum, na.rm = T, digits = 4)   %>% as.data.frame() %>% set_colnames(c("TWS_mv", "VeryHigh"))
GDP.NoDat.distr    <- zonal(GDP.NoDat, TWS_binned, sum, na.rm = T, digits = 4)     %>% as.data.frame() %>% set_colnames(c("TWS_mv", "NoData"))

# merge all results, and melt dataframe for plotting
GDP.distr <- Reduce(function(x, y) merge(x, y, by = "TWS_mv"), list(GDP.verylow.distr, GDP.low.distr, GDP.medium.distr ,
                                                                    GDP.high.distr, GDP.veryhigh.distr, GDP.NoDat.distr))
GDP.distr %<>% melt(id.var = "TWS_mv")

# plotting variables
a <- 0.85
b <- 9.5*1e12

fig <- ggplot(GDP.distr, aes(x = TWS_mv, y = value, fill = variable)) +
  annotate("rect", xmin = -5,   xmax = -2,   ymin = 0, ymax = b, fill= "#FFA5A5", alpha = a) +
  annotate("rect", xmin = -2,   xmax = -0.5, ymin = 0, ymax = b, fill= "#FFDFDF", alpha = a) +
  annotate("rect", xmin = -0.5, xmax = 0.5,  ymin = 0, ymax = b, fill= "#959595", alpha = a) +
  annotate("rect", xmin = 0.5,  xmax = 2,    ymin = 0, ymax = b, fill= "#F0EDF8", alpha = a) +
  annotate("rect", xmin = 2,    xmax = 3,    ymin = 0, ymax = b, fill= "#AAC2FF", alpha = a) +
  geom_bar(stat = "identity", position="stack", colour = "black") +
  scale_fill_manual(values = c("#bc2d18", "#d66a22", "#f2bc8f", "#9cd1bb", "#46a7af", "#d5d7da")) +
  scale_y_continuous(position = "right", breaks = seq(0, 1e13, by = 1e12), expand = c(0,0)) +
  scale_x_continuous(limits = c(-5, 3), breaks = seq(-5, 3, by = 1), expand = c(0,0),  
                     labels = c("-5", "-4", "-3", "-2", "-1", "0", "1", "2", "3")) +
  theme(panel.background =  element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_line(alpha(colour = "white", 0.3), linetype = "dashed", size = 0.5),
        axis.line = element_line(colour = "black"), panel.ontop = TRUE,
        legend.position = "none") +
  geom_vline(xintercept = 0, size = 1.5) +
  labs(x=expression(paste("Terrestrial Water Storage Rate of Change","  ","(cm", y^-1,")", sep="")),
       y = "GDP\n") +
  geom_vline(xintercept = 0, size = 1.5, alpha = 0.8) 
fig

ggsave("C:/Users/Tom/Desktop/UPDATE_economy.png", fig, dpi = 500, width = 14, height = 10, bg = "transparent") 

##################
## cdf analysis ##
##################

# calculate population, ag, and  GDP per bin and merge into one dataframe
FEED <- raster("./CropAllocationFoodFeedFuel_Geotiff/Cassidy_gs/FEED_0d05_gs.tif")
FOOD <- raster("./CropAllocationFoodFeedFuel_Geotiff/Cassidy_gs/FOOD_0d05_gs.tif")
NONFOOD <- raster("./CropAllocationFoodFeedFuel_Geotiff/Cassidy_gs/NONFOOD_0d05_gs.tif")
All_kcal <- FEED + FOOD + NONFOOD # sum of all kcal sets

GDP.per.TWSbin <- raster::zonal(GDP_ppp, TWS_binned, fun = "sum", digits = 4, na.rm = T) %>% as.data.frame() %>% set_colnames(c("TWS_mv", "GDP"))
POP.per.TWSbin <- raster::zonal(Pop, TWS_binned, fun = "sum", digits = 4, na.rm = T) %>% as.data.frame() %>% set_colnames(c("TWS_mv", "POP"))
AG.per.TWSbin <- raster::zonal(All_kcal, TWS_binned, fun = "sum", digits = 4, na.rm = T) %>% as.data.frame() %>% set_colnames(c("TWS_mv", "AG"))
Combined_df <- merge.default(GDP.per.TWSbin, POP.per.TWSbin, by = "TWS_mv")
Combined_df <- merge.default(Combined_df, AG.per.TWSbin, by = "TWS_mv")

# cumulatively sum each distribution and normalize with maximum value
Combined_df$GDP_runningSum <- cumsum(Combined_df$GDP)
Combined_df$GDP_cdf <- Combined_df$GDP_runningSum/max(Combined_df$GDP_runningSum)

Combined_df$POP_runningSum <- cumsum(Combined_df$POP)
Combined_df$POP_cdf <- Combined_df$POP_runningSum/max(Combined_df$POP_runningSum)

Combined_df$AG_runningSum <- cumsum(Combined_df$AG)
Combined_df$AG_cdf <- Combined_df$AG_runningSum/max(Combined_df$AG_runningSum)

# plot the CDFs together, highlighting gaps between curves 
cdf <- ggplot(Combined_df, aes(x= TWS_mv)) +
  geom_line(aes(y = POP_cdf), colour = "red", lwd = 0.75) + 
  geom_line(aes(y = GDP_cdf), colour = "blue", lwd = 0.75) +
  geom_line(aes(y = AG_cdf), colour = "palegreen3", lwd = 0.75) +
  scale_y_continuous(position = "right", breaks = seq(0, 1, by = 0.1), expand = c(0,0)) +
  scale_x_continuous(limits = c(-5, 3), breaks = seq(-5, 3, by = 1), expand = c(0,0),  
                     labels = c("-5", "-4", "-3", "-2", "-1", "0", "1", "2", "3")) +
  theme(legend.position = "none",
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        panel.background =  element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks = element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) 
cdf

ggsave("C:/Users/Tom/Desktop/UPDATE_cdfs.png", 
       cdf, dpi = 500, width = 3.615, height = 1.815, units = "in", bg = "transparent")

######################
## Accompanying map ##
######################
                    
# import necessary data, and is faster if clearing environment before doing so
rm(list = ls())
GridArea <- raster("./R_gis_exports/WGS84_cellArea_0d05res.tif") # Grid area estimate for WGS84 reference elipsoid
TWS_trend <- raster("./Rodell_SourceData/Rodell_etal_0d05.tif") # Rodell et al. (2018) TWS trends, resampled to 0.05 degrees
GDP_ppp <- raster("./GDP/Kummu/GDP_PPP_2015_0d05.tif") # Kummu et al. (2019) GDP (PPP) dataset for 2015
GDPpc <- raster("./GDP/Kummu/GDPpc_2015_0d05.tif") # Kummu et al. (2019) GDP (PPP) dataset for 2015
EQ_regions <- raster("./Earthquakes/EQ_rm_0d05.tif") # earthquake regions to remove
GADM_lvl0 <- raster("./GADM/GADM_level0_0d05.tif") # GADM national dataset

# Remove EQ regions, lakes, and oceans from input data
GDP_ppp[EQ_regions[] == 1 | is.na(GADM_lvl0[])] <- NA
GDPpc[EQ_regions[] == 1 | is.na(GADM_lvl0[])] <- NA
GridArea[EQ_regions[] == 1 | is.na(GADM_lvl0[])] <- NA
TWS_trend[EQ_regions[] == 1 | is.na(GADM_lvl0[])] <- NA

# Scale TWS trend dataset to normalized range of [-1, 1]
TWS_scaler <- TWS_trend/2; TWS_scaler[TWS_scaler > 1] <- 1; TWS_scaler[TWS_scaler < -1] <- -1

# create function to reclassify a raster into weighted percentiles
PercentileAssignment <- function(PercentileRaster, RawRaster, WeightRaster){
  PercentileRaster <- raster(RawRaster)
  PercentileRaster[] <- 0
  Ref.DF <- cbind(as.data.frame(RawRaster), as.data.frame(WeightRaster)) %>%  set_colnames(c("RAW", "WEIGHT"))
  pb <- txtProgressBar(min = 0, max = 99, style = 3)
  
  for(i in 0:99){
    j = 1 - (i*0.01)
    k = 0.99 - (i*0.01)
    PercentileRaster[RawRaster <= as.numeric(unname(weighted.quantile(Ref.DF[,1], Ref.DF[,2], j, na.rm = TRUE))) & 
                       RawRaster > as.numeric(unname(weighted.quantile(Ref.DF[,1], Ref.DF[,2], k, na.rm = TRUE)))] <- j
    setTxtProgressBar(pb, i)
  }
  return(PercentileRaster)
}

#####################
## Fig1c - Economy ##
#####################
GDPDens <- GDP_ppp/GridArea # Determines population per km2, using grid areas for WGS84 reference ellipsoid
GDPDens_percentile <- PercentileAssignment(GDPDens_percentile, GDPDens, GridArea) # determines area-weighted percentile distribution of population density
GDPDens_percentile[rmRAS[] == 1 | EQ_regions_RAS[] == 1] <- NA # reset NA regions to NA (instead of 0)

# Scale by TWS trend
GDP_map <- GDPDens_percentile*TWS_scaler

# make map plot
data(World)
Lakes_ply <- readOGR(dsn=paste("./Lakes", sep=""), layer="ne_10m_lakes")
Lakes_ply$scalerank %<>% as.numeric()
Lakes_large <- subset(Lakes_ply, scalerank < 4)

map <- 
  tm_shape(GDP_map, projection = "robin") +  tm_raster(style = "cont", palette = "RdBu", midpoint = 0) +
  tm_shape(World) +  tm_borders(lwd = 0.7) +
  tm_shape(Lakes_large) + tm_borders("black", lwd = 0.5) +
  tm_style("white", legend.show = F, earth.boundary = c(-180, -60, 180, 88), earth.boundary.color = "white",
           space.color = "white", legend.frame = F, frame = F)
map

# save map plot
tmap_save(map, "C:/Users/Tom/Desktop/FinalRun/Map_GDPppp.png", dpi = 500, 
          outer.margins = 0.01, height = 3, units = "in")

writeRaster(GDP_map, "C:/Users/Tom/Desktop/FinalRun/GTiff_economicimpact",
            format="GTiff", overwrite=TRUE)


########################################
## Supplementary Fig - GDP per capita ##
########################################
GDP_scale <- raster(TWS_trend)
# set thresholds
LOW <- 1000 # i.e. limit on very low
LOW2MED <- 2500 # i.e. limit on low 
MED2HIGH <- 10000 # i.e. limit on medium
HIGH <- 25000 # limit on high, anything higher is very high

GDP_scale[GDPpc <= LOW] <- 0 + ((GDPpc[GDPpc <= LOW] - min(GDPpc[], na.rm = T)) / (LOW - min(GDPpc[], na.rm = T)))
GDP_scale[GDPpc > LOW  & GDPpc <= LOW2MED] <- 1 + ((GDPpc[GDPpc > LOW  & GDPpc <= LOW2MED] - LOW) / (LOW2MED - LOW))
GDP_scale[GDPpc > LOW2MED  & GDPpc <= MED2HIGH] <- 2 + ((GDPpc[GDPpc > LOW2MED & GDPpc <= MED2HIGH] - LOW2MED) / (MED2HIGH - LOW2MED))
GDP_scale[GDPpc > MED2HIGH & GDPpc <= HIGH] <- 3 + ((GDPpc[GDPpc > MED2HIGH & GDPpc <= HIGH] - MED2HIGH) / (HIGH - MED2HIGH))
GDP_scale[GDPpc > HIGH] <- 4 + ((GDPpc[GDPpc > HIGH] - HIGH) / (max(GDPpc[], na.rm = T) - HIGH))
GDP_scale <- 5 - GDP_scale; GDP_scale <- GDP_scale/5
plot(GDP_scale)

GDP_scale[rmRAS == 1] <- NA

# Scale by TWS trend
GDP_percapita_map <- GDP_scale*TWS_scaler

# plot map 
data(World)
Lakes_ply <- readOGR(dsn=paste("./Lakes", sep=""), layer="ne_10m_lakes")
Lakes_ply$scalerank %<>% as.numeric()
Lakes_large <- subset(Lakes_ply, scalerank < 4)

# remove Greenland for plotting
Grl <- raster("./NaturalEarth/Greenland.tif")
Vuln_SCARCITY_m[Grl[] == 1] <- NA; Vuln_FLOOD_m[Grl[] == 1] <- NA; cmb.m[Grl[] == 1] <- NA

map <- 
  tm_shape(World, projection = "robin") +  tm_polygons(fill = "#c0c0c0") +
  tm_shape(GDP_percapita_map, projection = "robin") +  tm_raster(style = "cont", palette = "RdBu", midpoint = 0) +
  # tm_shape(WorldRegions) +   tm_raster(style = "cat", palette = "Set3") +
  tm_shape(World) +  tm_borders(lwd = 0.7) +
  tm_shape(Lakes_large) + tm_borders("black", lwd = 0.5) +
  tm_style("white", legend.show = F, earth.boundary = c(-180, -60, 180, 88), earth.boundary.color = "white",
           space.color = "white", legend.frame = F, frame = F)
map
                    
# save map plot
tmap_save(map, "C:/Users/Tom/Desktop/GDP_per_capita.png", dpi = 500, 
          outer.margins = 0.01, height = 3, units = "in")
