library(ggplot2); library(raster); library(dplyr); library(magrittr) 
library(reshape2); library(spatstat); library(e1071); library(rgdal); library(gdalUtils)
library(tmap); library(tmaptools)
if(!is.null(dev.list())) dev.off(); cat("\014");rm(list=ls())
setwd("Z:/2.active_projects/Xander/! GIS_files")

# import all data
TWS_trend <- raster("./Rodell_SourceData/Rodell_etal_0d05.tif") # Rodell et al. (2018) TWS trends, resampled to 0.05 res
GridArea <- raster("./R_gis_exports/WGS84_cellArea_0d05res.tif") # Grid area estimate for WGS84 reference elipsoid
FEED <- raster("./CropAllocationFoodFeedFuel_Geotiff/Cassidy_gs/FEED_0d05_gs.tif")
FOOD <- raster("./CropAllocationFoodFeedFuel_Geotiff/Cassidy_gs/FOOD_0d05_gs.tif")
NONFOOD <- raster("./CropAllocationFoodFeedFuel_Geotiff/Cassidy_gs/NONFOOD_0d05_gs.tif")
All_kcal <- FEED + FOOD + NONFOOD # sum of all kcal sets
EQ_regions <- raster("./Earthquakes/EQ_rm_0d05.tif") # earthquake regions to remove

# Remove EQ regions from input data
TWS_trend[EQ_regions[] == 1] <- NA; FEED[EQ_regions[] == 1] <- NA; FOOD[EQ_regions[] == 1] <- NA
NONFOOD[EQ_regions[] == 1] <- NA; GridArea[EQ_regions[] == 1] <- NA

# Reclassify TWS trend into 0.1 cm/yr increment bins
TWS_classes <- data.frame(low = seq(-40, 6.0, by = 0.1),
                          high = seq(-39.9, 6.1, by = 0.1),
                          ReCLASS = seq(-39.95, 6.05, by = 0.1))
TWS_binned <- raster::reclassify(TWS_trend, TWS_classes)

# Calculate sum of kcal per each reclassified region based on TWS rate of change
Feed.distr <-    zonal(FEED, TWS_binned, sum, na.rm = T, digits = 4)    %>% as.data.frame() %>% set_colnames(c("TWS_mv", "FEED_kcal"))
Food.distr <-    zonal(FOOD, TWS_binned, sum, na.rm = T, digits = 4)    %>% as.data.frame() %>% set_colnames(c("TWS_mv", "FOOD_kcal"))
Nonfood.distr <- zonal(NONFOOD, TWS_binned, sum, na.rm = T, digits = 4) %>% as.data.frame() %>% set_colnames(c("TWS_mv", "NONFOOD_kcal"))
KCAL_Global_types <- Reduce(function(x, y) merge(x, y, by = "TWS_mv"), list(Food.distr, Feed.distr, Nonfood.distr))

# convert kcal counts into trillions
KCAL_Global_types[,2:4] <- KCAL_Global_types[,2:4]/1e12
KCAL_Global_types %<>% melt(id.var = "TWS_mv")
KCAL_Global_types$variable %<>% as.factor()

# plotting variables
a <- 0.85
b <- 600

fig <- ggplot(KCAL_Global_types, aes(x = TWS_mv, y = value, fill = variable)) +
  annotate("rect", xmin = -5,   xmax = -2,   ymin = 0, ymax = b, fill= "#FFA5A5", alpha = a) +
  annotate("rect", xmin = -2,   xmax = -0.5, ymin = 0, ymax = b, fill= "#FFDFDF", alpha = a) +
  annotate("rect", xmin = -0.5, xmax = 0.5,  ymin = 0, ymax = b, fill= "#959595", alpha = a) +
  annotate("rect", xmin = 0.5,  xmax = 2,    ymin = 0, ymax = b, fill= "#F0EDF8", alpha = a) +
  annotate("rect", xmin = 2,    xmax = 3,    ymin = 0, ymax = b, fill= "#AAC2FF", alpha = a) +
  geom_bar(stat = "identity", position="stack", colour = "black") +
  scale_fill_manual(values = c("palegreen3", "burlywood4", "gray32")) +
  scale_y_continuous(position = "right", breaks = seq(0, 600, by = 50), expand = c(0,0)) +
  scale_x_continuous(limits = c(-5, 3), breaks = seq(-5, 3, by = 1), expand = c(0,0),
                     labels = c("-5", "-4", "-3", "-2", "-1", "0", "1", "2", "3")) +
  theme(panel.background =  element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_line(alpha(colour = "white", 0.3), linetype = "dashed", size = 0.5),
        axis.line = element_line(colour = "black"), panel.ontop = TRUE,
        legend.position = "none") +
  geom_vline(xintercept = 0, size = 1.5, alpha = 0.8) 
fig

ggsave("C:/Users/Tom/Desktop/UPDATE_agriculture.png", fig, dpi = 500, width = 14, height = 10, bg = "transparent")

#################
# Summary stats #
#################

Ag_stats <- cbind(as.data.frame(TWS_trend), as.data.frame(FEED), as.data.frame(FOOD), as.data.frame(NONFOOD)) %>%
  set_colnames(c("TWS", "FEED", "FOOD", "NONFOOD"))
Ag_stats$class <- ifelse(Ag_stats$TWS < -2, "SevereDry",
                         ifelse(Ag_stats$TWS > -2 & Ag_stats$TWS < -0.5, "ModDry",
                                ifelse(Ag_stats$TWS > -0.5 & Ag_stats$TWS < 0.5, "Static",
                                       ifelse(Ag_stats$TWS > 0.5 & Ag_stats$TWS < 2, "ModWet", "SevereWet"))))
Ag_stats_summary <- Ag_stats %>%
  group_by(class) %>%
  summarise(FeedSum = sum(FEED, na.rm = TRUE)/1e14,
            FoodSum = sum(FOOD, na.rm = TRUE)/1e14,
            NonFoodSum = sum(NONFOOD, na.rm = TRUE)/1e14) %>% as.data.frame()

write.csv(Ag_stats_summary,"C:/Users/Tom/Desktop/FinalRun/AG/Kcal_alloc_summ_trendclass.csv",
          row.names = FALSE)

Ag_stats$class %<>% as.factor()
Ag_stats$All <- Ag_stats$FEED + Ag_stats$FOOD + Ag_stats$NONFOOD

Median_summ <- Ag_stats %>%
  # group_by(class) %>%
  summarise(FEEDmedian = weighted.quantile(TWS, FEED, probs = 0.50, na.rm = TRUE),
            FOODmedian = weighted.quantile(TWS, FOOD, probs = 0.50, na.rm = TRUE),
            NONFOODmedian = weighted.quantile(TWS, NONFOOD, probs = 0.50, na.rm = TRUE),
            ALLmedian = weighted.quantile(TWS, All, probs = 0.50, na.rm = TRUE)) %>%  as.data.frame()

write.csv(Median_summ,"C:/Users/Tom/Desktop/FinalRun/AG/Kcal_alloc_weightedmedian.csv",
          row.names = FALSE)

######################
## Accompanying map ##
######################
# import necessary data, and is faster if clearing environment before doing so
rm(list = ls())
GridArea <- raster("./R_gis_exports/WGS84_cellArea_0d05res.tif") # Grid area estimate for WGS84 reference elipsoid
TWS_trend <- raster("./Rodell_SourceData/Rodell_etal_0d05.tif") # Rodell et al. (2018) TWS trends, resampled to 0.05 degrees

FEED <- raster("./CropAllocationFoodFeedFuel_Geotiff/Cassidy_gs/FEED_0d05_gs.tif")
FOOD <- raster("./CropAllocationFoodFeedFuel_Geotiff/Cassidy_gs/FOOD_0d05_gs.tif")
NONFOOD <- raster("./CropAllocationFoodFeedFuel_Geotiff/Cassidy_gs/NONFOOD_0d05_gs.tif")
All_kcal <- FEED + FOOD + NONFOOD # sum of all kcal sets

EQ_regions <- raster("./Earthquakes/EQ_rm_0d05.tif") # earthquake regions to remove
GADM_lvl0 <- raster("./GADM/GADM_level0_0d05.tif") # GADM national dataset

# Remove EQ regions, lakes, and oceans from input data
All_kcal[EQ_regions[] == 1 | is.na(GADM_lvl0[])] <- NA
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

#########################
## Fig1b - Agriculture ##
#########################
KcalDens <- All_kcal/GridArea # Determines population per km2, using grid areas for WGS84 reference ellipsoid
KcalDens_percentile <- PercentileAssignment(KcalDens_percentile, KcalDens, GridArea) # determines area-weighted percentile distribution of population density
KcalDens_percentile[rmRAS[] == 1 | EQ_regions_RAS[] == 1] <- NA # reset NA regions to NA (instead of 0)

# Scale by TWS trend
Kcal_map <- KcalDens_percentile*TWS_scaler

# make map plot
data(World)
Lakes_ply <- readOGR(dsn=paste("./Lakes", sep=""), layer="ne_10m_lakes")
Coastlines <- readOGR(dsn = "./NaturalEarth/Coastlines", layer = "ne_10m_coastline")
Aquifers <- readOGR(dsn = "./Aquifers", layer = "world_aquifer_systems")
Lakes_ply$scalerank %<>% as.numeric()
Lakes_large <- subset(Lakes_ply, scalerank < 4)

map <-
  tm_shape(Kcal_map, projection = "robin") +  tm_raster(style = "cont", palette = "RdBu", midpoint = 0, breaks = c(-1, 1)) +
  tm_shape(Coastlines) +  tm_lines("grey50", lwd = 0.1) +
  tm_shape(Aquifers) + tm_borders("black", lwd = 0.66) +
  tm_shape(Lakes_large) + tm_borders("black", lwd = 0.5) +
  tm_style("white", legend.show = F, earth.boundary = c(-180, -60, 180, 88), earth.boundary.color = "white",
           space.color = "white", legend.frame = F, frame = F)
map

# save map plot
tmap_save(map, "C:/Users/Tom/Desktop/Map_Kcal_wAquifers.png", dpi = 500,
          outer.margins = 0.01, height = 3, units = "in")


writeRaster(Kcal_map, "C:/Users/Tom/Desktop/FinalRun/GTiff_agricultureimpact.tif",
            format="GTiff", overwrite=TRUE)
