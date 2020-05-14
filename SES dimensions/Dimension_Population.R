library(ggplot2); library(raster); library(dplyr); library(magrittr); library(reshape2); 
library(spatstat); library(e1071); library(rgdal); library(gdalUtils); library(tmap); library(tmaptools)
if(!is.null(dev.list())) dev.off(); cat("\014");rm(list=ls())
setwd("Z:/2.active_projects/Xander/! GIS_files")

# import all data
TWS_trend <- raster("./Rodell_SourceData/Rodell_etal_0d05.tif") # Rodell et al. (2018) TWS trends, resampled to 0.05 res
Pop <- raster("./R_gis_exports/POP_2015_0d05_UNWPP.tif", sep="") # Population data from GWPv4 UNWPP 2015
WaterShortage <- raster("./WaterScarcityAtlas/Shortage_2001t2010_Kummu_0d05.tif", sep="") # Kummu et al water crowding data
GADM_lvl0 <- raster("./GADM/GADM_level0_0d05.tif") # GADM national dataset rasterized to 0.05
EQ_regions <- raster("./Earthquakes/EQ_rm_0d05.tif") # earthquake regions to remove

# Remove EQ regions from input data
Pop[EQ_regions[] == 1] <- NA
TWS_trend[EQ_regions[]] <- NA
WaterShortage[EQ_regions[]] <- NA

# Set NAs to -99 for zonal statistics
WaterShortage_id <- raster(TWS_trend)
WaterShortage_id[is.na(WaterShortage[])] <- -1 # index for no data
WaterShortage_id[WaterShortage <= 1e6/2000] <- 5 # Falkenmark stress level 5
WaterShortage_id[WaterShortage > 1e6/2000 & WaterShortage <= 1e6/1000] <- 4 # Falkenmark stress level 4
WaterShortage_id[WaterShortage > 1e6/1000 & WaterShortage <= 1e6/600] <- 3 # Falkenmark stress level 3
WaterShortage_id[WaterShortage > 1e6/600 & WaterShortage <= 1e6/100] <- 2 # Falkenmark stress level 2
WaterShortage_id[WaterShortage > 1e6/100] <- 1 # Falkenmark stress level 1
WaterShortage_id %<>% as.factor()

# split population raster by regions of water stress
# begin by initializing a raster for each class with same extent and resolution as rest of files
Pop.nd     <- raster(TWS_trend);  Pop.nd[] <- 0
Pop.1  <- raster(TWS_trend);      Pop.1[]  <- 0
Pop.2 <- raster(TWS_trend);       Pop.2[]  <- 0
Pop.3    <- raster(TWS_trend);    Pop.3[]  <- 0
Pop.4  <- raster(TWS_trend);      Pop.4[]  <- 0
Pop.5    <- raster(TWS_trend);    Pop.5[]  <- 0

# populate population rasters per water stress class
Pop.nd[WaterShortage_id == -1] <- Pop[WaterShortage_id == -1]
Pop.1[WaterShortage_id == 1]   <- Pop[WaterShortage_id == 1]
Pop.2[WaterShortage_id == 2]   <- Pop[WaterShortage_id == 2]
Pop.3[WaterShortage_id == 3]   <- Pop[WaterShortage_id == 3]
Pop.4[WaterShortage_id == 4]   <- Pop[WaterShortage_id == 4]
Pop.5[WaterShortage_id == 5]   <- Pop[WaterShortage_id == 5]

# Reclassify TWS trend into 0.1 cm/yr increment bins
TWS_classes <- data.frame(low = seq(-40, 6.0, by = 0.1), 
                          high = seq(-39.9, 6.1, by = 0.1), 
                          ReCLASS = seq(-39.95, 6.05, by = 0.1))
TWS_binned <- raster::reclassify(TWS_trend, TWS_classes)

# Calculate population within each trend bin for each water stress class
Pop.nd.distr <- zonal(Pop.nd, TWS_binned, sum, na.rm = T, digits = 4) %>% as.data.frame() %>% set_colnames(c("TWS_mv", "c.nd"))
Pop.1.distr  <- zonal(Pop.1, TWS_binned, sum, na.rm = T, digits = 4)  %>% as.data.frame() %>% set_colnames(c("TWS_mv", "c.1"))
Pop.2.distr  <- zonal(Pop.2, TWS_binned, sum, na.rm = T, digits = 4)  %>% as.data.frame() %>% set_colnames(c("TWS_mv", "c.2"))
Pop.3.distr  <- zonal(Pop.3, TWS_binned, sum, na.rm = T, digits = 4)  %>% as.data.frame() %>% set_colnames(c("TWS_mv", "c.3"))
Pop.4.distr  <- zonal(Pop.4, TWS_binned, sum, na.rm = T, digits = 4)  %>% as.data.frame() %>% set_colnames(c("TWS_mv", "c.4"))
Pop.5.distr  <- zonal(Pop.5, TWS_binned, sum, na.rm = T, digits = 4)  %>% as.data.frame() %>% set_colnames(c("TWS_mv", "c.5"))

# merge all results, and melt dataframe for plotting
Pop.distr <- Reduce(function(x, y) merge(x, y, by = "TWS_mv"), list(Pop.nd.distr, Pop.1.distr, Pop.2.distr,
                                                                    Pop.3.distr, Pop.4.distr, Pop.5.distr))
Pop.distr %<>% melt(id.var = "TWS_mv")

Pop.distr$class <- ifelse(Pop.distr$TWS_mv < -2, "SevereDry",
                          ifelse(Pop.distr$TWS_mv > -2 & Pop.distr$TWS_mv < -0.5, "ModDry",
                                 ifelse(Pop.distr$TWS_mv > -0.5 & Pop.distr$TWS_mv < 0.5, "Static",
                                        ifelse(Pop.distr$TWS_mv > 0.5 & Pop.distr$TWS_mv < 2, "ModWet", "SevereWet"))))


# calculate population weighted median results for each scarcity class
a <- TWS_trend %>% as.data.frame() %>% set_colnames(c("TWS"))
b <- Pop %>% as.data.frame() %>% set_colnames(c("Pop"))
c <- WaterShortage_id %>% as.data.frame() %>% set_colnames(c("c.id"))
wq_df <- cbind(a, b, c)

df_nd <- wq_df %>% filter(c.id == -1); wq_nd <- weighted.quantile(df_nd$TWS, df_nd$Pop, probs = 0.5, na.rm = T)
df_1 <- wq_df %>% filter(c.id == 1);   wq_1 <- weighted.quantile(df_1$TWS, df_1$Pop, probs = 0.5, na.rm = T)
df_2 <- wq_df %>% filter(c.id == 2);   wq_2 <- weighted.quantile(df_2$TWS, df_2$Pop, probs = 0.5, na.rm = T)
df_3 <- wq_df %>% filter(c.id == 3);   wq_3 <- weighted.quantile(df_3$TWS, df_3$Pop, probs = 0.5, na.rm = T)
df_4 <- wq_df %>% filter(c.id == 4);   wq_4 <- weighted.quantile(df_4$TWS, df_4$Pop, probs = 0.5, na.rm = T)
df_5 <- wq_df %>% filter(c.id == 5);   wq_5 <- weighted.quantile(df_5$TWS, df_5$Pop, probs = 0.5, na.rm = T)

## plot results
a <- 0.85
b <- 500*(10^6)
clrs = brewer.pal(n = 5, name = "Purples")

# shaded rectangles to add
fig <- ggplot(Pop.distr, aes(x = TWS_mv, y = value, fill = variable)) +
  annotate("rect", xmin = -5,   xmax = -2,   ymin = 0, ymax = b, fill= "#FFA5A5", alpha = a) +
  annotate("rect", xmin = -2,   xmax = -0.5, ymin = 0, ymax = b, fill= "#FFDFDF", alpha = a) +
  annotate("rect", xmin = -0.5, xmax = 0.5,  ymin = 0, ymax = b, fill= "#959595", alpha = a) +
  annotate("rect", xmin = 0.5,  xmax = 2,    ymin = 0, ymax = b, fill= "#F0EDF8", alpha = a) +
  annotate("rect", xmin = 2,    xmax = 3,    ymin = 0, ymax = b, fill= "#AAC2FF", alpha = a) +
  geom_bar(stat = "identity", position="stack", colour = "black") +
  scale_fill_manual(values = c("#808080", clrs)) + 
  scale_y_continuous(position = "right", breaks = seq(0, 500e6, by = 50e6), expand = c(0,0), 
                     sec.axis = dup_axis(name = element_blank())) +
  scale_x_continuous(limits = c(-5, 3), breaks = seq(-5, 3, by = 1), expand = c(0,0),  
                     labels = c("-5" ,"-4", "-3", "-2", "-1", "0", "1", "2", "3")) +
  theme(panel.background =  element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_line(alpha(colour = "white", 0.3), linetype = "dashed", size = 0.5),
        axis.line = element_line(colour = "black"), panel.ontop = TRUE,
        legend.position = "none"
        ) +
  geom_vline(xintercept = 0, size = 1.5, alpha = 0.8) 
fig

ggsave("C:/Users/Tom/Desktop/Update_population.png", fig, dpi = 500, width = 14, height = 10, bg = "transparent") 

########################
## Summary statistics ##
########################

# summary table per TWS trend class (5 classes) and water stress class (7 classes) = 35 population counts (in millions)
PopDisr_summary <- Pop.distr %>% group_by(class, variable) %>% summarise(Pop_Millions = sum(value)/1e6) %>% as.data.frame()
write.csv(PopDisr_summary,"C:/Users/Tom/Desktop/FinalRun/POPULATION/PopDistrSumm.csv", 
          row.names = FALSE)

# create summary dataframe
Summary_df <- cbind(as.data.frame(TWS_trend), as.data.frame(Pop), as.data.frame(WaterShortage_id)) %>% 
  set_colnames(c("TWS", "Pop", "Class"))
Summary_df$Class %<>% as.factor()
# print median TWS trend per water stress class
a <- Summary_df %>% 
  group_by(Class) %>%
  summarise(median = weighted.quantile(TWS, Pop, probs = 0.50, na.rm = TRUE)) %>%  as.data.frame()

write.csv(a,"C:/Users/Tom/Desktop/FinalRun/POPULATION/PopWeightedMedians.csv", 
          row.names = FALSE)

# Skewness calculation
Pop.distr <- Reduce(function(x, y) merge(x, y, by = "TWS_mv"), list(Pop.nd.distr, Pop.1.distr, Pop.2.distr, Pop.3.distr, 
                                                                    Pop.4.distr, Pop.5.distr))
SkewTable <- data.frame(Pop.distr[,1]) %>% set_colnames("Trend")
SkewTable$Pop <- Pop.distr$c.nd + Pop.distr$c.1 + Pop.distr$c.2 + Pop.distr$c.3 + Pop.distr$c.4 + Pop.distr$c.5
# need to simplify so that ~2 billion (2^31) vector data limit is not exceeded 
SkewTable$PopSimp <- as.integer(round(SkewTable$Pop/100, 0))
ExplodedSkewTable <- SkewTable[rep(rownames(SkewTable), SkewTable$PopSimp), ]
skewness(ExplodedSkewTable$Trend)

######################
## Accompanying map ##
######################
# import necessary data, and is faster if clearing environment before doing so
rm(list = ls())
GridArea <- raster("./R_gis_exports/WGS84_cellArea_0d05res.tif") # Grid area estimate for WGS84 reference elipsoid
TWS_trend <- raster("./Rodell_SourceData/Rodell_etal_0d05.tif") # Rodell et al. (2018) TWS trends, resampled to 0.05 degrees
Pop <- raster("./R_gis_exports/POP_2015_0d05_UNWPP.tif") # Population data from GWPv4 UNWPP 2015, aggregated to 0.05 degrees
EQ_regions <- raster("./Earthquakes/EQ_rm_0d05.tif") # earthquake regions to remove
GADM_lvl0 <- raster("./GADM/GADM_level0_0d05.tif") # GADM national dataset

# Remove EQ regions and oceans
Pop[EQ_regions[] == 1 | is.na(GADM_lvl0[])] <- NA
TWS_trend[EQ_regions[] == 1 | is.na(GADM_lvl0[])] <- NA
GridArea[EQ_regions[] == 1 | is.na(GADM_lvl0[])] <- NA

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

########################
## Fig1a - Population ##
########################
PopDens <- Pop/GridArea # Determines population per km2, using grid areas for WGS84 reference ellipsoid
PopDens_percentile <- PercentileAssignment(PopDens_percentile, PopDens, GridArea) # determines area-weighted percentile distribution of population density
PopDens_percentile[rmRAS[] == 1 | EQ_regions_RAS[] == 1] <- NA # reset NA regions to NA (instead of 0)

# Scale by TWS trend
Pop_map <- PopDens_percentile*TWS_scaler

# make map plot
data(World)
Lakes_ply <- readOGR(dsn=paste("./Lakes", sep=""), layer="ne_10m_lakes")
Lakes_ply$scalerank %<>% as.numeric()
Lakes_large <- subset(Lakes_ply, scalerank < 4)

map <- 
  tm_shape(Pop_map, projection = "robin") +  tm_raster(style = "cont", palette = "RdBu", midpoint = 0, breaks = c(-1, 1)) +
  tm_shape(World) +  tm_borders(lwd = 0.7) +
  tm_shape(Lakes_large) + tm_borders("black", lwd = 0.5) +
  tm_style("white", legend.show = F, earth.boundary = c(-180, -60, 180, 88), earth.boundary.color = "white",
           space.color = "white", legend.frame = F, frame = F)
map

# save map plot
tmap_save(map, "C:/Users/Tom/Desktop/FinalRun/Map_population.png", dpi = 500, 
          outer.margins = 0.01, height = 3, units = "in")

writeRaster(Pop_map, filename=paste("C:/Users/Tom/Desktop/FinalRun/", "GTiff_populationimpact", ".tif", sep=""),
            format="GTiff", overwrite=TRUE)
