library(raster); library(dplyr); library(magrittr); library(spatstat); library(rgdal); library(sf)
library(RColorBrewer); library(ggsci); library(tmap); library(tmaptools); library(lwgeom)

mainDir <- "Z:/2.active_projects/Xander/"

# import data
VSI_water_weighted <- raster(paste(mainDir, "VSI/rasters/", "SensAETW", ".tif", sep="")) # Vegetation sensitity index, water factor, weighted
TWS_trend <- raster(paste(mainDir, "! GIS_files/Rodell_SourceData/", "Rodell_etal_0d05", ".tif", sep="")) # Rodell et al. (2018) TWS trends
GridArea <- raster(paste(mainDir, "! GIS_files/R_gis_exports/", "WGS84_cellArea_0d05res", ".tif", sep="")) # Grid area (km2) for WGS84 projection
Lakes <- raster(paste(mainDir, "! GIS_files/Lakes/", "Lakes_0d05", ".tif", sep="")) # Lakes 
BioHotspots_RAS <- raster(paste(mainDir, "! GIS_files/Biodiversity/Hotspots/", "Hotspots_NoBorder_0d05", ".tif", sep="")) # Biodiversity hotspots
BioHotspots_VEC <- readOGR(dsn=paste(mainDir, "! GIS_files/Biodiversity/Hotspots", sep=""), 
                           layer="HotSpots_outline_singlefeatures")
AntA <- raster(paste(mainDir, "! GIS_files/NaturalEarth/", "Antarctica", ".tif", sep="")) # Antarctica
GADM_lvl0 <- raster(paste(mainDir, "! GIS_files/GADM/", "GADM_level0_0d05", ".tif", sep="")) # GADM national dataset

# Remove lake and ocean areas
rmRAS <- raster(GridArea)
rmRAS[] <- 0
rmRAS[Lakes == 1 | AntA == 1] <- 1; rmRAS[is.na(GADM_lvl0)] <- 1

GridArea[rmRAS == 1] <- NA
TWS_trend[rmRAS == 1] <- NA
BioHotspots_RAS[rmRAS == 1] <- NA
VSI_water_weighted[rmRAS == 1] <- NA

s <- raster(GridArea)
res(s) <- 0.05
VSI_worldextent <- raster::resample(VSI_water_weighted, s, method = "ngb")

# Scale water factor to 95th percentile 
df <- cbind(as.data.frame(VSI_worldextent), as.data.frame(GridArea)) %>%  set_colnames(c("VSI_wat", "GA"))
p95.vsi_w <- weighted.quantile(df$VSI_wat, df$GA, 0.95, na.rm=TRUE) # determine max value at 95th percentile
VSI_water_scale <- VSI_worldextent/p95.vsi_w; VSI_water_scale[VSI_water_scale > 1] <- 1

# cap TWS trends at normalized scores in range of [-1, 1]
TWS_scale <- TWS_trend/2; TWS_scale[TWS_scale > 1] <- 1; TWS_scale[TWS_scale < -1] <- -1

# pass GRACE TWS trend dataset through water sensitivity filter
EcoImpact <- TWS_scale*VSI_water_scale

# make map plot
data("World"); tmap_options(max.raster = c(plot = 25920000, view = 25920000))
Lakes.narm <- Lakes; Lakes.narm[Lakes.narm == 0] <- NA
e <- extent(c(-180, 180, -60, 88))
map <-  
  tm_shape(EcoImpact, projection="robin") + tm_raster(style = "cont", palette = "RdBu", midpoint = 0) +
  tm_shape(Lakes.narm) + tm_raster(style = "cat", palette = colorRampPalette(c("grey"))(1), n = 1, colorNA = NULL) +
  tm_shape(World) +  tm_borders("black", lwd = .5) +
  tm_shape(BioHotspots_VEC) + tm_borders("blue", lwd = .75) +
  tm_style("white", legend.show = F, frame = F,  bg.color = "white", 
           earth.boundary = e, earth.boundary.color = "white", earth.boudary.lwd = 2,
           space.color="white", legend.frame = T, legend.bg.color="white")
# save map plot
tmap_save(map, "C:/Users/Tom/Desktop/Fig1d_sup.png", dpi = 500, outer.margins = 0.01, height = 2, units = "in")

########################
## Summary statistics ##
########################

Sum_stats <- cbind(as.data.frame(EcoImpact), as.data.frame(GridArea), as.data.frame(BioHotspots)) %>% set_colnames(c("EcoImp", "GA", "Hotspot"))
Sum_stats$Hotspot %<>% as.factor()
Sum_stats <- Sum_stats[complete.cases(Sum_stats), ]

Median_summ <- Sum_stats %>% 
  group_by(Hotspot) %>%
  summarise(median = weighted.quantile(EcoImp, GA, probs = 0.50, na.rm = TRUE)) %>%  as.data.frame() 
Ordered <- Median_summ[order(Median_summ$median),]


writeRaster(AdaptCap, filename=paste(mainDir, "! GIS_files/R_gis_exports/", "AdaptCap_2015", ".tif", sep=""),
            format="GTiff", overwrite=TRUE)
# 
# writeRaster(EcoImpact, filename=paste(mainDir, "! GIS_files/R_gis_exports/wshdGRACE_Fig1/", "EcoImpact", ".tif", sep=""),
#             format="GTiff", overwrite=TRUE)

library(raster); library(dplyr); library(magrittr); library(rgdal); library(R.utils)
library(tigris); library(spatstat); library(ggplot2); library(RColorBrewer); library(ggsci);
library(tmap); library(tmaptools); library(reshape2)

mainDir <- "Z:/2.active_projects/Xander/"

# import data
VSI_water_weighted <- raster(paste(mainDir, "VSI/rasters/", "SensAETW", ".tif", sep="")) # Vegetation sensitity index, water factor, weighted
TWS_trend <- raster(paste(mainDir, "! GIS_files/Rodell_SourceData/", "Rodell_etal_0d05", ".tif", sep="")) # Rodell et al. (2018) TWS trends
GridArea <- raster(paste(mainDir, "! GIS_files/R_gis_exports/", "WGS84_cellArea_0d05res", ".tif", sep="")) # Grid area (km2) for WGS84 projection
Lakes <- raster(paste(mainDir, "! GIS_files/Lakes/", "Lakes_0d05", ".tif", sep="")) # Lakes 
AntA <- raster(paste(mainDir, "! GIS_files/NaturalEarth/", "Antarctica", ".tif", sep="")) # Antarctica
GADM_lvl0 <- raster(paste(mainDir, "! GIS_files/GADM/", "GADM_level0_0d05", ".tif", sep="")) # GADM national dataset

# Extend VSI_water_weighted from -60 to -90 lat
ext <- extent(-180, 180, -90, 90)
VSI_water_weighted.ex <- raster::extend(VSI_water_weighted, ext)

# Remove lake and ocean areas
rmRAS <- raster(GridArea); rmRAS[] <- 0
rmRAS[Lakes == 1 | AntA == 1] <- 1; rmRAS[is.na(GADM_lvl0)] <- 1
GridArea[rmRAS == 1] <- NA; TWS_trend[rmRAS == 1] <- NA; BioHotspots_RAS[rmRAS == 1] <- NA; VSI_water_weighted.ex[rmRAS == 1] <- NA

# reclassify cropland density matrix into 0.05 bins for analysis
VSI_reclassRanges <- data.frame(low = seq(0, 90, by = 10), 
                                high = seq(10, 100, by = 10), 
                                lab = seq(5, 95, by = 10))

VSI_bins <- reclassify(VSI_water_weighted.ex, VSI_reclassRanges)

# set up data frame to fill in with subsequent raster analysis in loop
df <- seq(5, 95, by = 10) %>% as.data.frame %>% set_names("MidVal")
df$p5 <- c(rep(NA, nrow(df)))
df$p25 <- c(rep(NA, nrow(df)))
df$p50 <- c(rep(NA, nrow(df)))
df$p75 <- c(rep(NA, nrow(df)))
df$p95 <- c(rep(NA, nrow(df)))

for (i in 1:nrow(df)) {
  
  tempRas_TWStrend <- raster(GridArea)
  tempRas_GridArea <- raster(GridArea)
  tempRas_TWStrend[] <- NA
  tempRas_GridArea[] <- NA
  
  Val <- 5 + (10*(i-1))
  print(Val)
  
  tempRas_TWStrend[VSI_bins == Val] <- TWS_trend[VSI_bins == Val]
  tempRas_GridArea[VSI_bins == Val] <- GridArea[VSI_bins == Val]
  
  temp_df_a <- tempRas_TWStrend %>% as.data.frame() %>% set_colnames(c("TWS"))
  temp_df_b <- tempRas_GridArea %>%  as.data.frame() %>% set_colnames(c("GA") )
  temp_df <- cbind(temp_df_a, temp_df_b)
  
  df$p5[i]  <- weighted.quantile(temp_df$TWS, temp_df$GA, 0.05, na.rm=TRUE)
  df$p25[i] <- weighted.quantile(temp_df$TWS, temp_df$GA, 0.25, na.rm=TRUE)
  df$p50[i] <- weighted.quantile(temp_df$TWS, temp_df$GA, 0.50, na.rm=TRUE)
  df$p75[i] <- weighted.quantile(temp_df$TWS, temp_df$GA, 0.75, na.rm=TRUE)
  df$p95[i] <- weighted.quantile(temp_df$TWS, temp_df$GA, 0.95, na.rm=TRUE)
}

# create splines for each data column to smooth lines for figure presentation
spline_p5 <- as.data.frame(spline(df$MidVal, df$p5, n = 50*length(df), method = "fmm")) %>% set_colnames(c("x", "p5"))
spline_p25 <- as.data.frame(spline(df$MidVal, df$p25, n = 50*length(df))) %>% set_colnames(c("x", "p25"))
spline_p50 <- as.data.frame(spline(df$MidVal, df$p50, n = 50*length(df))) %>% set_colnames(c("x", "p50"))
spline_p75 <- as.data.frame(spline(df$MidVal, df$p75, n = 50*length(df))) %>% set_colnames(c("x", "p75"))
spline_p95 <- as.data.frame(spline(df$MidVal, df$p95, n = 50*length(df))) %>% set_colnames(c("x", "p95"))

spline_df <- Reduce(function(x, y) merge(x, y, by = "x"), list(spline_p5, spline_p25, spline_p50 ,
                                                               spline_p75, spline_p95))

# plot results 
aa <- 0.8
bb <- 0.4

a <- 0.85
b <- 100

fig <- 
  ggplot() +
  geom_rect(data=NULL,aes(xmin=0, xmax=b, ymin=-5, ymax=-2), fill="#FEA6A6") +
  geom_rect(data=NULL,aes(xmin=0, xmax=b, ymin=-2, ymax=-0.5), fill="#FEE0E0") +
  geom_rect(data=NULL,aes(xmin=0, xmax=b, ymin=-0.5, ymax=0.5), fill="#AAAAAA") +
  geom_rect(data=NULL,aes(xmin=0, xmax=b, ymin=0.5, ymax=2), fill="#F1EEF9") +
  geom_rect(data=NULL,aes(xmin=0, xmax=b, ymin=2, ymax=3), fill="#ABC3FE") +
  geom_boxplot(data = df, aes(x = MidVal, ymin = p5, lower = p25, middle = p50, upper = p75, ymax = p95, fill = x),
               width = 8, lwd = 0.7, stat = "identity", outlier.alpha = 0, fill = "slategrey", alpha = bb) +
  geom_line(data = spline_df, aes(x = x, y = p5), color = "firebrick", lwd = 2, alpha = aa) +
  geom_line(data = spline_df, aes(x = x, y = p25), color = "darkorange2", lwd = 2, alpha = aa) +
  geom_line(data = spline_df, aes(x = x, y = p50), color = "darkslategrey", lwd = 2, alpha = aa) +
  geom_line(data = spline_df, aes(x = x, y = p75), color = "steelblue", lwd = 2, alpha = aa) +
  geom_line(data = spline_df, aes(x = x, y = p95), color = "dodgerblue4", lwd = 2, alpha = aa) +
  # geom_ribbon(aes(x = spline_df$x, ymin = spline_df$p5, ymax = spline_df$p25),  alpha = 0.05, fill = "firebrick") +
  # geom_ribbon(aes(x = spline_df$x, ymin = spline_df$p25, ymax = spline_df$p50), alpha = 0.05, fill = "darkorange2") +
  # geom_ribbon(aes(x = spline_df$x, ymin = spline_df$p50, ymax = spline_df$p75), alpha = 0.05, fill = "steelblue") +
  # geom_ribbon(aes(x = spline_df$x, ymin = spline_df$p75, ymax = spline_df$p95), alpha = 0.05, fill = "dodgerblue4") +
  scale_y_continuous(limits = c(-5, 3), breaks = seq(-5, 3, by = 1), expand = c(0,0),  
                    labels = c("-5", "-4", "-3", "-2", "-1", "0", "1", "2", "3"))+
  theme(panel.background =  element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_line(alpha(colour = "white", 0.3), linetype = "dashed", size = 0.5),
        axis.line = element_line(colour = "black"), panel.ontop = TRUE,
        legend.position = "none") +
  geom_hline(yintercept = 0, size = 1.5) +
coord_flip(xlim = c(0,100), ylim = c(-5, 3), expand = c(0))
fig; # ggsave("C:/Users/Tom/Desktop/VSI_percentiles.png", fig, dpi = 500, width = 14, height = 10, units = "in", bg = "transparent")

###################################################################
## calculate area per boxplot bin ##
###################################################################

ar <- seq(5, 95, by = 10) %>% as.data.frame %>% set_names("MidVal")
ar$ea <- c(rep(NA, nrow(df)))

for (i in 1:nrow(df)) {
  tempRas_GridArea <- raster(GridArea)
  tempRas_GridArea[] <- NA
  Val <- 5 + (10*(i-1))
  print(Val)
  
  tempRas_GridArea[VSI_bins == Val] <- GridArea[VSI_bins == Val]
  temp_df <- tempRas_GridArea %>%  as.data.frame() %>% set_colnames(c("GA") )
  
  ar$ea[i]  <- sum(temp_df$GA, na.rm=TRUE)
}

area.fig <- ggplot(ar, aes(x = MidVal, y = 1)) +
  geom_point(aes(size = ea)) + scale_size_continuous(range=c(1,15)) +
  theme(panel.background =  element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_blank())+
  scale_y_continuous(trans='log10')
area.fig
ggsave("C:/Users/Tom/Desktop/Areas.png", area.fig, dpi = 500, width = 10, height = 4, units = "in", bg = "transparent")
