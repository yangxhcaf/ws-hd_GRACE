library(raster); library(dplyr); library(magrittr); library(rgdal); library(R.utils); library(forcats);
library(tigris); library(spatstat); library(ggplot2); library(RColorBrewer); library(ggsci);
library(tmap); library(tmaptools)

mainDir <- "Z:/2.active_projects/Xander/"

# import data  
# WaterW <- raster(paste(mainDir, "VSI/rasters/", "SensAETW", ".tif", sep="")) # EVI sensitivity to water (water component of VSI) - weighted dataset
GridArea <- raster(paste(mainDir, "! GIS_files/R_gis_exports/", "WGS84_cellArea_0d05res", ".tif", sep="")) # Grid area (km2) for WGS84 projection
AdaptCap <- raster(paste(mainDir, "! GIS_files/Varis_AdaptiveCapacity/", "AC_2015_0d05", ".tif", sep="")) # VAris et al. AC for 2015 
BasinID <- raster(paste(mainDir, "! GIS_files/Aqueduct/2019_release/", "Aqueduct2019_ID", ".tif", sep="")) # basin delineations
Aqd_Stress <- raster(paste(mainDir, "! GIS_files/Aqueduct/2019_release/", "BWS_score", ".tif", sep="")) # water stress
Aqd_Flood <- raster(paste(mainDir, "! GIS_files/Aqueduct/2019_release/", "RFR_score", ".tif", sep="")) # flooding risk
TWS_trend <- raster(paste(mainDir, "! GIS_files/Rodell_SourceData/", "Rodell_etal_0d05", ".tif", sep="")) # Rodell et al. (2018) TWS trends
AntA <- raster(paste(mainDir, "! GIS_files/NaturalEarth/", "Antarctica", ".tif", sep="")) # Antarctica
GADM_lvl0 <- raster(paste(mainDir, "! GIS_files/GADM/", "GADM_level0_0d05", ".tif", sep="")) # GADM national dataset
Lakes <- raster(paste(mainDir, "! GIS_files/Lakes/", "Lakes_0d05", ".tif", sep="")) # Lakes 
Pop <- raster(paste(mainDir, "! GIS_files/R_gis_exports/", "POP_2015_0d05_UNWPP", ".tif", sep="")) # Population data from GWPv4 UNWPP 2015
WorldRegions <- raster(paste(mainDir, "! GIS_files/WorldRegions/", "MAgPIE_worldregions", ".tif", sep="")) # stress water security vulnerability

# Identify lakes, oceans, antarctica, and arid+low water use regions
rmRAS <- raster(GridArea)
rmRAS[] <- 0
rmRAS[Lakes == 1 | AntA == 1 | Aqd_Stress == 5] <- 1; rmRAS[is.na(GADM_lvl0)] <- 1

# remove these regions
Aqd_Stress[rmRAS == 1] <- NA
Aqd_Flood[rmRAS == 1] <- NA
AdaptCap[rmRAS == 1] <- NA
GridArea[rmRAS == 1] <- NA
TWS_trend[rmRAS == 1] <- NA

## Determine average TWS trend per basin (uncomment all but last line below if first run through)
# TWS_by_Grid <- (TWS_trend*GridArea) # weight by area
# a <- zonal(TWS_by_Grid, BasinID, fun = "sum", na.rm = TRUE) # sum per basin
# colnames(a) <- c("ID", "TWS_by_Grid_sum")
# a %<>% as.data.frame()
# b <- zonal(GridArea, BasinID, fun = "sum", na.rm = TRUE) # sum area per basin
# colnames(b) <- c("ID", "Area")
# b %<>% as.data.frame()
# c <- merge(a, b, by = "ID")
# c$TWS_mean_Basin <- c$TWS_by_Grid_sum/c$Area # get area weighted mean (aggregate)
## pair with basin shapefile 
# Aqd_2019 <- readOGR(dsn = "Z:/2.active_projects/Xander/! GIS_files/Aqueduct/2019_release", layer = "Aqueduct_2019_basinID")
# Aqd_2019_j <- geo_join(Aqd_2019, c, "aq30_id", "ID", by = NULL, how = "left")
# writeOGR(Aqd_2019_j, dsn = "Z:/2.active_projects/Xander/! GIS_files/Aqueduct/2019_release", "MeanTWS_aq30", driver = "ESRI Shapefile")
TWS_t_basin <- raster(paste(mainDir, "! GIS_files/Aqueduct/2019_release/", "TWS_mean", ".tif", sep="")) # reimport as raster (convert in QGIS)
TWS_t_basin[rmRAS == 1] <- NA

# Derive TWS scaling function for GRACE modification of Aqueduct stress and flooding risk
TWS_FLOOD_scale <- raster(GridArea); TWS_STRESS_scale <- raster(GridArea)
TWS_FLOOD_scale[TWS_t_basin <= -2] <- -2
TWS_FLOOD_scale[TWS_t_basin > -2 & TWS_t_basin <= -0.5] <- (((TWS_t_basin[TWS_t_basin > -2 & TWS_t_basin <= -0.5]+0.5)/1.5)*2)
TWS_FLOOD_scale[TWS_t_basin > -0.5 & TWS_t_basin <= 0.5] <- 0
TWS_FLOOD_scale[TWS_t_basin > 0.5 & TWS_t_basin <= 2] <- (((TWS_t_basin[TWS_t_basin > 0.5 & TWS_t_basin <= 2]-0.5)/1.5)*2)
TWS_FLOOD_scale[TWS_t_basin > 2] <- 2
TWS_STRESS_scale[] <-(-1)*TWS_FLOOD_scale[]

# Modify basin stress and flooding risk index by derived scalers
Stress_mod <- raster(GridArea); Flood_mod <- raster(GridArea)

Stress_mod[] <- Aqd_Stress[] + TWS_STRESS_scale[] + 2 #plus 2 to have minimum set to 0
Flood_mod[] <- Aqd_Flood[] + TWS_FLOOD_scale[] + 2 #plus 2 to have minimum set to 0

# Scale modified stress and flood datasets by min and max, and normalize from 0 to 1 using area based 5th and 95th percentiles
df_sm <- Stress_mod %>% as.data.frame() %>% set_colnames(c("SM"))
df_fm <- Flood_mod %>% as.data.frame() %>% set_colnames(c("FM"))
df_ga <- GridArea %>% as.data.frame() %>% set_colnames(c("GA"))
df <- cbind(df_sm, df_fm, df_ga)

p5.s <- weighted.quantile(df$SM, df$GA, 0.05, na.rm=TRUE) # determine max value at 95th percentile
p95.s <- weighted.quantile(df$SM, df$GA, 0.95, na.rm=TRUE) # determine max value at 95th percentile

p5.f <- weighted.quantile(df$FM, df$GA, 0.05, na.rm=TRUE) # determine max value at 95th percentile
p95.f <- weighted.quantile(df$FM, df$GA, 0.95, na.rm=TRUE) # determine max value at 95th percentile

Stress_01 <- (Stress_mod-p5.s)/(p95.s-p5.s); Stress_01[Stress_01 > 1] <- 1; Stress_01[Stress_01 < 0] <- 0
Flood_01 <- (Flood_mod-p5.f)/(p95.f-p5.f); Flood_01[Flood_01 > 1] <- 1; Flood_01[Flood_01 < 0] <- 0  

# ensure adaptive capacity is range [0,1]
AdaptCap_01 <- AdaptCap/max(AdaptCap[], na.rm = TRUE)

# Determine water security vulnerability by: WSV = Water Risk (flood and stress) - Adaptive capacity
WSV_flood  <- Flood_01 - AdaptCap_01; WSV_stress <- Stress_01 - AdaptCap_01;

#################
## Make plots ##
#################

# make map plot; change raster and palette per layer being exported
data("World"); tmap_options(max.raster = c(plot = 25920000, view = 25920000))
Lakes.narm <- Lakes; Lakes.narm[Lakes.narm == 0] <- NA
e <- extent(c(-180, 180, -60, 88))
map <-  
  tm_shape(World, projection = "robin") + tm_polygons(col = "grey")+
  tm_shape(WSV_stress, projection="robin") + tm_raster(style = "cont", palette = "-Spectral", midpoint = 0) +
  tm_shape(Lakes.narm, projection = "robin") + tm_raster(style = "cat", palette = "blue", colorNA = NULL) +
  tm_shape(World) +  tm_borders("black", lwd = .5) +
  tm_style("white", legend.show = F,
           frame = F, bg.color = "white", earth.boundary = e, earth.boundary.color = "white", earth.boudary.lwd = 2,
           space.color="white", legend.frame = T, legend.bg.color="white")
# save map plot
tmap_save(map, "C:/Users/Tom/Desktop/WSV_stress.png", dpi = 500, height = 3, units = "in") 

############################################
## Subsequent analysis, for parts c and d ##
############################################

############################### Analysis for part c

# world region ID index: 1 - NAM | 2 - FSU | 3 - POECD | 4 - PAS | 5 - CPA | 6 - SAS | 7 - SSA | 8 _ MENA | 9 - WEUR | 10 - LAM 
Global_df <- cbind(as.data.frame(GADM_lvl0), as.data.frame(Stress_01), as.data.frame(Flood_01), as.data.frame(AdaptCap_01),
                   as.data.frame(WorldRegions), as.data.frame(GridArea), as.data.frame(Pop)) %>% 
  set_colnames(c("GADM", "Stress_lvl", "Flood_lvl", "AC", "Region", "Area", "Pop"))
Global_df <- Global_df[complete.cases(Global_df),]
Global_df$GADM %<>% as.factor()

QuadPlot_HydroBASINS <- Global_df %>% 
  group_by(GADM) %>%
  summarise(Stress_median = weighted.quantile(Stress_lvl, Pop, probs = 0.50, na.rm = TRUE),
            Flood_median  = weighted.quantile(Flood_lvl,  Pop, probs = 0.50, na.rm = TRUE),
            AC_median     = weighted.quantile(AC,         Pop, probs = 0.50, na.rm = TRUE),
            Region_main   = round(weighted.quantile(Region,     Pop, probs = 0.50, na.rm = TRUE), digits = 0),
            Population    = sum(Pop, na.rm = TRUE)) %>%  as.data.frame()
QuadPlot_HydroBASINS$Region_main %<>% as.factor()

plot_df <- QuadPlot_HydroBASINS[order(-QuadPlot_HydroBASINS$Population),]

# figure 2c scatterplot 
figure <- ggplot(data = filter(plot_df, Population > 5e6), aes(x = AC_median, y = Flood_median, label = GADM)) +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major = element_line(colour = "white", linetype = "dashed", size = 0.5),
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10, color = "black")) +
  geom_jitter(shape = 21, width = 0.0, height = 0.0, aes(fill = Region_main, size = Population), alpha = 1) +
  scale_fill_brewer(palette = "Set3") +
  scale_size(range = c(2, 14))+
  # scale_alpha(range = c(0.5, 1))+
  # geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1) +
  # geom_text(size = 3.5)+
  coord_cartesian(xlim = c(-0.05, 1.05), ylim = c(-0.05, 1.1)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))
figure

ggsave("C:/Users/Tom/Desktop/Nations__Stress_v_AC.png", figure, dpi = 500, width = 6, height = 5, bg = "transparent")

map <-  
  tm_shape(World, projection = "robin") + tm_polygons(col = "grey")+
  tm_shape(WorldRegions) +   tm_raster(style = "cat", palette = "Set3") +
  tm_shape(Lakes.narm, projection = "robin") + tm_raster(style = "cat", palette = "blue", colorNA = NULL) +
  tm_shape(World) +  tm_borders("black", lwd = .5) +
  tm_style("white", legend.show = F,
           frame = F, bg.color = "white", earth.boundary = e, earth.boundary.color = "white", earth.boudary.lwd = 2,
           space.color="white", legend.frame = T, legend.bg.color="white")
# save map plot
tmap_save(map, "C:/Users/Tom/Desktop/WorldRegions.png", dpi = 500, height = 3, units = "in") # insert size map

########################################## Analysis for part d
# create data frames for area weighted quantiles to be calculated with
WSV_df <- cbind(as.data.frame(WSV_stress), as.data.frame(WSV_flood), as.data.frame(GridArea), 
                as.data.frame(Pop), as.data.frame(WorldRegions)) %>% 
  set_colnames(c("WSV_str", "WSV_fld", "Area", "Pop","Region"))
WSV_df <- WSV_df[complete.cases(WSV_df),]
WSV_df$Region %<>% as.factor()

# determine distribution statistics for the coupled raster grids
Reg.Result_AreaWeighted <- WSV_df %>% 
  group_by(Region) %>%
  summarise(STR_median = weighted.quantile(WSV_str, Pop, probs = 0.50, na.rm = TRUE),
            STR_p25 = weighted.quantile(WSV_str, Pop, probs = 0.25, na.rm = TRUE),
            STR_p75 = weighted.quantile(WSV_str, Pop, probs = 0.75, na.rm = TRUE),
            STR_p5 = weighted.quantile(WSV_str, Pop, probs = 0.05, na.rm = TRUE),
            STR_p95 = weighted.quantile(WSV_str, Pop, probs = 0.95, na.rm = TRUE),
            FLD_median = weighted.quantile(WSV_fld, Pop, probs = 0.50, na.rm = TRUE),
            FLD_p25 = weighted.quantile(WSV_fld, Pop, probs = 0.25, na.rm = TRUE),
            FLD_p75 = weighted.quantile(WSV_fld, Pop, probs = 0.75, na.rm = TRUE),
            FLD_p5 = weighted.quantile(WSV_fld, Pop, probs = 0.05, na.rm = TRUE),
            FLD_p95 = weighted.quantile(WSV_fld, Pop, probs = 0.95, na.rm = TRUE)) %>%  as.data.frame()

# 1 - NAM | 2 - FSU | 3 - POECD | 4 - PAS | 5 - CPA | 6 - SAS | 7 - SSA | 8 _ MENA | 9 - WEUR | 10 - LAM 
Stress.fig <- ggplot(Reg.Result_AreaWeighted, aes(x = fct_reorder(Region, STR_median), 
                                                  ymin = STR_p5, lower = STR_p25, middle = STR_median, 
                                                  upper = STR_p75, ymax = STR_p95, fill = Region)) +
  # scale_x_discrete(limsits=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
  geom_boxplot(stat = "identity") +  scale_fill_brewer(palette="Set3") +
  # scale_y_continuous(breaks = seq(-3, 3, by = 1), expand = c(0,0)) +
  theme(panel.background =  element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_line(alpha(colour = "white", 0.3), linetype = "dashed", size = 0.5),
        axis.line.x = element_line(color = "black", size = 1)) +
  coord_flip(ylim = c(-1, 1))
Stress.fig

Flood.fig <- ggplot(Reg.Result_AreaWeighted, aes(x = fct_reorder(Region, FLD_median), 
                                                 ymin = FLD_p5, lower = FLD_p25, middle = FLD_median, 
                                                 upper = FLD_p75, ymax = FLD_p95, fill = Region)) +
  # scale_x_discrete(limsits=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
  geom_boxplot(stat = "identity") +  scale_fill_brewer(palette="Set3") +
  # scale_y_continuous(breaks = seq(-3, 3, by = 1), expand = c(0,0)) +
  theme(panel.background =  element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_line(alpha(colour = "white", 0.3), linetype = "dashed", size = 0.5),
        axis.line.x = element_line(color = "black", size = 1)) +
  coord_flip(ylim = c(-1, 1))
Flood.fig

ggsave("C:/Users/Tom/Desktop/WSV_stress_bxplt.png", Stress.fig, dpi = 500, width = 5, height = 5, bg = "transparent")
ggsave("C:/Users/Tom/Desktop/WSV_flood_bxplt.png", Flood.fig, dpi = 500, width = 5, height = 5, bg = "transparent")
