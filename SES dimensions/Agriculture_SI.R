library(raster); library(dplyr); library(magrittr); library(spatstat)
library(ggplot2); library(RColorBrewer); library(reshape2); library(Weighted.Desc.Stat)
if(!is.null(dev.list())) dev.off(); cat("\014");rm(list=ls())
setwd("Z:/2.active_projects/Xander/! GIS_files/")

# import all data
TWS_trend <- raster("./Rodell_SourceData/Rodell_etal_0d05.tif") # TWS trends
Crp_DENS <- raster("./R_gis_exports/CroplandDensity_0d05.tif") # cropland density
Irr_DENS <- raster("./Irrigation/GMIA_v5_aei_pct.tif") # area equipped for irrigation (AEI) density
ActualIrr_pctIrrDens <- raster("./Irrigation/gmia_v5_aai_pct_aei.tif") # area actualyl irrigated as a percent of AEI
Irr_DENS_SW <-  raster("./Irrigation/GMIA_v5_aei_pct_SW.tif") # Area irrigated w/ surface water as percent of AEI
Irr_DENS_GW <-  raster("./Irrigation/GMIA_v5_aei_pct_GW.tif") # Area irrigated w/ groundwater as percent of AEI
GridArea <- raster("./R_gis_exports/WGS84_cellArea_0d05res.tif") # WGS84 cell area for 0.05d resolution
EQ_regions <- raster("./Earthquakes/EQ_rm_0d05.tif") # earthquake regions to remove
FEED <- raster("./CropAllocationFoodFeedFuel_Geotiff/Cassidy_gs/FEED_0d05_gs.tif") # feed calories
FOOD <- raster("./CropAllocationFoodFeedFuel_Geotiff/Cassidy_gs/FOOD_0d05_gs.tif") # food calories
NONFOOD <- raster("./CropAllocationFoodFeedFuel_Geotiff/Cassidy_gs/NONFOOD_0d05_gs.tif") # nonfood calories
All_kcal <- FEED + FOOD + NONFOOD # calorie sum per grid cell

# resample GMIA data to 0.05d resolution
Total_Irr_DENS <- raster::resample(Irr_DENS, GridArea, method = "bilinear"); Total_Irr_DENS[Total_Irr_DENS < 0] <- 0 # to correct small resample errors around 0
ActualIrr_relative <- raster::resample(ActualIrr_pctIrrDens, GridArea, method = "bilinear")
Irr_DENS_SW <- raster::resample(Irr_DENS_SW, GridArea, method = "bilinear")
Irr_DENS_GW <- raster::resample(Irr_DENS_GW, GridArea, method = "bilinear")

# remove earthquake regions from all data
TWS_trend[EQ_regions == 1] <- NA
Crp_DENS[EQ_regions == 1] <- NA
Total_Irr_DENS[EQ_regions == 1] <- NA
ActualIrr_relative[EQ_regions == 1] <- NA
Irr_DENS_SW[EQ_regions == 1] <- NA
Irr_DENS_GW[EQ_regions == 1] <- NA
FEED[EQ_regions == 1] <- NA
FOOD[EQ_regions == 1] <- NA
NONFOOD[EQ_regions == 1] <- NA
All_kcal[EQ_regions == 1] <- NA

ActualIrr_DENS <- Total_Irr_DENS*(ActualIrr_relative/100) # calculate actual irrigation density
Actual_SW.Irr <- Total_Irr_DENS*(Irr_DENS_SW/100)
Actual_GW.Irr <- Total_Irr_DENS*(Irr_DENS_GW/100)

# compare dominant irrigation source
GWSW_comp <- (Actual_GW.Irr - Actual_SW.Irr)/(ActualIrr_DENS+1e-3)
GWSW_comp[GWSW_comp >1] <- 1;GWSW_comp[GWSW_comp < -1] <- -1 # clip to -1, 1 range for erroneous denominator

if (max(Crp_DENS[], na.rm = T) < 10) { # check to see if already run
  Crp_DENS <- Crp_DENS*100 # so on scale of 0-100 instead of 0-1
}

# reclassify cropland and irrigation density rasters into 0.05 bins for analysis
Density_bins_rcl <- data.frame(low = seq(0, 95, by = 5),
                               high = seq(5, 100, by = 5),
                               lab = seq(2.5, 97.5, by = 5))
Density_bins_rcl$low[1] <- -1 # set minimum to -1 to include 0
Density_bins_rcl$high[20] <- 101 # set maximum to 101 to include 100

Cropland_bins <- reclassify(Crp_DENS, Density_bins_rcl)
Equipped_Irrigation_bins <- reclassify(Total_Irr_DENS, Density_bins_rcl)
Actual_Irrigation_bins <- reclassify(ActualIrr_DENS, Density_bins_rcl)

###### This section to calculate mean TWS trend per cropland and actual irrigation density combinations
df_TWS <- cbind(as.data.frame(TWS_trend), as.data.frame(Cropland_bins), 
                as.data.frame(Equipped_Irrigation_bins), as.data.frame(Actual_Irrigation_bins),
                as.data.frame(GridArea)) %>% 
  set_colnames(c("TWS", "Crop_Density", 
                 "Equipped_IrrDensity", "Actual_IrrDensity", "GA"))
df_TWS$Actual_IrrDensity_CropClip <- ifelse(is.na(df_TWS$Actual_IrrDensity), df_TWS$Crop_Density, df_TWS$Actual_IrrDensity)
df_TWS$Actual_IrrDensity_CropClip <- ifelse(df_TWS$Actual_IrrDensity > df_TWS$Crop_Density, df_TWS$Crop_Density, df_TWS$Actual_IrrDensity)
df_TWS <- df_TWS[complete.cases(df_TWS$TWS), ]

Results_TWS <- df_TWS %>% # Area actually irrigated results
  group_by(Crop_Density, Actual_IrrDensity_CropClip) %>%
  summarise(TWS_mv = spatstat::weighted.quantile(TWS, GA, probs = 0.50, na.rm = TRUE)) %>%  as.data.frame()

###### This section to calculate GW vs SW dependency per cropland and actual irrigation density combinations
df_gwsw <- cbind(as.data.frame(Cropland_bins), as.data.frame(Actual_Irrigation_bins), 
                 as.data.frame(GWSW_comp), as.data.frame(GridArea)) %>% 
  set_colnames(c("Crop_Density", "Actual_IrrDensity", "GWSW.comp", "GA"))
df_gwsw$Actual_IrrDensity_CropClip <- ifelse(is.na(df_gwsw$Actual_IrrDensity), df_gwsw$Crop_Density, df_gwsw$Actual_IrrDensity)
df_gwsw$Actual_IrrDensity_CropClip <- ifelse(df_gwsw$Actual_IrrDensity > df_gwsw$Crop_Density, df_gwsw$Crop_Density, df_gwsw$Actual_IrrDensity)
df_gwsw <- df_gwsw[complete.cases(df_gwsw$GWSW.comp), ]

Results_gwsw <- df_gwsw %>% # Area actually irrigated results
  group_by(Crop_Density, Actual_IrrDensity_CropClip) %>%
  summarise(GWSW_comp = spatstat::weighted.quantile(GWSW.comp, GA, probs = 0.50, na.rm = TRUE)) %>%  as.data.frame()

###### This section to calculate kcal yield and food kcal rate per cropland and actual irrigation density combinations
df_kcal <- cbind(as.data.frame(Cropland_bins), as.data.frame(Actual_Irrigation_bins), 
                 as.data.frame(All_kcal), as.data.frame(FOOD), as.data.frame(GridArea)) %>% 
  set_colnames(c("Crop_Density", "Actual_IrrDensity", "KCAL_t", "Food_kcal", "GA"))
df_kcal$Actual_IrrDensity_CropClip <- ifelse(is.na(df_kcal$Actual_IrrDensity), df_kcal$Crop_Density, df_kcal$Actual_IrrDensity)
df_kcal$Actual_IrrDensity_CropClip <- ifelse(df_kcal$Actual_IrrDensity > df_kcal$Crop_Density, df_kcal$Crop_Density, df_kcal$Actual_IrrDensity)
df_kcal <- df_kcal[complete.cases(df_kcal$KCAL_t), ]

Results_kcal <- df_kcal %>% # Area actually irrigated results
  group_by(Crop_Density, Actual_IrrDensity_CropClip) %>%
  summarise(Sum_kcal = sum(KCAL_t, na.rm = T),
            Sum_food = sum(Food_kcal, na.rm = T),
            Sum_Area = sum(GA, na.rm = T)) %>%  as.data.frame()

Results_kcal$yield <- Results_kcal$Sum_kcal/Results_kcal$Sum_Area
Results_kcal$food_pct <- Results_kcal$Sum_food/Results_kcal$Sum_kcal

# Create figures
library(wesanderson); 
pltmn <- wes_palette("Zissou1", 100, type = "continuous")
pltmn <- rev(pltmn)

# change fill and uncomment appropriate scale for each plot
ggplot(data = filter(Results_kcal, Crop_Density != 0 & Actual_IrrDensity_CropClip != 0), 
       aes(x = Crop_Density, y = Actual_IrrDensity_CropClip, fill = food_pct)) +  
  geom_tile(col = "grey55", width = 5, height = 5) +
  
  #### scale for red blue TWS (a)
  # scale_fill_distiller(palette = "RdBu", direction = 1, limits = c(-2, 2), oob = scales::squish)+ # (1)
  
  ##### scale for green brown source dependency (b)
  # scale_fill_distiller(palette = "BrBG", direction = -1, limits = c(-0.50, 0.50),
  #                      # trans = "log10",
  #                      oob = scales::squish) +
  
  ##### scale for kcal density (c)
  # scale_fill_distiller(palette = "PuBuGn", direction = 1, limits = c(1e8, 1.5e9),
  #                      trans = "log10",
  #                      oob = scales::squish) +

#### scale for food percentage (d)
# scale_fill_gradientn(colors = pltmn, limits = c(0.5, 0.9), oob = scales::squish)+ # (1)

theme(panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA),
      legend.title = element_blank())+
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100))

# save each figure individually
ggsave("C:/Users/Tom/Desktop/Heatmap_d_foodpct.png", width = 5, height = 4, 
       units = "in", dpi = 500, bg = "transparent")
