library(raster)
library(dplyr)
library(magrittr)
library(spatstat)
library(tidyr)
library(rlang)
library(ggplot2)
library(forcats)


mainDir <- "Z:/2.active_projects/Xander/"

# import all data
GridArea <- raster(paste(mainDir, "! GIS_files/R_gis_exports/", "WGS84_cellArea_0d05res", ".tif", sep="")) # Grid area (km2) for WGS84 projection
WSV_flood <- raster(paste(mainDir, "! GIS_files/R_gis_exports/", "WSV_flood-Varis", ".tif", sep="")) # flood water security vulnerability
WSV_stress <- raster(paste(mainDir, "! GIS_files/R_gis_exports/", "WSV_stress-Varis", ".tif", sep="")) # stress water security vulnerability
WorldRegions <- raster(paste(mainDir, "! GIS_files/WorldRegions/", "MAgPIE_worldregions", ".tif", sep="")) # stress water security vulnerability

# create data frames for area weighted quantiles to be calculated with
WSV_df <- cbind(as.data.frame(WSV_stress), as.data.frame(WSV_flood), as.data.frame(GridArea), as.data.frame(WorldRegions)) %>% 
  set_colnames(c("WSV_str", "WSV_fld", "Area", "Region"))
WSV_df <- WSV_df[complete.cases(WSV_df),]
WSV_df$Region %<>% as.factor()

# determine distribution statistics for the coupled raster grids
Reg.Result_AreaWeighted <- WSV_df %>% 
  group_by(Region) %>%
  summarise(STR_median = weighted.quantile(WSV_str, Area, probs = 0.50, na.rm = TRUE),
            STR_p25 = weighted.quantile(WSV_str, Area, probs = 0.25, na.rm = TRUE),
            STR_p75 = weighted.quantile(WSV_str, Area, probs = 0.75, na.rm = TRUE),
            STR_p5 = weighted.quantile(WSV_str, Area, probs = 0.05, na.rm = TRUE),
            STR_p95 = weighted.quantile(WSV_str, Area, probs = 0.95, na.rm = TRUE),
            FLD_median = weighted.quantile(WSV_fld, Area, probs = 0.50, na.rm = TRUE),
            FLD_p25 = weighted.quantile(WSV_fld, Area, probs = 0.25, na.rm = TRUE),
            FLD_p75 = weighted.quantile(WSV_fld, Area, probs = 0.75, na.rm = TRUE),
            FLD_p5 = weighted.quantile(WSV_fld, Area, probs = 0.05, na.rm = TRUE),
            FLD_p95 = weighted.quantile(WSV_fld, Area, probs = 0.95, na.rm = TRUE)) %>%  as.data.frame()

# 1 - NAM | 2 - FSU | 3 - POECD | 4 - PAS | 5 - CPA | 6 - SAS | 7 - SSA | 8 _ MENA | 9 - WEUR | 10 - LAM 
Stress.fig <- ggplot(Reg.Result_AreaWeighted, aes(x = fct_reorder(Region, STR_median), 
                                                 ymin = STR_p5, lower = STR_p25, middle = STR_median, 
                                                 upper = STR_p75, ymax = STR_p95)) +
  # scale_x_discrete(limsits=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
  geom_boxplot(stat = "identity", fill = "darkslategrey", alpha = "0.5") +
  # scale_y_continuous(breaks = seq(-3, 3, by = 1), expand = c(0,0)) +
  theme(panel.background =  element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_line(alpha(colour = "white", 0.3), linetype = "dashed", size = 0.5),
        axis.line.x = element_line(color = "black", size = 1)) +
  scale_fill_manual(values = c("dodgerblue4","firebrick3"))+
  coord_flip(ylim = c(-1, 1))
Stress.fig

Flood.fig <- ggplot(Reg.Result_AreaWeighted, aes(x = fct_reorder(Region, FLD_median), 
                                                 ymin = FLD_p5, lower = FLD_p25, middle = FLD_median, 
                                                 upper = FLD_p75, ymax = FLD_p95)) +
  # scale_x_discrete(limsits=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
  geom_boxplot(stat = "identity", fill = "darkslategrey", alpha = "0.5") +
  # scale_y_continuous(breaks = seq(-3, 3, by = 1), expand = c(0,0)) +
  theme(panel.background =  element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_line(alpha(colour = "white", 0.3), linetype = "dashed", size = 0.5),
        axis.line.x = element_line(color = "black", size = 1)) +
  scale_fill_manual(values = c("dodgerblue4","firebrick3"))+
  coord_flip(ylim = c(-1, 1))
Flood.fig

ggsave("C:/Users/Tom/Desktop/WSV_stress_bxplt.png", Stress.fig, dpi = 500, width = 5, height = 5, bg = "transparent")
ggsave("C:/Users/Tom/Desktop/WSV_flood_bxplt.png", Flood.fig, dpi = 500, width = 5, height = 5, bg = "transparent")
