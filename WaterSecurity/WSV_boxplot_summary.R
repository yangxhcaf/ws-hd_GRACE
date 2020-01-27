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










# Create South Asia subset for normalization
S.Asia_subset <- WSV_df %>% filter(Region == 6) # South Asia is represented by value of 6

# Function to create a new raster that represents the distribution of a selected raster based on the percentiles of a dataframe
PercentileAssignment <- function(QuantileRaster, RawRaster, DIS_Quantile_df){
  QuantileRaster[] <- 0 # make sure product raster begins with no current values
  QuantileRaster[RawRaster > as.numeric(unname(weighted.quantile(DIS_Quantile_df[,1], DIS_Quantile_df[,2], 0.99, na.rm = TRUE)))] <- 1
  for(i in 1:99){
    j = 1 - (0.01*i) # initiate upper bound
    k = 0.99 - (0.01*i) # initiate lower bound
    QuantileRaster[RawRaster <= as.numeric(unname(weighted.quantile(DIS_Quantile_df[,1], DIS_Quantile_df[,2], j, na.rm = TRUE))) & 
                     RawRaster > as.numeric(unname(weighted.quantile(DIS_Quantile_df[,1], DIS_Quantile_df[,2], k, na.rm = TRUE)))] <- j
  }
  return(QuantileRaster)
}

####################################################################################
# Reclassify the WSV scores based on the percentile distribution within South Asia #
####################################################################################

WSV_SAS_s <- S.Asia_subset %>% select(WSV_str, Area) # setting up South Asia dataframe to conform with function need
WSV_SAS_f <- S.Asia_subset %>% select(WSV_fld, Area) # setting up South Asia dataframe to conform with function need

# Initialize rasters to populate with the percentile distributions
WSV_stress_quantile <- raster(GridArea)
WSV_flood_quantile <- raster(GridArea)

# create percentile rasters
WSV_stress_quantile <- PercentileAssignment(WSV_stress_quantile, WSV_stress, WSV_SAS_s)
WSV_flood_quantile <- PercentileAssignment(WSV_flood_quantile, WSV_flood, WSV_SAS_f)

# remove ocean regions
WSV_stress_quantile[is.na(WorldRegions)] <- NA
WSV_flood_quantile[is.na(WorldRegions)] <- NA

####################################################################################
# Analyze each world region for presentation #
####################################################################################

Regional_WSV <- cbind(as.data.frame(WSV_stress_quantile), as.data.frame(WSV_flood_quantile),
                      as.data.frame(WorldRegions), as.data.frame(GridArea)) %>% set_colnames(c("Stress", "Flood", "Region", "Area"))
Regional_WSV <- Regional_WSV[complete.cases(Regional_WSV),]
Regional_WSV$Region %<>% as.factor()

# determine distribution statistics for the coupled raster grids
Reg.Result_AreaWeighted <- Regional_WSV %>% 
  group_by(Region) %>%
  summarise(STR_median = weighted.quantile(Stress, Area, probs = 0.50, na.rm = TRUE),
            STR_p25 = weighted.quantile(Stress, Area, probs = 0.25, na.rm = TRUE),
            STR_p75 = weighted.quantile(Stress, Area, probs = 0.75, na.rm = TRUE),
            STR_p5 = weighted.quantile(Stress, Area, probs = 0.05, na.rm = TRUE),
            STR_p95 = weighted.quantile(Stress, Area, probs = 0.95, na.rm = TRUE),
            FLD_median = weighted.quantile(Flood, Area, probs = 0.50, na.rm = TRUE),
            FLD_p25 = weighted.quantile(Flood, Area, probs = 0.25, na.rm = TRUE),
            FLD_p75 = weighted.quantile(Flood, Area, probs = 0.75, na.rm = TRUE),
            FLD_p5 = weighted.quantile(Flood, Area, probs = 0.05, na.rm = TRUE),
            FLD_p95 = weighted.quantile(Flood, Area, probs = 0.95, na.rm = TRUE))





# 
summ_DF_stress <- data.frame(x= summ_WSV_stress_quantile$MAgPIE.id, min=summ_WSV_stress_quantile$WeightedMedian, 
                             low=summ_WSV_stress_quantile$WeightedMedian, 
                             mid = summ_WSV_stress_quantile$WeightedMedian, 
                             top=summ_WSV_stress_quantile$Weightedp75, max= summ_WSV_stress_quantile$WeightedpHIGH)
summ_DF_stress$x %<>% as.factor()
summ_DF_stress$WSVtype <- "stress"

summ_DF_flood <- data.frame(x= summ_WSV_flood_quantile$MAgPIE.id, min=summ_WSV_flood_quantile$WeightedMedian, 
                            low=summ_WSV_flood_quantile$WeightedMedian, 
                            mid = summ_WSV_flood_quantile$WeightedMedian, 
                            top=summ_WSV_flood_quantile$Weightedp75, max= summ_WSV_flood_quantile$WeightedpHIGH)
summ_DF_flood$x %<>% as.factor()
summ_DF_flood$WSVtype <- "Flood"

summ_DF <- rbind(summ_DF_stress, summ_DF_flood)
summ_DF$min %<>% round(digits = 2)
summ_DF$low %<>% round(digits = 2)
summ_DF$mid %<>% round(digits = 2)
summ_DF$top <- ifelse(summ_DF$WSVtype == "Flood", trunc(summ_DF$top*100)/100, summ_DF$top)

# 1 - NAM | 2 - FSU | 3 - POECD | 4 - PAS | 5 - CPA | 6 - SAS | 7 - SSA | 8 _ MENA | 9 - WEUR | 10 - LAM 
FigInsert <- ggplot(summ_DF, aes(x = x, ymin = min, lower = low, middle = mid, upper = top, ymax = max, fill = factor(WSVtype))) +
  # scale_x_discrete(limsits=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")) +
  geom_boxplot(stat = "identity") +
  # scale_y_continuous(breaks = seq(-3, 3, by = 1), expand = c(0,0)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size= 1.5),
        axis.title = element_text(size = 11, color = "black"),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        # panel.grid.major.y = element_line(alpha(colour = "white"), linetype = "dashed", size = 0.5),
        axis.line = element_line(colour = "black"), 
        axis.text = element_text(face = "bold", size = 10, color = "black"),
        axis.title.y = element_text(color = "black"))+
  scale_fill_manual(values = c("dodgerblue4","firebrick3"))+
  coord_flip(ylim = c(0, 1))
FigInsert

ggsave("C:/Users/Tom/Desktop/SAS_normalize_WSVresults.png", FigInsert, dpi = 500, width = 10, height = 5, bg = "transparent")
