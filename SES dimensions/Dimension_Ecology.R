library(ggplot2); library(raster); library(dplyr); library(magrittr) 
library(reshape2); library(spatstat); library(e1071); library(rgdal); library(gdalUtils)
library(tmap); library(tmaptools)
if(!is.null(dev.list())) dev.off(); cat("\014");rm(list=ls())
setwd("Z:/2.active_projects/Xander/! GIS_files")

#########  import core data
TWS_trend <- raster("./Rodell_SourceData/Rodell_etal_0d05.tif") # Rodell et al. (2018) TWS trends
Global200FW <- raster("./Ecoregions/Global200/g200_fw_53.tif") # Global 200 freshwater ecoregions
Global200TERR <- raster("./Ecoregions/Global200/g200_terr_142.tif") # Global 200 freshwater ecoregions
deG_pumping <- raster("./deGraaf_pumping/headdrop2limit_hydr06.tif")
VSI_SensAETW <- raster(paste("Z:/2.active_projects/Xander/VSI/rasters/", "SensAETW", ".tif", sep="")) 
GridArea <- raster("./R_gis_exports/WGS84_cellArea_0d05res.tif") # Grid area (km2) for WGS84 projection
EQ_regions <- raster("./Earthquakes/EQ_rm_0d05.tif") # earthquake regions to remove
GADM_lvl0 <- raster("./GADM/GADM_level0_0d05.tif") # GADM national dataset
FPU_grid <- raster("./WaterScarcityAtlas/fpu30_hyde_compatible_v20d05.tif")

######### Extend vegetation sensitivity to full global extent and normalize by p95
VSI_SensAETW_extended <- raster::resample(VSI_SensAETW, GridArea, method = "ngb")
df <- cbind(as.data.frame(VSI_SensAETW_extended), as.data.frame(GridArea)) %>% set_colnames(c("VSI", "GA"))
VSI_norm <- VSI_SensAETW_extended/weighted.quantile(df$VSI, df$GA, 0.95, na.rm = T)
VSI_norm[VSI_norm > 1] <- 1

######### Normalize GW depletion depth to EFN threshold to 0-1 indicator
pump_ind <- raster(GridArea); pump_ind[] <- 0
## resample deGraaf to 0.05 degree resolution
EFN_d <- raster::resample(deG_pumping, GridArea, method = "bilinear")
pump_ind[EFN_d <= 0.25]                   <- 0.9 +  (((0.25 - EFN_d[EFN_d<= 0.25])/0.25)*0.1)
pump_ind[EFN_d <= 1 & EFN_d > 0.25] <- 0.8 +  (((1 - EFN_d[EFN_d <= 1 & EFN_d > 0.25])/0.75)*0.1)
pump_ind[EFN_d <= 2 & EFN_d > 1]   <- 0.7 +  (((2 - EFN_d[EFN_d <= 2 & EFN_d > 1])/1)*0.1)
pump_ind[EFN_d <= 4 & EFN_d > 2]   <- 0.5 +  (((4 - EFN_d[EFN_d <= 4 & EFN_d > 2])/2)*0.2)
pump_ind[EFN_d <= 8 & EFN_d > 4]   <- 0.3 +  (((8 - EFN_d[EFN_d <= 8 & EFN_d > 4])/4)*0.2)
pump_ind[EFN_d <= 16 & EFN_d > 8]  <- 0.1 +  (((16 - EFN_d[EFN_d <= 16 & EFN_d > 8])/8)*0.2)
pump_ind[EFN_d <= 32 & EFN_d > 16] <- 0 +  (((32 - EFN_d[EFN_d <= 32 & EFN_d > 16])/16)*0.1)
pump_ind[EFN_d > 32]  <- 0   

### create sensitivity dataset
VSI_norm[is.na(VSI_norm)] <- 0
Eco_sens <- stack(VSI_norm, pump_ind)
Eco_sens_i <- calc(Eco_sens, fun = mean)

### create priority dataset
G200_ind <- raster(GridArea); Global200TERR[is.na(Global200TERR)] <- 0; Global200FW[is.na(Global200FW)] <- 0
Global200TERR[Global200TERR >= 1] <- 1; Global200FW[Global200FW >= 1] <- 1
Eco_pri <- stack(Global200TERR, Global200FW)
Eco_pri_i <- calc(Eco_pri, fun = max)

### Create overall eco index
Eco_ind <- stack(Eco_sens_i, Eco_pri_i)
Eco_ind_i <- calc(Eco_ind, fun = mean)

# Remove EQ and ocaens from data
TWS_trend[EQ_regions[] == 1 | FPU_grid[] == 0] <- NA
Eco_ind_i[EQ_regions[] == 1 | FPU_grid[] == 0] <- NA

# create TWS scaler
TWS_scaler <- TWS_trend/2; TWS_scaler[TWS_scaler > 1] <- 1; TWS_scaler[TWS_scaler < -1] <- -1

# Create ecology map
Eco_map <- Eco_ind_i*TWS_scaler

writeRaster(Eco_ind_i, filename=paste("C:/Users/Tom/Desktop/FinalRun/GTiff_EcoIndicator", ".tif", sep=""),
            format="GTiff", overwrite=TRUE)

writeRaster(Eco_map, filename=paste("C:/Users/Tom/Desktop/FinalRun/GTiff_ecoimpact_pumping_g200_binary", ".tif", sep=""),
            format="GTiff", overwrite=TRUE)

# make map plot
data(World)
Coastlines <- readOGR(dsn = "./NaturalEarth/Coastlines", layer = "ne_10m_coastline")
Lakes_ply <- readOGR(dsn=paste("./Lakes", sep=""), layer="ne_10m_lakes")
Lakes_ply$scalerank %<>% as.numeric()
Lakes_large <- subset(Lakes_ply, scalerank < 4)

library(wesanderson) #wes_palette("Rushmore1", 3, type = "discrete") 
pri_plt <- c("grey",RColorBrewer::brewer.pal(3, "YlGnBu")); pri_plt <- c(pri_plt[1],pri_plt[3], pri_plt[4])
sens_plt <- wes_palette("Zissou1", 100, type = "continuous") 

Eco_pri_i[EQ_regions[] == 1 | FPU_grid[] == 0] <- NA
Eco_sens_i[EQ_regions[] == 1 | FPU_grid[] == 0] <- NA
Eco_ind_i[EQ_regions[] == 1 | FPU_grid[] == 0] <- NA
pump_ind[EQ_regions[] == 1 | FPU_grid[] == 0] <- NA
VSI_norm[EQ_regions[] == 1 | FPU_grid[] == 0] <- NA


map <- 
  tm_shape(Eco_ind_i, projection = "robin") +  tm_raster(style = "cont", palette = sens_plt,
                                                          midpoint = 0.5, breaks = c(0, 1)) +
  tm_shape(World) +  tm_borders(lwd = 0.7) +
  # tm_shape(Coastlines) +  tm_lines("grey50", lwd = 0.1) +
  tm_shape(Lakes_large) + tm_borders("black", lwd = 0.5) +
  tm_style("white", legend.show = F, earth.boundary = c(-180, -60, 180, 88), earth.boundary.color = "white",
           space.color = "white", legend.frame = F, frame = F)
map

# save map plot
tmap_save(map, "C:/Users/Tom/Desktop/EcoIndicator_combined_g200_binary.png", dpi = 500,
          outer.margins = 0.01, height = 3, units = "in")

############################## create LHS graph
TWS_trend <- raster("./Rodell_SourceData/Rodell_etal_0d05.tif") # Rodell et al. (2018) TWS trends
Global200FW <- raster("./Ecoregions/Global200/g200_fw_53.tif") # Global 200 freshwater ecoregions
Global200TERR <- raster("./Ecoregions/Global200/g200_terr_142.tif") # Global 200 freshwater ecoregions
GridArea <- raster("./R_gis_exports/WGS84_cellArea_0d05res.tif") # Grid area (km2) for WGS84 projection


fw_TWSxGA <- raster::zonal(TWS_trend*GridArea, Global200FW, fun = 'sum', na.rm = T, digits = 3)
fw_GA <- raster::zonal(GridArea, Global200FW, fun = 'sum', na.rm = T, digits = 3)

terr_TWSxGA <- raster::zonal(TWS_trend*GridArea, Global200TERR, fun = 'sum', na.rm = T, digits = 3)
terr_GA <- raster::zonal(GridArea, Global200TERR, fun = 'sum', na.rm = T, digits = 3)

fw_TWSxGA <- fw_TWSxGA %>% as.data.frame() %>% set_colnames(c("FW.id", "w.sum"))
fw_GA <- fw_GA %>% as.data.frame() %>% set_colnames(c("FW.id", "area"))

terr_TWSxGA <- terr_TWSxGA %>% as.data.frame() %>% set_colnames(c("TERR.id", "w.sum"))
terr_GA <- terr_GA %>% as.data.frame() %>% set_colnames(c("TERR.id", "area"))


fw_df <- merge(fw_TWSxGA, fw_GA, by = "FW.id", all = T)
terr_df <- merge(terr_TWSxGA, terr_GA, by = "TERR.id", all = T)


fw_df$meanTWS <- fw_df$w.sum/fw_df$area
terr_df$meanTWS <- terr_df$w.sum/terr_df$area

terr_df <- terr_df[order(-terr_df$meanTWS),]
fw_df <- fw_df[order(-fw_df$meanTWS),]

# set up data frame to fill in with subsequent raster percentile analysis in loop
df_hist <- seq(-10.05, 10.05, by = 0.1) %>% as.data.frame %>% set_names("MidVal")
df_hist$count <- c(rep(NA, nrow(df_hist)))

# need to run through twice, once with terr_df and once with df_df in line 141
for (i in 1:nrow(df_hist)) {
  
  df_hist$Low[i] <- -10.1 + (0.1*(i-1))
  df_hist$High[i] <- df_hist$Low[i] + 0.1
  print(df_hist$Low[i])
  
  df_hist$count[i] <- terr_df %>% filter(meanTWS >= df_hist$Low[i] & meanTWS < df_hist$High[i]) %>% nrow()
}

# need to run above for loop for terr and fw respectively
terr_hist <- df_hist
terr_hist <- terr_hist %>% select(MidVal, count)

fw_hist <- df_hist
fw_hist <- fw_hist %>% select(MidVal, count)

terr_hist$type <- 1; fw_hist$type <- 2

# merge all results, and melt dataframe for plotting
G200.distr <- rbind(fw_hist, terr_hist)
G200.distr$count %<>% as.numeric()
G200.distr$type %<>% as.factor()

a = 0.85
b = 25
fig <- ggplot(G200.distr, aes(x = MidVal, y = count, fill = type)) +
  annotate("rect", xmin = -5,   xmax = -2,   ymin = 0, ymax = b, fill= "#FFA5A5", alpha = a) +
  annotate("rect", xmin = -2,   xmax = -0.5, ymin = 0, ymax = b, fill= "#FFDFDF", alpha = a) +
  annotate("rect", xmin = -0.5, xmax = 0.5,  ymin = 0, ymax = b, fill= "#959595", alpha = a) +
  annotate("rect", xmin = 0.5,  xmax = 2,    ymin = 0, ymax = b, fill= "#F0EDF8", alpha = a) +
  annotate("rect", xmin = 2,    xmax = 3,    ymin = 0, ymax = b, fill= "#AAC2FF", alpha = a) +
  geom_bar(stat = "identity", position="stack", colour = "black") +
  scale_fill_manual(values = c("seagreen", "dodgerblue2")) + 
  scale_y_continuous(position = "right", breaks = seq(0, 30, by = 1), expand = c(0,0),
                     sec.axis = dup_axis(name = element_blank())) +
  scale_x_continuous(limits = c(-5, 3), breaks = seq(-5, 3, by = 1), expand = c(0,0),  
                     labels = c("-5" ,"-4", "-3", "-2", "-1", "0", "1", "2", "3")) +
  theme(panel.background =  element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_line(alpha(colour = "white", 0.3), linetype = "dashed", size = 0.5),
        axis.line = element_line(colour = "black"), panel.ontop = TRUE,
        legend.position = "none") +
  geom_vline(xintercept = 0, size = 1.5, alpha = 0.8) 
fig

# ggsave("C:/Users/Tom/Desktop/FinalRun/Dims_eco_mean.png", 
#        fig, dpi = 500, width = 14, height = 10, bg = "transparent") 

all_hist <- rbind(terr_hist, fw_hist)
library(Weighted.Desc.Stat)
w.skewness(all_hist$MidVal, all_hist$count)
