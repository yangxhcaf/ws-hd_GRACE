library(ggplot2)
library(dplyr)
library(raster)

mainDir <- "Z:/2.active_projects/Xander/"

# import all data
TWS_trend <- raster(paste(mainDir, "! GIS_files/GRACE/", "GRACE_coredata", ".tif", sep="")) # Rodell et al. (2018) TWS trends, resampled to 0.05 res
Crp_DENS <- raster(paste(mainDir, "! GIS_files/R_gis_exports/", "CroplandDensity_0d05", ".tif", sep="")) # Ramankutty cropland density (2000)
GridArea <- raster(paste(mainDir, "! GIS_files/R_gis_exports/", "WGS84_cellArea_0d05res", ".tif", sep="")) # Grid area (km2) for WGS84 projection

# reclassify cropland density matrix into 0.05 bins for analysis
Low <- seq(0, 0.95, by = 0.05)
High <- seq(0.05, 1.00, by = 0.05)
lab <- seq(0.025, 0.975, by = 0.05)
reclassMatrix <- cbind(Low, High, lab) %>% as.matrix() 
Density_bins <- reclassify(Crp_DENS, reclassMatrix)
Density_bins[Density_bins == 0] <- NA

# set up data frame to fill in with subsequent raster analysis in loop
df <- seq(0.025, 0.975, by = 0.05) %>% as.data.frame %>% set_names("MidVal")
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
  
  Val <- 0.025 + (0.05*(i-1))
  print(Val)
  
  tempRas_TWStrend[Density_bins == Val] <- TWS_trend[Density_bins == Val]
  tempRas_GridArea[Density_bins == Val] <- GridArea[Density_bins == Val]
  
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
spline_p5 <- as.data.frame(spline(df$MidVal, df$p5)) %>% set_colnames(c("x", "p5"))
spline_p25 <- as.data.frame(spline(df$MidVal, df$p25)) %>% set_colnames(c("x", "p25"))
spline_p50 <- as.data.frame(spline(df$MidVal, df$p50)) %>% set_colnames(c("x", "p50"))
spline_p75 <- as.data.frame(spline(df$MidVal, df$p75)) %>% set_colnames(c("x", "p75"))
spline_p95 <- as.data.frame(spline(df$MidVal, df$p95)) %>% set_colnames(c("x", "p95"))

spline_df <- cbind(spline_p5, spline_p25, spline_p50, spline_p75, spline_p95)
spline_df <- spline_df[,-c(3,5,7,9)]

# plot results 
fig <- 
  ggplot() +
  geom_line(data = spline_df, aes(x = x, y = p5), color = "firebrick", lwd = 1) +
  geom_line(data = spline_df, aes(x = x, y = p25), color = "darkorange2", lwd = 1) +
  geom_line(data = spline_df, aes(x = x, y = p50), color = "darkslategrey", lwd = 1.5) +
  geom_line(data = spline_df, aes(x = x, y = p75), color = "steelblue", lwd = 1) +
  geom_line(data = spline_df, aes(x = x, y = p95), color = "dodgerblue4", lwd = 1) +
  geom_ribbon(aes(x = spline_df$x, ymin = spline_df$p5, ymax = spline_df$p25), fill = "firebrick", alpha = "0.1") +
  geom_ribbon(aes(x = spline_df$x, ymin = spline_df$p25, ymax = spline_df$p50), fill = "darkorange2", alpha = "0.1") +
  geom_ribbon(aes(x = spline_df$x, ymin = spline_df$p50, ymax = spline_df$p75), fill = "steelblue", alpha = "0.1") +
  geom_ribbon(aes(x = spline_df$x, ymin = spline_df$p75, ymax = spline_df$p95), fill = "dodgerblue4", alpha = "0.1") +
  theme(panel.background =  element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_line(alpha(colour = "white", 0.3), linetype = "dashed", size = 0.5),
        axis.line.x = element_line(color = "black", size = 1)) +
  coord_flip(xlim = c(0.05,1.0), ylim = c(-3, 3), expand = c(0))
  # labs(x=expression(paste("Terrestrial Water Storage Rate of Change","  ","(cm", y^-1,")", sep="")),
  #      y = "GDP per capita (purchasing power parity)") 
fig

ggsave("C:/Users/Tom/Desktop/Cropland_density.png", fig, dpi = 500, width = 4, height = 5.5, bg = "transparent")
