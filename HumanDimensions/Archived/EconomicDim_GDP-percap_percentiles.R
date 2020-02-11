library(ggplot2)
library(dplyr)
library(raster)

mainDir <- "Z:/2.active_projects/Xander/"

# import all data
TWS_trend <- raster(paste(mainDir, "! GIS_files/GRACE/", "GRACE_coredata", ".tif", sep="")) # Rodell et al. (2018) TWS trends, resampled to 0.05 res
GDP.ppp <- raster(paste(mainDir, "! GIS_files/R_gis_exports/", "Kummu_GDP_0d05", ".tif", sep="")) # Kummu et al. (2018) GDP at PPP resampled to 0.05 res
HumPop <- raster(paste(mainDir, "! GIS_files/R_gis_exports/", "POP_2015_0d05", ".tif", sep="")) # WGPv4 corrected to UN pop count, resampled to 0.05 res
GridArea <- raster(paste(mainDir, "! GIS_files/R_gis_exports/", "WGS84_cellArea_0d05res", ".tif", sep="")) # Grid area (km2) for WGS84 projection

# calculate GDP.ppp per capita in each grid cell
GDP.ppp.perCap <- GDP.ppp/HumPop
GDP.ppp.perCap[is.na(HumPop) | HumPop == 0] <- NA # exclude areas where no population

# reclassify GRACE matrix into 0.1 cm/yr increment bins for percentile analysis
Low <- seq(-35.0, 6.5, by = 0.5)
High <- seq(-34.5, 7.0, by = 0.5)
lab <- seq(-34.75, 6.75, by = 0.5)
reclassMatrix <- cbind(Low, High, lab) %>% as.matrix() 
TWS_bins <- reclassify(TWS_trend, reclassMatrix)

# identify range of TWS trends that GDP is situated in
TWS_bins[is.na(GDP.ppp)] <- NA
minV <- minValue(TWS_bins)
maxV <- maxValue(TWS_bins)

# set up data frame to fill in with subsequent raster analysis in loop
df <- seq(minV, maxV, by = 0.5) %>% as.data.frame %>% set_names("MidVal")
df$p5 <- c(rep(NA, nrow(df)))
df$p25 <- c(rep(NA, nrow(df)))
df$p50 <- c(rep(NA, nrow(df)))
df$p75 <- c(rep(NA, nrow(df)))
df$p95 <- c(rep(NA, nrow(df)))

for (i in 1:nrow(df)) {
  
  tempRas_GDPppp.pc <- raster(GridArea)
  tempRas_Pop <- raster(GridArea)
  tempRas_GDPppp.pc[] <- NA
  tempRas_Pop[] <- NA
  
  Val <- minV + (0.5*(i-1))
  print(Val)
  
  tempRas_GDPppp.pc[TWS_bins == Val] <- GDP.ppp.perCap[TWS_bins == Val]
  tempRas_Pop[TWS_bins == Val] <- HumPop[TWS_bins == Val]
  
  check <- sum(tempRas_GDPppp.pc[], na.rm = TRUE)
  
  if (is.na(check) | check == 0) {
    df$p5[i]  <- 0
    df$p25[i] <- 0
    df$p50[i] <- 0
    df$p75[i] <- 0
    df$p95[i] <- 0
  } 
  else {
    temp_df_a <- tempRas_GDPppp.pc %>% as.data.frame() %>% set_colnames(c("GDP"))
    temp_df_b <- tempRas_Pop %>%  as.data.frame() %>% set_colnames(c("Pop") )
    temp_df <- cbind(temp_df_a, temp_df_b)
    
    df$p5[i]  <- weighted.quantile(temp_df$GDP, temp_df$Pop, 0.05, na.rm=TRUE)
    df$p25[i] <- weighted.quantile(temp_df$GDP, temp_df$Pop, 0.25, na.rm=TRUE)
    df$p50[i] <- weighted.quantile(temp_df$GDP, temp_df$Pop, 0.50, na.rm=TRUE)
    df$p75[i] <- weighted.quantile(temp_df$GDP, temp_df$Pop, 0.75, na.rm=TRUE)
    df$p95[i] <- weighted.quantile(temp_df$GDP, temp_df$Pop, 0.95, na.rm=TRUE)
  }
}

# plot results 
df_sub <- df %>% filter(df$MidVal > -3.5 & df$MidVal < 3.5)

# create splines for each data column to smooth lines for figure presentation
spline_p5 <- as.data.frame(spline(df_sub$MidVal, df_sub$p5)) %>% set_colnames(c("x", "p5"))
spline_p25 <- as.data.frame(spline(df_sub$MidVal, df_sub$p25)) %>% set_colnames(c("x", "p25"))
spline_p50 <- as.data.frame(spline(df_sub$MidVal, df_sub$p50)) %>% set_colnames(c("x", "p50"))
spline_p75 <- as.data.frame(spline(df_sub$MidVal, df_sub$p75)) %>% set_colnames(c("x", "p75"))
spline_p95 <- as.data.frame(spline(df_sub$MidVal, df_sub$p95)) %>% set_colnames(c("x", "p95"))
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
  coord_cartesian(xlim = c(-3, 3), ylim = c(20, 75000), expand = 0) +
  scale_y_continuous(trans='log10') +
  theme(panel.background =  element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_line(alpha(colour = "white", 0.3), linetype = "dashed", size = 0.5),
        axis.line = element_blank(),
        legend.position = "none") +
  # geom_vline(xintercept = 0, size = 1) +
  labs(x=expression(paste("Terrestrial Water Storage Rate of Change","  ","(cm", y^-1,")", sep="")),
       y = "GDP per capita (purchasing power parity)") 
fig

ggsave("C:/Users/Tom/Desktop/Population_distr.png", fig, dpi = 500, width = 7, height = 5, bg = "transparent")

