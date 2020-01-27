library(ggplot2)
library(dplyr)
library(raster)

mainDir <- "Z:/2.active_projects/Xander/"

# import all data
TWS_trend <- raster(paste(mainDir, "! GIS_files/GRACE/", "GRACE_coredata", ".tif", sep="")) # Rodell et al. (2018) TWS trends, resampled to 0.05 res
GDP <- raster(paste(mainDir, "! GIS_files/R_gis_exports/", "Kummu_GDP_0d05", ".tif", sep="")) # Kummu et al. (2019) GDP (PPP) dataset
Pop <- raster(paste(mainDir, "! GIS_files/R_gis_exports/", "POP_2015_0d05_UNWPP", ".tif", sep="")) # GWPv4 UNWPP 2015 population dataset 

## Reclassify GRACE to 0.1 increment bins
GRACE_reclassRanges <- data.frame(low = seq(-40, 6.0, 0.1), high = seq(-39.9, 6.1, 0.1), ReCLASS = seq(1:461))
GRACE_Reclass <- reclassify(TWS_trend, GRACE_reclassRanges)

# calculate population and GDP per bin and merge into one dataframe
GDP.per.TWSbin <- zonal(GDP, GRACE_Reclass, fun = "sum", na.rm = TRUE) %>% as.data.frame() %>% set_colnames(c("ID", "GDP"))
POP.per.TWSbin <- zonal(Pop, GRACE_Reclass, fun = "sum", na.rm = TRUE) %>% as.data.frame() %>% set_colnames(c("ID", "POP"))
Combined_df <- merge.default(GDP.per.TWSbin, POP.per.TWSbin, by = "ID")

# cumulatively sum each distribution and normalize with maximum value
Combined_df$GDP_runningSum <- cumsum(Combined_df$GDP)
Combined_df$GDP_cdf <- Combined_df$GDP_runningSum/max(Combined_df$GDP_runningSum)

Combined_df$POP_runningSum <- cumsum(Combined_df$POP)
Combined_df$POP_cdf <- Combined_df$POP_runningSum/max(Combined_df$POP_runningSum)

# plot the CDFs together, highlighting gaps between curves 
p1 <- ggplot(Combined_df, aes(x= ID)) +
  geom_ribbon(data=subset(Combined_df, ID <= 400.5), 
              aes(ymin=GDP_cdf,ymax=POP_cdf), fill="grey", alpha="0.5") +
  geom_ribbon(data=subset(Combined_df, ID > 400.5), 
              aes(ymin=POP_cdf,ymax=GDP_cdf), fill="grey", alpha="0.5") +
  geom_line(aes(y = POP_cdf), colour = "black", lwd = 1.5) + 
  geom_line(aes(y = GDP_cdf), colour = "darkorange2", lwd = 1.5) +
  scale_y_continuous(position = "right", breaks = seq(0, 1, by = 0.1), expand = c(0,0)) +
  scale_x_continuous(limits   = c(350, 431), breaks = seq(360.5, 430.5, by = 10), expand = c(0,0)) +
  theme(panel.background =  element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_line(alpha(colour = "white", 0.3), linetype = "dashed", size = 0.5),
        axis.line.x = element_line(color = "black", size = 1)) +
  # geom_vline(xintercept    = 400.5, size = 1) +
  labs(x=expression(paste("Terrestrial Water Storage Rate of Change","  ","(cm", y^-1,")", sep="")),
       y = "GDP distribution\n") 
p1

# ggsave("/Figures/GDP_cdf.png", p1, dpi = 500, width = 6, height = 5.5, bg = "transparent")

