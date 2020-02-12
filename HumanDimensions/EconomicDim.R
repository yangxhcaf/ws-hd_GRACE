library(ggplot2); library(raster); library(dplyr); library(magrittr) 
library(e1071); library(reshape2);library(spatstat); library(RColorBrewer)

mainDir <- "Z:/2.active_projects/Xander/"

# import all data
TWS_trend <- raster(paste(mainDir, "! GIS_files/GRACE/", "GRACE_coredata", ".tif", sep="")) # Rodell et al. (2018) TWS trends, resampled to 0.05 res
GDP_ppp <- raster(paste(mainDir, "! GIS_files/GDP/Kummu/", "GDP_PPP_2015_0d05", ".tif", sep="")) # Kummu et al. (2019) GDP (PPP) dataset for 2015
GDPpc <- raster(paste(mainDir, "! GIS_files/GDP/Kummu/", "GDPpc_2015_0d05", ".tif", sep="")) # Kummu et al. (2019) GDP (PPP) dataset for 2015
Pop <- raster(paste(mainDir, "! GIS_files/R_gis_exports/", "POP_2015_0d05_UNWPP", ".tif", sep="")) # GWPv4 UNWPP 2015 population dataset 


# classify GDPpc based on IMF classes
IMF_reclassRanges <- data.frame(low = c(0, 500, 2500, 10000, 25000), high = c(500, 2500, 10000, 25000, 1e12), ReCLASS = seq(1:5))
GDPpc_IMFclass <- reclassify(GDPpc, IMF_reclassRanges)

# split GDP_PPP raster by IMF GDP_pc classes
# begin by initializing a raster for each class with same extent and resolution as rest of files
GDP.verylow  <- raster(TWS_trend); GDP.verylow[]  <- 0
GDP.low      <- raster(TWS_trend); GDP.low[]      <- 0
GDP.medium   <- raster(TWS_trend); GDP.medium[]   <- 0
GDP.high     <- raster(TWS_trend); GDP.high[]     <- 0
GDP.veryhigh <- raster(TWS_trend); GDP.veryhigh[] <- 0
GDP.NoDat    <- raster(TWS_trend); GDP.NoDat[]    <- 0

# populate population rasters per water stress class
GDP.verylow[GDPpc_IMFclass == 1]     <- GDP_ppp[GDPpc_IMFclass == 1]
GDP.low[GDPpc_IMFclass == 2]     <- GDP_ppp[GDPpc_IMFclass == 2]
GDP.medium[GDPpc_IMFclass == 3]     <- GDP_ppp[GDPpc_IMFclass == 3]
GDP.high[GDPpc_IMFclass == 4]     <- GDP_ppp[GDPpc_IMFclass == 4]
GDP.veryhigh[GDPpc_IMFclass == 5]     <- GDP_ppp[GDPpc_IMFclass == 5]
GDP.NoDat[GDPpc_IMFclass !=  1 & GDPpc_IMFclass !=  2 & GDPpc_IMFclass !=  3 & GDPpc_IMFclass !=  4 & GDPpc_IMFclass !=  5] <-
  GDP_ppp[GDPpc_IMFclass !=  1 & GDPpc_IMFclass !=  2 & GDPpc_IMFclass !=  3 & GDPpc_IMFclass !=  4 & GDPpc_IMFclass !=  5]


# Reclassify emerging trend into 0.1 cm/yr increment bins
ReclassRanges <- data.frame(low = seq(-40, 6.0, 0.1), high = seq(-39.9, 6.1, 0.1), ReCLASS = seq(1:461))
EmergingTrend_Reclass <- reclassify(TWS_trend, ReclassRanges)

# Calculate GDP distribution for each water stress class
GDP.verylow.distr  <- zonal(GDP.verylow, EmergingTrend_Reclass, sum)      %>% as.data.frame() %>% set_colnames(c("ID", "VeryLow"))
GDP.low.distr      <- zonal(GDP.low, EmergingTrend_Reclass, sum)   %>% as.data.frame() %>% set_colnames(c("ID", "Low"))
GDP.medium.distr   <- zonal(GDP.medium, EmergingTrend_Reclass, sum)  %>% as.data.frame() %>% set_colnames(c("ID", "Medium"))
GDP.high.distr     <- zonal(GDP.high, EmergingTrend_Reclass, sum)     %>% as.data.frame() %>% set_colnames(c("ID", "High"))
GDP.veryhigh.distr <- zonal(GDP.veryhigh, EmergingTrend_Reclass, sum)   %>% as.data.frame() %>% set_colnames(c("ID", "VeryHigh"))
GDP.NoDat.distr    <- zonal(GDP.NoDat, EmergingTrend_Reclass, sum)     %>% as.data.frame() %>% set_colnames(c("ID", "NoData"))

# merge all results, and melt dataframe for plotting
GDP.distr <- Reduce(function(x, y) merge(x, y, by = "ID"), list(GDP.verylow.distr, GDP.low.distr, GDP.medium.distr ,
                                                                GDP.high.distr, GDP.veryhigh.distr, GDP.NoDat.distr))
GDP.distr %<>% melt(id.var = "ID")

## plot histogram results
# For reference: bin ID vs emerging trend:
# 390.5 = -1; 400.5 = 0; 410.5 = 1
hist <- ggplot(GDP.distr, aes(x = ID, y = value, fill = variable)) +
  geom_bar(stat = "identity", position="stack", colour = "black") +
  scale_fill_manual(values = c("#bc2d18", "#d66a22", "#f2bc8f", "#9cd1bb", "#46a7af", "#d5d7da")) +
  scale_y_continuous(position = "right", breaks = seq(0, 1e13, by = 1e12), expand = c(0,0)) +
  scale_x_continuous(limits = c(350, 431), breaks = seq(350.5, 430.5, by = 10), expand = c(0,0),  
                     labels = c("-5", "-4", "-3", "-2", "-1", "0", "1", "2", "3")) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size= 1.5),
        axis.title = element_text(size = 11, color = "black"),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major.x = element_line(alpha(colour = "white"), linetype = "dashed", size = 0.5),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(alpha(colour = "white"), linetype = "dashed", size = 0.5),
        axis.line = element_line(colour = "black"), 
        axis.text = element_text(face = "bold", size = 10, color = "black"),
        legend.position = "none") +
  geom_vline(xintercept = 400.5, size = 1.5)
hist

ggsave("C:/Users/Tom/Desktop/GDP_hist.png", hist, dpi = 500, width = 14, height = 10, units = "in", bg = "transparent")

##################
## cdf analysis ##
##################

# calculate population and GDP per bin and merge into one dataframe
GDP.per.TWSbin <- zonal(GDP_ppp, EmergingTrend_Reclass, fun = "sum", na.rm = TRUE) %>% as.data.frame() %>% set_colnames(c("ID", "GDP"))
POP.per.TWSbin <- zonal(Pop, EmergingTrend_Reclass, fun = "sum", na.rm = TRUE) %>% as.data.frame() %>% set_colnames(c("ID", "POP"))
Combined_df <- merge.default(GDP.per.TWSbin, POP.per.TWSbin, by = "ID")

# cumulatively sum each distribution and normalize with maximum value
Combined_df$GDP_runningSum <- cumsum(Combined_df$GDP)
Combined_df$GDP_cdf <- Combined_df$GDP_runningSum/max(Combined_df$GDP_runningSum)

Combined_df$POP_runningSum <- cumsum(Combined_df$POP)
Combined_df$POP_cdf <- Combined_df$POP_runningSum/max(Combined_df$POP_runningSum)

# plot the CDFs together, highlighting gaps between curves 
cdf <- ggplot(Combined_df, aes(x= ID)) +
  geom_line(aes(y = POP_cdf), colour = "red", lwd = 0.75) + 
  geom_line(aes(y = GDP_cdf), colour = "blue", lwd = 0.75) +
  scale_y_continuous(position = "right", breaks = seq(0, 1, by = 0.1), expand = c(0,0)) +
  scale_x_continuous(limits = c(350, 431), breaks = seq(350.5, 430.5, by = 10), expand = c(0,0),  
                     labels = c("-5", "-4", "-3", "-2", "-1", "0", "1", "2", "3")) +
  theme(legend.position = "none",
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        panel.background =  element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks = element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) 
cdf

ggsave("C:/Users/Tom/Desktop/GDP_cdf.png", cdf, dpi = 500, width = 3.615, height = 1.815, units = "in", bg = "transparent")
