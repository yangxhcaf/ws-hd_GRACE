library(ggplot2); library(dplyr); library(raster); library(magrittr)

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

####################
## plot histogram ##
####################

hist <- ggplot(Combined_df, aes(x = ID, y = GDP)) +
  geom_bar(stat = "identity", colour = "black", fill = "slategray4") +
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

# save figure
ggsave("C:/Users/Tom/Desktop/GDP_hist.png", hist, dpi = 500, width = 14, height = 10, units = "in", bg = "transparent")

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
#################
# create figure #
#################

# Reclassify GRACE to 0.1 increment bins
GRACE_reclassRanges <- data.frame(low = seq(-40, 6.0, 0.1), high = seq(-39.9, 6.1, 0.1), ReCLASS = seq(1:461))
GRACE_Reclass <- reclassify(TWS_trend, GRACE_reclassRanges)

# Calculate sum of kcal per each reclassified region based on TWS rate of change
FEED_Distr <- zonal(FEED, GRACE_Reclass, sum) %>% as.data.frame() %>% set_colnames(c("ID", "FEED_kcal"))
FOOD_Distr <- zonal(FOOD, GRACE_Reclass, sum) %>% as.data.frame() %>% set_colnames(c("ID", "FOOD_kcal"))
NONFOOD_Distr <- zonal(NONFOOD, GRACE_Reclass, sum) %>% as.data.frame() %>% set_colnames(c("ID", "NONFOOD_kcal"))
KCAL_Global_types <- Reduce(function(x, y) merge(x, y, by = "ID"), list(FOOD_Distr, FEED_Distr, NONFOOD_Distr))

# convert kcal counts into trillions
KCAL_Global_types[,2:4] <- KCAL_Global_types[,2:4]/1e12 
KCAL_Global_types %<>% melt(id.var = "ID")
KCAL_Global_types$variable %<>% as.factor()

fig <- ggplot(KCAL_Global_types, aes(x = ID, y = value, fill = variable)) +
  geom_bar(stat = "identity", position="stack", colour = "black") +
  scale_fill_manual(values = c("palegreen3", "burlywood4", "gray32")) + 
  scale_y_continuous(position = "right", breaks = seq(0, 1750, by = 250), expand = c(0,0)) +
  scale_x_continuous(limits = c(350, 431), breaks = seq(350.5, 430.5, by = 10), expand = c(0,0),  
                     labels = c("-5", "-4", "-3", "-2", "-1", "0", "1", "2", "3")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size= 1.5),
        axis.title = element_text(size = 11, color = "black"),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.major.x = element_line(alpha(colour = "white"), linetype = "dashed", size = 0.5),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(alpha(colour = "white"), linetype = "dashed", size = 0.5),
        axis.line = element_line(colour = "black"), 
        axis.text = element_text(face = "bold", size = 10, color = "black"),
        axis.title.y = element_text(color = "black"), 
        legend.position = "none") +
  geom_vline(xintercept = 400.5, size = 1.5) +
  labs(x=expression(paste("Terrestrial Water Storage Rate of Change","  ","(cm", y^-1,")", sep="")),
       y = "Global Food Production kcal (trillions)\n") 
fig

# ggsave("/Figures/GDP_cdf.png", p1, dpi = 500, width = 6, height = 5.5, bg = "transparent")
