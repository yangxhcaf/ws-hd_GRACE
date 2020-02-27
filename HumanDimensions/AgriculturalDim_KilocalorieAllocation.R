library(ggplot2); library(raster); library(dplyr); library(spatstat)
library(reshape2); library(magrittr)

mainDir <- "Z:/2.active_projects/Xander/"

# import all data
TWS_trend <- raster(paste(mainDir, "! GIS_files/Rodell_SourceData/", "Rodell_etal_0d05", ".tif", sep="")) # Rodell et al. (2018) TWS trends
FEED <- raster(paste(mainDir, "! GIS_files/CropAllocationFoodFeedFuel_Geotiff/", "GlbFeedkcal_Resample_0d05", ".tif", sep="")) 
FOOD <- raster(paste(mainDir, "! GIS_files/CropAllocationFoodFeedFuel_Geotiff/", "GlbFoodkcal_Resample_0d05", ".tif", sep="")) 
NONFOOD <- raster(paste(mainDir, "! GIS_files/CropAllocationFoodFeedFuel_Geotiff/", "GlbNonFoodkcal_Resample_0d05", ".tif", sep="")) 

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

# plotting variables
a <- 0.25
b <- 1600

fig <- ggplot(KCAL_Global_types, aes(x = ID, y = value, fill = variable))  +
  geom_rect(data=NULL,aes(xmin=350.5, xmax=380.5, ymin=0, ymax=b), fill="#FFA5A5", alpha = a) +
  geom_rect(data=NULL,aes(xmin=380.5, xmax=395.5, ymin=0, ymax=b), fill="#FFDFDF", alpha = a) +
  geom_rect(data=NULL,aes(xmin=395.5, xmax=405.5, ymin=0, ymax=b), fill="#959595", alpha = 0.1*a) +
  geom_rect(data=NULL,aes(xmin=405.5, xmax=420.5, ymin=0, ymax=b), fill="#F0EDF8", alpha = a) +
  geom_rect(data=NULL,aes(xmin=420.5, xmax=430.5, ymin=0, ymax=b), fill="#AAC2FF", alpha = a) +
  geom_bar(stat = "identity", position="stack", colour = "black") +
  scale_fill_manual(values = c("palegreen3", "burlywood4", "gray32")) + 
  scale_y_continuous(position = "right", breaks = seq(0, 1750, by = 250), expand = c(0,0)) +
  scale_x_continuous(limits = c(350.5, 430.5), breaks = seq(350.5, 430.5, by = 10), expand = c(0,0),  
                     labels = c("-5", "-4", "-3", "-2", "-1", "0", "1", "2", "3")) +
  theme(panel.background =  element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_line(alpha(colour = "white", 0.3), linetype = "dashed", size = 0.5),
        axis.line = element_line(colour = "black"), panel.ontop = TRUE,
        legend.position = "none") +
  geom_vline(xintercept = 400.5, size = 1.5) +
  labs(x=expression(paste("Terrestrial Water Storage Rate of Change","  ","(cm", y^-1,")", sep="")),
       y = "Global Food Production kcal (trillions)\n") 
fig; # ggsave("C:/Users/Tom/Desktop/Ag_distribution.png", fig, dpi = 500, width = 14, height = 10, bg = "transparent") 

#################
# Summary stats #
#################

Ag_stats <- cbind(as.data.frame(TWS_trend), as.data.frame(FEED), as.data.frame(FOOD), as.data.frame(NONFOOD)) %>% 
  set_colnames(c("TWS", "FEED", "FOOD", "NONFOOD"))

Ag_stats$class <- ifelse(Ag_stats$TWS < -2, "SevereDry",
                         ifelse(Ag_stats$TWS > -2 & Ag_stats$TWS < -0.5, "ModDry",
                                ifelse(Ag_stats$TWS > -0.5 & Ag_stats$TWS < 0.5, "Static",
                                       ifelse(Ag_stats$TWS > 0.5 & Ag_stats$TWS < 2, "ModWet", "SevereWet"))))
Ag_stats_summary <- Ag_stats %>%
  group_by(class) %>%
  summarise(FeedSum = sum(FEED, na.rm = TRUE)/1e14,
            FoodSum = sum(FOOD, na.rm = TRUE)/1e14,
            NonFoodSum = sum(NONFOOD, na.rm = TRUE)/1e14) %>% as.data.frame()

Ag_stats$class %<>% as.factor()
Ag_stats$All <- Ag_stats$FEED + Ag_stats$FOOD + Ag_stats$NONFOOD

Median_summ <- Ag_stats %>% 
  # group_by(class) %>%
  summarise(FEEDmedian = weighted.quantile(TWS, FEED, probs = 0.50, na.rm = TRUE),
            FOODmedian = weighted.quantile(TWS, FOOD, probs = 0.50, na.rm = TRUE),
            NONFOODmedian = weighted.quantile(TWS, NONFOOD, probs = 0.50, na.rm = TRUE),
            ALLmedian = weighted.quantile(TWS, All, probs = 0.50, na.rm = TRUE)) %>%  as.data.frame()
