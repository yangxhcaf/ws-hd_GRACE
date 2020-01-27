library(ggplot2)
library(raster)
library(dplyr) 
library(magrittr) 
library(e1071) 
library(reshape2)
library(spatstat)

mainDir <- "Z:/2.active_projects/Xander/"

# import all data
TWS_trend <- raster(paste(mainDir, "! GIS_files/GRACE/", "GRACE_coredata", ".tif", sep="")) # Rodell et al. (2018) TWS trends, resampled to 0.05 res
Pop <- raster(paste(mainDir, "! GIS_files/R_gis_exports/", "POP_2015_0d05_UNWPP", ".tif", sep="")) # Population data from GWPv4 UNWPP 2015
WaterStress <- raster(paste(mainDir, "! GIS_files/QGIS_exports/", "Aqueduct2019_bwscat", ".tif", sep="")) # wRI HydroBASIN dataset of water stress class
WaterStress[is.na(WaterStress[])] <- -99
WaterStress %<>% as.factor()

# split population raster by regions of water stress
# begin by initializing a raster for each class with same extent and resolution as rest of files
Pop.Low     <- raster(TWS_trend); Pop.Low[]     <- 0
Pop.LowMed  <- raster(TWS_trend); Pop.LowMed[]  <- 0
Pop.MedHigh <- raster(TWS_trend); Pop.MedHigh[] <- 0
Pop.High    <- raster(TWS_trend); Pop.High[]    <- 0
Pop.ExHigh  <- raster(TWS_trend); Pop.ExHigh[]  <- 0
Pop.Arid    <- raster(TWS_trend); Pop.Arid[]    <- 0
Pop.NoDat   <- raster(TWS_trend); Pop.NoDat[]   <- 0

# populate population rasters per water stress class
Pop.Low[WaterStress == 0]     <- Pop[WaterStress == 0]
Pop.LowMed[WaterStress == 1]  <- Pop[WaterStress == 1]
Pop.MedHigh[WaterStress == 2] <- Pop[WaterStress == 2]
Pop.High[WaterStress == 3]    <- Pop[WaterStress == 3]
Pop.ExHigh[WaterStress == 4]  <- Pop[WaterStress == 4]
Pop.Arid[WaterStress == -1]    <- Pop[WaterStress == -1]
Pop.NoDat[WaterStress == -99]  <- Pop[WaterStress == -99]

# Reclassify emerging trend into 0.1 cm/yr increment bins
ReclassRanges <- data.frame(low = seq(-40, 6.0, 0.1), high = seq(-39.9, 6.1, 0.1), ReCLASS = seq(1:461))
EmergingTrend_Reclass <- reclassify(TWS_trend, ReclassRanges)

# Calculate population within each trend bin for each water stress class
Pop.Low.distr      <- zonal(Pop.Low, EmergingTrend_Reclass, sum)      %>% as.data.frame() %>% set_colnames(c("ID", "Low"))
Pop.LowMed.distr   <- zonal(Pop.LowMed, EmergingTrend_Reclass, sum)   %>% as.data.frame() %>% set_colnames(c("ID", "LowMed"))
Pop.MedHigh.distr  <- zonal(Pop.MedHigh, EmergingTrend_Reclass, sum)  %>% as.data.frame() %>% set_colnames(c("ID", "MedHigh"))
Pop.High.distr     <- zonal(Pop.High, EmergingTrend_Reclass, sum)     %>% as.data.frame() %>% set_colnames(c("ID", "High"))
Pop.ExHigh.distr   <- zonal(Pop.ExHigh, EmergingTrend_Reclass, sum)   %>% as.data.frame() %>% set_colnames(c("ID", "ExHigh"))
Pop.Arid.distr     <- zonal(Pop.Arid, EmergingTrend_Reclass, sum)     %>% as.data.frame() %>% set_colnames(c("ID", "Arid"))
Pop.NoDat.distr    <- zonal(Pop.NoDat, EmergingTrend_Reclass, sum)    %>% as.data.frame() %>% set_colnames(c("ID", "NoDat"))

# merge all results, and melt dataframe for plotting
Pop.distr <- Reduce(function(x, y) merge(x, y, by = "ID"), list(Pop.Low.distr, Pop.LowMed.distr, Pop.MedHigh.distr ,
                                                                Pop.High.distr, Pop.ExHigh.distr, Pop.Arid.distr, Pop.NoDat.distr))
Pop.distr %<>% melt(id.var = "ID")
Pop.distr$class <- ifelse(Pop.distr$ID < 380.5, "SevereDry",
                                 ifelse(Pop.distr$ID > 380.5 & Pop.distr$ID < 395.5, "ModDry",
                                        ifelse(Pop.distr$ID > 395.5 & Pop.distr$ID < 405.5, "Static",
                                               ifelse(Pop.distr$ID > 405.5 & Pop.distr$ID < 420.5, "ModWet", "SevereWet"))))

PopDisr_summary <- Pop.distr %>% group_by(class, variable) %>% summarise(SevWet = sum(value)/1e6) %>% as.data.frame()

Median_df <- cbind(as.data.frame(TWS_trend), as.data.frame(Pop), as.data.frame(WaterStress)) %>% set_colnames(c("TWS", "Pop", "Class"))
Median_df$Class <- ifelse(is.na(Median_df$Class), -99, Median_df$Class) 
Median_df$Class %<>% as.factor()

Median_summ <- Median_df %>% 
  group_by(Class) %>%
  summarise(median = weighted.quantile(TWS, Pop, probs = 0.50, na.rm = TRUE)) %>%  as.data.frame()

## plot results
# For reference: bin ID vs emerging trend:
# 390.5 = -1; 400.5 = 0; 410.5 = 1
fig <- ggplot(Pop.distr, aes(x = ID, y = value, fill = variable)) +
  geom_bar(stat = "identity", position="stack", colour = "black") +
  scale_fill_manual(values = c("#ffffa3", "#ffe600", "#ff9a00", "#ff1900", "#bb0007", "#808080", "#252525")) + 
  scale_y_continuous(position = "right", breaks = seq(0, 500e6, by = 50e6), expand = c(0,0), 
                     sec.axis = dup_axis(name = element_blank())) +
  scale_x_continuous(limits = c(350.5, 430.5), breaks = seq(350.5, 430.5, by = 10), expand = c(0,0),  
                     labels = c("-5" ,"-4", "-3", "-2", "-1", "0", "1", "2", "3")) +
  theme(panel.background =  element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major = element_line(alpha(colour = "white", 0.3), linetype = "dashed", size = 0.5),
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  geom_vline(xintercept = 400.5, size = 1.5, alpha = 0.8) +
  labs(x=expression(paste("Terrestrial Water Storage Rate of Change","  ","(cm", y^-1,")", sep="")),
       y = "Populaiton (in millions)") 
fig

## Skewness calculation
Pop.distr <- Reduce(function(x, y) merge(x, y, by = "ID"), list(Pop.Low.distr, Pop.LowMed.distr, Pop.MedHigh.distr ,
                                                                Pop.High.distr, Pop.ExHigh.distr, Pop.Arid.distr, Pop.NoDat.distr))
SkewTable <- data.frame(Pop.distr[,1]) %>% set_colnames("Trend")
SkewTable$Pop <- Pop.distr$Low + Pop.distr$LowMed + Pop.distr$MedHigh + Pop.distr$High + Pop.distr$ExHigh + Pop.distr$Arid + Pop.distr$NoDat 
# need to simplify so that ~2 billion (2^31) vector data limit is not exceeded 
SkewTable$PopSimp <- as.integer(round(SkewTable$Pop/100, 0))
ExplodedSkewTable <- SkewTable[rep(rownames(SkewTable), SkewTable$PopSimp), ]
skewness(ExplodedSkewTable$Trend)
