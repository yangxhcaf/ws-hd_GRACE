library(ggplot2); library(raster); library(dplyr); library(magrittr) 
library(reshape2); library(spatstat); library(e1071)

mainDir <- "Z:/2.active_projects/Xander/"

# import all data
TWS_trend <- raster(paste(mainDir, "! GIS_files/Rodell_SourceData/", "Rodell_etal_0d05", ".tif", sep="")) # Rodell et al. (2018) TWS trends, resampled to 0.05 res
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

# Reclassify TWS trend into 0.1 cm/yr increment bins
TWS_classes <- data.frame(low = seq(-40, 6.0, 0.1), high = seq(-39.9, 6.1, 0.1), ReCLASS = seq(1:461))
TWS_binned <- reclassify(TWS_trend, TWS_classes)

# Calculate population within each trend bin for each water stress class
Pop.Low.distr      <- zonal(Pop.Low, TWS_binned, sum)      %>% as.data.frame() %>% set_colnames(c("ID", "Low"))
Pop.LowMed.distr   <- zonal(Pop.LowMed, TWS_binned, sum)   %>% as.data.frame() %>% set_colnames(c("ID", "LowMed"))
Pop.MedHigh.distr  <- zonal(Pop.MedHigh, TWS_binned, sum)  %>% as.data.frame() %>% set_colnames(c("ID", "MedHigh"))
Pop.High.distr     <- zonal(Pop.High, TWS_binned, sum)     %>% as.data.frame() %>% set_colnames(c("ID", "High"))
Pop.ExHigh.distr   <- zonal(Pop.ExHigh, TWS_binned, sum)   %>% as.data.frame() %>% set_colnames(c("ID", "ExHigh"))
Pop.Arid.distr     <- zonal(Pop.Arid, TWS_binned, sum)     %>% as.data.frame() %>% set_colnames(c("ID", "Arid"))
Pop.NoDat.distr    <- zonal(Pop.NoDat, TWS_binned, sum)    %>% as.data.frame() %>% set_colnames(c("ID", "NoDat"))

# merge all results, and melt dataframe for plotting
Pop.distr <- Reduce(function(x, y) merge(x, y, by = "ID"), list(Pop.Low.distr, Pop.LowMed.distr, Pop.MedHigh.distr,
                                                                Pop.High.distr, Pop.ExHigh.distr, Pop.Arid.distr, Pop.NoDat.distr))
Pop.distr %<>% melt(id.var = "ID")
Pop.distr$class <- ifelse(Pop.distr$ID < 380.5, "SevereDry",
                          ifelse(Pop.distr$ID > 380.5 & Pop.distr$ID < 395.5, "ModDry",
                                 ifelse(Pop.distr$ID > 395.5 & Pop.distr$ID < 405.5, "Static",
                                        ifelse(Pop.distr$ID > 405.5 & Pop.distr$ID < 420.5, "ModWet", "SevereWet"))))

## plot results; For reference: bin ID vs emerging trend:390.5 = -1; 400.5 = 0; 410.5 = 1
a <- 0.5
b <- 500*(10^6)

fig <- ggplot(Pop.distr, aes(x = ID, y = value, fill = variable)) +
  geom_rect(data=NULL,aes(xmin=350.5, xmax=380.5, ymin=0, ymax=b), fill="#FFA5A5", alpha = a) +
  geom_rect(data=NULL,aes(xmin=380.5, xmax=395.5, ymin=0, ymax=b), fill="#FFDFDF", alpha = a) +
  geom_rect(data=NULL,aes(xmin=395.5, xmax=405.5, ymin=0, ymax=b), fill="#959595", alpha = 1.5*a) +
  geom_rect(data=NULL,aes(xmin=405.5, xmax=420.5, ymin=0, ymax=b), fill="#F0EDF8", alpha = a) +
  geom_rect(data=NULL,aes(xmin=420.5, xmax=430.5, ymin=0, ymax=b), fill="#AAC2FF", alpha = a) +
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
        axis.line = element_line(colour = "black"), panel.ontop = TRUE,
        legend.position = "none") +
  geom_vline(xintercept = 400.5, size = 1.5, alpha = 0.8) +
  labs(x=expression(paste("Terrestrial Water Storage Rate of Change","  ","(cm", y^-1,")", sep="")),
       y = "Populaiton (in millions)") 
fig
ggsave("C:/Users/Tom/Desktop/PopulationDistr_UNWPP.png", fig, dpi = 500, width = 14, height = 10, bg = "transparent")

########################
## Summary statistics ##
########################

# summary table per TWS trend class (5 classes) and water stress class (7 classes) = 35 population counts (in millions)
PopDisr_summary <- Pop.distr %>% group_by(class, variable) %>% summarise(SevWet = sum(value)/1e6) %>% as.data.frame()

# create summary dataframe
Summary_df <- cbind(as.data.frame(TWS_trend), as.data.frame(Pop), as.data.frame(WaterStress)) %>% set_colnames(c("TWS", "Pop", "Class"))
Summary_df$Class <- ifelse(is.na(Summary_df$Class), -99, Summary_df$Class) 
Summary_df$Class %<>% as.factor()
# print median TWS trend per water stress class
Summary_df %>% 
  group_by(Class) %>%
  summarise(median = weighted.quantile(TWS, Pop, probs = 0.50, na.rm = TRUE)) %>%  as.data.frame()

# Skewness calculation
Pop.distr <- Reduce(function(x, y) merge(x, y, by = "ID"), list(Pop.Low.distr, Pop.LowMed.distr, Pop.MedHigh.distr ,
                                                                Pop.High.distr, Pop.ExHigh.distr, Pop.Arid.distr, Pop.NoDat.distr))
SkewTable <- data.frame(Pop.distr[,1]) %>% set_colnames("Trend")
SkewTable$Pop <- Pop.distr$Low + Pop.distr$LowMed + Pop.distr$MedHigh + Pop.distr$High + Pop.distr$ExHigh + Pop.distr$Arid + Pop.distr$NoDat 
# need to simplify so that ~2 billion (2^31) vector data limit is not exceeded 
SkewTable$PopSimp <- as.integer(round(SkewTable$Pop/100, 0))
ExplodedSkewTable <- SkewTable[rep(rownames(SkewTable), SkewTable$PopSimp), ]
skewness(ExplodedSkewTable$Trend)
