library(raster); library(R.utils)library(ggsci);library(tmap); library(tmaptools)

mainDir <- "Z:/2.active_projects/Xander/"

# import data
TotalImpact <- raster(paste(mainDir, "! GIS_files/R_gis_exports/", "TWSImpact_Summary", ".tif", sep="")) # Total SES impact
Lakes <- raster(paste(mainDir, "! GIS_files/Lakes/", "Lakes_0d05", ".tif", sep="")) # Lakes 

# make map plot
data("World"); tmap_options(max.raster = c(plot = 25920000, view = 25920000))
Lakes.narm <- Lakes; Lakes.narm[Lakes.narm == 0] <- NA
e <- extent(c(-180, 180, -60, 88))

#c("#C0C0C0", rev(pal_locuszoom("default")(4)))
my_palette <- c("#C0C0C0", "#46B8DAFF", "#EEA236FF", "#D43F3AFF")

map <-  
  tm_shape(TotalImpact, projection="robin") + tm_raster(style = "cat", palette = my_palette) +
  tm_shape(Lakes.narm) + tm_raster(style = "cat", palette = colorRampPalette(c("grey"))(1), n = 1, colorNA = NULL) +
  tm_shape(World) +  tm_borders("black", lwd = .5) +
  tm_style("white", legend.show = F,
           frame = F, bg.color = "white", earth.boundary = e, earth.boundary.color = "white", earth.boudary.lwd = 2,
           space.color="white", legend.frame = T, legend.bg.color="white")
# save map plot
tmap_save(map, "C:/Users/Tom/Desktop/ImpactSummary_cats.png", dpi = 500, outer.margins = 0.01, height = 3, units = "in")
