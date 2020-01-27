# Water security and the human dimensions of changing global freshwater availability
Data sources and scripts used in data preparation and analysis are summarized below. The scripts themselves can be found in the folders above.

### Data sources
Data | Source
------------ | -------------
TWS trends | [Rodell et al. (2018) source data](https://www.nature.com/articles/s41586-018-0123-1)
Population |  [GWPv4](https://sedac.ciesin.columbia.edu/data/collection/gpw-v4)
Water stress | [WRI Aqueduct (2019 release)](https://www.wri.org/aqueduct)
Flooding risk | [WRI Aqueduct (2019 release)](https://www.wri.org/aqueduct)
Crop production  | [EarthStat](http://www.earthstat.org/crop-allocation-food-feed-nonfood/)
Cropland density | [Ramankutty et al. (2008)](http://www.earthstat.org/cropland-pasture-area-2000/)
Irrigation type | [LP DAAC](https://croplands.org/downloadLPDAAC)
GDP(PPP) | [Kummu et al. (2018)](https://datadryad.org/stash/dataset/doi:10.5061/dryad.dk1j0)
Vegetation sensitivity | [Seddon et al. (2016)](https://ora.ox.ac.uk/objects/uuid:896bf37f-a56b-4bc0-9595-8c9201161973)
Biodiversity hotspots | [CEPF](https://zenodo.org/record/3261807#.XXZPNyhKh9M)
Adaptive capcity | [Varis et al. (2019)](https://datadryad.org/stash/dataset/doi:10.5061/dryad.h2v2398)


### Scripts
Folder | Script | Description
------------ | ------------- |  -------------
**Data Prep** | WGS84_gridArea_cellcenter.R | Calculates raster cell areas according to the WGS84 reference ellipsoid. 
**Human Dimensions** | PopulationDim.R | Analyzes the population distribution relative to TWS trends.
" | AgriculturalDim_CroplandDensity.R | Plots the relationship between cropland density and area-weighted TWS trends.
" | AgriculturalDim_KilocalorieAllocation.R | Analyzes cropland distribution relative to TWS trends.
" | AgriculturalDim_IrrigationType.R |  Analayzes relationship between irrigation project types and water availability trends.
" | EconomicDim_GDP-percap_percentiles.R | Analyzes GDP per capita percentiles across the TWS trend spectrum.
" | EconomicDim_GDP_Pop_cdf.R | Plots the CDF curves of GDP and population across the TWS trend spectrum.
" | EcologicalDim.R | Uses TWS trends, vegetation sensitivity to water anomolies, and biodiversity hotspots.
**Water Security** | WSV_prep.R | Prepares the modified hazard level and adaptive capacity datasets for analysis.
" | WSV_boxplot_summary.R | Summarizes water security vulnerability to flooding and water stress in 10 world regions.
