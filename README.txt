This repository contains scripts to reproduce the results in 'The social-ecological dimensions of changing global freshwater availability' manuscript. The scripts are categorized into 'Data preparation', 'SES analysis', 'Vulnerability analysis', and 'Supplementary' sections. All data come from open sources whose locations are listed below the file tree.

├── Data preparation
│   ├── WGS84_cellArea.R
│   │        # Estimates raster cell area based on WGS84 reference ellipsoid at desired resolution
│   ├── TWS_resample.R
│   │        # Converts Rodell et al.'s source TWS trend data to raster at 0.05* resolution
│   │        # Serves as template for rest of simple resampling processes
│   ├── kcal_resampling.R
│   │        # Converts Cassidy et al.'s global calorie production (0.0833* res) to 0.05* preserving global totals
│   ├── EQ_removal.R
│   │        # Creates identifier raster to remove trends produced by earthquake interference
│   ├── FPU_shortage.R
│   │	    # Converts .csv FPU waer shortage results with .tif FPU identifiers into shapefile 
│   ├── FPU_floods.R
│   │        # Determines maximum flood count in each FPU using DFO's flood archive over 1985-2001 period.
│   ├── Precip_GPCC.R
│   │        # Creates total annual precipitation per 0.5* grid cell from monthly GPCC data for all years 
│   ├── Precip_CRU.R
│   │        # Creates total annual precipitation per 0.5* grid cell from monthly CRU TS data for all years 
│   ├── Precip_UDEL.R
│   │        # Creates total annual precipitation per 0.5* grid cell from monthly UDEL data for all years 
│   └── Precip_GPCC.R
│           # Creates total annual precipitation per 0.5* grid cell from monthly GPCC data for all years 
│   
├── Social-ecological systems analysis
│   ├── Dimension_Population.R
│   │        # Assesses population distribution against TWS trends and produces associated map
│   ├── Dimension_Agriculture.R
│   │        # Assesses calorie production distribution against TWS trends and produces associated map
│   ├── Agriculture_SI.R
│   │        # Assesses relationship between cropland and irrigation density with TWS trends, irrigation sources, 
│   │        # calorie yields, and food calorie allocation rates
│   ├── Dimension_Economy.R
│   │        # Assesses GDP at PPP distribution against TWS trends and produces associated map
│   ├── Dimension_Ecology.R
│   │        # 1. Derives ecological indicator based on prioritization and water sensitivity
│   │        # 2. Assesses global 200 ecoregions against TWS trends and produces associated map using derived indicator
│   └── SES_hotspots.R
│           # 1. Equally weights all dimensions into combined analysis of TWS pressures on collective SES
│           # 2. Isolate global hotspots and compare to adaptive capacity using population percentiles
│   
├── Vulnerability analysis
│   ├── Trends_flooding.R
│   │        # Creates categorical raster based on combinations of TWS trends and flood occurrence
│   ├── Trends_shortage.R
│   │        # Creates categorical raster based on combinations of TWS trends and water shortage estimates
│   ├── Hazard_classification.R
│   │        # Categorizes flood occurrence and water shortage estimates per FPU to hazard levels on 0-5 scale
│   ├── Hazard_modifier.R
│   │        # Develops hazard modification layer based on FPU averaged ratios of TWS trends to long-term 
│   │        # mean annual precipitation depths
│   ├── Vulnerability_analysis.R
│   │        # Performs vulnerability analysis by modifying flooding and water shortage hazard levels (developed in 
│   │        # 'Hazard_classification.R') using modifying effect developed in 'Hazard_modifier.R', and subtracts 
│   │        # adaptive capacity from normalized result. Produces vulnerability maps and summary plots
│   └── Vulnerability_analysis_sensitivity.R
│   │        # Creates plots showing effect of 'Hazard_modifier.R' on vulnerability outcomes
│   │
├── Supplementary 
│   ├── Dimensions_descriptiveStats.R
│   │        # Calculates mean, sd, skewness, median for all dimension plots
│   ├── Calories_in_shortage.R
    └──        # Calculates calorie totals per combination of TWS trend and water shortage


DATA SOURCES
TWS trends: [Rodell et al. (2018) source data](https://www.nature.com/articles/s41586-018-0123-1)
Population: [GWPv4](https://sedac.ciesin.columbia.edu/data/collection/gpw-v4)
Cropland density: [Ramankutty et al. (2008)](http://www.earthstat.org/cropland-pasture-area-2000/)
Calorie production: [EarthStat](http://www.earthstat.org/crop-allocation-food-feed-nonfood/)
Irrigation:
GDP: [Kummu et al. (2018)](https://datadryad.org/stash/dataset/doi:10.5061/dryad.dk1j0)
GDP per capita: [Kummu et al. (2018)](https://datadryad.org/stash/dataset/doi:10.5061/dryad.dk1j0)
Global 200:
Vegetation sensitivity: [Seddon et al. (2016)](https://ora.ox.ac.uk/objects/uuid:896bf37f-a56b-4bc0-9595-8c9201161973)
EFN sensitivity:
Flood occurrence:
Water shortage:
Adaptive capacity: [Varis et al. (2019)](https://datadryad.org/stash/dataset/doi:10.5061/dryad.h2v2398)
Food production units:
GPCC precipitation:
CRU precipitation:
UDEL precipitation:
