# **MHI Index**

[![Lifecycle:experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
![contributions welcome](https://img.shields.io/badge/contributions-welcome-brightgreen.svg?style=flat)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

# MHI - Disproportionate exposure to micro-urban heat islands across vulnerable populations in Lima city, Peru

## Study description
Climate change constitutes an unprecedented challenge for public health and one of its main direct effects are extreme temperatures which impact human health by compromising the body's homeostasis and worsening non-communicable disease. Extreme heat exposure varies between intra-urban areas and this difference is called the micro-urban heat island (MHI) effect. Here we aimed to assess MHI distribution among socioeconomic levels in Lima, Peru. We conducted a cross-sectional study in Lima, Peru at the block-level (mean population size per block was 130 inhabitants). The mean land surface temperature (LST) from 2017 to 2021 were estimated using the TIRS sensor of the Landsat 8 satellite at 500 meters scale and extracted to block level. MHI estimates were calculated based on the difference on mean LST values (2017 to 2021) per block and the block with lowest mean LST registered in the same period. Socioeconomic data were obtained from the 2017 Peruvian census. A principal component analysis (PCA) was performed to construct a socioeconomic index based on ten socioeconomic variables. In addition, a mixture analysis based on quantile g-computation was conducted to estimate the joint and specific effects of socioeconomic variables on MHI.
![](https://github.com/healthinnovation/MOV/blob/main/Figures/Fig3.png)

## Repository structure

1. [raw_data](https://github.com/healthinnovation/MHI/tree/main/raw_data) -  raw data (block geometries and LST) to merge with output INEI 2017 census.
2. [output/csv](https://github.com/healthinnovation/MHI/tree/main/ouput/csv) - Database created from raw data and used in the final analysis.
3. [Figures](https://github.com/healthinnovation/MHI/tree/main/images) - Figures in the main text
    - Figure 1. Study area is located on the central coast southwestern of Peru.
    - Figure 2. Diagram of the MHI construction process.
    - Figure 3. MHI estimates distribution by socioeconomic characteristics.
    - Figure 4. MHI distribution per rank of socioeconomic characteristics.
    - Figure 5.  Bivariate plot of MHI and the PCA index based on socioeconomic variables and income per capita.
    - Figure 6. Scaled effect size of each socioeconomic variable on MHI estimation by Metropolitan Lima zones.
5. [MHI/script/R](https://github.com/healthinnovation/MHI/tree/main/script/R) - Scripts in R language used to analyze and visualyze data results.
    - box_ridges_qgcomp.R - Boxviolin plots, ridges plots and Quantile G Computaiton analysis.
    - data_managemen_pca.R - Data management and Principal Component Analysis.
    - map_biscale.R - Create biscalemap showed in Figure 5
    - mhi.R - MHI calculation process
6. README.md
