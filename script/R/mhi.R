library(terra)
library(exactextractr)
library(sf)

blocks <- st_read("raw_data/gpkg/blocks_lima.gpkg")
LST_2017 <- rast("raw_data/tiff/LST_2017_mean.tif")
LST_2021 <- rast("raw_data/tiff/LST_2021_mean.tif")
LST_2017_2021 <- rast("raw_data/tiff/LST_2017_2021_mean.tif")




