library(terra)
library(exactextractr)
library(sf)
library(tidyverse)

blocks <- st_read(
  "raw_data/gpkg/blocks_lima.gpkg"
  ) %>% 
  st_transform(32718)

LST_2017 <- rast("raw_data/tiff/LST_2017_mean.tif")
LST_2021 <- rast("raw_data/tiff/LST_2021_mean.tif")
LST_2017_2021 <- rast("raw_data/tiff/LST_2017_2021_mean.tif")

blocks_temp <- blocks %>% 
  mutate(
    LST_2017 = exact_extract(LST_2017,blocks,fun = "mean"),
    LST_2021 = exact_extract(LST_2021,blocks,fun = "mean"),
    LST_2017_2021 = exact_extract(LST_2017_2021,blocks,fun = "mean"),
    delta_2021_2017 = LST_2017 - LST_2021,
    LST_2017_2021_min = min(LST_2017_2021),
    MHI = LST_2017_2021 - LST_2017_2021_min
  )

mhi <- blocks_temp %>% 
  select(OBJECTID:Ingresos_mean,LST_2017:MHI,geom)

write_sf(mhi,"ouput/gpkg/mhi_29_11_2022.gpkg")
write_csv(mhi %>% st_set_geometry(NULL),"ouput/csv/mhi_29_11_2022.csv")