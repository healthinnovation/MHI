library(terra)

# Merges the 64 tiles written by script/Py/ee_lst_allyear_c2_port.py into one raster.
# Usage: Rscript script/R/c2_port_mosaic.R <tile folder> <output tif>
# tiles/LST_2017_2021_C2_median -> raw_data/tiff/LST_2017_2021_C2_median.tif
# tiles/LST_2017_2021_C2_mean   -> raw_data/tiff/LST_2017_2021_C2_mean.tif
# tiles/LST_2017_2021_C2_summer_median -> raw_data/tiff/LST_2017_2021_C2_summer_median.tif
# tiles/LST_2017_2021_C2_summer_mean   -> raw_data/tiff/LST_2017_2021_C2_summer_mean.tif

args <- commandArgs(trailingOnly = TRUE)
tl <- list.files(args[1], full.names = TRUE)
stopifnot(length(tl) == 64)
m <- do.call(merge, lapply(tl, rast))
m <- classify(m, cbind(-9999, NA))
stopifnot(compareGeom(m, rast("raw_data/tiff/LST_2017_2021_mean.tif")))
stopifnot(crs(m, describe = TRUE)$code == "32718")
writeRaster(m, args[2], overwrite = TRUE, datatype = "FLT4S", gdal = "COMPRESS=DEFLATE")
