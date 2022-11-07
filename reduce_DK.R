# Reduce the size of input rasters

library(terra)
library(raster)
library(magrittr)
library(dplyr)

root <- getwd()

mapdir <- root %>%
  paste0(., '/data/input_maps/DK_2014/')

mapnames <- c('DK_clay_SD.tif'
              , 'DK_SOC_mean.tif'
              , 'DK_SOC_SD.tif'
              )

othernames <- mapdir %>%
  list.files %>%
  .[.%in% mapnames == FALSE]

othernames_full <- mapdir %>%
  paste0(., othernames)

othermaps <- othernames_full %>% rast

mapnames_full <- mapdir %>%
  paste0(., mapnames)

maps <- mapnames_full %>% rast

dcs <- 'C:/Users/au542768/Dropbox/GEODATA/drainage_data/DCs_2022.shp' %>% vect

out_crs <- dcs %>% crs

mapdir_new <- root %>%
  paste0(., '/data/input_maps/DK_2014_lite/') %T>%
  dir.create()

maps %>% minmax %>% log

crs(maps) <- out_crs

mapnames_new <- mapdir_new %>%
  paste0(., mapnames)

rounded <- maps %>%
  round(digits = 1)

crs(rounded) <- out_crs

rounded %>% writeRaster(
  filename = mapnames_new
  , overwrite = TRUE
  , progress = TRUE
  , datatype = 'FLT4s'
  , filetype = 'GTiff'
)

mapnames_new %>% rast

othernames_new <- mapdir_new %>%
  paste0(., othernames)

crs(othermaps) <- out_crs

othermaps

othermaps %>% writeRaster(
  filename = othernames_new
  , overwrite = TRUE
  , progress = TRUE
  , datatype = 'FLT4s'
  , filetype = 'GTiff'
)

# END
