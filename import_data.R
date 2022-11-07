# Import data_raw

library(terra)
library(magrittr)
library(dplyr)

root <- getwd()

## 1: Load data_raw

# Load EC rasters

EC_list <- root %>%
  paste0(., '/data_raw/EC_raster/') %>%
  list.files(full.names = TRUE) %>%
  lapply(
    function(x) rast(x)
  )


# Load covariates

sites <- root %>%
  paste0(., '/data_raw/covariates/') %>%
  list.dirs(full.names = FALSE) %>%
  extract(nchar(.) > 0)

cov_list <- root %>%
  paste0(., '/data_raw/covariates/') %>%
  list.dirs %>%
  extract(-1) %>%
  lapply(
    function(x)
    {
      out <- x %>% list.files(full.names = TRUE) %>%
        rast
      return(out)
    }
  )

cov_list2 <- list()

for(i in 1:length(cov_list))
{
  cov_list2[[i]] <- crop(cov_list[[i]], EC_list[[i]])
}

cov_list <- cov_list2

rm(cov_list2)

# Load orthophotos

ortho_files <- root %>%
  paste0(., '/data_raw/ortho/') %>%
  list.files(full.names = TRUE)

ortho_list <- list()

for(i in 1:length(sites))
{
  ortho_list[[i]] <- ortho_files %>%
    grep(sites[i]
         , .
         , value = TRUE
    ) %>%
    rast
}

# Load field shapefiles

DK_fields <- list()

for(i in 1:length(sites))
{
  DK_fields[[i]] <- root %>%
    paste0(., '/data_raw/general/', sites[i], '/', sites[i], '_field.shp') %>%
    vect
  values(DK_fields[[i]]) <- NULL
}

# Load bare soil areas

field_bare_list <- list()

for(i in 1:length(sites))
{
  field_bare_list[[i]] <- root %>%
    paste0(., '/data_raw/general/', sites[i], '/', sites[i], '_field2.shp') %>%
    vect
  values(field_bare_list[[i]]) <- NULL
}

# Load soil observations

obs <- sites %>%
  sapply(
    function(x)
    {
      x %>%
        paste0(root, '/data_raw/soil_observations/'
               , ., '_coords_tex.txt'
        ) %>%
        read.table(header = TRUE
                   , sep = '\t'
        )
    }

  )

mincols <- lapply(obs, function(x) ncol(x)) %>%
  unlist %>%
  min

obs %<>% lapply(function(x)
  select(x, 1:mincols)
)

coord_cols <- obs[[1]] %>%
  colnames %>%
  .[2:3]

crs_all <- crs(DK_fields[[1]])

obs %<>% lapply(function(x)
  vect(x
       , geom = coord_cols
       , crs = crs_all
       , keepgeom = TRUE
  )
)

# Assign the correct crs

for(i in 1:length(sites))
{
  crs(cov_list[[i]])   <- crs_all
  crs(EC_list[[i]])    <- crs_all
  crs(ortho_list[[i]]) <- crs_all
}

# Assign names to lists

names(cov_list) <- sites
names(EC_list) <- sites
EC_list <- lapply(
  EC_list
  , function(x)
  {

    names(x) <- 'EC_00_25_cm'
    return(x)
  }
)
names(field_bare_list) <- sites
names(DK_fields) <- sites
ortho_list <- lapply(
  ortho_list
  , function(x)
  {

    x <- subset(x
                , c(3,2,1))
    names(x) <- c('red', 'green', 'blue')
    return(x)
  }
)
names(ortho_list) <- sites

# Load input maps

DK_2014 <- root %>%
  paste0(., '/data_raw/input_maps/DK_2014_lite/') %>%
  list.files(full.names = TRUE) %>%
  rast

soilgrids <- root %>%
  paste0(., '/data_raw/input_maps/soilgrids/') %>%
  list.files(full.names = TRUE) %>%
  rast

divisors <- c(rep(10, 6), rep(100, 2))

soilgrids <- ifel(soilgrids == 32767.00, NA, soilgrids/divisors)


# END
