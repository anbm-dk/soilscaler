## code to prepare `DATASET` dataset goes here

# Import data-raw

library(terra)
library(magrittr)
library(dplyr)

root <- getwd()

## 1: Load data-raw

# Load EC rasters

EC_list <- root %>%
  paste0(., "/data-raw/EC_raster/") %>%
  list.files(full.names = TRUE) %>%
  lapply(
    function(x) rast(x)
  )


# Load covariates

sites <- root %>%
  paste0(., "/data-raw/covariates/") %>%
  list.dirs(full.names = FALSE) %>%
  extract(nchar(.) > 0)

cov_list <- root %>%
  paste0(., "/data-raw/covariates/") %>%
  list.dirs() %>%
  extract(-1) %>%
  lapply(
    function(x) {
      out <- x %>%
        list.files(full.names = TRUE) %>%
        rast()
      return(out)
    }
  )

cov_list2 <- list()

for (i in 1:length(cov_list))
{
  cov_list2[[i]] <- crop(cov_list[[i]], EC_list[[i]])
}

cov_list <- cov_list2

rm(cov_list2)

# Load orthophotos

ortho_files <- root %>%
  paste0(., "/data-raw/ortho/") %>%
  list.files(full.names = TRUE)

ortho_list <- list()

for (i in 1:length(sites))
{
  ortho_list[[i]] <- ortho_files %>%
    grep(sites[i],
      .,
      value = TRUE
    ) %>%
    rast()
}

# Load field shapefiles

DK_fields <- list()

for (i in 1:length(sites))
{
  DK_fields[[i]] <- root %>%
    paste0(., "/data-raw/general/", sites[i], "/", sites[i], "_field.shp") %>%
    vect()
  values(DK_fields[[i]]) <- NULL
}

# Load bare soil areas

field_bare_list <- list()

for (i in 1:length(sites))
{
  field_bare_list[[i]] <- root %>%
    paste0(., "/data-raw/general/", sites[i], "/", sites[i], "_field2.shp") %>%
    vect()
  values(field_bare_list[[i]]) <- NULL
}

# Load soil observations

obs <- sites %>%
  sapply(
    function(x) {
      x %>%
        paste0(
          root, "/data-raw/soil_observations/",
          ., "_coords_tex.txt"
        ) %>%
        read.table(
          header = TRUE,
          sep = "\t"
        )
    }
  )

mincols <- lapply(obs, function(x) ncol(x)) %>%
  unlist() %>%
  min()

obs %<>% lapply(function(x) {
  select(x, 1:mincols)
})

obs %<>% lapply(function(x) {
  out <- x
  colnames(out) <- c(
    "ID", "UTME", "UTMN", "clay", "silt", "sand_f", "sand_c",
    "SOM"
  )
  out %<>%
    mutate(SOC = round(SOM * 0.587, 3)) %>%
    select(-SOM)
  return(out)
})

coord_cols <- obs[[1]] %>%
  colnames() %>%
  .[2:3]

crs_all <- crs(DK_fields[[1]])

obs %<>% lapply(function(x) {
  vect(x,
    geom = coord_cols,
    crs = crs_all,
    keepgeom = TRUE
  )
})

# Assign the correct crs

for (i in 1:length(sites))
{
  crs(cov_list[[i]]) <- crs_all
  crs(EC_list[[i]]) <- crs_all
  crs(ortho_list[[i]]) <- crs_all
}

# Assign names to lists

names(cov_list) <- sites
names(EC_list) <- sites
EC_list <- lapply(
  EC_list,
  function(x) {
    names(x) <- "EC_00_25_cm"
    return(x)
  }
)
names(field_bare_list) <- sites
names(DK_fields) <- sites
ortho_list <- lapply(
  ortho_list,
  function(x) {
    x <- subset(
      x,
      c(3, 2, 1)
    )
    names(x) <- c("red", "green", "blue")
    return(x)
  }
)
names(ortho_list) <- sites

# Load input maps

DK_2014 <- root %>%
  paste0(., "/data-raw/input_maps/DK_2014_lite/") %>%
  list.files(full.names = TRUE) %>%
  rast()

soilgrids <- root %>%
  paste0(., "/data-raw/input_maps/soilgrids/") %>%
  list.files(full.names = TRUE) %>%
  rast()

divisors <- c(rep(10, 6), rep(100, 2))

soilgrids <- ifel(soilgrids == 32767.00, NA, soilgrids / divisors)


# Round off EC

DK_EC <- lapply(
  EC_list,
  function(x) {
    out <- round(x, 2)
    return(out)
  }
)


# Round off orthophotos

DK_RGB <- lapply(
  ortho_list,
  function(x) {
    out <- x %>%
      round(0) %>%
      as.int()
    return(out)
  }
)


# Round off covariates

decimals <- c(3, 3, 2, 2, 4, 2, 0, 3, 2, 2, 3, 3, 2, 2)

f1 <- function(x) {
  out <- base::round(x, digits = decimals)
  return(out)
}

cov_list3 <- lapply(cov_list, function(r) {
  out <- app(r, f1)
  crs(out) <- crs(DK_EC[[1]])
  return(out)
})


# Crop covariates to field extents

DK_topo <- list()

for (i in 1:length(cov_list))
{
  DK_topo[[i]] <- mask(cov_list3[[i]], EC_list[[i]])
}


# Crop input data to areas surrounding fields

field_extents <- lapply(
  field_bare_list,
  function(x) {
    out <- x %>%
      ext() %>%
      extend(250) %>%
      as.polygons()
    return(out)
  }
) %>%
  vect()

crs(field_extents) <- crs_all

DK_soil_30m <- c(
  DK_2014[[1]], DK_2014[[6]], DK_2014[[7]], DK_2014[[8]],
  DK_2014[[3]]
)

DK_unc_30m <- c(DK_2014[[2]], DK_2014[[4]])

DK_30m <- DK_soil_30m %>%
  terra::mask(
    .,
    field_extents
  )

DK_30m_unc <- DK_unc_30m %>%
  terra::mask(
    .,
    field_extents
  )

names(DK_30m) <- c("clay", "silt", "sand_f", "sand_c", "SOC")
names(DK_30m_unc) <- c("clay_SD", "SOC_SD")

field_extents_longlat <- field_extents %>% project(soilgrids)

DK_soilgrids <- soilgrids[[c(1, 7)]]
DK_soilgrids_unc <- soilgrids[[c(2, 8)]]

names(DK_soilgrids) <- c("clay", "SOC")
names(DK_soilgrids_unc) <- c("clay_PI", "SOC_PI")

DK_soilgrids %<>% mask(field_extents_longlat) %>% round(1)
DK_soilgrids_unc %<>% mask(field_extents_longlat) %>% round(1)

# To do
# 1: Crop covariates to field extent (OK)
# 2: Round off decimals (covariates, EC, ortho) [OK]
# 3: Import input maps (OK)
# 4: Crop input maps to areas surrounding fields (OK)
# 5: Give names to all the lists (OK)
# 6: Documentation

# NB: Take into account non-exportable objects. Use wrap.

DK_fields %<>% vect
DK_fields$Site <- sites
names(DK_topo) <- sites

DK_fields %<>% wrap
DK_observations <- lapply(obs, function(x) wrap(x))
DK_EC %<>% lapply(function(x) wrap(x))
DK_RGB %<>% lapply(function(x) wrap(x))
DK_topo %<>% lapply(function(x) wrap(x))
DK_30m %<>% wrap
DK_30m_unc %<>% wrap
DK_soilgrids <- wrap(DK_soilgrids)
DK_soilgrids_unc <- wrap(DK_soilgrids_unc)

usethis::use_data(
  DK_fields,
  DK_observations,
  DK_EC,
  DK_RGB,
  DK_topo,
  DK_30m,
  DK_30m_unc,
  DK_soilgrids,
  DK_soilgrids_unc,
  overwrite = TRUE
)

# END
