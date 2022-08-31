# Test for basic workflow

library(terra)
library(magrittr)
library(dplyr)

root <- getwd()


## 1: Load data

# Load EC rasters

EC_list <- root %>%
  paste0(., '/data/EC_raster/') %>%
  list.files(full.names = TRUE) %>%
  lapply(
    function(x) rast(x)
  )


# Load covariates

sites <- root %>%
  paste0(., '/data/covariates/') %>%
  list.dirs(full.names = FALSE) %>%
  extract(nchar(.) > 0)

cov_list <- root %>%
  paste0(., '/data/covariates/') %>%
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
  paste0(., '/data/ortho/') %>%
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

field_list <- list()

for(i in 1:length(sites))
{
  field_list[[i]] <- root %>%
    paste0(., '/data/general/', sites[i], '/', sites[i], '_field.shp') %>%
    vect
  values(field_list[[i]]) <- NULL
}

# Load bare soil areas

field_bare_list <- list()

for(i in 1:length(sites))
{
  field_bare_list[[i]] <- root %>%
    paste0(., '/data/general/', sites[i], '/', sites[i], '_field2.shp') %>%
    vect
  values(field_bare_list[[i]]) <- NULL
}

# Load soil observations

obs <- sites %>%
  sapply(
    function(x)
    {
      x %>%
        paste0(root, '/data/soil_observations/'
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

crs_all <- crs(field_list[[1]])

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
names(field_list) <- sites
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
  paste0(., '/data/input_maps/DK_2014_lite/') %>%
  list.files(full.names = TRUE) %>%
  rast

soilgrids <- root %>%
  paste0(., '/data/input_maps/soilgrids/') %>%
  list.files(full.names = TRUE) %>%
  rast

divisors <- c(rep(10, 6), rep(100, 2))

soilgrids <- ifel(soilgrids == 32767.00, NA, soilgrids/divisors)

# Run a model
# Define parameters

observations <- obs
target <- 'clay'

useDK_map <- FALSE
if(useDK_map)
{
  coarse <- DK_2014$DK_clay_mean
  coarse_unc <- DK_2014$DK_clay_SD
} else {
  coarse <- soilgrids$DK_00_20_clay_mean
  coarse_unc <- soilgrids$DK_00_20_clay_unc
}

fine <- EC_list

flatten_coarse <- FALSE

# Add checks for consistency

# Extract coarse

if(!is.null(coarse_unc))
{
  coarse <- c(coarse, coarse_unc)
  names(coarse) <- c('coarse', 'coarse_unc')
}

coarse_resampled <- list()

if(!flatten_coarse)
{
  for(i in 1:length(fine))
  {
    if(terra::crs(coarse, proj = TRUE) ==
       terra::crs(fine[[i]], proj = TRUE))
    {
      coarse_resampled[[i]] <- terra::resample(coarse, fine[[i]])
    } else {
      coarse_resampled[[i]] <- terra::project(coarse, fine[[i]])
    }
    coarse_resampled[[i]] %<>% terra::mask(., mask = fine[[i]])
    observations[[i]] %<>%
      terra::extract(coarse_resampled[[i]], .) %>%
      dplyr::select(., -1) %>%
      merge(observations[[i]], .)
  }
} else {
  for(i in 1:length(observations))
  {
    if(terra::crs(coarse, proj = TRUE) !=
       terra::crs(observations[[i]], proj = TRUE))
    {
      coarse_i <- observations[[i]] %>%
        terra::project(., y = coarse) %>%
        terra::extract(coarse, .) %>%
        dplyr::select(., -1)

    } else {
      coarse_i <- observations[[i]] %>%
        terra::extract(coarse, .) %>%
        dplyr::select(., -1)
    }
    mean_i <- coarse_i %>%
      apply(., 2, mean)
    observations[[i]] %<>% merge(., coarse_i)
    coarse_resampled[[i]] <- fine[[i]]*0 + mean_i
  }
}


# Plot results

library(ggplot2)
library(gridExtra)
library(rasterVis)

plotted <- coarse_resampled
lyr <- 1

mybreaks <- function(x, interval = 100)
{
  out <- seq(from = x[1], to = x[2], by = interval)
  return(out)
}

if(length(names(plotted[[1]])) > 1)
{
  plotlist <- lapply(
    plotted
    , function(x)
    {
      out <- gplot(x[[lyr]]) +
        geom_tile(aes(fill = value)) +
        facet_wrap(~ variable) +
        coord_equal() +
        scale_x_continuous(breaks = mybreaks) +
        scale_y_continuous(breaks = mybreaks) +
        scale_fill_continuous(na.value = NA) +
        theme(axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank())
      return(out)
    }
  )
} else {
  plotlist <- lapply(
    plotted
    , function(x)
    {
      out <- gplot(x) +
        geom_tile(aes(fill = value)) +
        coord_equal() +
        scale_x_continuous(breaks = mybreaks) +
        scale_y_continuous(breaks = mybreaks) +
        scale_fill_continuous(na.value = NA) +
        theme(axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank())
      return(out)
    }
  )
}

do.call("grid.arrange"
        , c(plotlist
            , ncol = 3
            )
        )


# To do
# 1: Crop covariates to field extent
# 2: Round off decimals (covariates, EC, ortho)
# 3: Import input maps (OK)
# 4: Crop input maps to areas surrounding fields
# 5: Give names to all the lists (OK)

# # Playing around with colors
#
# cov_list[[1]] %>% plot
#
# r <- cov_list[[1]][[c(1, 2, 12)]]
#
# hue <- atan_2(r[[1]], r[[2]]) %>%
#   stretch(0, 1) %T>% plot
#
# sat <- cov_list[[1]][[12]] %>%
#   stretch(0, 1, 0.02, 0.98)
#
# val <- cov_list[[1]][[6]] %>%
#   stretch(0, 1)
#
# hsv_estrup <- c(hue, sat, val) %>%
#   as.data.frame(xy = TRUE, na.rm = FALSE) %>%
#   na.omit
#
# hsv_estrup %<>% as_tibble %>%
#   mutate(hsv = hsv(h = aspcos
#                    , s = slope
#                    , v = slope
#                    )) %>%
#   mutate(hsv_f = as.factor(hsv)
#          ) %>%
#   mutate(hsv_n = as.numeric(hsv_f)
#   ) %>%
#   select(x, y, hsv)
#
# estrup_rgb <- hsv_estrup$hsv %>%
#   col2rgb %>%
#   t %>%
#   as.data.frame %>%
#   bind_cols(hsv_estrup[, 1:2], .) %>%
#   rast
#
# estrup_rgb %>% plotRGB()
#
# library(colorspace)
#
# plot(cov_list[[1]][[12]])
#
# add(r) <- (acos(r[[1]]) + asin(r[[2]])) * (-1)
#
# names(r)[4] <- 'new'
#
# r[[3]] %<>% '*'(-1)
#
# r %<>% stretch(0, 255)
#
# plot(r)
#
# RGB(r) <- c(1, 2, 4, 3)
#
# plot(r)
#
# plotRGB(r, stretch = 'lin')

# END
