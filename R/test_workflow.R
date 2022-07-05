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
