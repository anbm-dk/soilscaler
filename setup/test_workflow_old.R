# Test for basic workflow

library(terra)
library(magrittr)
library(dplyr)

root <- getwd()


# Test model

useDK_map <- TRUE
if(useDK_map)
{
  my_input <- DK_2014$DK_clay_mean
  my_input_unc <- DK_2014$DK_clay_SD
} else {
  my_input <- soilgrids$DK_00_20_clay_mean
  my_input_unc <- soilgrids$DK_00_20_clay_unc
}

my_covariates <- list()

for(i in 1:length(sites))
{
  my_covariates[[i]] <- c(
    cov_list[[i]]
    , EC_list[[i]]
    , ortho_list[[i]]
  ) %>%
    signif(digits = 4)
}

getwd() %>% paste0(., "/R/make_downscaler.R") %>% source()

downscaler1 <- make_downscaler(
  observations = obs
  , target     = "clay"
  , model_type = "ranger"
  , input      = my_input
  , input_unc  = my_input_unc
  , make_maps  = TRUE
  , unc_factor = 1
  , flatten_input      = TRUE
  , input_as_covariate = TRUE
  , covariates         = my_covariates
  , scale_covariates   = "by_input"
  , center_covariates  = TRUE
  , scale_obs    = "no"
  , center_obs   = TRUE
  , results_plot = TRUE
)

downscaler1

downscaler1$output_maps[[5]] %>% plot

downscaler1$model

# Plot the maps for all sites in one figure

# library(ggplot2)
# library(gridExtra)
# library(rasterVis)
#
# plotted <- input_resampled
# lyr <- 1
#
# mybreaks <- function(x, interval = 100)
# {
#   out <- seq(from = x[1], to = x[2], by = interval)
#   return(out)
# }
#
# if(length(names(plotted[[1]])) > 1)
# {
#   plotlist <- lapply(
#     plotted
#     , function(x)
#     {
#       out <- gplot(x[[lyr]]) +
#         geom_tile(aes(fill = value)) +
#         facet_wrap(~ variable) +
#         coord_equal() +
#         scale_x_continuous(breaks = mybreaks) +
#         scale_y_continuous(breaks = mybreaks) +
#         scale_fill_continuous(na.value = NA) +
#         theme(axis.text.x=element_blank(),
#               axis.text.y=element_blank(),
#               axis.ticks=element_blank(),
#               axis.title.x=element_blank(),
#               axis.title.y=element_blank())
#       return(out)
#     }
#   )
# } else {
#   plotlist <- lapply(
#     plotted
#     , function(x)
#     {
#       out <- gplot(x) +
#         geom_tile(aes(fill = value)) +
#         coord_equal() +
#         scale_x_continuous(breaks = mybreaks) +
#         scale_y_continuous(breaks = mybreaks) +
#         scale_fill_continuous(na.value = NA) +
#         theme(axis.text.x=element_blank(),
#               axis.text.y=element_blank(),
#               axis.ticks=element_blank(),
#               axis.title.x=element_blank(),
#               axis.title.y=element_blank())
#       return(out)
#     }
#   )
# }
#
# do.call("grid.arrange"
#         , c(plotlist
#             , ncol = 3
#             )
#         )


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
# names(r)[4] <- "new"
#
# r[[3]] %<>% "*"(-1)
#
# r %<>% stretch(0, 255)
#
# plot(r)
#
# RGB(r) <- c(1, 2, 4, 3)
#
# plot(r)
#
# plotRGB(r, stretch = "lin")

# END
