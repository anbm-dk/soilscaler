# Script for tersting

# install.packages("devtools")
library(devtools)

test_download <- FALSE

if (test_download) {
  install_github("anbm-dk/soilscaler")
  library(soilscaler)
} else {
  load_all()
}


# Required packages

library(terra)
library(magrittr)
library(dplyr)
library(caret)


# Optional

library(Cubist)

# Load data

my_obs <- list_unwrap(DK_observations, "EPSG:25832")
EC <- list_unwrap(DK_EC, "EPSG:25832")
RGB <- list_unwrap(DK_RGB, "EPSG:25832")
my_input <- DK_soilgrids %>% unwrap() %>% subset(1)
crs(my_input) <- "EPSG:4326"

my_covariates <- list()

for(i in 1:length(my_obs))
{
  my_covariates[[i]] <- c(
    EC[[i]],
    RGB[[i]]
  )
}

getwd() %>% paste0(., "/R/make_downscaler.R") %>% source()
getwd() %>% paste0(., "/R/check_downscaler.R") %>% source()

names(my_obs) <- NULL

# Check function

check_downscaler(
  obs = my_obs,
  cov = my_covariates,
  input = my_input,
  targ_name = "clay"
)

# / check function

downscaler1 <- make_downscaler(
  obs           = my_obs,
  targ_name     = "clay",
  model_type    = "lm",
  input         = my_input,
  make_maps     = TRUE,
  flatten_input = TRUE,
  input_as_cov  = TRUE,
  cov           = my_covariates,
  scale_cov     = "by_input",
  center_cov    = TRUE,
  scale_obs     = TRUE,
  center_obs    = FALSE,
  plot_results  = TRUE,
  keep_models   = TRUE
)

downscaler1$accuracy

downscaler1

varImp(downscaler1$model_general)

downscaler1$output_maps[[5]] %>% plot

downscaler1$model_general

par(mfrow = c(2, 3))

for (i in 1:length(my_obs)) {
  plot(my_obs[[i]], "clay", main = names(my_obs)[i])
}

dev.off()

# END
