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

names(my_obs) <- NULL

# Check function

check_downscaler <- function(
    obs           = NULL,
    targ_name     = NULL,
    cov           = NULL,
    input         = NULL,
    input_unc     = NULL,
    model_type    = "lm",
    make_maps     = TRUE,
    unc_factor    = 1,
    flatten_input = TRUE,
    input_as_cov  = FALSE,
    scale_cov     = c("no", "by_input", "by_SD"),
    scale_obs     = FALSE,
    center_cov    = TRUE,
    center_obs    = TRUE,
    plot_results  = FALSE,
    keep_obs      = FALSE,
    keep_cov      = FALSE,
    keep_models   = TRUE
) {

  out <- character()
  ind <- i
  # check if the observations are a list
  if (!is.list(obs)) {
    out[i] <- "`obs` is not a list"
    ind <- ind + 1
  } else {
    # Check if they are SpatVector objects
    obs_not_vect <- lapply(
      obs, function(x) {!is(x, "SpatVector")}) %>% unlist() %>% sum()
    if (obs_not_vect > 0) {
      out[i] <- "Some elements in `obs` are not SpatVector objects"
      ind <- ind + 1
    } else {
      # Check if they are points
      obs_not_pts <- lapply(
        obs, function(x) {!is.points(x, "SpatVector")}) %>% unlist() %>% sum()
      if (obs_not_pts > 0) {
        out[i] <- "Some elements in `obs` are not points"
        ind <- ind + 1
      }
    }
  }
  # Check if they have the same numbers of columns
  # Check if they have the same names
  # Check if the target variable is present


  # Check if the covariates are a list
  # Check if the covariates are spatrasters
  # Check if the number of elements match the observations
  # Check if covariate extents match observations
  # check for covariate names

  # check if input and input uncertainty are in the same format
  # check if they are spatrasters (or a list of them)
  # if they are a list, check if the number of elements match obs and cov

  # Warnings:
  # observations with missing covariates
  # observations with missing input


  allSame <- function(x) { length(unique(x)) == 1 }



}

# / check fundtion

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
