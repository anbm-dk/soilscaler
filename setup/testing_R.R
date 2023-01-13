# Extra tests

library(devtools)

load_all()


# Required packages

library(terra)
library(magrittr)
library(dplyr)
library(caret)


# Optional

library(Cubist)


# Load data

fields       <- DK_fields %>% vect
obs          <- DK_observations %>% lapply(function(x) vect(x))
EC           <- DK_EC   %>% lapply(function(x) rast(x))
RGB          <- DK_RGB  %>% lapply(function(x) rast(x))
topo         <- DK_topo %>% lapply(function(x) rast(x))
soil_30m     <- DK_30m  %>% rast
soil_30m_unc <- DK_30m_unc %>% rast
sg           <- DK_soilgrids %>% rast
sg_unc       <- DK_soilgrids_unc %>% rast


useDK_map <- TRUE
if(useDK_map)
{
  my_input <- soil_30m[[1]]
  my_input_unc <- soil_30m_unc[[1]]
} else {
  my_input <- sg[[1]]
  my_input_unc <- sg_unc[[1]]
}

my_covariates <- list()

for(i in 1:length(obs))
{
  my_covariates[[i]] <- c(
    topo[[i]],
    EC[[i]],
    RGB[[i]]
  )
}

getwd() %>% paste0(., "/R/make_downscaler.R") %>% source()

downscaler1 <- make_downscaler(
  obs           = obs,
  targ_name     = "clay",
  model_type    = "lm",
  input         = my_input,
  input_unc     = my_input_unc,
  make_maps     = TRUE,
  unc_factor    = 1,
  flatten_input = TRUE,
  input_as_cov  = TRUE,
  cov           = my_covariates, # change this name
  scale_cov     = "by_input",
  center_cov    = TRUE,
  scale_obs     = FALSE,
  center_obs    = TRUE,
  results_plot  = TRUE,
)

downscaler1$accuracy

downscaler1

downscaler1$output_maps[[5]] %>% plot

downscaler1$model


# END
