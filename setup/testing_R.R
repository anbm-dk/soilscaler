# Extra tests

# install.packages("devtools")
library(devtools)

load_all()



# install_github("anbm-dk/soilscaler")
# library(soilscaler)


# Required packages

library(terra)
library(magrittr)
library(dplyr)
library(caret)
library(rlang)  # For quosures


# Optional

library(Cubist)

# Load data

fields       <- DK_fields %>% unwrap
obs          <- DK_observations %>% lapply(function(x) unwrap(x))
EC           <- DK_EC   %>% lapply(function(x) unwrap(x))
RGB          <- DK_RGB  %>% lapply(function(x) unwrap(x))
# topo         <- DK_topo %>% lapply(function(x) rast(x))
# soil_30m     <- DK_30m  %>% rast
# soil_30m_unc <- DK_30m_unc %>% rast
sg           <- DK_soilgrids %>% unwrap
sg_unc       <- DK_soilgrids_unc %>% unwrap


# useDK_map <- TRUE
# if(useDK_map)
# {
#   my_input <- soil_30m[[1]]
#   my_input_unc <- soil_30m_unc[[1]]
# } else {
#   my_input <- sg[[1]]
#   my_input_unc <- sg_unc[[1]]
# }

my_input <- sg[[1]]
my_input_unc <- sg_unc[[1]]

my_covariates <- list()

for(i in 1:length(obs))
{
  my_covariates[[i]] <- c(
    # topo[[i]],
    EC[[i]],
    RGB[[i]]
  )
}

# getwd() %>% paste0(., "/R/make_downscaler.R") %>% source()

downscaler1 <- make_downscaler(
  obs           = obs,
  targ_name     = "clay",
  model_type    = "lm",
  input         = my_input,
  # input_unc     = my_input_unc,
  make_maps     = FALSE,
  # unc_factor    = 1,
  flatten_input = TRUE, # Needs fixing
  input_as_cov  = TRUE,
  cov           = my_covariates,
  scale_cov     = "by_input",
  center_cov    = TRUE,
  scale_obs     = TRUE,
  center_obs    = TRUE,
  results_plot  = TRUE,
  keep_models   = TRUE
)

downscaler1$accuracy

downscaler1

varImp(downscaler1$model_general)

downscaler1$output_maps[[5]] %>% plot

downscaler1$model_general


# END
