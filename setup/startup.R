# Setup

install.packages(
  c(
    "devtools",
    "roxygen2",
    "testthat",
    "knitr",
    "pkgload")
)

library(devtools)
library(usethis)

# use_devtools()

## 1: Github stuff

# use_git_config(
#   user.name = "Anders Bjørn Møller"
#   , user.email = "perserkongen@gmail.com"
# )

# use_git()

R.version.string

# update.packages(ask = FALSE, checkBuilt = TRUE)

# usethis::git_default_branch_configure()
#
# usethis::create_github_token()

# gitcreds::gitcreds_set() # For personal access token

## 2: Other stuff

# usethis::use_data_raw()  # Creates folder for raw data

# usethis::use_readme_rmd()  # Creates and rmd for the readme

devtools::build_readme()  # Renders the readme

usethis::use_build_ignore(
  "setup"
)

use_pipe(export = TRUE)  # Make maggrittr pipes available to users

devtools::document()

# devtools::run_examples()

devtools::check(document = FALSE)

# What I need

# 1 input data imported into R [OK]
# - Input soil maps
# - Soil observations
# - EC and drone images
# - dems?

# 2 Downscaling function [OK]
# - Input soil map
# - Observations as list of spatial point datasets
# - Covariates as a list of spatraster objects
# - Inputs for caret [wip]
# -- method
# -- tuning grid
# -- trcontrol
# -- always use leave site out CV
# -- option to produce predictions for each site
# -- checks for site names, variable names, covariate names across datasets
# -- options for uncertainty estimates
# -- include bootstrapping for uncertainty
# -- output folder
# -- specify target variable
# -- option for scaling and centering covariates
# -- use input map as a covariate or as a baseline
# -- local or general baseline?
# -- list of approaches, so the function runs an experiment?
# - attach input maps, observations and input maps?
# - add another level for leave site out, to make maps independent
# - use uncertainties from input maps?

# 2 prediction function [to do]
# - input scaling object
# - covariates for new site (optional)
# - else, produce maps for the existing locations only

# 3 helper functions [to do]
# - check if the input data are valid
# - prepare data for train
# - function to import data?

# 4 Organize data to make them easily accessible [OK]

# END
