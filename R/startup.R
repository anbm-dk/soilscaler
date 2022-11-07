# Setup

install.packages(c("devtools", "roxygen2", "testthat", "knitr", "pkgload"))

library(devtools)
library(usethis)

# use_devtools()

# devtools::dev_sitrep() Doesn't work

use_git_config(
  user.name = "Anders Bjørn Møller"
  , user.email = "perserkongen@gmail.com"
)

use_git()

R.version.string

# update.packages(ask = FALSE, checkBuilt = TRUE)

# usethis::git_default_branch_configure()
#
# usethis::create_github_token()

# gitcreds::gitcreds_set() # For personal access token


# Make description and namespace

usethis::create_package("soilscaler")

# What I need

# 1 imput data imported into R
# - Input soil maps
# - Soil observations
# - EC and drone images
# - dems?

# 2 Downscaling fumction
# - Input soil map
# - Observations as list of spatial point datasets
# - Covariates as a list of spatraster objects
# - Inputs for caret
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

# 2 prediction function
# - input scaling object
# - covariates for new site (optional)
# - else, produce maps for the existing locations only

# 3 helper functions
# - check if the input data are valid
# - prepare data for train
# - function to import data?

# 4 Organize data to make them easily accessible

# END
