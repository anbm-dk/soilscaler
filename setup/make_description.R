# Make description

library(desc)
library(usethis)

use_mit_license()

x <- desc(file = "DESCRIPTION")

# Title and description

desc$set(
  Package = "soilscaler"
  , Title = "Downscale digital soil maps to higher resolutions"
  , Description = "Functions to downscale coarse-resolution soil maps to field scale, using high-resolution covariates and soil observations from other locations."
  , URL = "https://github.com/anbm-dk/soilscaler"
  , BugReports = "https://github.com/anbm-dk/soilscaler/issues"
)

# Set dependencies

desc$set_dep("terra")
desc$set_dep("magrittr")
desc$set_dep("dplyr")
desc$set_dep("Rdpack") # For references
desc$set_dep("caret")
desc$set_dep("ggplot2")

# Set authors

desc$del("Authors@R")

desc$add_author(
  "Anders Bjørn"
  , "Møller"
  , email = "anbm@agro.au.dk"
  , orcid = "0000-0002-2737-5780"
)

desc$get_authors()

# Last check

desc

# Write description

desc$write(file = "DESCRIPTION")


# END
