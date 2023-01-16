# Make description

library(desc)
library(usethis)

use_mit_license()

x <- desc(file = "DESCRIPTION")

# Title and description

x$set(
  Package = "soilscaler",
  Title = "Downscale digital soil maps to higher resolutions",
  Description = "Functions to downscale coarse-resolution soil maps to field scale, using high-resolution covariates and soil observations from other locations.",
  URL = "https://github.com/anbm-dk/soilscaler",
  BugReports = "https://github.com/anbm-dk/soilscaler/issues"
)

# Set dependencies

x$set_dep("terra")
x$set_dep("magrittr")
x$set_dep("dplyr")
x$set_dep("Rdpack") # For references
x$set_dep("caret")
x$set_dep("ggplot2")
x$set_dep("rlist")

# Set authors

x$del("Authors@R")

x$add_author(
  "Anders Bjørn",
  "Møller",
  email = "anbm@agro.au.dk",
  orcid = "0000-0002-2737-5780"
)

x$get_authors()

# Last check

x

# Write description

x$write(file = "DESCRIPTION")


# END
