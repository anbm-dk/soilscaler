# Extra tests

library(devtools)

load_all()

library(terra)
library(magrittr)

fields       <- DK_fields %>% vect
obs          <- DK_observations %>% lapply(function(x) vect(x))
EC           <- DK_EC   %>% lapply(function(x) rast(x))
RGB          <- DK_RGB  %>% lapply(function(x) rast(x))
topo         <- DK_topo %>% lapply(function(x) rast(x))
soil_30m     <- DK_30m  %>% lapply(function(x) rast(x))
soil_30m_unc <- DK_30m_unc %>% lapply(function(x) rast(x))
sg           <- DK_soilgrids %>% rast
sg_unc       <- DK_soilgrids_unc %>% rast


# END
