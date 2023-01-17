# Data documentation

#' Five agricultural fields in Denmark.
#'
#' Field extents for the research fields described in Møller et al. (2020)
#'
#' @format ## `DK_fields`
#' A wrapped spatvector with five geometries. Unwrap using terra::vect(DK_fields)
#' \describe{
#'   \item{Site}{The name of the research field}
#'   ...
#' }
#' @source <https://doi.org/10.1016/j.geoderma.2020.114852>
"DK_fields"

# DK_observations
#' Soil observations for five agricultural fields in Denmark.
#'
#' Field extents for the research fields described in Møller et al. (2020)
#'
#' @format ## `DK_observations`
#' List of wrapped spatvectors. Unwrap using DK_observations %>% lapply(function(x) terra::vect(x))
#' @source <https://doi.org/10.1016/j.geoderma.2020.114852>
"DK_observations"

# DK_EC
# DK_RGB
# DK_soilgrids
# DK_soilgrids_unc

# END
