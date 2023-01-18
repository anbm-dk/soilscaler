# Data documentation

#' Five agricultural fields in Denmark.
#'
#' Polygon field extents for the research fields described in Møller et al. (2020)
#'
#' @format ## `DK_fields`
#' A wrapped SpatVector polygons dataset with five geometries.
#' \describe{
#'   \item{Site}{The name of the research field.}
#'   ...
#' }
#' @source <https://doi.org/10.1016/j.geoderma.2020.114852>
#' @examples
#' library(terra)
#' my_fields <- unwrap(DK_fields)
#' crs(my_fields) <- "EPSG:25832"
"DK_fields"

#' Soil observations from five agricultural fields in Denmark.
#'
#' Points with measured soil properties from the five research fields described in Møller et al. (2020).
#'
#' @format ## `DK_observations`
#' List of wrapped SpatVector point datasets.
#' @source <https://doi.org/10.1016/j.geoderma.2020.114852>
#' @examples
#' list_unwrap(
#'   DK_observations,
#'   "EPSG:25832"
#'   )
"DK_observations"

#' Electrical conductivity for five agricultural fields in Denmark.
#'
#' Raster layers showing the electrical conductivity for the topsoil (0 - 25 cm) in the five research fields described in Møller et al. (2020). Depth-specific values based on an inversion of data from a DUALEM21 sensor.
#'
#' @format ## `DK_EC`
#' List of wrapped SpatRaster objects with one layer each.
#' @source <https://doi.org/10.1016/j.geoderma.2020.114852>
#' @examples
#' list_unwrap(
#'   DK_EC,
#'   "EPSG:25832"
#'   )
"DK_EC"

#' Orthophotos for five agricultural fields in Denmark.
#'
#' Raster datasets showing RGB orthophotos for of the five research fields described in Møller et al. (2020). Based on averages from several images.
#'
#' @format ## `DK_RGB`
#' List of wrapped SpatRaster objects with three layers each.
#' @source <https://doi.org/10.1016/j.geoderma.2020.114852>
#' @examples
#' list_unwrap(
#'   DK_RGB,
#'   "EPSG:25832"
#'   )
"DK_RGB"

#' 250-m soil maps for five agricultural fields in Denmark.
#'
#' Raster dataset showing soil maps from SoilGrids250m 2.0 for the five research fields described in Møller et al. (2020). Cropped to the areas surrounding the fields.
#'
#' @format ## `DK_soilgrids`
#' Wrapped SpatRaster dataset.
#' @source <https://soilgrids.org/>
#' @examples
#' library(terra)
#' DK_soilgrids <- unwrap(DK_soilgrids)
#' crs(DK_soilgrids) <- "EPSG:4326"
"DK_soilgrids"

#' Uncertainties for 250-m soil maps for five agricultural fields in Denmark.
#'
#' Raster dataset showing uncertainties for soil maps from SoilGrids250m 2.0 for the five research fields described in Møller et al. (2020). The maps provide uncertainties as a 90% prediction interval. Cropped to the areas surrounding the fields.
#'
#' @format ## `DK_soilgrids_unc`
#' Wrapped SpatRaster dataset.
#' @source <https://soilgrids.org/>
#' @examples
#' library(terra)
#' DK_soilgrids_unc <- unwrap(DK_soilgrids_unc)
#' crs(DK_soilgrids_unc) <- "EPSG:4326"
"DK_soilgrids_unc"
