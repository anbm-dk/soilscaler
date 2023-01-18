#' Unwrap a list of spatial objects
#'
#' @param x A list of wrapped spatial objects created by `terra::wrap`
#' @param crs_new CRS to be assigned to the objects in the list (optional).
#' @returns A list of spatial objects
#' @export
#' @importFrom Rdpack reprompt
#' @examples
#' list_unwrap(DK_observations, "EPSG:25832")

list_unwrap <- function(
  x = NULL,
  crs_new = NULL
) {
  out <- base::lapply(
    x,
    function(x) {
      out2 <- terra::unwrap(x)
      if(!base::is.null(crs_new))
      {terra::crs(out2) <- crs_new}
      return(out2)
    }
  )
  return(out)
}

# END
