#' Check objects to be used in a downscaler model
#'
#' @param obs List of spatvector points with observations.
#' @param targ_name Name of target variable.
#' @param cov List of SpatRaster objects containing the covariates for prediction.
#' @param input SpatRaster or list of SpatRaster objects containing the coarse resolution input map.
#' @returns A list with messages, warnings, indices for problematic objects, and an indication if the downscaler model should proceed.
#' @export
#' @importFrom Rdpack reprompt
#' @importFrom rlang .data
#' @importFrom magrittr %<>%
#' @examples
#' library(magrittr)
#' library(terra)
#'
#' data(DK_observations)
#' data(DK_soilgrids)
#' data(DK_EC)
#'
#' my_obs <- list_unwrap(DK_observations, "EPSG:25832")
#'
#' my_input <- DK_soilgrids %>% unwrap() %>% subset(1)
#' crs(my_input) <- "EPSG:4326"
#'
#' my_cov <- list_unwrap(DK_EC, "EPSG:25832")
#'
#' check_downscaler(
#' obs           = my_obs,
#' targ_name     = "clay",
#' input         = my_input,
#' cov           = my_cov
#' )
#'
#' check_downscaler(
#' obs           = my_obs,
#' targ_name     = "clayclay",
#' input         = my_input,
#' cov           = my_cov
#' )


check_downscaler <- function(
    obs           = NULL,
    targ_name     = NULL,
    cov           = NULL,
    input         = NULL
) {
  . <- NULL  # To avoid warnings in the package check
  out_stopfunction <- FALSE
  out_warnings <- character()
  out_messages <- character()
  out_objects <- list()
  ind_w <- 1
  ind_m <- 1
  # Check if the target variable is a character
  targ_name_ok <- TRUE
  targ_not_chr <- !is.character(targ_name)
  if (targ_not_chr) {
    out_warnings[ind_w] <- "`targ_name` is not a character."
    targ_name_ok <- FALSE
    out_stopfunction <- TRUE
    ind_w <- ind_w + 1
  }
  if (targ_name_ok) {
    if (length(targ_name) > 1) {
      targ_name <- targ_name[1]
      out_messages[ind_m] <- "`targ_name` contains multiple elements. Using only the first element."
      ind_m <- ind_m + 1
    }
  }
  # Check observations
  # check if the observations are a list
  obs_ok <- TRUE
  if (!is.list(obs)) {
    out_warnings[ind_w] <- "`obs` is not a list."
    obs_ok <- FALSE
    out_stopfunction <- TRUE
    ind_w <- ind_w + 1
  }
  if (obs_ok) {
    # Check if they are SpatVector objects
    obs_not_vec <- lapply(
      obs, function(x) {!is(x, "SpatVector")}) %>%
      unlist()
    if (sum(obs_not_vec) > 0) {
      out_warnings[ind_w] <- "Some elements in `obs` are not SpatVector objects."
      out_objects$obs_not_SpatVector <- obs_not_vec
      obs_ok <- FALSE
      out_stopfunction <- TRUE
      ind_w <- ind_w + 1
    }
  }
  if (obs_ok) {
    # Check if they are points
    obs_not_pts <- lapply(
      obs, function(x) {!is.points(x)}) %>%
      unlist()
    if (sum(obs_not_pts) > 0) {
      out_warnings[ind_w] <- "Some elements in `obs` are not points."
      out_objects$obs_not_points <- obs_not_pts
      obs_ok <- FALSE
      out_stopfunction <- TRUE
      ind_w <- ind_w + 1
    }
  }
  if (obs_ok) {
    # Check if they have the same number of columns
    obs_ncol <- lapply(
      obs, function(x) {terra::values(x) %>% ncol()}
    ) %>%
      unlist()
    obs_col_diff <- obs_ncol %>%
      unique() %>%
      length() %>%
      magrittr::equals(1) %>%
      magrittr::not()
    if (obs_col_diff) {
      out_messages[ind_m] <- "The elements in `obs` contain different numbers of columns."
      out_objects$obs_ncol <- obs_ncol
      ind_m <- ind_m + 1
    }
  }
  if (obs_ok & targ_name_ok) {
    # Check if the target variable is present
    targ_not_present <- lapply(
      obs, function(x) {!targ_name %in% (terra::values(x) %>% colnames())}
    ) %>%
      unlist()
    if (sum(targ_not_present) > 0) {
      out_warnings[ind_w] <- "The target variable is missing from one or more elements in `obs`."
      out_objects$targ_name_missing <- targ_not_present
      obs_ok <- FALSE
      out_stopfunction <- TRUE
      ind_w <- ind_w + 1
    }
  }
  # Check covariates
  cov_ok <- TRUE
  # Check if the covariates are a list
  if (!is.list(cov)) {
    out_warnings[ind_w] <- "`cov` is not a list."
    cov_ok <- FALSE
    out_stopfunction <- TRUE
    ind_w <- ind_w + 1
  }
  # Check if the covariates are spatrasters
  if (cov_ok) {
    cov_not_rast <- lapply(
      cov, function(x) {!is(x, "SpatRaster")}) %>%
      unlist()
    if (sum(cov_not_rast) > 0) {
      out_warnings[ind_w] <- "Some elements in `cov` are not SpatRaster objects."
      out_objects$cov_not_SpatRaster <- cov_not_rast
      cov_ok <- FALSE
      out_stopfunction <- TRUE
      ind_w <- ind_w + 1
    }
  }
  # check covariate names
  if (cov_ok) {
    cov_all_names <- lapply(cov, function(x) { return(names(x)) } ) %>%
      unlist() %>%
      unique()
    cov_missing <- sapply(
      1:length(cov), function(x) {
        out <- !(cov_all_names %in% names(cov[[x]]))
        return(out)
      },
      simplify = TRUE
    ) %>%
      t()
    colnames(cov_missing) <- cov_all_names
    if (sum(cov_missing > 0)) {
      out_warnings[ind_w] <- "Some elements in `cov` are missing layers, or names are not the same."
      out_objects$cov_layer_missing <- cov_missing
      cov_ok <- FALSE
      out_stopfunction <- TRUE
      ind_w <- ind_w + 1
    }
  }
  # Check if lengths of cov and obs match
  if (obs_ok & cov_ok) {
    obs_cov_length <- length(obs) == length(cov)
    if (!obs_cov_length) {
      out_warnings[ind_w] <- "`obs` and `cov` do not have the same length."
      out_objects$list_length <- data.frame(
        obs = length(obs),
        cov = length(cov)
      )
      cov_ok <- FALSE
      out_stopfunction <- TRUE
      ind_w <- ind_w + 1
    }
  }
  # Check if covariate extents match observations
  if (obs_ok & cov_ok) {
    obs_cov_overlap <- lapply(
      1:length(obs),
      function(x) {
        out <- terra::intersect(
          terra::ext(obs[[x]]),
          terra::ext(cov[[x]])
        ) %>%
          length()
        return(out)
      }
    ) %>%
      unlist()
    if (sum(!obs_cov_overlap) > 0) {
      out_warnings[ind_w] <- "Some elements in `obs` do not overlap with the same element in `cov`."
      out_objects$obs_cov_mismatch <- !obs_cov_overlap
      cov_ok <- FALSE
      out_stopfunction <- TRUE
      ind_w <- ind_w + 1
    }
  }
  # Check input layers
  input_ok <- TRUE
  # Check if input is NULL
  if (is.null(input)) {
    input_ok <- FALSE
  }
  # Check if input is a list
  input_list <- FALSE
  if (input_ok) {
    input_list <- is.list(input)
  }
  # Check if input is a spatRaster
  input_rast <- FALSE
  if (input_ok & !input_list) {
    input_rast <- is(input, "SpatRaster")
  }
  # Stop if input is not a list or a spatraster
  if (input_ok & !input_list & !input_rast) {
    out_warnings[ind_w] <- "`input` is neither a list or a SpatRaster."
    input_ok <- FALSE
    out_stopfunction <- TRUE
    ind_w <- ind_w + 1
  }
  # Check overlaps between input raster, obs and cov
  if (input_ok & input_rast & obs_ok & cov_ok) {
    input_overlaps <- sapply(
      1:length(obs),
      function(x) {
        ext_obs_x <- project(
          ext(obs[[x]]),
          crs(obs[[x]]),
          crs(input)
        )
        ext_cov_x <- project(
          ext(obs[[x]]),
          crs(obs[[x]]),
          crs(input)
        )
        out1 <- terra::intersect(
          terra::ext(input),
          ext_obs_x
        ) %>%
          length()
        out2 <- terra::intersect(
          terra::ext(input),
          ext_cov_x
        ) %>%
          length()
        return(c(out1, out2))
      },
      simplify = TRUE
    ) %>%
      t()
    colnames(input_overlaps) <- c("input_obs", "input_cov")
    if (sum(!input_overlaps[, 1]) > 0) {
      out_warnings[ind_w] <- "Some elements in `obs` do not overlap with `input`."
      out_objects$input_obs_mismatch <- !input_overlaps[, 1]
      input_ok <- FALSE
      out_stopfunction <- TRUE
      ind_w <- ind_w + 1
    }
    if (sum(!input_overlaps[, 2]) > 0) {
      out_warnings[ind_w] <- "Some elements in `cov` do not overlap with `input`."
      out_objects$input_obs_mismatch <- !input_overlaps[, 2]
      input_ok <- FALSE
      out_stopfunction <- TRUE
      ind_w <- ind_w + 1
    }
  }
  # If input is a list, check if the elements are spatrasters
  if (input_ok & input_list) {
    input_not_rast <- lapply(
      input, function(x) {!is(x, "SpatRaster")}) %>%
      unlist()
    if (sum(input_not_rast) > 0) {
      out_warnings[ind_w] <- "Some elements in `input` are not SpatRaster objects."
      out_objects$input_not_SpatRaster <- input_not_rast
      input_ok <- FALSE
      out_stopfunction <- TRUE
      ind_w <- ind_w + 1
    }
  }
  # Check if the length of input matches cov and obs, if input is a list
  if (obs_ok & cov_ok & input_ok & input_list) {
    obs_input_length <- length(obs) == length(input)
    cov_input_length <- length(cov) == length(input)
    if (!obs_input_length) {
      out_warnings[ind_w] <- "`obs` and `input` do not have the same length."
      input_ok <- FALSE
      out_stopfunction <- TRUE
      ind_w <- ind_w + 1
    }
    if (!cov_input_length) {
      out_warnings[ind_w] <- "`cov` and `input` do not have the same length."
      input_ok <- FALSE
      out_stopfunction <- TRUE
      ind_w <- ind_w + 1
    }
    if ((!obs_input_length) | (!cov_input_length)) {
      out_objects$list_length <- data.frame(
        obs = length(obs),
        cov = length(cov),
        input = length(input)
      )
    }
  }
  # Check extents for input list elements
  if (input_ok & input_list & obs_ok & cov_ok) {
    input_overlaps <- sapply(
      1:length(input),
      function(x) {
        out1 <- terra::intersect(
          terra::ext(input[[x]]),
          terra::ext(obs[[x]])
        ) %>%
          length()
        out2 <- terra::intersect(
          terra::ext(input[[x]]),
          terra::ext(cov[[x]])
        ) %>%
          length()
        return(c(out1, out2))
      },
      simplify = TRUE
    ) %>%
      t()
    colnames(input_overlaps) <- c("input_obs", "input_cov")
    if (sum(!input_overlaps[, 1]) > 0) {
      out_warnings[ind_w] <- "Some elements in `input` do not overlap with the same element in `obs`."
      out_objects$input_obs_mismatch <- !input_overlaps[, 1]
      input_ok <- FALSE
      out_stopfunction <- TRUE
      ind_w <- ind_w + 1
    }
    if (sum(!input_overlaps[, 2]) > 0) {
      out_warnings[ind_w] <- "Some elements in `input` do not overlap with the same element in `cov`."
      out_objects$input_cov_mismatch <- !input_overlaps[, 1]
      input_ok <- FALSE
      out_stopfunction <- TRUE
      ind_w <- ind_w + 1
    }
  }
  # Add checks for uncertainty later
  # check if input and input uncertainty are in the same format
  # Warnings later in the main function:
  # observations with missing covariates
  # observations with missing input
  if (obs_ok) {
    out_messages[ind_m] <- "`obs` ok."
    ind_m <- ind_m + 1
  }
  if (cov_ok) {
    out_messages[ind_m] <- "`cov` ok."
    ind_m <- ind_m + 1
  }
  if (input_ok) {
    out_messages[ind_m] <- "`input` ok."
    ind_m <- ind_m + 1
  }
  if (out_stopfunction) {
    out_warnings[ind_w] <- "Function will stop."
    ind_w <- ind_w + 1
  }
  if (length(out_warnings) == 0) { out_warnings[1] <- "No warnings." }
  out_list <- list(
    messages = out_messages,
    warnings = out_warnings,
    stop_function = out_stopfunction
  )
  if (length(out_objects) > 0) { out_list$objects <- out_objects }
  return(out_list)
}

# END
