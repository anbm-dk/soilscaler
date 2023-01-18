#' Train a downscaler model
#'
#' @param obs List of spatvector points with observations.
#' @param targ_name Name of target variable.
#' @param cov List of SpatRaster objects containing the covariates for prediction.
#' @param input SpatRaster or list of SpatRaster objects containing the coarse resolution input map.
#' @param input_unc SpatRaster containing the uncertainty for the coarse-resolution input map.
#' @param model_type Model type passed to train. Ideally, I should use do.call, so users can pass a list of arguments to the training.
#' @param make_maps Should the function produce maps for the input sites when run?
#' @param unc_factor Multiplication factor for the uncertainties. Not implemented.
#' @param flatten_input Should the input be transformed to a mean value for each site?
#' @param input_as_cov Transform the input map to a covariate for mapping?
#' @param scale_cov Should the function scale the covariates relative to the input maps or their own standard deviations?  Default is no scaling.
#' @param scale_obs Should the function scale the observations relative to the input maps? Back-transformation is automatic for predictions. Default is no scaling.
#' @param center_cov Should the function center the covariates?
#' @param center_obs Should the function center the observations? Back-transformation is automatic for predictions.
#' @param results_plot Should the output contain a plot with the results?
#' @param keep_cov Append the covariates to the output object?
#' @param keep_models Keep the models for mapping as part of the output object?
#' @returns A downscaler model
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
#' ds <- make_downscaler(
#' obs           = my_obs,
#' targ_name     = "clay",
#' input         = my_input,
#' make_maps     = FALSE,
#' cov           = my_cov
#' )
#'
#' ds
#'

# Function to train a downscaler model

make_downscaler <- function(
    obs           = NULL,
    targ_name     = NULL,
    cov           = NULL,
    input         = NULL,
    input_unc     = NULL,
    model_type    = "lm",
    make_maps     = TRUE,
    unc_factor    = 1,
    flatten_input = TRUE,
    input_as_cov  = TRUE,
    scale_cov     = c("no", "by_input", "by_SD"),
    scale_obs     = FALSE,
    center_cov    = TRUE,
    center_obs    = TRUE,
    results_plot  = FALSE,
    keep_cov      = FALSE,
    keep_models   = FALSE
) {
  # To do:
  # Include option to use input map as a covariate or not [OK]
  # The use of input maps should be optional [OK]
  # Include a step to get the final predictions when make_maps is FALSE [OK]
  # Include log transformation
  # Use do.call to train the models
  # Add bootstrapping sequence for uncertainty assessment
  # Include random alterations to basemap in this procedure, based on uncertainty

  # Add checks for consistency

  # check for number and names of sites
  # check for covariate names
  # check for names in observation objects
  # check for sitenames
  # Check for number of sites

  targ_col <- targ_name
  sitenames <- names(obs)
  . <- NULL  # To avoid warnings in the package check.

  if(length(scale_cov) > 1) {
    scale_cov <- "no"
  }

  if (is.null(input)) {
    input_unc    <- NULL
    input_as_cov <- FALSE
    scale_obs    <- FALSE
    center_obs   <- FALSE
    if (scale_cov == "by_input") {
      scale_cov <- "no"
      message(
        strwrap(
          prefix = "\n",
          initial = "",
          "No input map provided. Covariate scaling skipped. You can use scale_cov = 'by_SD' to scale covariates by their standard deviations instead."
          )
        )
    }
  }

  if (!is.null(input)) {
    # Extract input
    if (!is.null(input_unc)) {
      input <- c(input, input_unc)
      names(input) <- c("input", "input_unc")
    }

    listproj <- function(x) {
      out <- lapply(
        x,
        function(x2) {
          out2 <- terra::crs(
            x2,
            proj = TRUE
          )
          return(out2)
        }
      )
      return(out)
    }

    crs_in  <- terra::crs(input, proj = TRUE)
    crs_cov <- cov %>% listproj
    crs_obs <- obs %>% listproj
    input_resampled <- list()
    mean_in <- list()

    # Extract input values
    for (i in 1:length(sitenames)) {
      # Project or resample input maps
      if (crs_in == crs_cov[[i]]) {
        input_resampled[[i]] <- input %>%
          terra::resample(
            .,
            cov[[i]][[1]]
          )
      } else {
        input_resampled[[i]] <- input %>%
          terra::project(
            .,
            cov[[i]][[1]]
          )
      }

      input_resampled[[i]] %<>%
        terra::mask(
          .,
          cov[[i]][[1]]
        )

      mean_in[[i]] <- input_resampled[[i]] %>%
        terra::global(
          .,
          na.rm = TRUE
        ) %>%
        unlist()

      if (flatten_input) {
        input_resampled[[i]] <- cov[[i]][[1]] * 0 + mean_in[[i]]
        names(input_resampled[[i]]) <- c("input", "input_unc")[1:terra::nlyr(input)]
      }

      obs[[i]] %<>%
        terra::extract(
          input_resampled[[i]],
          .,
          bind = TRUE
        )
    }

    mean_in %<>% dplyr::bind_rows(.)
  }

  # Option to scale and/or center observations
  if (scale_obs | center_obs) {
    args_sca_cen <- list(
      targ = rlang::quo(
        .data[[targ_name]]
      ),
      mean_targ = rlang::quo(
        mean(.data$targ,  na.rm = TRUE)
      )
    )

    if (scale_obs) {
      args_sca_cen %<>%
        rlist::list.append(
          .,
          mean_input = rlang::quo(
            mean(.data$input,  na.rm = TRUE)
          ),
          targ_sca = rlang::quo(
            .data$targ * .data$mean_input / .data$mean_targ
          )
        )

      targ_col <- "targ_sca"
    }

    if (center_obs & !scale_obs) {
      args_sca_cen %<>%
        rlist::list.append(
          .,
          targ_cen = rlang::quo(
            .data$targ - .data$mean_targ
          )
        )

      targ_col <- "targ_cen"
    }

    if (scale_obs & center_obs) {
      args_sca_cen %<>%
        rlist::list.append(
          .,
          targ_sca_mean = rlang::quo(
            mean(.data$targ_sca,  na.rm = TRUE)
          ),
          targ_sca_cen = rlang::quo(
            .data$targ_sca - .data$targ_sca_mean
          )
        )

      targ_col <- "targ_sca_cen"
    }

    for (i in 1:length(sitenames)) {
      args_sca_cen <- rlang::new_quosures(args_sca_cen)

      newvalues <- terra::values(obs[[i]]) %>%
            dplyr::mutate(., !!! args_sca_cen)

      terra::values(obs[[i]]) <- newvalues
    }
  }

  # Option to center and/or scale covariates
  # Should there be an option to keep both standard and scaled covariates?

  if (scale_cov == "by_SD") {
    cov %<>% lapply(
      .,
      function(x) {
        out <- terra::scale(
          x,
          center = FALSE
        )
        return(out)
      }
    )
  }

  if (scale_cov == "by_input") {
    for (i in 1:length(sitenames)) {
      means_i <- terra::global(
        cov[[i]],
        "mean",
        na.rm = TRUE
        ) %>% unlist()
      cov[[i]] %<>% "*"(input_resampled[[i]][[1]])
      cov[[i]] %<>% "/"(means_i)
    }
  }

  if (center_cov) {
    cov %<>% lapply(function(x) {
      out <- terra::scale(
        x,
        scale = FALSE
      )
      return(out)
    })
  }

  # Extract covariates

  for (i in 1:length(sitenames)) {
    obs[[i]] %<>%
      terra::extract(
        cov[[i]],
        .,
        bind = TRUE
      )
  }

  # Select data for model

  covnames <- names(cov[[1]])
  if(input_as_cov) {
    if (!is.null(input_unc)) covnames %<>% c("input_unc", .)
    if (!is.null(input)) covnames %<>% c("input", .)
  }

  for (i in 1:length(sitenames)) {
    obs[[i]]$site <- sitenames[i]
  }

  thesecolumns <- c(targ_col, covnames, "site")

  trdat <- terra::vect(obs) %>%
    terra::values(.) %>%
    dplyr::select(
      ., tidyselect::all_of(thesecolumns)
      ) %>%
    stats::na.omit(.)

  # Create folds for cross validation

  folds <- sitenames %>%
    lapply(
      function(x) {
        out <- c(1:nrow(trdat))[trdat$site != x]
        return(out)
        }
      )

  # Create formula

  fm <- covnames %>%
    base::paste(., collapse = " + ") %>%
    paste0(targ_col, " ~ ", .) %>%
    stats::as.formula(.)

  # TrainControl object

  trc <- caret::trainControl(
    index = folds,
    savePredictions = "final"
  )

  # Train the model

  model_general <- caret::train(
    fm,
    trdat,
    method = model_type,
    trControl = trc
    # , importance = "impurity"
  )

  # Train an independent prediction model for each site

  models_leave_site_out <- list()

  for (i in 1:length(sitenames)) {
    # Select data

    trdat_i <- trdat[folds[[i]], ]

    # Create folds for cross validation

    folds_i <- sitenames[-i] %>%
      lapply(
        function(x) {
          out <- c(1:nrow(trdat_i))[trdat_i$site != x]
          return(out)
        }
      )

    # Train control object

    trc_i <- caret::trainControl(
      index = folds_i
    )

    # Train the model

    models_leave_site_out[[i]] <- caret::train(
      fm,
      trdat_i,
      method = model_type,
      trControl = trc_i
      # , importance = "impurity"
    )
  }

  # Make predictions
  if (make_maps) {
    # Make prediction maps for each site
    if (input_as_cov) {
      for (i in 1:length(sitenames)) {
        cov[[i]] %<>% c(input_resampled[[i]], .)
      }
    }

    output <- list()

    for (i in 1:length(sitenames)) {
      output[[i]] <- terra::predict(
        cov[[i]],
        models_leave_site_out[[i]],
        na.rm = TRUE
      )
      if (center_obs) {
        baseline <- unlist(mean_in[i, 1])
        output[[i]] %<>% "+"(baseline)
      }
      # also modify output maps in case of log transformations
      names(output[[i]]) <- "output"
    }

    # Extract predictions to the observations for the final accuracy assessment
    for (i in 1:length(sitenames)) {
      obs[[i]] %<>%
        terra::extract(
          output[[i]],
          .,
          bind = TRUE
        )
    }
  } else {
    # Make predictions for observation points
    for (i in 1:length(sitenames)) {
      outcov_i <- obs[[i]] %>%
        terra::values(.) %>%
        dplyr::select(
          .,
          tidyselect::all_of(covnames)
          )

      output_i <- obs[[i]] %>%
        terra::values() %>%
        nrow() %>%
        numeric()

      notna <- rowSums(is.na(outcov_i)) == 0

      output_i[notna] <- caret::predict.train(
        models_leave_site_out[[i]],
        outcov_i
      )

      if (center_obs) {
        baseline <- unlist(mean_in[i, 1])
        output_i %<>% "+"(baseline)
      }
      # also modify output in case of log transformations

      obs[[i]]$output <- output_i
    }
  }

  # Accuracy for input and output maps

  rmse_na <- function(x, y) {
    out <- base::sqrt(
      base::mean(
        (x - y)^2,
        na.rm = TRUE
      )
    )
    return(out)
  }

  # List of quosures with arguments for summarise
  args_acc <- list(
    RMSE_out = rlang::quo(
      rmse_na(
        .data[[targ_name]],
        .data$output
      )
    ),
    cor_out = rlang::quo(
      stats::cor(
        .data[[targ_name]],
        .data$output,
        use = "pairwise.complete.obs"
      )
    )
  )

  # Add arguments for input accuracy if applicable
  if (!is.null(input))
  {
    args_acc %<>%
      rlist::list.append(
        .,
        RMSE_in = rlang::quo(
          rmse_na(
            .data[[targ_name]],
            .data$input
          )
        ),
        cor_in = rlang::quo(
          stats::cor(
            .data[[targ_name]],
            .data$input,
            use = "pairwise.complete.obs"
          )
        )
      ) %>%
      rlist::list.subset(
        .,
        c("RMSE_in", "RMSE_out", "cor_in", "cor_out")
      )
  }

  # Convert list of quosures to a ´quosures´ class object
  args_acc <- rlang::new_quosures(args_acc)

  results <- list()

  results$accuracy <- obs %>%
    terra::vect(.) %>%
    terra::values(.) %>%
    dplyr::group_by(., .data$site) %>%
    dplyr::summarise(., !!! args_acc)

  if (keep_cov) {
    results$cov <- cov
  }

  results$obs <- obs

  if (keep_models) {
    results$model_general <- model_general
    results$models_leave_site_out <- models_leave_site_out
  }

  if (make_maps) {
    results$output_maps <- output
  }

  if (results_plot) {
    results$plot <- obs %>%
      terra::vect(.) %>%
      terra::values(.) %>%
      tibble::tibble(.) %>%
      ggplot2::ggplot(
        .,
        ggplot2::aes_string(
          x = ".data[[targ_name]]",
          y = "output",
          col = "site"
        )
      ) +
      ggplot2::geom_point() +
      ggplot2::geom_abline() +
      ggplot2::coord_equal()
  }

  return(results)
}

# END
