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
#' @param save_cov Append the covariates to the output object?
#' @param save_models Keep the models for mapping as part of the output object?
#' @returns A downscaler model
#' @examples
#' add(1, 1)
#' add(10, 1)
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
    save_cov      = FALSE,
    save_models   = FALSE
) {
  # To do:
  # Include option to use input map as a covariate or not [OK]
  # The use of input maps should be optional
  # Add bootstrapping sequence for uncertainty assessment
  # Include random alterations to basemap in this procedure, based on uncertainty
  # Include log transformation
  # Use do.call to train the model

  # Add checks for consistency

  # check for number and names of sites
  # check for covariate names
  # check for names in observation objects
  # check for sitenames
  # Check for number of sites

  targ <- targ_name
  sitenames <- names(obs)

  if (is.null(input)) {
    input_unc <- NULL
    input_as_cov <- FALSE
    scale_obs <- FALSE
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
        names(input_resampled[[i]]) <- c("input", "input_unc")[1:nlyr(input)]
      }

      obs[[i]] %<>%
        terra::extract(
          input_resampled[[i]],
          .,
          bind = TRUE
        )
    }

    mean_in %<>% bind_rows()
  }

  # Calculate the mean value of the target variable
  mean_targ <- obs %>%
    lapply(
      .,
      function(x) {
        out <- x %>%
          terra::values(.) %>%
          dplyr::select(., .data[[targ_name]]) %>%
          as.matrix() %>%
          mean(., na.rm = TRUE)
      }
    ) %>%
    unlist()

  # Option to scale and/or center observations
  if (scale_obs) {
    f1 <- function(x) {
      out <- x %>%
        terra::values(.) %>%
        dplyr::mutate(
          .,
          targ     = .data[[targ_name]],
          targ_sca = targ * mean_in[i, 1] / mean_targ[i]
        )
      terra::values(x) <- out
      return(x)
    }

    targ <- "targ_sca"
  }

  if (center_obs) {
    f1 <- function(x) {
      out <- x %>%
        terra::values(.) %>%
        dplyr::mutate(
          .,
          targ     = .data[[targ_name]],
          targ_cen = targ - mean_targ[i]
        )
      terra::values(x) <- out
      return(x)
    }

    targ <- "targ_cen"
  }

  if (scale_obs & center_obs) {
    f1 <- function(x) {
      out <- x %>%
        terra::values(.) %>%
        dplyr::mutate(
          .,
          targ          = .data[[targ_name]],
          targ_sca      = targ * mean_in[i, 1] / mean_targ[i],
          targ_sca_mean = mean(targ_sca,  na.rm = TRUE),
          targ_sca_cen  = targ_sca - targ_sca_mean
        )
      terra::values(x) <- out
      return(x)
    }

    targ <- "targ_sca_cen"
  }

  if (scale_obs | center_obs) {
    for (i in 1:length(sitenames)) {
      obs[[i]] %<>% f1()
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
    for (i in 1:length(sitenames))
    {
      means_i <- global(cov[[i]], "mean", na.rm = TRUE) %>% unlist()
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

  for (i in 1:length(sitenames))
  {
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

  for (i in 1:length(sitenames))
  {
    obs[[i]]$site <- sitenames[i]
  }

  thesecolumns <- c(targ, covnames, "site")

  trdat <- vect(obs) %>%
    terra::values(.) %>%
    dplyr::select(., all_of(thesecolumns)) %>%
    na.omit()

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
    paste0(targ, " ~ ", .) %>%
    stats::as.formula(.)

  # TrainControl object

  trc <- caret::trainControl(
    index = folds,
    savePredictions = "final"
  )

  # Train the model

  model <- caret::train(
    fm,
    trdat,
    method = model_type,
    trControl = trc
    # , importance = "impurity"
  )

  # If making maps, train an independent model for each site

  if (make_maps) {
    map_models <- list()

    for (i in 1:length(sitenames))
    {
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

      map_models[[i]] <- caret::train(
        fm,
        trdat_i,
        method = model_type,
        trControl = trc_i
        # , importance = "impurity"
      )
    }
  }

  # Make prediction maps for each site

  if (make_maps) {
    if (input_as_cov) {
      for (i in 1:length(sitenames))
      {
        cov[[i]] %<>% c(input_resampled[[i]], .)
      }
    }

    output <- list()

    for (i in 1:length(sitenames))
    {
      output[[i]] <- terra::predict(
        cov[[i]],
        map_models[[i]],
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

    # print(str(output))
    # print(str(obs[[1]]))

    for (i in 1:length(sitenames))
    {
      obs[[i]] %<>%
        terra::extract(
          output[[i]],
          .,
          bind = TRUE
        )
    }
  }

  # Accuracy for input and output maps
  # Include a step to get the final predictions when make_maps is FALSE

  rmse_na <- function(x, y) {
    out <- base::sqrt(
      base::mean(
        (x - y)^2,
        na.rm = TRUE
      )
    )
    return(out)
  }

  # Function to specify namespace in do.call
  getfun <- function(x) {
    if(length(grep("::", x)) > 0) {
      parts <- strsplit(x, "::")[[1]]
      getExportedValue(parts[1], parts[2])
    } else {
      x
    }
  }

  # List with arguments for summarise
  args_acc <- list(
    quote(.),
    RMSE_out = quote(
      rmse_na(
        .data[[targ_name]],
        output
      )
    ),
    cor_out = quote(
      cor(
        .data[[targ_name]],
        output,
        use = "pairwise.complete.obs"
      )
    )
  )
  if (!is.null(input))
  {
    args_acc %<>%
      rlist::list.insert(
        ., 1,
        RMSE_in = quote(
          rmse_na(
            .data[[targ_name]],
            input
          )
        )
      ) %>%
      rlist::list.insert(
        ., 3,
        cor_in = quote(
          cor(
            .data[[targ_name]],
            input,
            use = "pairwise.complete.obs"
          )
        )
      )
  }
  # mysummary <- function(mydata, myargs) {
  #   mydatalist <- list(mydata)
  #   allargs <- c(mydatalist, myargs)
  #   out <- do.call(
  #     what = getfun("dplyr::summarise"),
  #     args = allargs
  #   )
  #   return(out)
  # }
  results <- list()
  results$accuracy <- obs %>%
    terra::vect(.) %>%
    terra::values(.) %>%
    dplyr::group_by(., site) %>%
    mysummary(., args_acc)
    # list(.) %>%
    # c(., args_acc) %>%
    # do.call(
    #   what = getfun("dplyr::summarise")
    # )
    # dplyr::summarise(.,
    #   RMSE_in  = rmse_na(.data[[targ_name]], input),
    #   RMSE_out = rmse_na(.data[[targ_name]], output),
    #   cor_in = cor(
    #     .data[[targ_name]],
    #     input,
    #     use = "pairwise.complete.obs"
    #   ),
    #   cor_out = cor(
    #     .data[[targ_name]],
    #     output,
    #     use = "pairwise.complete.obs"
    #   )
    # )
  if (save_cov) {
    results$cov <- cov
  }
  results$obs <- obs
  results$model <- model
  if (make_maps) {
    if (save_models) {
      results$map_models <- map_models
    }
    results$output_maps <- output
  }
  if (results_plot) {
    results$plot <- obs %>%
      vect() %>%
      values() %>%
      tibble() %>%
      ggplot(., aes(x = .data[[targ_name]],
                    y = output, col = site)) +
      geom_point() +
      geom_abline() +
      coord_equal()
  }
  return(results)
}

# END
