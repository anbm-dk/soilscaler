# Function to train a downscaler model

make_downscaler <- function(
    observations = NULL         # List of spatvector points with observations
    , target = NULL             # Name of target variable
    , model_type = 'lm'         # Ideally, I should use do.call, so users can pass a list of arguments to the training
    , input = NULL              # SpatRaster containing the coarse resolution input map
    , input_unc = NULL          # SpatRaster containing the uncertainty for the coarse resolution input map
    , make_maps = TRUE          # Should the function produce maps when run?
    , unc_factor = 1            # not implemented
    , flatten_input = TRUE
    , input_as_covariate = TRUE  # not implemented
    , covariates = NULL
    , scale_covariates = c('no', 'by_input', 'by_SD')
    , center_covariates = TRUE
    , scale_obs = c('no', 'by_input', 'by_SD')
    , center_obs = TRUE
    , results_plot = FALSE
    , save_covariates = FALSE
    , save_map_models = FALSE
  )
{
  require(caret)

  # To do: add bootstrapping sequence for uncertainty assessment
  # Include random alterations to basemap in this procedure, based on uncertainty
  # Include option to use input map as a covariate or not
  # Include log transformation

  target_name <- target

  # Add checks for consistency

  # check for number and names of sites
  # check for covariate names
  # check for names in observation objects
  # check for sitenames

  sitenames <- sites  # this is a placeholder

  # Extract input

  if(!is.null(input_unc))
  {
    input <- c(input, input_unc)
    names(input) <- c('input', 'input_unc')
  }

  input_resampled <- list()

  if(!flatten_input)
  {
    for(i in 1:length(covariates))
    {
      if(terra::crs(input, proj = TRUE) ==
         terra::crs(covariates[[i]], proj = TRUE))
      {
        input_resampled[[i]] <- terra::resample(input, covariates[[i]][[1]])
      } else {
        input_resampled[[i]] <- terra::project(input, covariates[[i]][[1]])
      }
      input_resampled[[i]] %<>% terra::mask(., mask = covariates[[i]][[1]])
      observations[[i]] %<>%
        terra::extract(input_resampled[[i]], .) %>%
        dplyr::select(., -1) %>%
        base::cbind(observations[[i]], .)
    }
  } else {
    for(i in 1:length(observations))
    {
      if(terra::crs(input, proj = TRUE) !=
         terra::crs(observations[[i]], proj = TRUE))
      {
        input_i <- observations[[i]] %>%
          terra::project(., y = input) %>%
          terra::extract(input, .) %>%
          dplyr::select(., -1)

      } else {
        input_i <- observations[[i]] %>%
          terra::extract(input, .) %>%
          dplyr::select(., -1)
      }
      mean_i <- input_i %>%
        apply(., 2, mean)
      observations[[i]] %<>% base::cbind(., input_i)
      input_resampled[[i]] <- covariates[[i]][[1]]*0 + mean_i
      names(input_resampled[[i]]) <- c('input', 'input_unc')
    }
  }

  # Option to scale and/or center observations

  if(scale_obs == 'by_input')
  {
    observations %<>%
      lapply(.
             , function(x)
             {
               out <- x %>% terra::values(.) %>%
                 dplyr::mutate(
                   .,
                   target_scaled = .data[[target]] * input / mean(.data[[target]]
                                                                  , na.rm = TRUE)
                 )
               terra::values(x) <- out
               return(x)
             }
      )
    target <- 'target_scaled'
  }
  if(scale_obs == 'by_SD')
  {
    observations %<>%
      lapply(.
             , function(x)
             {
               out <- x %>% terra::values(.) %>%
                 dplyr::mutate(
                   .,
                   target_scaled = .data[[target]] / sd(.data[[target]]
                                                        , na.rm = TRUE)
                 )
               terra::values(x) <- out
               return(x)
             }
      )
    target <- 'target_scaled'
  }
  if(center_obs)
  {
    observations %<>%
      lapply(.
             , function(x)
             {
               out <- x %>% terra::values(.) %>%
                 dplyr::mutate(
                   .,
                   target_centered = .data[[target]] - mean(.data[[target]]
                                                            , na.rm = TRUE)
                 )
               terra::values(x) <- out
               return(x)
             }
      )
    target <- 'target_centered'
  }

  # Option to center and/or scale covariates
  # Should there be an option to keep both standard and scaled covariates?

  if(scale_covariates == 'by_SD')
  {
    covariates %<>% lapply(function(x)
    {
      out <- x %>% terra::scale(
        .
        , center = FALSE
      )
      return(out)
    }
    )
  }
  if(scale_covariates == 'by_input')
  {
    for(i in 1:length(covariates))
    {
      means_i <- global(covariates[[i]], "mean", na.rm = TRUE) %>% unlist
      covariates[[i]] %<>% '*' (input_resampled[[i]][[1]])
      covariates[[i]] %<>% '/' (means_i)
    }
  }
  if(center_covariates)
  {
    covariates %<>% lapply(function(x)
    {
      out <- x %>% terra::scale(
        .
        , scale = FALSE
      )
      return(out)
    }
    )
  }

  # Extract covariates

  for(i in 1:length(observations))
  {
    observations[[i]] %<>%
      terra::extract(covariates[[i]], .) %>%
      dplyr::select(., -1) %>%
      base::cbind(observations[[i]], .)
  }

  # Select data for model

  covnames <- names(covariates[[1]])
  if(!is.null(input_unc)) covnames %<>% c('input_unc', .)
  if(!is.null(input)) covnames %<>% c('input', .)

  for(i in 1:length(sitenames))
  {
    observations[[i]]$site <- sitenames[i]
  }

  thesecolumns <- c(target, covnames, 'site')

  trdat <- vect(observations) %>%
    terra::values(.) %>%
    dplyr::select(., all_of(thesecolumns)) %>%
    na.omit()

  # Create folds for cross validation

  folds <- lapply(unique(trdat$site), function(x)
  {
    out <- c(1:nrow(trdat))[trdat$site != x]
    return(out)
  }
  )

  # Create formula

  fm <- covnames %>% base::paste(., collapse = ' + ') %>%
    paste0(target, ' ~ ', .) %>%
    stats::as.formula(.)

  # Traincontrol object

  trc <- trainControl(
    index = folds
    , savePredictions = 'final'
  )

  # Train the model

  model <- caret::train(
    fm
    , trdat
    , method = model_type
    , trControl = trc
    # , importance = 'impurity'
  )

  # If making maps, train an independent model for each site

  if(make_maps)
  {
    map_models <- list()

    for(i in 1:length(covariates))
    {
      # Select data

      trdat_i <- trdat[folds[[i]], ]

      # Create folds for cross validation

      folds_i <- lapply(unique(trdat_i$site), function(x)
      {
        out <- c(1:nrow(trdat_i))[trdat_i$site != x]
        return(out)
      }
      )

      # Train control object

      trc_i <- trainControl(
        index = folds_i
      )

      # Train the model

      map_models[[i]] <- caret::train(
        fm
        , trdat_i
        , method = model_type
        , trControl = trc_i
        # , importance = 'impurity'
      )
    }
  }

  # Make prediction maps for each site

  if(make_maps)
  {
    # Merge layers for prediction
    # Only if users want to use the input as a covariate, so I should make this optional

    if(exists('input_resampled'))
    {
      for(i in 1:length(covariates))
      {
        covariates[[i]] %<>% c(input_resampled[[i]], .)
      }
    }

    output <- list()

    for(i in 1:length(covariates))
    {
      output[[i]] <- predict(
        covariates[[i]]
        , map_models[[i]]
        , na.rm = TRUE
      )
      if(center_obs)
      {
        output[[i]] %<>% '+' (input_resampled[[i]][[1]])
      }
      # also modify output maps in case of scaling and transformations
      names(output[[i]]) <- 'output'
    }

    # Extract predictions to the observations for the final accuracy assessment

    for(i in 1:length(observations))
    {
      observations[[i]] %<>%
        terra::extract(output[[i]], .) %>%
        dplyr::select(., -1) %>%
        base::cbind(observations[[i]], .)
    }
  }

  # Accuracy for input and output maps
  # Include a step to get the final predictions when make_maps is FALSE

  rmse_na <- function(x, y)
  {
    out <- base::sqrt(
      base::mean((x - y)^2
                 , na.rm = TRUE)
    )
    return(out)
  }

  results <- list()
  results$accuracy <- observations %>%
    terra::vect(.) %>%
    terra::values(.) %>%
    dplyr::group_by(., site) %>%
    dplyr::summarise(.
                     , RMSE_in  = rmse_na(.data[[target_name]], input)
                     , RMSE_out = rmse_na(.data[[target_name]], output)
                     , cor_in  = cor(.data[[target_name]]
                                     , input
                                     , use = "pairwise.complete.obs")
                     , cor_out = cor(.data[[target_name]]
                                     , output
                                     , use = "pairwise.complete.obs")
    )
  if(save_covariates) {results$covariates <- covariates}
  results$observations <- observations
  results$model <- model
  if(make_maps)
  {
    if(save_map_models) {results$map_models <- map_models}
    results$output_maps <- output
  }
  if(results_plot)
  {
    require(ggplot2)

    results$plot <- observations %>%
      vect %>%
      values %>%
      tibble() %>%
      ggplot(., aes(x = .data[[target_name]], y = output, col = site)) +
      geom_point() +
      geom_abline() +
      coord_equal()
  }

  return(results)
}



# END
