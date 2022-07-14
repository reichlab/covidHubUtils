#' Load all available forecasts submitted on `forecast_dates` from Zoltar.
#'
#' This function will throw a warning and return empty data.frame when
#' no forecasts are submitted on any dates in `forecast_dates` for selected `models`,
#' `locations`, `types` and `target`.
#'
#' @param models Character vector of model abbreviations.
#' Default all models that submitted forecasts meeting the other criteria.
#' @param forecast_dates A 2 dimensional list of forecast dates to retrieve forecasts.
#' This function will return the latest forecasts
#' for each sub-list of dates.
#' Default to  `NULL` which would include all valid forecast dates in Zoltar.
#' @param locations a vector of strings of fips code or CBSA codes or location names,
#' such as "Hampshire County, MA", "Alabama", "United Kingdom".
#' A US county location names must include state abbreviation. 
#' Default to `NULL` which would include all locations with available forecasts in Zoltar.
#' @param types Character vector specifying type of forecasts to load: `"quantile"`
#' and/or `"point"`. Default to all valid forecast types in Zoltar.
#' @param targets character vector of targets to retrieve, for example
#' `c('1 wk ahead cum death', '2 wk ahead cum death')`.
#' Default to all valid targets in Zoltar.
#' @param as_of character for date time to load forecasts submitted as of this time.
#' It could use the format of one of the three examples:
#' `"2021-01-01", "2020-01-01 01:01:01" and "2020-01-01 01:01:01 UTC".`
#' If you would like to set a timezone, it has to be UTC now.
#' If not, helper function will append the default timezone to your input based on `hub` parameter.
#' Default to `NULL` to load the latest version.
#' @param hub character vector, where the first element indicates the hub
#' from which to load forecasts. Possible options are `"US"` and `"ECDC"`.
#' @param verbose logical for printing messages on zoltar job status. Default to `TRUE`.
#'
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar%
#' @importFrom parallel makeCluster stopCluster
#'
#' @return data.frame with columns `model`, `forecast_date`, `location`, `horizon`,
#' `temporal_resolution`, `target_variable`, `target_end_date`, `type`, `quantile`, `value`,
#' `location_name`, `population`, `geo_type`, `geo_value`, `abbreviation`
#'
#' @export
load_forecasts_zoltar <- function(models = NULL,
                                  forecast_dates = NULL,
                                  locations = NULL,
                                  types = NULL,
                                  targets = NULL,
                                  as_of = NULL,
                                  hub = c("US", "ECDC"),
                                  verbose = TRUE) {

  # set up Zoltar connection
  zoltar_connection <- setup_zoltar_connection(staging = FALSE)
  
  hub <- match.arg(hub,
                   choices = c("US", "ECDC"),
                   several.ok = TRUE
  )
  
  # construct Zoltar project url
  project_url <- get_zoltar_project_url(
    hub = hub,
    zoltar_connection = zoltar_connection
  )
  # get information about all models in project
  all_models <- zoltr::models(
    zoltar_connection = zoltar_connection,
    project_url = project_url
  )

  # get all valid timezeros in project
  all_valid_timezeros <- zoltr::timezeros(
    zoltar_connection = zoltar_connection,
    project_url = project_url
  )$timezero_date

  # Convert location names to fips codes or country abbreviations
  locations <- name_to_fips(locations, hub)
  
  `%dopar%` <- foreach::`%dopar%`

  if (!is.null(forecast_dates)) {
    if (is.null(models)) {
      models <- all_models$model_abbr
      models <- sort(models, method = "radix")
    }
    
    
    # set 2 workers
    if (verbose) {
      cl <- parallel::makeCluster(2, setup_strategy = "sequential", outfile = "")
    } else {
      cl <- parallel::makeCluster(2, setup_strategy = "sequential")
    }

    doParallel::registerDoParallel(cl)
    forecasts <- foreach::foreach(i = 1:length(models), .combine = rbind) %dopar% {
      curr_model <- models[i]

      model_url <- all_models[all_models$model_abbr == curr_model, ]$url

      model_forecasts_history <- zoltr::forecasts(
        zoltar_connection = zoltar_connection,
        model_url = model_url
      )$timezero_date

      # get the latest of each subset of forecast_dates
      latest_dates <- purrr::map(
        forecast_dates,
        function(a_list) {
          intersection <- intersect(
            as.character(a_list),
            as.character(model_forecasts_history)
          )
          if (length(intersection) > 0) {
            max(intersection)
          }
        }
      )

      # unlist and drop duplicates
      latest_dates <- unique(unlist(latest_dates, use.names = FALSE))
      latest_dates <- latest_dates[!is.na(latest_dates)]

      if (length(latest_dates) != 0) {
        forecast <- zoltr::do_zoltar_query(
          zoltar_connection = zoltar_connection,
          project_url = project_url,
          query_type = "forecasts",
          units = locations,
          timezeros = latest_dates,
          models = curr_model,
          targets = targets,
          types = types,
          verbose = verbose,
          as_of = date_to_datetime(as_of, hub)
        )

        if (nrow(forecast) == 0) {
          if (verbose){
            warning(paste0(
              "Warning in load_forecasts_zoltar: Forecasts are not available for current model ",
              curr_model
            ))
            forecast <- data.frame()
          }
        }
        forecast <- reformat_forecasts(forecast, verbose)
      } else {
        if (verbose) {
          warning(paste0(
            "Warning in load_forecasts_zoltar: No available forecast dates for current model ",
            curr_model
          ))
          forecast <-  data.frame()
        }
      }
    }
    # shut down workers
    doParallel::stopImplicitCluster()
    parallel::stopCluster(cl)
  } else {
    # load forecasts submitted on all dates
    forecasts <- zoltr::do_zoltar_query(
      zoltar_connection = zoltar_connection,
      project_url = project_url,
      query_type = "forecasts",
      units = locations,
      timezeros = forecast_dates,
      models = models,
      targets = targets,
      types = types,
      verbose = verbose,
      as_of = date_to_datetime(as_of, hub)
    )
    forecasts <- reformat_forecasts(forecasts, verbose)
  }
  
  if (!is.null(forecasts) && !is.null(nrow(forecasts))) {
    if (nrow(forecasts) > 0 && "location" %in% colnames(forecasts)) {
      # append location, population information
      forecasts <- forecasts %>%
        join_with_hub_locations(hub = hub)
    } else {
      forecasts <- data.frame()
      warning("Warning in load_forecasts_zoltar: Forecasts are not available.\n Please check your parameters.")
    }
  } else {
    forecasts <- data.frame()
    warning("Warning in load_forecasts_zoltar: Forecasts are not available.\n Please check your parameters.")
  }
  return(forecasts)
}


#' Reformat forecast data frame returned from zoltar query
#' This function will throw a warning and return an empty data.frame with
#' columns `model`, `forecast_date`, `location`, `type`, `quantile` and `value`, when
#' no forecasts are available in `zoltar_query_result`.
#'
#' @param zoltar_query_result dataframe returned by [zoltr::do_zoltar_query]
#' @param verbose logical for printing error messages when checking `zoltar_query_result`. Default to `TRUE`.
#'
#' @return data.frame with columns `model`, `forecast_date`, `location`, `horizon`,
#' `temporal_resolution`, `target_variable`, `target_end_date`, `type`, `quantile`, `value`
#'
#' @export
reformat_forecasts <- function(zoltar_query_result, verbose = TRUE) {
  if (nrow(zoltar_query_result) == 0) {
    if (verbose) {
      warning("Warning in reformat_forecasts: Forecasts are not available.\n Please check your parameters.")
    }
   
  } else {
    zoltar_query_result <- zoltar_query_result %>%
      # convert value column to double and select columns
      dplyr::mutate(value = as.double(value)) %>%
      # keep only required columns
      dplyr::select(model, timezero, unit, target, class, quantile, value) %>%
      dplyr::rename(
        location = unit, forecast_date = timezero,
        type = class
      ) %>%
      # cast type
      dplyr::mutate(location = as.character(location),
                    type = as.character(type),
                    quantile = as.double(quantile)) %>%
      # create horizon and target_end_date columns
      tidyr::separate(target,
        into = c("horizon", "temporal_resolution", "ahead", "target_variable"),
        remove = FALSE, extra = "merge"
      ) %>%
      dplyr::mutate(target_end_date = as.Date(
                      calc_target_end_date(forecast_date, 
                                           as.numeric(horizon), 
                                           temporal_resolution))) %>%
      dplyr::select(
        model, forecast_date, location, horizon, temporal_resolution,
        target_variable, target_end_date, type, quantile, value
      )
  }

  return(zoltar_query_result)
}
