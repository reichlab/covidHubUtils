#' Load all available forecasts submitted on `forecast_dates` from Zoltar.
#'
#' This function will throw a warning and return an empty data frame when
#' no forecasts are submitted on any dates in `forecast_dates` for selected `models`,
#' `locations`, `types` and `target`.
#'
#' @param models Character vector of model abbreviations.
#' Default all models that submitted forecasts meeting the other criteria.
#' @param forecast_dates A 2 dimensional list of forecast dates to retrieve forecasts.
#' This function will return the latest forecasts
#' for each sub-list of dates.
#' Default to  `NULL` which would include all valid forecast dates in Zoltar.
#' @param locations list of fips. Default to all locations with available forecasts in Zoltar.
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

  # construct Zoltar project url
  project_url <- get_zoltar_project_url(
    hub = hub,
    zoltar_connection = zoltar_connection
  )
  # get all valid timezeros in project
  all_models <- zoltr::models(
    zoltar_connection = zoltar_connection,
    project_url = project_url
  )

  # get all valid timezeros in project
  all_valid_timezeros <- zoltr::timezeros(
    zoltar_connection = zoltar_connection,
    project_url = project_url
  )$timezero_date

  `%dopar%` <- foreach::`%dopar%`

  if (!is.null(forecast_dates)) {
    # set 2 workers
    cl <- parallel::makeCluster(2, setup_strategy = "sequential")
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
          max(intersect(
            as.character(a_list),
            as.character(model_forecasts_history)
          ))
        }
      )

      # unlist and drop duplicates
      latest_dates <- unique(unlist(latest_dates, use.names = FALSE))
      
      if (length(latest_dates) != 0 & !is.na(latest_dates)) {
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
  
        forecast <- reformat_forecasts(forecast)
      }
    }
    # shut down workers
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
    forecasts <- reformat_forecasts(forecasts)
  }

  if (nrow(forecasts) == 0) {
    warning("Warning in load_forecasts_zoltar: Forecasts are not available.\n Please check your parameters.")
  }

  # append location, population information
  forecasts <- forecasts %>%
    join_with_hub_locations(hub = hub)

  return(forecasts)
}


#' Reformat forecast data frame returned from zoltar query
#' This function will throw a warning and return an empty data.frame with
#' columns `model`, `forecast_date`, `location`, `type`, `quantile` and `value`, when
#' no forecasts are available in `zoltar_query_result`.
#'
#' @param zoltar_query_result dataframe returned by [zoltr::do_zoltar_query]
#'
#' @return data.frame with columns `model`, `forecast_date`, `location`, `horizon`,
#' `temporal_resolution`, `target_variable`, `target_end_date`, `type`, `quantile`, `value`
#'
#' @export
reformat_forecasts <- function(zoltar_query_result) {
  if (nrow(zoltar_query_result) == 0) {
    warning("Warning in reformat_forecasts: Forecasts are not available.\n Please check your parameters.")
    # convert value column to double and select columns
    zoltar_query_result <- zoltar_query_result %>%
      dplyr::mutate(value = as.double(value))
  }

  zoltar_query_result <- zoltar_query_result %>%
    # keep only required columns
    dplyr::select(model, timezero, unit, target, class, quantile, value) %>%
    dplyr::rename(
      location = unit, forecast_date = timezero,
      type = class
    ) %>%
    # create horizon and target_end_date columns
    tidyr::separate(target,
      into = c("horizon", "temporal_resolution", "ahead", "target_variable"),
      remove = FALSE, extra = "merge"
    ) %>%
    dplyr::mutate(target_end_date = as.Date(
      calc_target_end_date(forecast_date, as.numeric(horizon), temporal_resolution)
    )) %>%
    dplyr::select(
      model, forecast_date, location, horizon, temporal_resolution,
      target_variable, target_end_date, type, quantile, value
    )

  return(zoltar_query_result)
}
