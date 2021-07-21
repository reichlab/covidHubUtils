#' Load all available forecasts submitted on `forecast_dates` from Zoltar.
#'
#' This function will throw a warning and return an empty data frame when
#' no forecasts are submitted on any dates in `forecast_dates` for selected `models`,
#' `locations`, `types` and `target`.
#'
#' @param models Character vector of model abbreviations.
#' Default all models that submitted forecasts meeting the other criteria.
#' @param forecast_dates A list of forecast dates to retrieve forecasts.
#' Default to all valid forecast dates in Zoltar.
#' If this is a 1-D list, this function will return all available forecasts
#' submitted on these dates.
#' If this is a 2-D list, this function will return the latest forecasts
#' for each sub-list of dates.
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
#' 
#' @return data.frame with columns `model`, `forecast_date`, `location`, `horizon`,
#' `temporal_resolution`, `target_variable`, `target_end_date`, `type`, `quantile`, `value`,
#' `location_name`, `population`, `geo_type`, `geo_value`, `abbreviation`
#'
#' @export
load_forecasts_zoltar <- function(
                                  models = NULL,
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
  all_valid_timezeros <- zoltr::timezeros(
    zoltar_connection = zoltar_connection,
    project_url = project_url
  )$timezero_date
  
  `%dopar%` <- foreach::`%dopar%`
  
  if (!is.null(forecast_dates) & length(forecast_dates) > 1) {
    # set 4 workers
    doParallel::registerDoParallel(cores = 4)
    forecasts <- foreach::foreach(i = 1:length(forecast_dates), .combine = rbind) %dopar% {
      # take intersection of forecast_dates[[i]] and all_valid_timezeros
      valid_forecast_dates <- intersect(
        as.character(forecast_dates[[i]]),
        as.character(all_valid_timezeros)
      )

      if (length(valid_forecast_dates) == 0) {
        stop("Error in load_forecasts: All forecast_dates are invalid.")
      }

      # one query for each valid date in forecast_dates[[i]],
      # with all models relevant for that date
      forecast <- zoltr::do_zoltar_query(
        zoltar_connection = zoltar_connection,
        project_url = project_url,
        query_type = "forecasts",
        units = locations,
        timezeros = valid_forecast_dates,
        models = models,
        targets = targets,
        types = types,
        verbose = verbose,
        as_of = date_to_datetime(as_of, hub)
      )

      forecast <- forecast %>%
        reformat_forecasts() %>%
        # return the latest
        dplyr::group_by(model) %>%
        dplyr::filter(forecast_date == max(forecast_date)) %>%
        dplyr::ungroup()
    }
    # shut down workers
    doParallel::stopImplicitCluster()
  } else {
    if (!is.null(forecast_dates)) {
      forecast_dates <- intersect(
        as.character(forecast_dates[[1]]),
        as.character(all_valid_timezeros)
      )
    }

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
    warning("Warning in do_zoltar_query: Forecasts are not available.\n Please check your parameters.")
    # convert value column to double and select columns
    zoltar_query_result <- zoltar_query_result %>%
      dplyr::mutate(value = as.double(value)) %>%
      dplyr::rename(location = unit, forecast_date = timezero, type = class) %>%
      dplyr::select(model, forecast_date, location, type, quantile, value)
  } else {
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
  }

  return(zoltar_query_result)
}
