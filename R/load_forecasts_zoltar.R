#' Load all available forecasts submitted on forecast_dates from Zoltar.
#' 
#' The function will throw a warning and return an empty data frame when 
#' no forecasts are submitted on any dates in forecast_dates for selected models, 
#' locations, types and target.
#' 
#' @param models Character vector of model abbreviations.
#' Default all models that submitted forecasts meeting the other criteria.
#' @param forecast_dates The forecast date of forecasts to retrieve.
#' Default to all valid forecast dates in Zoltar.
#' The function will throw an error if all dates in this parameter are invalid forecast dates in Zoltar.
#' @param locations list of fips. Default to all locations with available forecasts in Zoltar.
#' @param types Character vector specifying type of forecasts to load: “quantile” 
#' or “point”. Default to all valid forecast types in Zoltar.
#' @param targets character vector of targets to retrieve, for example
#' c('1 wk ahead cum death', '2 wk ahead cum death'). 
#' Default to all valid targets in Zoltar.
#' @param as_of character for date time to load forecasts submitted as of this time. 
#' It could use the format of one of the three examples: 
#' "2021-01-01", "2020-01-01 01:01:01" and "2020-01-01 01:01:01 UTC".
#' If you would like to set a timezone, it has to be UTC now. 
#' If not, helper function will append the default timezone to your input based on hub parameter. 
#' Default to NULL to load the latest version.
#' @param hub character vector, where the first element indicates the hub
#' from which to load forecasts. Possible options are "US" and "ECDC"
#' @param verbose a boolean for printing messages on zoltar job status. Default to TRUE.
#'
#' @return data frame with columns model, forecast_date, location, horizon, 
#' temporal_resolution, target_variable, target_end_date, type, quantile, value,
#' location_name, population, geo_type, geo_value, abbreviation
#' 
#' @export
load_forecasts_zoltar <- function (
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
  
  if (!is.null(forecast_dates)){
    # construct Zoltar project url
    project_url <- get_zoltar_project_url(hub = hub, 
                                          zoltar_connection = zoltar_connection)
    
    # get all valid timezeros in project
    all_valid_timezeros <- zoltr::timezeros(zoltar_connection = zoltar_connection,
                                            project_url = project_url)$timezero_date
    
    # take intersection of forecast_dates and all_valid_timezeros
    valid_forecast_dates <- intersect(as.character(forecast_dates), 
                                      as.character(all_valid_timezeros))
    if (length(valid_forecast_dates) == 0) {
      stop("Error in load_forecasts: All forecast_dates are invalid.")
    }
  } 
  
  forecasts <- zoltr::do_zoltar_query(zoltar_connection = zoltar_connection,
                                      project_url = project_url,
                                      query_type = "forecasts",
                                      units = locations, 
                                      timezeros = valid_forecast_dates,
                                      models = models,
                                      targets = targets,
                                      types = types,
                                      verbose = verbose,
                                      as_of = date_to_datetime(as_of, hub))
  if (nrow(forecasts) == 0){
    warning("Warning in do_zoltar_query: Forecasts are not available.\n Please check your parameters.")
    # convert value column to double and select columns
    forecasts <- forecasts %>%
      dplyr::mutate(value = as.double(value)) %>%
      dplyr::rename(location = unit, forecast_date = timezero, type = class) %>%
      dplyr::select(model, forecast_date, location, type, quantile, value)
  } else {
    forecasts <- forecasts %>%
      # keep only required columns
      dplyr::select(model, timezero, unit, target, class, quantile, value) %>%
      dplyr::rename(location = unit, forecast_date = timezero,
                    type = class) %>%
      # create horizon and target_end_date columns
      tidyr::separate(target, into=c("horizon","temporal_resolution","ahead","target_variable"),
                      remove = FALSE, extra = "merge") %>%
      dplyr::mutate(target_end_date = as.Date(
        calc_target_end_date(forecast_date, as.numeric(horizon), temporal_resolution)
      )) %>%
      dplyr::select(model, forecast_date, location, horizon, temporal_resolution,
                    target_variable, target_end_date, type, quantile, value) %>%
      join_with_hub_locations(hub = hub)
  }
  
  return(forecasts)
}
