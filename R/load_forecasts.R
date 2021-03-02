#' Load covid forecasts from Zoltar.  
#' Return all available forecasts submitted on forecast_dates
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
#' Default to NULL which stands for all valid targets in Zoltar.
#' @param as_of a date in YYYY-MM-DD format to load forecasts submitted as of this date. 
#' Default to NULL to load the latest version.
#' @param verbose a boolean for printing messages on zoltar job status poll
#' @param hub character vector, where the first element indicates the hub
#' from which to load forecasts. Possible options are "US" and "ECDC"
#'
#' @return data frame with columns model, forecast_date, location, horizon, 
#' temporal_resolution, target_variable, target_end_date, type, quantile, value,
#' location_name, population, geo_type, geo_value, abbreviation
#' 
#' @export
load_forecasts <- function (
  models = NULL,
  forecast_dates = NULL,
  locations = NULL,
  types = NULL,
  targets = NULL,
  as_of = NULL,
  verbose = TRUE, 
  hub = c("US", "ECDC")) {
  
  # set up Zoltar connection
  zoltar_connection <- setup_zoltar_connection()
  
  # get project url
  project_url <- get_zoltar_project_url(hub = hub, 
                                        zoltar_connection = zoltar_connection)
  
  if (!is.null(forecast_dates)){
    # get all valid timezeros in project
    all_valid_timezeros <- zoltr::timezeros(zoltar_connection = zoltar_connection,
                                            project_url = project_url)$timezero_date
    
    # take intersection of forecast_dates and all_valid_timezeros
    valid_forecast_dates <- intersect(as.character(forecast_dates), 
                                      as.character(all_valid_timezeros))
    if (length(valid_forecast_dates) == 0) {
      stop("Error in load_forecasts: All forecast_dates are invalid.")
    }
  } else {
    valid_forecast_dates <- forecast_dates
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
                                      as_of = as_of)
  if (nrow(forecasts) == 0){
    warning("Warning in do_zotar_query: Forecasts are not available.\n Please check your parameters.")
    # convert value column to double and select columns
    forecasts <- forecasts %>%
      dplyr::mutate(value = as.double(value)) %>%
      tidyr::separate(target, into=c("horizon","temporal_resolution","ahead",
                                     "target_variable"),
                      remove = FALSE, extra = "merge") %>%
      dplyr::rename(location = unit, forecast_date = timezero, type = class) %>%
      dplyr::select(model, forecast_date, location, horizon, temporal_resolution,
                    target_variable, type, quantile, value)
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
