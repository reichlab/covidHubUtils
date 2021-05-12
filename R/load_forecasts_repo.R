#' Load all available forecasts submitted on forecast_dates from local hub repo.
#' 
#' The function will throw an error when no forecasts are submitted on 
#' any dates in forecast_dates for selected models, locations, types and target.
#' 
#' This function will drop rows with NULLs in value column.
#' 
#' @param file_path path to the data-processed folder within a local clone of the hub repo
#' @param models Character vector of model abbreviations.
#' Default all models that submitted forecasts meeting the other criteria.
#' @param forecast_dates The forecast date of forecasts to retrieve.
#' Default to all valid forecast dates.
#' The function will throw an error if all dates in this parameter are invalid forecast dates.
#' @param locations list of fips. Default to all locations with available forecasts.
#' @param types Character vector specifying type of forecasts to load: “quantile” 
#' or “point”. Default to all valid forecast types.
#' @param targets character vector of targets to retrieve, for example
#' c('1 wk ahead cum death', '2 wk ahead cum death'). 
#' Default to NULL which stands for all valid targets.
#' @param hub character vector, where the first element indicates the hub
#' from which to load forecasts. Possible options are "US" and "ECDC"
#' @param verbose whether or not to print out diagnostic messages. Default is TRUE
#' @return data frame with columns model, forecast_date, location, horizon, 
#' temporal_resolution, target_variable, target_end_date, type, quantile, value,
#' location_name, population, geo_type, geo_value, abbreviation
#' 
#' @export
load_forecasts_repo <- function (
  file_path,
  models = NULL,
  forecast_dates = NULL,
  locations = NULL,
  types = NULL,
  targets = NULL, 
  hub = c("US", "ECDC"), 
  verbose = TRUE) {
  
  #validate file path to data-processed folder
  if (!dir.exists(file_path)){
    stop("Error in load_forecasts_repo: data-processed folder does not 
         exist in the local_hub_repo directory.")
  }
  
  # validate models
  all_valid_models <- list.dirs(file_path, full.names = FALSE)
  all_valid_models <- all_valid_models[nchar(all_valid_models) > 0]
  if (!is.null(models)){
    models <- unlist(purrr::map(models, function(model)
      match.arg(model, choices = all_valid_models)))
  } else {
    models <- all_valid_models
  }
  
  # get valid location codes
  if (hub[1] == "US") {
    valid_location_codes <- covidHubUtils::hub_locations$fips
  } else if (hub[1] == "ECDC") {
    valid_location_codes <- covidHubUtils::hub_locations_ecdc$location
  }
  
  # validate locations
  if (!is.null(locations)){
    locations <- match.arg(locations, choices = valid_location_codes, several.ok = TRUE)
  } else {
    locations <- valid_location_codes
  }
  
  # validate types
  if (!is.null(types)){
    types <- match.arg(types, choices = c("point", "quantile"), several.ok = TRUE)
  } else {
    types <- c("point", "quantile")
  }
  
  # get valid targets
  if (hub[1] == "US") {
    all_valid_targets <- covidHubUtils::hub_targets_us
  } else if (hub[1] == "ECDC") {
    all_valid_targets <- covidHubUtils::hub_targets_ecdc
  }
  
  # validate targets
  if (!is.null(targets)){
    targets <- match.arg(targets, choices = all_valid_targets, several.ok = TRUE)
  } else {
    targets <- all_valid_targets
  }
  
  # validate forecast_dates
  if(!is.null(forecast_dates)){
    forecast_dates <- as.Date(forecast_dates)
  } else {
    forecast_dates <- seq(as.Date("2020-01-25"), Sys.Date(), by = "days")
  }
  
  # get paths to all forecast files
  forecast_files <- get_forecast_file_path(models, file_path, forecast_dates, 
                                           latest = FALSE, 
                                           verbose = verbose)
  
  # read in the forecast files
  forecasts <- load_forecast_files_repo(file_paths = forecast_files, 
                                        locations = locations, 
                                        types = types, 
                                        targets = targets, 
                                        hub = hub)
  return(forecasts)
}
