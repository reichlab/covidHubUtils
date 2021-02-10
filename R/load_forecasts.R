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
#' Default to NULL to load the latest version. Only available when source is "zoltar" now. 
#' @param source string specifying where forecasts will be loaded from: either 
#' "local_hub_repo" or "zoltar". Default to "zoltar"
#' @param hub_repo_path path to local clone of the reichlab/covid19-forecast-hub
#' repository
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
  source = "zoltar", 
  hub_repo_path) {
  
  # validate source
  source <- match.arg(source, choices = c("local_hub_repo", "zoltar"))
  
  if (source == "local_hub_repo") {
    # validate hub repo path
    if (missing(hub_repo_path) | !dir.exists(hub_repo_path)) {
      stop("Error in load_forecasts: Please provide a vaid path to hub repo.")
    } 
    
    if (!is.null(as_of)){
      if (as_of != Sys.Date()){
        stop("Error in load_forecasts: as_of parameter is not available for `local_hub_repo` source now.")
      } 
    }
    
    # path to data-processed folder in hub repo
    data_processed <- file.path(hub_repo_path, "data-processed/")
    
    forecasts <- load_forecasts_repo(file_path = data_processed, 
                                     models = models, 
                                     forecast_dates = forecast_dates, 
                                     locations = locations, 
                                     types = types, 
                                     targets = targets)
    
  } else {
    forecasts <- load_forecasts_zoltar(models = models,
                                       forecast_dates = forecast_dates,
                                       locations = locations,
                                       types = types,
                                       targets = targets,
                                       as_of = as_of)
  }
  
  return(forecasts)
}
