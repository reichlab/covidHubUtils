#' Load covid forecasts from local hub repo or Zoltar.  
#' Return the most recent forecast from each model within 
#' forecast_date_window_size days of the last_forecast_date
#' 
#' @param models Character vector of model abbreviations.
#' If missing, forecasts for all models that submitted forecasts 
#' meeting the other criteria are returned.
#' @param last_forecast_date The forecast date of forecasts to retrieve 
#' in 'yyyy-mm-dd' format.
#' @param forecast_date_window_size The number of days across which to 
#' look for recent forecasts. Default to 0, which means to only look 
#' at the last_forecast_date only. 
#' @param locations list of fips. Defaults to all locations with available forecasts.
#' @param types Character vector specifying type of forecasts to load: “quantile” 
#' or “point”. Defaults to all types  with available forecasts
#' @param targets character vector of targets to retrieve, for example
#' c('1 wk ahead cum death', '2 wk ahead cum death'). Defaults to all targets.
#' @param source string specifying where forecasts will be loaded from: either 
#' "local_hub_repo" or "zoltar"
#' @param hub_repo_path path to local clone of the reichlab/covid19-forecast-hub
#' repository
#' @param as_of a date in YYYY-MM-DD format to load forecasts submitted as of this date. 
#' Default to NULL to load the latest version. This parameter is only supported 
#' when loading forecasts from Zoltar.
#'
#' @return data frame with columns model, forecast_date, location, horizon, 
#' temporal_resolution, target_variable, target_end_date, type, quantile, value,
#' location_name, population, geo_type, geo_value, abbreviation
#' 
#' @export
load_latest_forecasts <- function (
  models = NULL,
  last_forecast_date,
  forecast_date_window_size = 0,
  locations = NULL,
  types = NULL,
  targets = NULL,
  source = "local_hub_repo",
  hub_repo_path,
  as_of = NULL) {
  
  # validate source
  source <- match.arg(source, choices = c("local_hub_repo", "zoltar"))
  
  # check date format and generate dates of forecasts to load
  forecast_dates <- tryCatch({
    as.character(as.Date(last_forecast_date) +
                   seq(from = -forecast_date_window_size, to = 0, by = 1))
    }, error = function(err){
      stop("Error in load_latest_forecasts: Please provide a valid date object or
           string in format YYYY-MM-DD in latest_forrecast_date.")
      }
    )

  if (source == "local_hub_repo") {
    # validate hub repo path
    if (missing(hub_repo_path) | !dir.exists(hub_repo_path)) {
      stop("Error in load_latest_forecasts: Please provide a vaid path to hub repo.")
    } 
    
    # path to data-processed folder in hub repo
    data_processed <- file.path(hub_repo_path, "data-processed/")
    
    forecasts <- load_latest_forecasts_repo(file_path = data_processed, 
                                            models = models, 
                                            forecast_dates = forecast_dates, 
                                            locations = locations, 
                                            types = types, 
                                            targets = targets)
  } else {
    forecasts <- load_latest_forecasts_zoltar(models = models, 
                                              forecast_dates = forecast_dates,
                                              locations = locations, 
                                              types = types,
                                              targets = targets,
                                              as_of = as_of)
  }
  
  return(forecasts)
}
