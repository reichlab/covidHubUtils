#' load covid forecasts from local files or Zoltar.  
#' Return the most recent forecast from each model within 
#' forecast_date_window_size days of the last_forecast_date
#' 
#' @param models Character vector of model abbreviations.
#' If missing, forecasts for all models that submitted forecasts 
#' meeting the other criteria are returned.
#' @param last_forecast_date The forecast date of forecasts to retrieve.
#' Defaults to the most recent forecast date in Zoltar or the hub repo.
#' @param forecast_date_window_size The number of days across which to 
#' look for recent forecasts. Defaults to 1, which means to only look 
#' at the last_forecast_date. 
#' @param locations list of fips. Defaults to all locations with available forecasts.
#' @param types Character vector specifying type of forecasts to load: “quantile” 
#' or “point”. Defaults to c(“quantile”, “point”)
#' @param targets character vector of targets to retrieve, for example
#' c('1 wk ahead cum death', '2 wk ahead cum death'). Defaults to all targets.
#' @param source string specifying where forecasts will be loaded from: either 
#' "local_hub_repo" or "zoltar"
#' @param hub_repo_path path to local clone of the reichlab/covid19-forecast-hub
#' repository
#'
#' @return data frame with columns model, forecast_date,location, target, 
#' type, quantile, value, horizon and target_end_date.
#' @export
load_forecasts <- function (
  models,
  last_forecast_date,
  forecast_date_window_size = 1,
  locations,
  types,
  targets,
  source = "local_hub_repo",
  hub_repo_path) {
  
  # validate source
  source <- match.arg(source, choices = c("local_hub_repo", "zoltar"))
  
  # validate locations
  all_valid_fips <- covidHubUtils::hub_locations %>%
    pull(fips)
  
  if (!missing(locations)){
    locations <- match.arg(locations, 
                           choices = all_valid_fips, 
                           several.ok = TRUE)
  } else{
    locations <- all_valid_fips
  }
  
  # validate types
  types <- match.arg(types, choices = c("point", "quantile"), several.ok = TRUE)
  
  # path to data-processed folder in hub repo
  data_processed <- file.path(hub_repo_path, "data-processed/")
  
  # dates of forecasts to load
  forecast_dates <- as.character(last_forecast_date +
                                   seq(from = -forecast_date_window_size, to = 0, by = 1))

  if (source == "local_hub_repo") {
    forecasts <- covidHubUtils:::load_forecasts_repo(file_path = data_processed, 
                                     models = models, 
                                     forecast_dates = forecast_dates, 
                                     locations = locations, 
                                     types = types, 
                                     targets = targets)
  } else {
    forecasts <- covidHubUtils:::load_forecasts_zoltar(models = models, 
                                       forecast_dates = forecast_dates, 
                                       locations = locations, 
                                       types = types,
                                       targets = targets)
  }
  
  return(forecasts)
}
