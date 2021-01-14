#' Load covid forecasts from local hub repo or Zoltar.  
#' Return the most recent forecast from each model within 
#' forecast_date_window_size days of the last_forecast_date
#' 
#' @param models Character vector of model abbreviations.
#' If missing, forecasts for all models that submitted forecasts 
#' meeting the other criteria are returned.
#' @param last_forecast_date The forecast date of forecasts to retrieve 
#' in 'yyyy-mm-dd' format. Defaults to the most recent forecast date in 
#' Zoltar or the hub repo.
#' @param forecast_date_window_size The number of days across which to 
#' look for recent forecasts. Defaults to 0, which means to only look 
#' at the last_forecast_date only. 
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
#' @return data frame with columns model, forecast_date, location, horizon, 
#' temporal_resolution, target_variable, target_end_date, type, quantile, value,
#' location_name, population, geo_type, geo_value, abbreviation
#' 
#' @export
load_latest_forecasts <- function (
  models,
  last_forecast_date,
  forecast_date_window_size = 0,
  locations,
  types,
  targets,
  source = "local_hub_repo",
  hub_repo_path) {
  
  # validate models
  if (missing(hub_repo_path)) {
    all_valid_models <- get_all_models(source = source)
  } else {
    all_valid_models <- get_all_models(
      source = source,
      hub_repo_path = hub_repo_path)
  }
  
  if (!missing(models)){
    models <- purrr::map(models, function(model)
      match.arg(model, choices = all_valid_models) )
  } else {
    models <- all_valid_models
  }
  
  
  
  # validate source
  source <- match.arg(source, choices = c("local_hub_repo", "zoltar"))
  
  # validate locations
  all_valid_fips <- covidHubUtils::hub_locations$fips
  
  if (!missing(locations)){
    locations <- match.arg(locations, 
                           choices = all_valid_fips, 
                           several.ok = TRUE)
  } else{
    locations <- all_valid_fips
  }
  
  # validate types
  if (!missing(types)){
    types <- match.arg(types, choices = c("point", "quantile"), several.ok = TRUE)
  } else {
    types <- c("point", "quantile")
  }
  
  # validate targets
  # set up Zoltar connection
  zoltar_connection <- zoltr::new_connection()
  if(Sys.getenv("Z_USERNAME") == "" | Sys.getenv("Z_PASSWORD") == "") {
    zoltr::zoltar_authenticate(zoltar_connection, "zoltar_demo","Dq65&aP0nIlG")
  } else {
    zoltr::zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"),Sys.getenv("Z_PASSWORD"))
  }
  
  # construct Zoltar project url
  the_projects <- zoltr::projects(zoltar_connection)
  project_url <- the_projects[the_projects$name == "COVID-19 Forecasts", "url"]
  
  # validate targets 
  all_valid_targets <- zoltr::targets(zoltar_connection, project_url)$name
  
  
  if (!missing(targets)){
    targets <- match.arg(targets, choices = all_valid_targets, several.ok = TRUE)
  } else {
    targets <- all_valid_targets
  }
  
  
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
                                       targets = targets)
  }
  
  return(forecasts)
}
