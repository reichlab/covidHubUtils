#' Load all available forecasts submitted on forecast_dates from local hub repo.
#' 
#' The function will throw an error when no forecasts are submitted on 
#' any dates in forecast_dates for selected models, locations, types and target.
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
  
  # validate locations
  if (!is.null(locations)){
    all_valid_fips <- covidHubUtils::hub_locations$fips
    locations <- match.arg(locations, choices = all_valid_fips, several.ok = TRUE)
  } 
  
  # validate types
  if (!is.null(types)){
    types <- match.arg(types, choices = c("point", "quantile"), several.ok = TRUE)
  } 
  
  # validate targets
  if (!is.null(targets)){
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
    
    targets <- match.arg(targets, choices = all_valid_targets, several.ok = TRUE)
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
