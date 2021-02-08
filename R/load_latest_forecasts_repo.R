#' Load the most recent forecast submitted in a time window
#' from reichlab/covid19-forecast-hub repo.
#' 
#' @param file_path path to local clone of the reichlab/covid19-forecast-hub/data-processed
#' @param models character vector of model abbreviations.
#' If missing, forecasts for all models that submitted forecasts 
#' meeting the other criteria are returned.
#' @param forecast_dates date vector to load the most recent forecast from
#' @param locations list of valid fips code. Defaults to all locations with 
#' available forecasts.
#' @param types character vector specifying type of forecasts to load: “quantile” 
#' or “point”. Defaults to all types  with available forecasts
#' @param targets character vector of targets to retrieve, for example
#' c('1 wk ahead cum death', '2 wk ahead cum death'). Defaults to all targets
#' with available forecasts.
#' 
#' @return data frame with columns model, forecast_date, location, horizon,
#' temporal_resolution, target_variable, target_end_date, type, quantile, value,
#' location_name, population, geo_type, geo_value, abbreviation
#'
#' @export
load_latest_forecasts_repo <- function(file_path, models = NULL, forecast_dates, 
                                       locations = NULL, types = NULL, targets = NULL){
  
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
  
  forecast_dates <- as.Date(forecast_dates)
  
  # get paths to all forecast files
  forecast_files <- get_forecast_file_path(models, file_path, forecast_dates, latest = TRUE)

  # read in the forecast files
  forecasts <- load_forecast_files_repo(file_paths = forecast_files, 
                                        locations = locations, 
                                        types = types, 
                                        targets = targets)
  return(forecasts)
  
}

#' Generate forecast file paths with given data-processed folder path for selected models
#' 
#' @param models character vector of model abbreviations.
#' @param file_path path to local clone of the reichlab/covid19-forecast-hub/data-processed
#' @param forecast_dates date vector to generate file paths
#' @param latest boolean to generate path to the latest forecast file from each model
#' 
#' @return a list of paths to forecast files submitted on a range of forecast dates from selected models

get_forecast_file_path <- function(models, file_path, forecast_dates, latest = FALSE){
  
  forecast_files <- purrr::map(
    models,
    function(model) {
      if (substr(file_path, nchar(file_path), nchar(file_path)) == "/") {
        file_path <- substr(file_path, 1, nchar(file_path) - 1)
      }
      
      results_path <- file.path(
        file_path,
        paste0(model, "/", forecast_dates, "-", model, ".csv"))
      results_path <- results_path[file.exists(results_path)]
      
      if (latest){
        results_path <- tail(results_path, 1)
      }
      
      if (length(results_path) == 0) {
        return(NULL)
      } else {
        return(results_path)
      }
    }
  ) %>% unlist()
  
  return(forecast_files)
}

#' Read in a set of forecast files from a repository
#'
#' @param file_paths paths to csv forecast files to read in.  It is expected that
#' the file names are in the format "*YYYY-MM-DD-<model_name>.csv".
#' @param locations list of valid fips codes. Defaults to all locations with
#' available forecasts.
#' @param types character vector specifying type of forecasts to load: “quantile”
#' or “point”. Defaults to all types in the forecast file
#' @param targets character vector of targets to retrieve, for example
#' c('1 wk ahead cum death', '2 wk ahead cum death'). Defaults to all targets
#' in the forecast file.
#'
#' @return data frame with columns model, forecast_date, location, horizon,
#' temporal_resolution, target_variable, target_end_date, type, quantile, value,
#' location_name, population, geo_type, geo_value, abbreviation
#'
#' @details The model_name in the file name will be used for the value of the
#' model column in the return result.
#'
#' @export
load_forecast_files_repo <- function(file_paths,
                                     locations = NULL,
                                     types = NULL,
                                     targets = NULL) {
  # validate file_paths exist
  files_exist <- file.exists(file_paths)
  if (!any(files_exist)) {
    stop("In load_forecast_files_repo, no files exist at the provided file_paths.")
  } else if (!all(files_exist)) {
    warning("In load_forecast_files_repo, at least one file did not exist at the provided file_paths.")
    file_paths <- file_paths[files_exist]
  }

  # read in each file
  all_forecasts <- purrr::map_dfr(
    file_paths,
    function(file_path) {
      # extract model name from file name
      model <- strsplit(file_path, .Platform$file.sep) %>%
        `[[`(1) %>%
        tail(1)
      date_start_ind <- regexpr("\\d\\d\\d\\d\\-\\d\\d\\-\\d\\d\\-", model)
      if (date_start_ind == -1) {
        stop("In load_forecast_files_repo, incorrect file name format: must include date in YYYY-MM-DD format")
      }
      model <- substr(model, date_start_ind + 11, nchar(model) - 4)

      single_forecast <- readr::read_csv(
          file_path,
          col_types = readr::cols(
            forecast_date = readr::col_date(format = ""),
            target = readr::col_character(),
            target_end_date = readr::col_date(format = ""),
            location = readr::col_character(),
            type = readr::col_character(),
            quantile = readr::col_double(),
            value = readr::col_double()
          ))
      
      if (!is.null(types)) {
        single_forecast <- single_forecast %>%
          dplyr::filter(tolower(type) %in% tolower(types))
      }
      if (!is.null(locations)) {
        single_forecast <- single_forecast %>%
          dplyr::filter(tolower(location) %in% tolower(locations))
      }
      if (!is.null(targets)) {
        single_forecast <- single_forecast %>%
          dplyr::filter(tolower(target) %in% tolower(targets))
      }
        
      single_forecast <- single_forecast %>%
        dplyr::transmute(
          model = model,
          forecast_date = forecast_date,
          location = location,
          target = tolower(target),
          target_end_date = target_end_date,
          type = type,
          quantile = quantile,
          value = value)
      
    return(single_forecast)
    }) %>%
    tidyr::separate(target,
      into = c("horizon", "temporal_resolution", "ahead", "target_variable"),
      remove = FALSE, extra = "merge") %>%
    dplyr::select(model, forecast_date, location, horizon, temporal_resolution,
                  target_variable, target_end_date, type, quantile, value) %>%
    dplyr::left_join(covidHubUtils::hub_locations, by = c("location" = "fips"))
  
  return(all_forecasts)
}
