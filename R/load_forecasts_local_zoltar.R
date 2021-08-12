#' Load the most recent forecasts submitted in a time window from zoltar.
#'
#' The function will throw a warning and return an empty data frame when
#' no forecasts are submitted on any dates in forecast_dates for selected models,
#' locations, types and target.
#'
#' @param models Character vector of model abbreviations.
#' Default to NULL so that forecasts for all models that submitted forecasts
#' meeting the other criteria are returned.
#' @param forecast_dates date vector to load the most recent forecast from.
#' Default to all valid forecast dates in Zoltar.
#' The function will throw an error if all dates in this parameter are invalid forecast dates in Zoltar.
#' @param locations list of valid location codes. Default to all locations with available forecasts in Zoltar.
#' @param types character vector specifying type of forecasts to load: “quantile”
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
#' @param verbose a boolean for printing messages on zoltar job status. Default to TRUE.
#' @param local_zoltpy_path path to local clone of zolpy repository.
#' @param zoltar_module_path path to local zoltar module
#' @param hub character vector, where the first element indicates the hub
#' from which to load forecasts. Possible options are "US" and "ECDC"
#'
#' @return data frame with columns model, forecast_date, location, horizon,
#' temporal_resolution, target_variable, target_end_date, type, quantile, value,
#' location_name, population, geo_type, geo_value, abbreviation
#'
#' @export
load_forecasts_local_zoltar <- function(models = NULL,
                                        forecast_dates = NULL,
                                        locations = NULL,
                                        types = NULL,
                                        targets = NULL,
                                        as_of = NULL,
                                        hub = c("US", "ECDC"),
                                        verbose = TRUE,
                                        local_zoltpy_path,
                                        zoltar_module_path) {
 
  # construct Zoltar project url
  project_url <- get_zoltar_project_url(
    hub = hub,
    zoltar_connection = zoltar_connection
  )
  # get information about all models in project
  all_models <- zoltr::models(
    zoltar_connection = zoltar_connection,
    project_url = project_url
  )
  
  # get all valid model abbrs
  all_valid_model_abbrs <- unique(all_models$model_abbr)
  
  # get all valid timezeros in project
  all_valid_timezeros <- zoltr::timezeros(
    zoltar_connection = zoltar_connection,
    project_url = project_url
  )$timezero_date
  
  if (!is.null(models)){
    models <- match.arg(models, 
                        choices = all_valid_model_abbrs, 
                        several.ok = TRUE)
  }
  
  original_wd <- setwd(local_zoltpy_path)
  if (!is.null(forecast_dates)) {
    
    if (is.null(models)){
      models <- all_models$model_abbr
    }
    
    cl <- parallel::makeCluster(2, setup_strategy = "sequential")
    doParallel::registerDoParallel(cl)
    forecasts <- foreach::foreach(i = 1:length(models), .combine = rbind) %dopar% {
      curr_model <- models[i]
      model_url <- all_models[all_models$model_abbr == curr_model, ]$url
      model_forecasts_history <- zoltr::forecasts(
        zoltar_connection = zoltar_connection,
        model_url = model_url
      )$timezero_date
      
      # get the latest of each subset of forecast_dates
      latest_dates <- purrr::map(
        forecast_dates,
        function(a_list) {
          max(intersect(
            as.character(a_list),
            as.character(model_forecasts_history)
          ))
        }
      )
      
      # unlist and drop duplicates
      latest_dates <- unique(unlist(latest_dates, use.names = FALSE))
      latest_dates <- latest_dates[!is.na(latest_dates)]
      
      if (length(latest_dates) != 0) {
        # create a json file as filter
        filter <- list(
          models = c(models),
          timezeros = c(latest_dates)
        )
        
        if (!is.null(locations)) {
          filter <- c(filter, units = c(locations))
        }
      
        if (!is.null(types)) {
          filter <- c(filter, types = c(types))
        }
      
        if (!is.null(targets)) {
          filter <- c(filter, targets = c(targets))
        }
      
        if (!is.null(as_of)) {
          filter <- c(filter, as_of = c(date_to_datetime(as_of, hub)))
        }
  
        temp_filter_filepath <- tempfile(pattern = "filter", fileext = ".json")
  
        jsonlite::write_json(filter, temp_filter_filepath)
        
        temp_result_filepath <- tempfile(pattern = "query", fileext = ".csv")
  
        query_command <- paste0(
          "pipenv run python3 cli/bulk_data_query.py ",
          zoltar_module_path, " ",
          temp_filter_filepath, " ",
          temp_result_filepath
        )
      
        system(query_command)

        forecast <- readr::read_csv(temp_result_filepath) %>%
          reformat_forecasts()
      }
    }
    # shut down workers
    parallel::stopCluster(cl)
  } else {
    
    # create a json file as filter
    filter <- list()
    
    if (!is.null(models)){
      filter <- c(filter, models = c(models))
    }
    if (!is.null(locations)) {
      filter <- c(filter, units = c(locations))
    }
    
    if (!is.null(types)) {
      filter <- c(filter, types = c(types))
    }
    
    if (!is.null(targets)) {
      filter <- c(filter, targets = c(targets))
    }
    
    if (!is.null(as_of)) {
      filter <- c(filter, as_of = c(date_to_datetime(as_of, hub)))
    }
    
    temp_filter_filepath <- tempfile(pattern = "filter", fileext = ".json")
    
    jsonlite::write_json(filter, temp_filter_filepath)
    
    temp_result_filepath <- tempfile(pattern = "query", fileext = ".csv")
    
    query_command <- paste0(
      "pipenv run python3 cli/bulk_data_query.py ",
      zoltar_module_path, " ",
      temp_filter_filepath, " ",
      temp_result_filepath
    )
    
    forecasts <- readr::read_csv(temp_result_filepath) %>%
      reformat_forecasts()
    
  }
  
  system(query_command)

  if (!is.null(forecasts)) {
    if (nrow(forecasts) > 0) {
      # append location, population information
      forecasts <- forecasts %>%
        join_with_hub_locations(hub = hub)
    } else {
      warning("Warning in load_forecasts_zoltar: Forecasts are not available.\n Please check your parameters.")
    }
  } else {
    warning("Warning in load_forecasts_zoltar: Forecasts are not available.\n Please check your parameters.")
  }

  return(forecasts)
}
