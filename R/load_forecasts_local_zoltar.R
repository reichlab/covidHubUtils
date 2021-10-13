#' Load all available forecasts submitted on `forecast_dates` from local Zoltar module.
#'
#' @description
#' Please follow instructions to set up required environment prior to using this function
#' for the first time
#' See \url{http://reichlab.io/covidHubUtils/articles/covidHubUtils-zoltar_sqlite_setup.html}{vignette} for more details.
#'
#'
#' @details 
#' \itemize{
#'   \item This function will throw an error when no forecasts are submitted on
#' any dates in `forecast_dates` for selected `models`,
#' `locations`, `types` and `target`.
#'   \item By default. `test-load_forecasts_local_zoltar()` is skipped. Please modify 
#' `local_zoltpy_path` and `zoltar_sqlite_file` on top of the unit test file to run tests.
#'}
#'
#' @param models Character vector of model abbreviations.
#' Default all models that submitted forecasts meeting the other criteria.
#' @param forecast_dates A 2 dimensional list of forecast dates to retrieve forecasts.
#' This function will return the latest forecasts
#' for each sub-list of dates.
#' Default to  `NULL` which would include all valid forecast dates in Zoltar.
#' @param locations list of fips. Default to all locations with available forecasts in Zoltar.
#' @param types Character vector specifying type of forecasts to load: `"quantile"`
#' and/or `"point"`. Default to all valid forecast types in Zoltar.
#' @param targets character vector of targets to retrieve, for example
#' `c('1 wk ahead cum death', '2 wk ahead cum death')`.
#' Default to all valid targets in Zoltar.
#' @param as_of character for date time to load forecasts submitted as of this time.
#' It could use the format of one of the three examples:
#' `"2021-01-01", "2020-01-01 01:01:01" and "2020-01-01 01:01:01 UTC".`
#' If you would like to set a timezone, it has to be UTC now.
#' If not, helper function will append the default timezone to your input based on `hub` parameter.
#' Default to `NULL` to load the latest version.
#' @param verbose logical for printing messages on zoltar job status. Default to `TRUE`.
#' @param local_zoltpy_path path to local clone of `zolpy` repository.
#' @param zoltar_sqlite_file path to local sqlite file, 
#' either a relative path w.r.t. `local_zoltpy_path` or an absolute path.
#' @param hub character vector, where the first element indicates the hub
#' from which to load forecasts. Possible options are `"US"` and `"ECDC"`.
#'
#' @return data.frame with columns `model`, `forecast_date`, `location`, `horizon`,
#' `temporal_resolution`, `target_variable`, `target_end_date`, `type`, `quantile`, `value`,
#' `location_name`, `population`, `geo_type`, `geo_value`, `abbreviation`
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
                                        zoltar_sqlite_file) {
  # set up environment variables
  Sys.setenv("DJANGO_SETTINGS_MODULE"="forecast_repo.settings.local_sqlite3")
  Sys.setenv("MAX_NUM_QUERY_ROWS"="20_000_000")

  # construct Zoltar project id
  project_id <- get_zoltar_project_id_local_zoltar(zoltar_sqlite_file, hub)
  
  # get information about all models in project
  all_models <- get_models_local_zoltar(zoltar_sqlite_file, project_id)

  # get all valid model abbrs
  all_valid_model_abbrs <- unique(all_models$model_abbr)

  if (!is.null(models)) {
    models <- match.arg(models,
      choices = all_valid_model_abbrs,
      several.ok = TRUE
    )
  }

  original_wd <- setwd(local_zoltpy_path)
  
  if (!is.null(forecast_dates)) {
    if (is.null(models)) {
      models <- all_models$model_abbr
    }

    cl <- parallel::makeCluster(2, setup_strategy = "sequential")
    doParallel::registerDoParallel(cl)
    forecasts <- foreach::foreach(i = 1:length(models), .combine = rbind) %dopar% {
      curr_model <- models[i]
      model_id <- all_models[all_models$model_abbr == curr_model, ]$id
      model_forecasts_history <- get_model_forecast_history_local_zoltar(
        zoltar_sqlite_file, model_id)$forecast_date
        
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
        filter <- c(
          models = list(curr_model),
          timezeros = list(latest_dates)
        )

        if (!is.null(locations)) {
          filter <- c(filter, units = list(locations))
        }

        if (!is.null(types)) {
          filter <- c(filter, types = list(types))
        }

        if (!is.null(targets)) {
          filter <- c(filter, targets = list(targets))
        }

        if (!is.null(as_of)) {
          filter <- c(filter, as_of = list(date_to_datetime(as_of, hub)))
        }

        temp_filter_filepath <- tempfile(pattern = paste("filter", Sys.getpid(), sep = ""), fileext = ".json")

        filter_json <- jsonlite::toJSON(filter)
        
        write(filter_json, temp_filter_filepath)

        temp_result_filepath <- tempfile(pattern = paste("query", Sys.getpid(), sep = ""), fileext = ".csv")

        query_command <- paste0(
          "pipenv run python3 cli/bulk_data_query.py ",
          zoltar_sqlite_file, " ",
          temp_filter_filepath, " ",
          temp_result_filepath
        )

        system(query_command)

        forecast <- readr::read_csv(
          temp_result_filepath,
          show_col_types = FALSE) %>%
          reformat_forecasts()
      }
    }
    # shut down workers
    doParallel::stopImplicitCluster()
    parallel::stopCluster(cl)
  } else {

    # create a json file as filter
    filter <- NULL

    if (!is.null(models)) {
      filter <- c(filter, models = list(models))
    }
    if (!is.null(locations)) {
      filter <- c(filter, units = list(locations))
    }

    if (!is.null(types)) {
      filter <- c(filter, types = list(types))
    }

    if (!is.null(targets)) {
      filter <- c(filter, targets = list(targets))
    }

    if (!is.null(as_of)) {
      filter <- c(filter, as_of = list(date_to_datetime(as_of, hub)))
    }

    temp_filter_filepath <- tempfile(pattern = "filter", fileext = ".json")
    
    filter_json <- jsonlite::toJSON(filter)
    
    write(filter_json, temp_filter_filepath)

    temp_result_filepath <- tempfile(pattern = "query", fileext = ".csv")

    query_command <- paste0(
      "pipenv run python3 cli/bulk_data_query.py ",
      zoltar_sqlite_file, " ",
      temp_filter_filepath, " ",
      temp_result_filepath
    )

    system(query_command)
    
    forecasts <- readr::read_csv(
      temp_result_filepath,
      show_col_types = FALSE) %>%
      reformat_forecasts()
  }

  setwd(original_wd)

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

#' Get Zoltar project id from local zoltar sqlite file
#'
#' @param zoltar_sqlite_file path to local sqlite file, 
#' either a relative path w.r.t. `local_zoltpy_path` or an absolute path.
#' @param hub character vector, where the first element indicates the hub
#' from which to load forecasts. Possible options are `"US"` and `"ECDC"`
#'
#' @return project id
get_zoltar_project_id_local_zoltar <- function(zoltar_sqlite_file,
                                  hub = c("US", "ECDC")) {
  # build connection to local sqlite file
  conn <- DBI::dbConnect(RSQLite::SQLite(), zoltar_sqlite_file)
  
  # create SQL query command 
  command <- "SELECD id, name FROM forecast_app_project;"
  
  # execute query
  the_projects <- DBI::dbGetQuery(conn, command)

  # get the URL to the right forecast hub project
  if (hub[1] == "US") {
    project_id <- the_projects[the_projects$name == "COVID-19 Forecasts", "url"]
  } else if (hub[1] == "ECDC") {
    project_id <- the_projects[the_projects$name == "ECDC European COVID-19 Forecast Hub", "url"]
  }
  
  return(project_id)
}

#' Get id and abbrevations of all models available in a specified project
#' from local zoltar sqlite file
#'
#' @param zoltar_sqlite_file path to local sqlite file, 
#' either a relative path w.r.t. `local_zoltpy_path` or an absolute path.
#' @param project_id one valid id number of a project available in 
#' local zoltar sqlite file. This should come from the result of [get_zoltar_project_id()]
#'
#' @return data.frame with `id` and `model_abbr` columns
get_models_local_zoltar <- function(zoltar_sqlite_file, project_id){
  # build connection to local sqlite file
  conn <- DBI::dbConnect(RSQLite::SQLite(), zoltar_sqlite_file)
  
  # create SQL query command 
  command <- "SELECT id, abbreviation FROM forecast_app_forecastmodel WHERE project_id = ?project_id;"
  query <- DBI::sqlInterpolate(conn, command, project_id = project_id)
  
  # execute query
  the_models <- DBI::dbGetQuery(conn, query) %>%
    dplyr::rename(model_abbr = abbreviation)
  
  return (the_models)
}

#' Get forecast submission history of a specified model from local zoltar sqlite file
#'
#' @param zoltar_sqlite_file path to local sqlite file, 
#' either a relative path w.r.t. `local_zoltpy_path` or an absolute path.
#' @param forecast_model_id one valid id number of a forecast model available in 
#' local zoltar sqlite file. This should come from the result of [get_models()]
#'
#' @return data.frame with `id` and `forecast_date` columns
get_model_forecast_history_local_zoltar <- function(zoltar_sqlite_file, forecast_model_id){
  # build connection to local sqlite file
  conn <- DBI::dbConnect(RSQLite::SQLite(), zoltar_sqlite_file)
  
  # create SQL query command 
  command <- "SELECT f.id, tz.timezero_date FROM forecast_app_forecast AS f 
  JOIN forecast_app_timezero AS tz ON f.time_zero_id = tz.id WHERE forecast_model_id = ?forecast_model_id;"
  query <- DBI::sqlInterpolate(conn, command, forecast_model_id = forecast_model_id)
  
  forecast_history <- DBI::dbGetQuery(conn, query) %>%
    dplyr::mutate(forecast_date = timezero_date)
  
  return (forecast_history)
}

