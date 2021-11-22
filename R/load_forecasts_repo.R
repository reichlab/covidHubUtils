#' Load all available forecasts submitted on `forecast_dates` from
#' a local clone of `reichlab/covid19-forecast-hub` repo.
#'
#' This function will throw an error when no forecasts are submitted on
#' any dates in `forecast_dates` for selected `models`,
#' `locations`, `types` and `target`.
#'
#' This function will drop rows with `NULL` in `value` column.
#'
#' @param file_path path to the data-processed folder within a local clone of the hub repo
#' @param models Character vector of model abbreviations.
#' Default all models that submitted forecasts meeting the other criteria.
#' @param forecast_dates A 2 dimensional list of forecast dates to retrieve forecasts.
#' This function will return the latest forecasts
#' for each sub-list of dates.
#' Default to  `NULL` which would include all valid forecast dates.
#' The function will throw an error if all dates in this parameter are invalid forecast dates.
#' @param locations  a vector of strings for fips code, CBSA codes, location names
#' such as "Hampshire COunty, MA", "United Kingdom","Alabama"
#' for a US county location names must include state abbreviation.
#' list of fips. Default to all locations with available forecasts.
#' @param types Character vector specifying type of forecasts to load: `"quantile"`
#' and/or `"point"`. Default to all valid forecast types.
#' @param targets character vector of targets to retrieve, for example
#' `c('1 wk ahead cum death', '2 wk ahead cum death')`.
#' Default to `NULL` which stands for all valid targets.
#' @param hub character vector, where the first element indicates the hub
#' from which to load forecasts. Possible options are `"US"` and `"ECDC"`.
#' @param verbose logical to print out diagnostic messages. Default is `TRUE`
#'
#' @return data.frame with columns `model`, `forecast_date`, `location`, `horizon`,
#' `temporal_resolution`, `target_variable`, `target_end_date`, `type`, `quantile`, `value`,
#' `location_name`, `population`, `geo_type`, `geo_value`, `abbreviation`
#'
#' @export
load_forecasts_repo <- function(
                                file_path,
                                models = NULL,
                                forecast_dates = NULL,
                                locations = NULL,
                                types = NULL,
                                targets = NULL,
                                hub = c("US", "ECDC"),
                                verbose = TRUE) {

  # validate file path to data-processed folder
  if (!dir.exists(file_path)) {
    stop("Error in load_forecasts_repo: data-processed folder does not 
         exist in the local_hub_repo directory.")
  }

  # validate models
  all_valid_models <- list.dirs(file_path, full.names = FALSE)
  all_valid_models <- all_valid_models[nchar(all_valid_models) > 0]
  
  if (!is.null(models)) {
    models <- unlist(purrr::map(models, function(model) {
      match.arg(model, choices = all_valid_models)
    }))
  } else {
    models <- all_valid_models
  }
  
  models <- sort(models, method = "radix")

  # Convert location names to fips codes or country abbreviations
  locations <- name_to_fips(locations, hub)
  
  # get valid location codes
  if (hub[1] == "US") {
    valid_location_codes <- covidHubUtils::hub_locations$fips
  } else if (hub[1] == "ECDC") {
    valid_location_codes <- covidHubUtils::hub_locations_ecdc$location
  }

  # validate locations
  if (!is.null(locations)) {
    locations <- match.arg(locations, choices = valid_location_codes, several.ok = TRUE)
  } else {
    locations <- valid_location_codes
  }

  # validate types
  if (!is.null(types)) {
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
  if (!is.null(targets)) {
    targets <- match.arg(targets, choices = all_valid_targets, several.ok = TRUE)
  } else {
    targets <- all_valid_targets
  }

  # validate forecast_dates
  if (!is.null(forecast_dates)) {
    # get paths to all forecast files
    forecast_files <- purrr::map(forecast_dates,
      get_forecast_file_path,
      models = models,
      file_path = file_path,
      latest = TRUE,
      verbose = verbose
    )
    # drop duplicates
    forecast_files <- unique(unlist(forecast_files, use.names = FALSE))
  } else {
    forecast_dates <- seq(as.Date("2020-01-25"), Sys.Date(), by = "days")

    forecast_files <- get_forecast_file_path(
      models = models,
      file_path = file_path,
      forecast_dates = forecast_dates,
      latest = FALSE,
      verbose = verbose
    )
  }

  if (!is.null(forecast_files)) {
    # read in the forecast files
    forecasts <- load_forecast_files_repo(
      file_paths = forecast_files,
      locations = locations,
      types = types,
      targets = targets,
      hub = hub
    )
  } else{
    forecasts <- NULL
  }

  return(forecasts)
}

#' Generate paths to forecast files submitted on a range of forecast dates from selected models
#'
#' @param models character vector of model abbreviations.
#' @param file_path path to local clone of the reichlab/covid19-forecast-hub/data-processed
#' @param forecast_dates date vector to look for forecast files
#' @param latest boolean to only generate path to the latest forecast file from each model
#' @param verbose whether or not to print out diagnostic messages. Default is TRUE
#'
#' @return a list of paths to forecast files submitted on a range of forecast dates from selected models

get_forecast_file_path <- function(models,
                                   file_path,
                                   forecast_dates,
                                   latest = FALSE,
                                   verbose = TRUE) {
  forecast_files <- purrr::map(
    models,
    function(model) {
      if (substr(file_path, nchar(file_path), nchar(file_path)) == "/") {
        file_path <- substr(file_path, 1, nchar(file_path) - 1)
      }

      results_path <- file.path(
        file_path,
        paste0(model, "/", forecast_dates, "-", model, ".csv")
      )
      results_path <- results_path[file.exists(results_path)]

      if (latest) {
        results_path <- tail(results_path, 1)
      }

      if (length(results_path) == 0) {
        if (verbose) {
          message <- paste(
            "Warning in get_forecast_file_path: Couldn't find forecasts for model",
            model, "on the following forceast dates:",
            forecast_dates
          )
          warning(message)
        }
        return(NULL)
      } else {
        return(results_path)
      }
    }
  ) %>% unlist()
  return(forecast_files)
}

#' Read in a set of forecast files from a repository
#' without rows having NULLs in value column.
#'
#' @param file_paths paths to csv forecast files to read in.  It is expected that
#' the file names are in the format "*YYYY-MM-DD-<model_name>.csv".
#' @inheritParams load_forecasts_repo
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
                                     targets = NULL,
                                     hub = c("US", "ECDC")) {

  # validate file_paths exist
  if (is.null(file_paths) | missing(file_paths)) {
    stop("In load_forecast_files_repo, file_paths are not provided.")
  }

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
        lazy = FALSE,
        col_types = readr::cols(
          forecast_date = readr::col_date(format = ""),
          target = readr::col_character(),
          target_end_date = readr::col_date(format = ""),
          location = readr::col_character(),
          type = readr::col_character(),
          quantile = readr::col_double(),
          value = readr::col_character()
        )
      )

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
          value = value
        )

      # drop rows with NULL in value column
      single_forecast <- single_forecast[!single_forecast$value == "NULL", ] %>%
        dplyr::mutate(value = as.double(value))

      return(single_forecast)
    }
  ) %>%
    tidyr::separate(target,
      into = c("horizon", "temporal_resolution", "ahead", "target_variable"),
      remove = FALSE, extra = "merge"
    ) %>%
    dplyr::select(
      model, forecast_date, location, horizon, temporal_resolution,
      target_variable, target_end_date, type, quantile, value
    ) %>%
    join_with_hub_locations(hub = hub)
  return(all_forecasts)
}
