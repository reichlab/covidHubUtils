#' Load all available forecasts submitted on `forecast_dates` from local Zoltar module.
#'
#' @description
#' Please follow these steps to set up required environment prior to using this function
#' for the first time:
#' \itemize{
#'   \item Create a local clone of `reichlab/zoltpy` repository.
#'   \item Set the `DJANGO_SETTINGS_MODULE` environment variable 
#'   to `forecast_repo.settings.local_sqlite3` and  `MAX_NUM_QUERY_ROWS=20_000_000` 
#'   or another number
#'   \item Call `pipenv install` from the directory of local zolpy in command line.
#' }
#'
#' @details 
#' \itemize{
#'   \item This function will throw an error when no forecasts are submitted on
#' any dates in `forecast_dates` for selected `models`,
#' `locations`, `types` and `target`.
#'   \item By default. `test-load_forecasts_local_zoltar()` is skipped. Please modify 
#' `local_zoltpy_path` and `zoltar_module_path` on top of the unit test file to run tests.
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
#' @param zoltar_module_path path to local zoltar module w.r.t. `local_zoltpy_path`
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
                                        zoltar_module_path) {

  # set up Zoltar connection
  zoltar_connection <- setup_zoltar_connection(staging = FALSE)

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

        jsonlite::write_json(filter, temp_filter_filepath)

        temp_result_filepath <- tempfile(pattern = paste("query", Sys.getpid(), sep = ""), fileext = ".csv")

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

    jsonlite::write_json(filter, temp_filter_filepath)

    temp_result_filepath <- tempfile(pattern = "query", fileext = ".csv")

    query_command <- paste0(
      "pipenv run python3 cli/bulk_data_query.py ",
      zoltar_module_path, " ",
      temp_filter_filepath, " ",
      temp_result_filepath
    )

    system(query_command)
    
    forecasts <- readr::read_csv(temp_result_filepath) %>%
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
