 #' Load the most recent forecasts of all that were submitted
#' on `forecast_dates` from a local clone of `reichlab/covid19-forecast-hub` repo.
#'
#' This function will drop rows with NULLs in value column.
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' Please use [load_forecasts_repo()] instead.
#'
#' @inheritParams load_forecasts_repo
#'
#' @export
load_latest_forecasts_repo <- function(file_path,
                                       models = NULL,
                                       forecast_dates,
                                       locations = NULL,
                                       types = NULL,
                                       targets = NULL,
                                       hub = c("US", "ECDC"),
                                       verbose = TRUE) {
  lifecycle::deprecate_warn("0.1.5",
    "load_latest_forecasts_repo()",
    details =
      "This function has been superseded by the latest load_forecasts_repo(). Please switch your code to using the new function."
  )

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

  forecast_dates <- as.Date(forecast_dates)

  # get paths to all forecast files
  forecast_files <- get_forecast_file_path(models, file_path,
    forecast_dates,
    latest = TRUE,
    verbose = verbose
  )

  forecasts <- load_forecast_files_repo(
    file_paths = forecast_files,
    locations = locations,
    types = types,
    targets = targets,
    hub = hub
  )

  return(forecasts)
}