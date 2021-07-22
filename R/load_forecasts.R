#' Load all available forecasts submitted around forecast dates from Zoltar
#' or local hub repo.
#'
#' If \code{date_window_size} is 0, this function returns all available forecasts
#' submitted on every day in \code{dates}.
#'
#' If \code{date_window_size} is not 0, this function will look for all the latest
#' forecasts that are submitted within window size for each day in \code{dates}.
#'
#' @param models Character vector of model abbreviations.
#' Default all models that submitted forecasts meeting the other criteria.
#' @param dates The forecast date of forecasts to retrieve.
#' A vector of one or more Date objects or character strings in format “YYYY-MM-DD”
#' Default to all valid forecast dates.
#' @param date_window_size The number of days across which to
#' look for the most recent forecasts for each date in dates parameter.
#' Default to 0, which means to only look at the dates parameter only.
#' @param locations list of location codes. Default to all locations with available forecasts.
#' @param types Character vector specifying type of forecasts to load: `"quantile"`
#' and/or `"point"`. Default to all valid forecast types.
#' @param targets character vector of targets to retrieve, for example
#' `c('1 wk ahead cum death', '2 wk ahead cum death')`.
#' Default to `NULL` which stands for all valid targets.
#' @param source string specifying where forecasts will be loaded from: either
#' `"local_hub_repo"` or `"zoltar"`. Default to `"zoltar"`.
#' @param hub_repo_path path to local clone of the forecast hub
#' repository
#' @param data_processed_subpath folder within the hub_repo_path that contains
#' forecast submission files.  Default to `"data-processed/"`, which is
#' appropriate for the covid19-forecast-hub repository.
#' @param as_of character for date time to load forecasts submitted as of this time from Zoltar.
#' Ignored if `source = "local_hub_repo"`.
#' It could use the format of one of the three examples:
#' `"2021-01-01", "2020-01-01 01:01:01" and "2020-01-01 01:01:01 UTC"`.
#' If you would like to set a timezone, it has to be UTC now.
#' If not, helper function will append the default timezone to your input based on `hub` parameter.
#' Default to NULL to load the latest version.
#' @param hub character vector, where the first element indicates the hub
#' from which to load forecasts. Possible options are `"US"` and `"ECDC"`.
#' @param verbose logical to print out diagnostic messages. Default is `TRUE`.
#'
#' @return data.frame with columns `model`, `forecast_date`, `location`, `horizon`,
#' `temporal_resolution`, `target_variable`, `target_end_date`, `type`, `quantile`, `value`,
#' `location_name`, `population`, `geo_type`, `geo_value`, `abbreviation`
#'
#' @examples
#' # load forecasts from US forecast hub
#' load_forecasts(
#'   models = "COVIDhub-ensemble",
#'   dates = "2020-12-07",
#'   locations = "US",
#'   types = c("point", "quantile"),
#'   targets = paste(1:4, "wk ahead inc case"),
#'   source = "zoltar",
#'   verbose = FALSE,
#'   as_of = NULL
#' )
#'
#' # load forecasts from ECDC forecast hub
#' load_forecasts(
#'   models = "ILM-EKF",
#'   hub = c("ECDC", "US"),
#'   dates = "2021-03-08",
#'   locations = "GB",
#'   targets = paste(1:4, "wk ahead inc death"),
#'   source = "zoltar"
#' )
#' @export
load_forecasts <- function(
                           models = NULL,
                           dates = NULL,
                           date_window_size = 0,
                           locations = NULL,
                           types = NULL,
                           targets = NULL,
                           source = "zoltar",
                           hub_repo_path,
                           data_processed_subpath = "data-processed/",
                           as_of = NULL,
                           hub = c("US", "ECDC"),
                           verbose = TRUE) {

  # validate source
  source <- match.arg(source, choices = c("local_hub_repo", "zoltar"))

  if (!is.null(dates)) {
    # 2d array
    all_forecast_dates <- purrr::map(
      dates, function(date) {
        return(as.Date(date) + seq(from = -date_window_size, to = 0))
        }
      )
  } else {
    all_forecast_dates <- dates
  }

  if (source == "local_hub_repo") {
    # validate hub repo path
    if (missing(hub_repo_path) | !dir.exists(hub_repo_path)) {
      stop("Error in load_forecasts: Please provide a vaid path to hub repo.")
    }

    if (!is.null(as_of)) {
      if (as_of != Sys.Date()) {
        stop("Error in load_forecasts: as_of parameter is not available for `local_hub_repo` source now.")
      }
    }

    # path to data-processed folder in hub repo
    data_processed <- file.path(hub_repo_path, data_processed_subpath)

    forecasts <- load_forecasts_repo(
      file_path = data_processed,
      models = models,
      forecast_dates = all_forecast_dates,
      locations = locations,
      types = types,
      targets = targets,
      verbose = verbose,
      hub = hub
    )
  } else {
    forecasts <- load_forecasts_zoltar(
      models = models,
      forecast_dates = all_forecast_dates,
      locations = locations,
      types = types,
      targets = targets,
      as_of = as_of,
      verbose = verbose,
      hub = hub
    )
  }
  return(forecasts)
}
