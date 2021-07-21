#' Combine [load_truth()] and [pivot_forecasts_wider()]
#'
#' @param forecast_data required data.frame with forecasts in the format returned
#' by [load_forecasts()].
#' It has columns `model`, `forecast_date`, `location`, `target`, `type`, `quantile`,
#' `value`, `horizon` and `target_end_date`.
#' @param truth_data optional data.frame from one truth source in the format returned
#' by [load_truth()]. It needs to have columns `model`, `target_variable`,
#' `target_end_date`, `location` and `value`.
#' `Model` column can be "Observed Data (a truth source)".
#' @param models_to_plot characters of model abbreviations
#' @param forecast_dates_to_plot date string vectors for forecast dates to plot.
#' Default to all forecast dates available in `forecast_data`.
#' @param horizons_to_plot forecasts are plotted for the horizon time steps after
#' the forecast date.
#' @param quantiles_to_plot vector of quantiles to include in the plot
#' @param locations_to_plot  optional character vector of location fips codes.
#' Default to all locations available in `forecast_data`.
#' @param plot_truth logical to indicate whether truth data should be plotted.
#' Default to `TRUE`.
#' @param truth_source character specifying where the truth data will
#' be loaded from if truth_data is not provided. Currently support `"JHU"`,
#' `"USAFacts"`, `"NYTimes"` and `"HealthData"`.
#' Optional if `truth_data` is provided.
#' @param  target_variable_to_plot string specifying target type. It should be one of
#' `"cum death"`, `"inc case"`, `"inc death"` and `"inc hosp"`.
#' @param  truth_as_of the plot includes the truth data that would have been
#' in real time as of the `truth_as_of` date.
#' @param hub character, which hub to use. Default is `"US"`, other option is
#' `"ECDC"`
#'
#' @return data.frame with columns `model`,
#' `forecast_date`, `location`, `target_variable`, `type`, `quantile`, `value`,
#' `horizon` and `target_end_date`.
#'
#' @export
get_plot_forecast_data <- function(forecast_data,
                                   truth_data = NULL,
                                   models_to_plot,
                                   forecast_dates_to_plot,
                                   horizons_to_plot,
                                   quantiles_to_plot,
                                   locations_to_plot,
                                   plot_truth = TRUE,
                                   truth_source,
                                   target_variable_to_plot,
                                   truth_as_of = NULL,
                                   hub = c("US", "ECDC")) {

  # get lists of valid parameter choices based on `hub`
  if (hub[1] == "US") {
    valid_location_codes <- covidHubUtils::hub_locations$fips
    valid_target_variables <- c(
      "cum death", "inc case",
      "inc death", "inc hosp"
    )
    valid_truth_sources <- c("JHU", "USAFacts", "NYTimes", "HealthData")
  } else if (hub[1] == "ECDC") {
    valid_location_codes <- covidHubUtils::hub_locations_ecdc$location
    valid_target_variables <- c("inc case", "inc death")
    valid_truth_sources <- c("JHU", "jhu", "ECDC", "ecdc")
  }

  # validate locations_to_plot
  if (missing(locations_to_plot)) {
    locations_to_plot <- unique(forecast_data$location)
  }

  locations_to_plot <- intersect(
    as.character(locations_to_plot),
    as.character(valid_location_codes)
  )

  # validate forecast_dates_to_plot
  if (missing(forecast_dates_to_plot)) {
    forecast_dates_to_plot <- unique(forecast_data$forecast_date)
  } else {
    forecast_dates_to_plot <- as.Date(forecast_dates_to_plot)
    if (!all(forecast_dates_to_plot %in% forecast_data$forecast_date)) {
      stop("Error in get_plot_forecast_data: Not all forecast_dates are available in forecast data.")
    }
  }

  # validate truth data if provided
  if (!is.null(truth_data)) {
    # check if truth_data has all needed columns
    columns_check <- all(c(
      "model", "target_variable",
      "target_end_date", "location", "value"
    )
    %in% colnames(truth_data))
    if (columns_check == FALSE) {
      stop("Error in get_plot_forecast_data: Please provide columns model, 
           target_variable, target_end_date, location and value in truth_data.")
    } else {
      # check if all fips codes in location column are valid
      if (!all(truth_data$location %in% valid_location_codes)) {
        stop("Error in get_plot_forecast_data: Please make sure all fips codes in location column are valid.")
      }
      # check if truth_data has data from specified location
      if (!all(locations_to_plot %in% truth_data$location)) {
        stop("Error in get_plot_forecast_data: Please provide a valid locations_to_plot.")
      }
      # check if truth_data has specified target variable
      if (!(target_variable_to_plot %in% truth_data$target_variable)) {
        stop("Error in get_plot_forecast_data: Please provide a valid target variable.")
      }
    }
  } else {
    # validate truth_source
    truth_source <- match.arg(truth_source,
      choices = valid_truth_sources,
      several.ok = FALSE
    )
  }

  # create temporal resolution for loading truth
  if (target_variable_to_plot == "inc hosp") {
    temporal_resolution <- "daily"
  } else {
    temporal_resolution <- "weekly"
  }


  # warning for truth_as_of
  if (!is.null(truth_as_of)) {
    warning("Warning in get_plot_forecast_data: Currently versioned truth data is not supported.")
  }

  # filter to include selected models, forecast dates, locations and target variable
  forecast_data <- forecast_data %>%
    dplyr::filter(
      model %in% models_to_plot,
      forecast_date %in% forecast_dates_to_plot,
      location %in% locations_to_plot,
      target_variable == target_variable_to_plot
    )

  if (!missing(horizons_to_plot)) {
    forecast_data <- forecast_data %>%
      dplyr::filter(horizon <= horizons_to_plot)
  }

  forecasts <- pivot_forecasts_wider(forecast_data, quantiles_to_plot) %>%
    dplyr::mutate(truth_forecast = "forecast")

  if (hub[1] == "US") {
    forecasts <- forecasts %>%
      dplyr::rename(abbr = location, location = full_location_name)
  } else if (hub[1] == "ECDC") {
    forecasts <- forecasts %>%
      dplyr::rename(abbr = location, location = location_name)
  }

  if (plot_truth) {
    if (is.null(truth_data)) {
      # call load_truth if the user did not provide truth_data
      truth <- load_truth(
        truth_source = truth_source,
        target_variable = target_variable_to_plot,
        locations = locations_to_plot,
        temporal_resolution = temporal_resolution,
        hub = hub
      ) %>%
        dplyr::rename(point = value) %>%
        dplyr::mutate(truth_forecast = "truth")
    } else {
      # process truth_data for plotting
      truth <- truth_data %>%
        dplyr::filter(
          location %in% locations_to_plot,
          target_variable == target_variable_to_plot
        )

      # add location info if user-provided truth does not have them
      if ((!"location_name" %in% colnames(truth)) | (!"full_location_name" %in% colnames(truth))) {
        truth <- truth %>%
          dplyr::select(model, target_variable, target_end_date, location, value) %>%
          join_with_hub_locations(hub = hub)
      }

      truth <- truth %>%
        dplyr::rename(point = value) %>%
        dplyr::mutate(truth_forecast = "truth", point = as.numeric(point))
    }

    if (hub[1] == "US") {
      truth <- truth %>%
        dplyr::rename(abbr = location, location = full_location_name)
    } else if (hub[1] == "ECDC") {
      truth <- truth %>%
        dplyr::rename(abbr = location, location = location_name)
    }

    plot_data <- dplyr::bind_rows(forecasts, truth)
    return(plot_data)
  } else {
    return(forecasts)
  }
}
