#' Combines load_truth() and pivot_forecasts_wider(), returns the data.
#'
#' @param forecast_data forecasts data frame from load_forecast(). 
#' It has columns model, forecast_date,location, target, type, quantile, 
#' value, horizon and target_end_date.
#' @param truth_data optional data frame with forecasts in the format returned 
#' by load_truth().
#' @param models_to_plot characters of model abbreviations 
#' @param forecast_dates_to_plot date string vectors for forecast dates to plot.
#' Default to all forecast dates available in forecast_data.
#' @param horizons_to_plot forecasts are plotted for the horizon time steps after 
#' the forecast date.
#' @param quantiles_to_plot vector of quanitles to include in the plot
#' @param locations_to_plot  optional character vector of location fips codes.
#' Default to all locations available in forecast_data.
#' @param plot_truth boolean to indicate whether truth data should be plotted.
#' Default to TRUE.
#' @param truth_source character specifying where the truth data will
#' be loaded from if truth_data is not provided. 
#' Otherwise, this character specifies the data source to plot. 
#' Currently support "JHU","USAFacts" and "NYTimes".
#' @param  target_variable_to_plot string specifying target type. It should be one of 
#' "cum death", "inc case", "inc death" and "inc hosp". Note, "inc hosp" is not supported
#' in load_truth() now.
#' @param  truth_as_of the plot includes the truth data that would have been 
#' in real time as of the truth_as_of date.
#' 
#' @return data frame with columns model, 
#' forecast_date, location,  target_variable, type, quantile, value, horizon and 
#' target_end_date.
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
                                   truth_as_of = NULL){
  
  # validate truth_source
  if (target_variable_to_plot != "inc hosp"){
    truth_source <- match.arg(truth_source, 
                              choices = c("JHU","USAFacts", "NYTimes"), 
                              several.ok = FALSE)
  } else {
    if (missing(truth_source) | is.na(truth_source)){
      stop("Error in get_plot_forecast_data: Please provide truth_source when target_variable is inc hosp.")
    } else {
      truth_source <- truth_source
    }
  }
  
  # validate locations_to_plot
  if (missing(locations_to_plot)){
    locations_to_plot <- unique(forecast_data$location)
  }
  
  all_valid_fips <- covidHubUtils::hub_locations$fips
  
  locations_to_plot <- intersect(as.character(locations_to_plot), 
                                 as.character(all_valid_fips))
  
  # validate forecast_dates_to_plot
  if (missing(forecast_dates_to_plot)){
    forecast_dates_to_plot <- unique(forecast_data$forecast_date)
  } else {
    forecast_dates_to_plot <- as.Date(forecast_dates_to_plot)
    if (!all(forecast_dates_to_plot %in% forecast_data$forecast_date)){
      stop ("Error in get_plot_forecast_data: Not all forecast_dates are available in forecast data.")
    }
  }
  
  # validate truth data if provided
  if (!is.null(truth_data)){
    # check if truth_data has all needed columns
    columns_check <- all(c("model", "target_variable", 
                           "target_end_date", "location", "value") 
                         %in% colnames(truth_data))
    if(columns_check == FALSE){
      stop("Error in get_plot_forecast_data: Please provide columns model, 
           target_variable, target_end_date, location and value in truth_data.")
    } else {
      # check if truth_data has data from specified source
      if (!(paste0("Observed Data (",truth_source,")") %in% truth_data$model)){
        stop("Error in get_plot_forecast_data: Please provide a valid truth_source to plot.")
      }
      # check if all dips codes in location column are valid
      if (!all(truth_data$location %in% all_valid_fips)){
        stop("Error in get_plot_forecast_data: Please make sure all dips codes in location column are valid.")
      }
      # check if truth_data has data from specified location
      if (!all(locations_to_plot %in% truth_data$location)){
        stop("Error in get_plot_forecast_data: Please provide a valid locations_to_plot.")
      }
      # check if truth_data has specified target variable
      if (!(target_variable_to_plot %in% truth_data$target_variable)){
        stop("Error in get_plot_forecast_data: Please provide a valid target variable.")
      }
    }
  }
  
  # warning for truth_as_of
  if(!is.null(truth_as_of)){
    warning("Warning in get_plot_forecast_data: Currently versioned truth data is not supported.")
  }
  
  # filter to include selected models, forecast dates, locations and target variable
  forecast_data <- forecast_data %>%
    dplyr::filter(model %in% models_to_plot,
                  forecast_date %in% forecast_dates_to_plot,
                  location %in% locations_to_plot,
                  target_variable == target_variable_to_plot)
  
  if (!missing(horizons_to_plot)){
    forecast_data <- forecast_data %>%
      dplyr::filter(horizon <= horizons_to_plot)
  }
  
  forecasts<- pivot_forecasts_wider(forecast_data, quantiles_to_plot) %>%
    dplyr::mutate(truth_forecast = "forecast") %>%
    dplyr::mutate(full_location_name = 
                    ifelse(geo_type == "county",
                           paste(location_name,abbreviation, sep = ", "),
                           location_name)) %>%
    dplyr::rename(fips = location, location = full_location_name)
  
  
  if (plot_truth){
    if (is.null(truth_data)){
      if (target_variable_to_plot == "inc hosp"){
        stop("Error in get_plot_forecast_data: inc hosp target is not supported in load_truth.
             Please provide truth_data.")
      } else {
        # call load_truth if the user did not provide truth_data
        truth <- load_truth(truth_source = truth_source,
                            target_variable = target_variable_to_plot,
                            locations = locations_to_plot) %>%
          dplyr::rename(point = value) %>%
          dplyr::mutate(truth_forecast = "truth")
      }
    } else {
      # process truth_data for plotting
      truth <- truth_data %>%
        dplyr::filter(model == paste0("Observed Data (",truth_source,")"), 
                      location %in% locations_to_plot,
                      target_variable == target_variable_to_plot) %>%
        dplyr::left_join(covidHubUtils::hub_locations, by = c("location" = "fips")) %>%
        dplyr::rename(point = value) %>%
        dplyr::mutate(truth_forecast = "truth",
                      point = as.numeric(point))
    }
    
    # add location name
    truth <- truth %>%
      dplyr::mutate(full_location_name = 
                      ifelse(geo_type == "county",
                             paste(location_name,abbreviation, sep = ", "),
                             location_name)) %>%
      dplyr::rename(fips = location, location = full_location_name)
    
    plot_data <- dplyr::bind_rows(forecasts, truth)
    return (plot_data)
  } else {
    return (forecasts)
  }
  
}
