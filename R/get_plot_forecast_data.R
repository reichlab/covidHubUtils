#' Combines load_truth() and pivot_forecasts_wider(), returns the data.
#'
#' @param forecast_data forecasts data frame from load_forecast(). 
#' It has columns model, forecast_date,location, target, type, quantile, 
#' value, horizon and target_end_date.
#' @param truth_data optional data frame with forecasts in the format returned 
#' by load_truth().
#' @param model_to_plot characters of model abbreviations 
#' @param horizons_to_plot forecasts are plotted for the horizon time steps after 
#' the forecast date.
#' @param quantiles_to_plot vector of quanitles to include in the plot
#' @param location_to_plot character vector of one location. 
#' @param plot_truth boolean to indicate whether truth data should be plotted.
#' Default to TRUE.
#' @param truth_source character specifying where the truth data will
#' be loaded from if truth_data is not provided. 
#' Otherwise, this character specifies the data source to plot. 
#' Currently support "JHU","USAFacts" and "NYTimes".
#' @param target_variable string specifying target type. It should be one of 
#' "cum death", "inc case", "inc death" and "inc hosp"
#' @param  truth_as_of the plot includes the truth data that would have been 
#' in real time as of the truth_as_of date.
#' 
#' @return data frame with columns model, 
#' forecast_date, location, target_variable, type, quantile, value, horizon and 
#' target_end_date.
#' 
#' @export
get_plot_forecast_data <- function(forecast_data, 
                                   truth_data = NULL,
                                   model_to_plot,
                                   horizons_to_plot,
                                   quantiles_to_plot,
                                   location_to_plot,
                                   plot_truth = TRUE,
                                   truth_source,
                                   target_variable,
                                   truth_as_of = NULL){
  
  # validate truth_source
  truth_source <- match.arg(truth_source, 
                            choices = c("JHU","USAFacts", "NYTimes"), 
                            several.ok = FALSE)
  # validate location
  all_valid_fips <- covidHubUtils::hub_locations$fips
  
  
  if (!missing(location_to_plot)){
    location_to_plot <- match.arg(location_to_plot, 
                                  choices = all_valid_fips, 
                                  several.ok = FALSE)
  } else {
    stop("Error in get_plot_forecast_data: Please provide a location_to_plot parameter.")
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
      # check if truth_data has data from specified location
      if (!(location_to_plot %in% truth_data$location)){
        stop("Error in get_plot_forecast_data: Please provide a valid location_to_plot.")
      }
      # check if truth_data has specified target variable
      if (!(target_variable %in% truth_data$target_variable)){
        stop("Error in get_plot_forecast_data: Please provide a valid target variable.")
      }
    }
  }
  
  # warning for truth_as_of
  if(!is.null(truth_as_of)){
    warning("Warning in get_plot_forecast_data: Currently versioned truth data is not supported.")
  }
  
  # filter horizons and locations. Only plot one location now.
  forecast_data <- forecast_data %>%
    dplyr::filter(model == model_to_plot,
                  location == location_to_plot,
                  target_variable == target_variable)
  
  if (!missing(horizons_to_plot)){
    forecast_data <- forecast_data %>%
      dplyr::filter(horizon <= horizons_to_plot)
  }
  
  forecasts<- pivot_forecasts_wider(forecast_data, quantiles_to_plot) %>%
    dplyr::mutate(truth_forecast = "forecast")
  
  if (is.null(truth_data)){
    # call load_truth if the user did not provide truth_data
    truth <- load_truth(truth_source,
                        target_variable,
                        locations = location_to_plot) %>%
      dplyr::rename(point = value) %>%
      dplyr::mutate(truth_forecast = "truth")
    
  } else {
    # process truth_data for plotting
    truth <- truth_data %>%
      dplyr::filter(model == paste0("Observed Data (",truth_source,")"), 
                    location == location_to_plot,
                    target_variable == target_variable) %>%
      dplyr::rename(point = value) %>%
      dplyr::mutate(truth_forecast = "truth",
                    point = as.numeric(point))
  }
   
  if (plot_truth){
    plot_data <- dplyr::bind_rows(forecasts, truth)
    return (plot_data)
  } else {
    return (forecasts)
  }
  
}
