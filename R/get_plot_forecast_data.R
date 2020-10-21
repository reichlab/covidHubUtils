#' Combines load_truth() and pivot_forecasts_wider(), returns the data.
#'
#' @param data forecasts data frame from load_forecast(). It has columns model, 
#' forecast_date,location, target, type, quantile, value, horizon and 
#' target_end_date.
#' @param horizon forecasts are plotted for the horizon time steps after the 
#' forecast date
#' @param quantiles_to_plot vector of quanitles to include in the plot
#' @param location character vector of one location. 
#' @param truth_source character vector specifying where the truths will
#' be loaded from: currently support "JHU","USAFacts", "NYTimes"
#' @param target_variable string specifying target type. It should be one of 
#' "Cumulative Cases","Cumulative Deaths","Incident Cases" and "Incident Deaths."
#' @param  truth_as_of the plot includes the truth data that would have been 
#' in real time as of the truth_as_of date.
#' 
#' @return data frame with columns model, 
#' forecast_date, location, inc_cum, death_case, type, quantile, value, horizon and 
#' target_end_date.
#' 
#' @export
get_plot_forecast_data <- function(data, 
                                   horizon,
                                   quantiles_to_plot,
                                   location,
                                   truth_source,
                                   target_variable,
                                   truth_as_of = NULL){
  
  # validate location
  all_valid_fips <- covidHubUtils::hub_locations %>%
    pull(fips)
  
  if (!missing(location)){
    location <- match.arg(location, 
                           choices = all_valid_fips, 
                           several.ok = FALSE)
  } else {
    stop("Error in get_plot_forecast_data: Please provide a location parameter.")
  }
  
  forecasts<- covidHubUtils::pivot_forecasts_wider(data, quantiles_to_plot) %>%
    dplyr::mutate(truth_forecast = "forecast") %>%
    # Filter horizons and locations. Only plot one location now.
    dplyr::filter(horizon <= horizon, location == location)
  
  # Load truth from remote git hub repo. 
  # Not using truth_as_of here.
  truth <- covidHubUtils::load_truth(truth_source,
              target_variable, 
              #truth_end_date = ,
              locations = location) %>%
    dplyr::rename(point = value) %>%
    dplyr::mutate(truth_forecast = "truth")
  
  plot_data <- bind_rows(forecasts, truth)
  
}