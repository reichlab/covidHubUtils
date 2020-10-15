#' Combine forecasts and truth to plot using plot_forecast().
#' It will load truth with the corrsponding target and locations in forecast data.
#' 
#' @param data forecasts data frame from load_forecast(). It has columns model, 
#' forecast_date,location, target, type, quantile, value, horizon and 
#' target_end_date.
#' @param horizon forecasts are plotted for the horizon time steps after the 
#' forecast date
#' @param quantiles_to_plot vector of quanitles to include in the plot
#' @param target_variable string specifying target type. It should be one of 
#' "Cumulative Cases","Cumulative Deaths","Incident Cases" and "Incident Deaths."
#' @param  truth_as_of the plot includes the truth data that would have been 
#' in real time as of the truth_as_of date. Default to today's date
#' 
#' @return data frame with columns model, 
#' forecast_date, location, target, type, quantile, value, horizon and 
#' target_end_date.
#' 
#' @export
get_plot_forecast_data <- function(data, 
                                   horizon,
                                   quantiles_to_plot,
                                   location,
                                   truth_source,
                                   target_variable,
                                   truth_as_of = Sys.Date()){
  
  forecasts<- covidHubUtils::pivot_forecasts_wider(data, quantiles_to_plot) %>%
    dplyr::mutate(truth_forecast = "forecast") %>%
    dplyr::filter(horizon <= horizon, location == location)
  
  
  truth <- covidHubUtils::load_truth(truth_source,
              target_variable, 
              #? truth_as_of?
              truth_end_date = truth_as_of,
              locations = location) %>%
    dplyr::rename(point = value) %>%
    dplyr::mutate(truth_forecast = "truth")
  
  plot_data <- bind_rows(forecasts, truth)
  
}