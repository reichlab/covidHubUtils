#' Plot forecasts with truth for only one model, one target and one location
#'
#' @param forecast_data data frame with truth and forecasts from load_forecasts()
#' @param model model_abbr specifying model to plot
#' @param target_variable string specifying target type. It should be one of 
#' "Cumulative Cases","Cumulative Deaths","Incident Cases" and "Incident Deaths,"
#' @param location string for fips code or 'US'
#' @param intervals values indicating which central prediction interval levels 
#' to plot, defaults to c(.5, .8, .95)
#' @param horizon forecasts are plotted for the horizon time steps after the 
#' forecast date
#' @param truth_source character specifying where the truths will be loaded from.
#' Currently supports "jhu","usafacts", "nytimes". Default to "jhu".
#' @param truth_as_of the plot includes the truth data that would have been 
#' in real time as of the truth_as_of date (not using this parameter when truth data 
#' is from github repo)

#' 
#' @return ggplot graph
#' 
#' @export
plot_forecast <- function(forecast_data,
                          model,
                          target_variable,
                          location,
                          intervals = c(.5, .8, .95),
                          horizon,
                          truth_source = "JHU",
                          truth_as_of = NULL){
  
  # Validate target_variable
  target_variable <- match.arg(target_variable, 
                               choices = c("Cumulative Cases",
                                           "Cumulative Deaths",
                                           "Incident Cases",
                                           "Incident Deaths"), 
                               several.ok = FALSE)
  # Validate location fips code
  all_valid_fips <- covidHubUtils::hub_locations %>%
    pull(fips)
  location <- match.arg(location, 
                         choices = all_valid_fips, 
                         several.ok = FALSE)
  
  # Validate truth_source
  truth_source <- match.arg(truth_source, 
                            choices = c("JHU","USAFacts", "NYTimes"), 
                            several.ok = FALSE)
  
  
  # Generate quantiles based on given intervals
  quantiles_to_plot <- unlist(lapply(intervals, function(interval){
    c(0.5 - as.numeric(interval)/2,
     0.5 + as.numeric(interval)/2)
  }))
  
  
  #Prediction interval shades
  blues <- RColorBrewer::brewer.pal(n=length(quantiles_to_plot)/2+1, "Blues")
  
  # Include truth from remote git hub repo by default
  # Not using truth_as_of if we are loading truth from git hub repos
  plot_data = covidHubUtils::get_plot_forecast_data (data = forecast_data, 
                                                     horizon = horizon,
                                                     quantiles_to_plot = quantiles_to_plot,
                                                     location = location,
                                                     truth_source = truth_source,
                                                     target_variable = target_variable
                                                     #truth_as_of = truth_as_of
                                                     )
 
  
  ggplot(data = plot_data) +
    
    # Plot all prediction intervals
    geom_ribbon(data = plot_data %>%
                  dplyr::filter(type == "quantile"),
                mapping = aes(x = target_end_date,
                              ymin=lower, ymax=upper,
                              fill=`Prediction Interval`)) +
  
    geom_line(data = plot_data %>%
                dplyr::filter(!is.na(point)),
              mapping = aes(x = target_end_date, y = point, color = model)) +
    
    geom_point(data = plot_data %>%
                 dplyr::filter(!is.na(point)),
               mapping = aes(x = target_end_date, y = point, color = model)) +
  
    scale_fill_manual(values = blues[1:(length(blues)-1)]) +
    scale_color_manual(values = c(tail(blues,1),"black")) +
    scale_x_date(name = NULL, date_breaks="1 month", date_labels = "%b %d") +
    ylab(target_variable) +
    labs(title=paste0("Weekly COVID-19 ", target_variable, " in ", 
                      location,": observed and forecasted") ,
         caption=paste0("source: ", toupper(truth_source)," (observed data), ",
                        model," (forecasts)")) 

}
