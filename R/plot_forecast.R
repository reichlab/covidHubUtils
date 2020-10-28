#' Plot forecasts with truth for only one model, one target and one location
#'
#' @param forecast_data data frame with truth and forecasts from load_forecasts()
#' @param model model_abbr specifying model to plot
#' @param target_variable string specifying target type. It should be one of 
#' Cumulative Deaths","Incident Cases" and "Incident Deaths,"
#' @param location string for fips code or 'US'
#' @param intervals values indicating which central prediction interval levels 
#' to plot, defaults to c(.5, .8, .95). NULL means only plotting point forecasts.
#' @param horizon forecasts are plotted for the horizon time steps after the 
#' forecast date
#' @param truth_source character specifying where the truths will be loaded from.
#' @param plot boolean for showing the plot. Default to TRUE.
#' Currently supports "JHU","USAFacts", "NYTimes". Default to "JHU".
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
                          plot = TRUE,
                          truth_as_of = NULL){
  # validate model
  if (!missing(model)){
    if (!model %in% forecast_data$model) {
      stop("Error in plot_forecast: model is not in forecast_data.")
    }
  } else {
    stop("Error in plot_forecast: Please provide a model parameter.")
  }
  
  # validate target_variable
  target_variable <- match.arg(target_variable, 
                               choices = c("Cumulative Deaths",
                                           "Incident Cases",
                                           "Incident Deaths"), 
                               several.ok = FALSE)
  
  inc_cum = ifelse(
    unlist(strsplit(target_variable, " "))[1] == "Cumulative",
    "cum", 
    "inc")
  
  death_case = ifelse(
    unlist(strsplit(target_variable, " "))[2] == "Cases",
    "case",
    "death")

  if (!(inc_cum %in% forecast_data$inc_cum & death_case %in% forecast_data$death_case)){
    stop("Error in plot_forecast: target_variable is not in forecast data.")
  }
  
  # validate location fips code
  all_valid_fips <- covidHubUtils::hub_locations$fips
  
  location <- match.arg(location, 
                         choices = all_valid_fips, 
                         several.ok = FALSE)
  
  if (!location %in% forecast_data$location){
    stop("Error in plot_forecast: location is not in forecast_data.")
  }
  
  # validate truth_source
  truth_source <- match.arg(truth_source, 
                            choices = c("JHU","USAFacts", "NYTimes"), 
                            several.ok = FALSE)
  
  
  # generate quantiles based on given intervals
  
  quantiles_to_plot <- unlist(lapply(intervals, function(interval){
    c(0.5 - as.numeric(interval)/2,
     0.5 + as.numeric(interval)/2)
  }))
  
  
  # prediction interval shades
  if (!is.null(intervals)){
    blues <- RColorBrewer::brewer.pal(n=length(quantiles_to_plot)/2+1, "Blues")
  } else {
    blues <- RColorBrewer::brewer.pal(n=4, "Blues")
  }
  
  # include truth from remote git hub repo by default
  # not using truth_as_of if we are loading truth from git hub repos
  plot_data = covidHubUtils::get_plot_forecast_data (data = forecast_data, 
                                                     model_to_plot = model,
                                                     horizons_to_plot = horizon,
                                                     quantiles_to_plot = quantiles_to_plot,
                                                     location_to_plot = location,
                                                     truth_source = truth_source,
                                                     target_variable = target_variable,
                                                     truth_as_of = truth_as_of
                                                     )
 
  
  graph <- ggplot2::ggplot(data = plot_data)
    
  if (!is.null(intervals)){
    # plot all prediction intervals
    graph <- graph  +
      ggplot2::geom_ribbon(data = plot_data %>%
                  dplyr::filter(type == "quantile"),
                mapping = aes(x = target_end_date,
                              ymin=lower, ymax=upper,
                              fill=`Prediction Interval`)) +
      
      ggplot2::scale_fill_manual(values = blues[1:(length(blues)-1)])
      
  }
  
  # plot point forecasts and truth 
  graph <- graph +
    
    ggplot2::geom_line(data = plot_data %>%
                dplyr::filter(!is.na(point)),
              mapping = aes(x = target_end_date, y = point, color = truth_forecast)) +
    
    ggplot2::geom_point(data = plot_data %>%
                 dplyr::filter(!is.na(point)),
               mapping = aes(x = target_end_date, y = point, color = truth_forecast)) +
    
    ggplot2::scale_color_manual(name = "Model", 
                       label = c(model, paste0("Observed Data (",truth_source,")")), 
                                 values = c(tail(blues,1),"black")) +
    ggplot2::scale_x_date(name = NULL, date_breaks="1 month", date_labels = "%b %d") +
    ggplot2::ylab(target_variable) +
    ggplot2::labs(title = paste0("Weekly COVID-19 ", target_variable, " in ", 
                                 location,": observed and forecasted") ,
                  caption = paste0("source: ", truth_source," (observed data), ",
                                   model," (forecasts)")) 
  if (plot){
    print(graph)
  }
}
