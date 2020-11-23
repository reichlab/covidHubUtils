#' Plot forecasts with truth for only one model, one target and one location
#'
#' @param forecast_data data frame with truth and forecasts from load_forecasts()
#' @param truth_data optional data frame with forecasts in the format returned 
#' by load_truth().
#' @param model model_abbr specifying model to plot. Optional if there is only
#' one model available in forecast data.
#' @param target_variable string specifying target type. It should be one of 
#' "cum death", "inc case", "inc death"
#' @param location string for fips code or 'US'. Optional if there is only one
#' location available in forecast data.
#' @param intervals values indicating which central prediction interval levels 
#' to plot. NULL means only plotting point forecasts.
#' If not provided, it will default to c(.5, .8, .95).
#' @param horizon forecasts are plotted for the horizon time steps after the 
#' forecast date. Default to all available horizons in forecast data. 
#' @param truth_source character specifying where the truth data will
#' be loaded from if truth_data is not provided. 
#' Otherwise, this character specifies the data source to plot. 
#' Currently support "JHU","USAFacts" and "NYTimes".
#' @param plot boolean for showing the plot. Default to TRUE.
#' Currently supports "JHU","USAFacts", "NYTimes". Default to "JHU".
#' @param truth_as_of the plot includes the truth data that would have been 
#' in real time as of the truth_as_of date (not using this parameter when truth data 
#' is from github repo)
#' @param title optional text for the title of the plot. if left as "default",
#' the title will be automatically generated. if "none", no title will be plotted. 
#' @param show.caption logical, if TRUE, caption will be included showing data sources

#' 
#' @return invisible ggplot object
#' 
#' @export
plot_forecast <- function(forecast_data,
                          truth_data = NULL, 
                          model,
                          target_variable,
                          location,
                          intervals,
                          horizon,
                          truth_source = "JHU",
                          plot = TRUE,
                          truth_as_of = NULL, 
                          title = "default", 
                          show.caption = TRUE){
 
  # title format
  if(is.na(title))
    stop("title argument interpretable as a character.")
  
  # optional model and location
  if (length(unique(forecast_data$model)) == 1){
    model = unique(forecast_data$model)
  } else {
    if (!missing(model)){
      if (!(model %in% forecast_data$model)) {
        stop("Error in plot_forecast: model is not available in forecast data.")
      }
    } else {
      stop("Error in plot_forecast: Please select a model to plot.")
    }
  }
  
  if (length(unique(forecast_data$location)) == 1){
    location = unique(forecast_data$location)
  } else {
    if (!missing(location)){
      if (!(location %in% forecast_data$location)) {
        stop("Error in plot_forecast: location is not available in forecast data.")
      }
    } else {
      stop("Error in plot_forecast: Please select a location to plot.")
    }
  }
  
  # validate truth_source
  truth_source <- match.arg(truth_source, 
                            choices = c("JHU","USAFacts", "NYTimes"), 
                            several.ok = FALSE)
  
  # validate location fips code
  all_valid_fips <- covidHubUtils::hub_locations$fips
  
  location <- match.arg(location, 
                        choices = all_valid_fips, 
                        several.ok = FALSE)
  
  if (!location %in% forecast_data$location){
    stop("Error in plot_forecast: location is not available in forecast_data.")
  }
  
  # validate target_variable
  target_variable <- match.arg(target_variable, 
                               choices = c("cum death",
                                           "inc case",
                                           "inc death"), 
                               several.ok = FALSE)
  
  if (!(target_variable %in% forecast_data$target_variable)){
    stop("Error in plot_forecast: Please provide a valid target variable.")
  }
  
  
  # validate truth data if provided
  if (!is.null(truth_data)){
    # check if truth_data has all needed columns
    columns_check <- all(c("model", "target_variable", 
                          "target_end_date", "location","value") 
                        %in% colnames(truth_data))
    if(!columns_check){
      stop("Error in plot_forecast: Please provide columns model, 
           target_variable, target_end_date, location and value in truth_data.")
    } else {
      # check if truth_data has data from specified source
      if (!(paste0("Observed Data (",truth_source,")") %in% truth_data$model)) {
        stop("Error in plot_forecast: Please provide a valid truth_source to plot.")
      }
      # check if truth_data has data from specified location
      if (!(location %in% truth_data$location)){
        stop("Error in plot_forecast: Please provide a valid location to plot.")
      }
      # check if truth_data has specified target variable
      if (!(target_variable %in% truth_data$target_variable)){
        stop("Error in plot_forecast: Please provide a valid target variable.")
      }
    }
  }
  
  
  # if interval is missing, default to a reduced set of intervals available in forecast data
  if (missing(intervals)){
    lower_bounds <- unique(forecast_data[forecast_data$type == 'quantile' &
                                           forecast_data$quantile < 0.5,]$quantile)
    
    # if quantile forecasts are not available, plot point forecasts only
    if ('NA' %in% lower_bounds){
      intervals = NULL
    } else {
      intervals <- lapply(lower_bounds, function(l){
        1-as.numeric(2*l)
      })
      
      # for readability
      if (all(c(.5, .8, .95) %in% intervals)){
        intervals <- c(.5, .8, .95)
      }
    }
  }
  
  # generate quantiles based on given intervals
  quantiles_to_plot <- unlist(lapply(intervals, function(interval){
    c(0.5 - as.numeric(interval)/2,
     0.5 + as.numeric(interval)/2)
  }))
  
  
  # prediction interval shades
  if (!is.null(intervals)){
    
    colourCount = length(quantiles_to_plot)/2+1
    getPalette = colorRampPalette(RColorBrewer::brewer.pal(4, "Blues"))
    blues = getPalette(colourCount)
  } else {
    blues <- RColorBrewer::brewer.pal(n=4, "Blues")
  }
  
  # include truth from remote git hub repo by default
  # not using truth_as_of if we are loading truth from git hub repos
  plot_data = get_plot_forecast_data (forecast_data = forecast_data, 
                                      truth_data = truth_data,
                                      model_to_plot = model,
                                      horizons_to_plot = horizon,
                                      quantiles_to_plot = quantiles_to_plot,
                                      location_to_plot = location,
                                      plot_truth = TRUE,
                                      truth_source = truth_source,
                                      target_variable = target_variable)
 
  # generate caption and full target variable
  if(show.caption){
    if(!is.null(truth_as_of)){
      caption <- paste0("source: ", truth_source," (observed data as of ",
        as.Date(truth_as_of), "), ", model, " (forecasts)")
    } else {
      caption <- paste0("source: ", truth_source," (observed data), ",
        model," (forecasts)")
    }
  } else {
    caption <- NULL
  }
  
  if (target_variable == "cum death"){
    full_target_variable = "Cumulative Deaths"
  } else if (target_variable == "inc case"){
    full_target_variable = "Incident Cases"
  } else if (target_variable == "inc death"){
    full_target_variable = "Incident Deaths"
  } else if (target_variable == "inc hosp"){
    full_target_variable = "Incident Hospitalizations"
  }
  
  # generate title if specified as "default", otherwise leave as is
  if(title == "default") {
    title <- paste0("Weekly COVID-19 ", full_target_variable, " in ", 
      location,": observed and forecasted")
  }
  if(title == "none") {
    title <- NULL
  }

  
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
              mapping = aes(x = target_end_date, 
                            y = point, 
                            color = truth_forecast)) +
    ggplot2::geom_point(data = plot_data %>%
                 dplyr::filter(!is.na(point)),
               mapping = aes(x = target_end_date, 
                             y = point, 
                             color = truth_forecast)) +
    ggplot2::scale_color_manual(name = "Model", 
                                label = unique(plot_data$model),
                                values = c("truth" = "black",
                                           "forecast" = tail(blues,1))) +
    ggplot2::scale_x_date(name = NULL, date_breaks="1 month", date_labels = "%b %d") +
    ggplot2::ylab(full_target_variable) +
    ggplot2::labs(title = title ,
                  caption = caption)
  
  if (plot){
    print(graph)
  }
  
  return (invisible(graph))
}
