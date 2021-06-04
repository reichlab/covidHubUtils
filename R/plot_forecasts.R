#' Plot forecasts and optional truth for only one selected target variable.
#' Faceted plots for multiple models, locations and forecast dates are 
#' supported with specified facet formula. 
#' 
#' @param forecast_data data frame with truth and forecasts from load_forecasts()
#' @param truth_data optional data frame from one truth source in the format returned 
#' by load_truth(). It needs to have columns model, target_variable, 
#' target_end_date, location and value. 
#' Model column can be "Observed Data (a truth source)".
#' @param hub character, which hub to use. 
#' Default is "US", other option is "ECDC".
#' @param models vector of strings specifying models to plot. 
#' Default to all models in forecast_data.
#' @param target_variable string specifying target type. It should be one of 
#' "cum death", "inc case", "inc death" and "inc hosp". 
#' "cum death" and "inc hosp" are only available in forecasts from US hub now.
#' If there is only one target_variable in forecast_data, this parameter is optional. 
#' @param locations string for fips code, 'US' or abbreviation of European countries. 
#' Default to all locations in forecast_data.
#' @param facet interpretable facet option for ggplot. Function will error 
#' if multiple locations are passed in without location in the facet formula.
#' @param facet_scales argument for scales in ggplot2::facet_wrap. Default to "fixed".
#' @param facet_nrow number of rows for facetting; optional.
#' @param facet_ncol number of columns for facetting; optional.
#' @param forecast_dates date string vectors for forecast dates to plot. 
#' Default to forecast_dates present in the data.
#' @param intervals values indicating which central prediction interval levels 
#' to plot. NULL means only plotting point forecasts.
#' If not provided, it will default to c(.5, .8, .95).
#' When plotting 6 models or more, the plot will be reduced to show .95 interval only.
#' @param horizon forecasts are plotted for the horizon time steps after the 
#' forecast date. Default to all available horizons in forecast data. 
#' @param truth_source character specifying where the truth data will
#' be loaded from if truth_data is not provided. Currently support "JHU",
#' "USAFacts", "NYTimes", "HealthData" and "ECDC".
#' Optional if truth_data is provided. 
#' @param use_median_as_point boolean for using median quantiles as point forecasts in plot. 
#' Default to FALSE.
#' @param plot_truth boolean for showing truth data in plot. Default to FALSE.
#' @param plot boolean for showing the plot. Default to TRUE.
#' Currently supports "JHU","USAFacts", "NYTimes". Default to "JHU".
#' @param fill_by_model boolean for specifying colors in plot.
#' If TRUE, separate colors will be used for each model.
#' If FALSE, only blues will be used for all models. Default to FALSE.
#' @param fill_transparency numeric value used to set transparency of intervals.
#' 0 means fully transparent, 1 means opaque.
#' @param truth_as_of the plot includes the truth data that would have been 
#' in real time as of the truth_as_of date (not using this parameter when truth data 
#' is from github repo)
#' @param title optional text for the title of the plot. If left as "default",
#' the title will be automatically generated. If "none", no title will be plotted. 
#' @param subtitle optional text for the subtitle of the plot. If left as "default",
#' the subtitle will be automatically generated. If "none", no subtitle will be plotted. 
#' @param show_caption logical, if TRUE, caption will be included showing data sources

#' @importFrom grDevices dev.size
#' @return invisible ggplot object
#' 
#' @examples
#' \dontrun{
#' us_forecasts <-load_latest_forecasts(
#'   models =c("COVIDhub-ensemble","COVIDhub-baseline",
#'   "JHU_CSSE-DECOM","CovidAnalytics-DELPHI",
#'   "LANL-GrowthRate", "Microsoft-DeepSTIA"),
#'   last_forecast_date = Sys.Date(),
#'   forecast_date_window_size = 6,
#'   locations = "US",
#'   types = c("point","quantile"),
#'   targets = paste(1:4, "wk ahead inc case"),
#'   source = "zoltar")
#'  
#' plot_forecasts(forecast_data = us_forecasts,
#'   models = c("COVIDhub-ensemble","COVIDhub-baseline","JHU_CSSE-DECOM",
#'   "CovidAnalytics-DELPHI","LANL-GrowthRate", "Microsoft-DeepSTIA"),
#'   target_variable = "inc case", locations = c("US"), intervals = c(.95),
#'   horizon = 4, truth_source = "JHU", plot = TRUE, fill_by_model = TRUE,
#'   facet = ~model, facet_scales = "fixed", title = "default",
#'   show_caption = TRUE)
#' 
#' 
#' ecdc_forecasts <- load_latest_forecasts(models=c("ILM-EKF"),
#'   hub = c("ECDC","US"), last_forecast_date = "2021-03-08",
#'   forecast_date_window_size = 0,
#'   locations = c("GB"),
#'   targets = paste(1:4, "wk ahead inc death"),
#'   source = "zoltar")
#' plot_forecasts(forecast_data = ecdc_forecasts, 
#'                hub  = c("ECDC","US"), 
#'                locations ="GB", 
#'                truth_source = "JHU")
#' }
#' 
#' @export
plot_forecasts <- function(forecast_data,
                           truth_data = NULL,
                           hub = c("US", "ECDC"),
                           models,
                           target_variable,
                           locations,
                           facet = NULL,
                           facet_scales = "fixed",
                           facet_nrow = NULL,
                           facet_ncol = NULL,
                           forecast_dates,
                           intervals,
                           horizon,
                           truth_source,
                           use_median_as_point = FALSE,
                           plot_truth = TRUE,
                           plot = TRUE,
                           fill_by_model = FALSE,
                           fill_transparency = 1.0,
                           truth_as_of = NULL, 
                           title = "default", 
                           subtitle = "default",
                           show_caption = TRUE){
 
  # title format
  if(is.na(title))
    stop("Error in plot_forecasts: title argument interpretable as a character.")
  
  # subtitle format
  if(is.na(subtitle))
    stop("Error in plot_forecasts: subtitle argument interpretable as a character.")
  
  # optional models parameter. Default to all models in forecast_data
  if (!missing(models)){
    if (!all(models %in% forecast_data$model)) {
      stop("Error in plot_forecasts: Not all models are available in forecast data.")
    }
  } else {
    models <- unique(forecast_data$model)
  }
  
  # optional locations parameter. Default to all locations in forecast_data
  if (missing(locations)){
    locations <- unique(forecast_data$location)
  }
  
  # get lists of valid parameter choices based on `hub`
  if (hub[1] == "US") {
    valid_location_codes <- covidHubUtils::hub_locations$fips
    valid_target_variables <-  c("cum death","inc case",
                                 "inc death","inc hosp")
    valid_truth_sources <- c("JHU","USAFacts", "NYTimes", "HealthData")
  } else if (hub[1] == "ECDC") {
    valid_location_codes <- covidHubUtils::hub_locations_ecdc$location
    valid_target_variables <- c("inc case","inc death")
    valid_truth_sources <- c("JHU", "jhu", "ECDC", "ecdc")
  }
  
  # validate locations
  if (!all(locations %in% forecast_data$location)){
    stop("Error in plot_forecasts: Not all locations are available in forecast_data.")
  }

  locations <- match.arg(locations, choices = valid_location_codes, several.ok = TRUE)
  
  if (length(locations) > 1) {
    if(is.null(facet)) {
      stop("Error in plot_forecasts: Passed in multiple locations without a facet command")
    }
  }

  # validate target variable 
  if (missing(target_variable)) {
    # optional target_variable parameter when there's only one 
    # target variable in forecast_data
    if (length(unique(forecast_data$target_variable)) == 1) {
      target_variable <- unique(forecast_data$target_variable)
    } else {
      stop("Error in plot_forecasts: Target variable unspecified and more than one target_variable in data.")
    }
  } else {
    if (!(target_variable %in% forecast_data$target_variable)){
      stop("Error in plot_forecasts: Please provide a valid target variable.")
    }
    
    target_variable <- match.arg(target_variable, 
                                 choices = valid_target_variables,
                                 several.ok = FALSE)
  }
  
  # validate truth data if provided
  if (!is.null(truth_data)){
    # check if truth_data has all needed columns
    columns_check <- all(c("model", "target_variable", 
                          "target_end_date", "location","value") 
                        %in% colnames(truth_data))
    if(!columns_check){
      stop("Error in plot_forecasts: Please provide columns model, 
           target_variable, target_end_date, location and value in truth_data.")
    } else {
      # check if all fips codes in location column are valid
      if (!all(truth_data$location %in% valid_location_codes)){
        stop("Error in plot_forecasts: Please make sure all fips codes in location column are valid.")
      }
      # check if truth_data has data from specified location
      if (!all(locations %in% truth_data$location)){
        stop("Error in plot_forecasts: Please provide a valid location to plot.")
      }
      # check if truth_data has specified target variable
      if (!(target_variable %in% truth_data$target_variable)){
        stop("Error in plot_forecasts: Please provide a valid target variable.")
      }
    }
  } else {
    # validate truth_source 
    truth_source <- match.arg(truth_source, 
                              choices = valid_truth_sources, 
                              several.ok = FALSE)
    
    if(target_variable == "inc hosp"){
      if (truth_source != "HealthData"){
        stop("Error in plot_forecasts: Incident hopsitalization truth data is only available from HealthData.gov now.")
      }
    } else {
      if (truth_source == "HealthData"){
        stop("Error in plot_forecasts: This function does not support selected target_variable from HealthData.")
      }
    }
  }
  
  if (show_caption){
    if (missing(truth_source)){
      stop("Error in plot_forecasts: Please provide truth_source for caption.")
    }
  }
  
  # validate forecast_dates
  if (missing(forecast_dates)){
    forecast_dates <- unique(forecast_data$forecast_date)
  } else {
    forecast_dates <- as.Date(forecast_dates)
    if (!all(forecast_dates %in% forecast_data$forecast_date)){
      stop ("Error in plot_forecasts: Not all forecast_dates are available in forecast data.")
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
      # only plot .95 interval if more than 5 models are selected
      if (length(unique(models)) > 5){
        intervals <- c(.95)
      } else {
        # generate intervals from lower bounds
        intervals <- lapply(lower_bounds, function(l){
          1-as.numeric(2*l)
        })
        
        # for readability
        if (all(c(.5, .8, .95) %in% intervals)){
          intervals <- c(.5, .8, .95)
        }
      }
    }
  }
  
  if (!is.null(intervals) & length(unique(models)) > 5){
    intervals <- c(.95)
  }
  
  # generate quantiles based on given intervals
  quantiles_to_plot <- unlist(lapply(intervals, function(interval){
    c(0.5 - as.numeric(interval)/2,
     0.5 + as.numeric(interval)/2)
  }))
  
  if (use_median_as_point){
    if (0.5 %in% forecast_data$quantile) {
      # plot medians instead
      quantiles_to_plot <- append(quantiles_to_plot, 0.5)
    } else {
      stop("Error in plot_forecasts: Median quantiles are not available in forecast_data.")
    }
  }
  
  # set colors
  if (fill_by_model){
    if (length(unique(models)) <= 5){
      color_families <- c("Blues", "Oranges", "Greens", "Purples", "Reds")
      if (use_median_as_point){
        colourCount <- (length(quantiles_to_plot) -1)/ 2 + 1
      } else {
        colourCount <- length(quantiles_to_plot) / 2 + 1
      }
      model_colors <- purrr::map(
        color_families[1:length(unique(models))],
        function(color_family){
          getPalette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(4, color_family))
          if (colourCount < 4) {
            # choose the first few from a larger set of colors, to keep higher saturation
            getPalette(4) %>% tail(colourCount)
          } else {
            getPalette(colourCount)
          }
        })
      
      ribbon_colors <- RColorBrewer::brewer.pal(max(4, colourCount), "Greys")
      ribbon_colors <- ribbon_colors[seq_len(length(ribbon_colors) - 1)] %>%
        tail(colourCount)
      forecast_colors <- unlist(lapply(model_colors, tail, n = 1))
      interval_colors <- unlist(lapply(model_colors, head, n = colourCount-1))
    } else {
      # interpolate color pallets to more than 5 colors
      if (use_median_as_point) {
        colourCount <- (length(quantiles_to_plot)-1)/2
      } else {
        colourCount <- length(quantiles_to_plot)/2
      }
        
      modelCount <- length(unique(models))
      # use 8 instead of 9 to avoid black and grey shades
      getPalette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set1"))
      model_colors <- getPalette(modelCount)
      
      ribbon_colors <- RColorBrewer::brewer.pal(4, "Greys")[1:3]
      forecast_colors <- unlist(lapply(model_colors, tail, n = 1))
      # create 95% PI color
      interval_colors <- unlist(lapply(model_colors, function(color){
        colorspace::lighten(color, 0.4)}
        ))
    }
  } else {
    # only use blue
    if (use_median_as_point){
      colourCount = max((length(quantiles_to_plot)-1)/2+1, 2)
    } else {
      colourCount = max(length(quantiles_to_plot)/2+1, 2)
    }
    getPalette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(4, "Blues"))
    blues = getPalette(colourCount)
    forecast_colors <- rep(tail(blues,1), length(unique(models)))
    interval_colors <- rep(blues[1:(length(blues)-1)],
                           length(unique(models)))
    ribbon_colors <- blues[1:(length(blues)-1)]
  }
  
  if (!is.null(truth_as_of)){
    warning("Warning in plot_forecasts: truth_as_of is not used to load versioned truth data.
            Will be available soon.")
  }
  
  # include truth from remote git hub repo by default
  # not using truth_as_of if we are loading truth from git hub repos
  plot_data = get_plot_forecast_data (forecast_data = forecast_data, 
                                      truth_data = truth_data,
                                      models_to_plot = models,
                                      forecast_dates_to_plot = as.Date(forecast_dates),
                                      horizons_to_plot = horizon,
                                      quantiles_to_plot = quantiles_to_plot,
                                      locations_to_plot = locations,
                                      plot_truth = plot_truth,
                                      truth_source = truth_source,
                                      target_variable_to_plot = target_variable,
                                      hub = hub)
 

  # generate caption and full target variable
  if(show_caption){
    if(!is.null(truth_as_of)){
      caption <- paste0("source: ", truth_source," (observed data as of ",
                        as.Date(truth_as_of), "), ", 
                        paste(models, collapse = ', '), " (forecasts)")
      
      caption <- paste(strwrap(caption, grDevices::dev.size("px")[1]), collapse="\n")
    } else {
      caption <- paste0("source: ", truth_source," (observed data), ",
                        paste(models, collapse = ', ')," (forecasts)")
      
      caption <- paste(strwrap(caption, grDevices::dev.size("px")[1]), collapse="\n")
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
  
  # split plot data
  plot_data_forecast <- plot_data %>%
    dplyr::filter(truth_forecast == "forecast")
  
  plot_data_truth <- plot_data %>%
    dplyr::filter(!is.na(point),truth_forecast == "truth") %>%
    dplyr::rename(truth_model = model) %>%
    dplyr::select(-forecast_date)
  
  # generate title if specified as "default", otherwise leave as is
  if(title == "default") {
    if (target_variable == "inc hosp"){
      title <- paste0("Daily COVID-19 ", full_target_variable, 
                       ": observed and forecasted")
    } else {
      title <- paste0( "Weekly COVID-19 ", full_target_variable, 
                       ": observed and forecasted")
    }
  }
  
  if(title == "none") {
    title <- NULL
  }
  
  # generate subtitle if specified as "default", otherwise leave as is
  if (subtitle == "default"){
    subtitle <- paste0("Selected location(s): ", 
                      paste(unique(plot_data_forecast$location), collapse = ', '),
                      "\nSelected forecast date(s): ",
                      paste(unique(plot_data_forecast$forecast_date), collapse = ', ')
                      )
  }
  
  if (subtitle == "none"){
    subtitle <- NULL
  }
  
  

  # generate plot
  graph <- ggplot2::ggplot(data = plot_data_forecast, ggplot2::aes(x= target_end_date))
  
  # plot selected prediction intervals
  if (!is.null(intervals)){
    graph <- graph  +
      ggplot2::geom_ribbon(data = plot_data_forecast %>%
                  dplyr::filter(type == "quantile"),
                mapping = ggplot2::aes(ymin=lower, 
                                       ymax=upper,
                                       group = interaction(`Prediction Interval`, model, 
                                                           location, forecast_date),
                                       fill = interaction(`Prediction Interval`, model)),
                alpha = fill_transparency, show.legend=FALSE) +
      ggplot2::scale_fill_manual(name = "Prediction Interval", values = interval_colors) +
      # create a transparent layer with grey colors to get prediction interval legend
      ggnewscale::new_scale_fill() +
      ggplot2::geom_ribbon(data = plot_data_forecast %>%
                             dplyr::filter(type == "quantile"),
                           mapping = ggplot2::aes(ymin=lower, 
                                                  ymax=upper, 
                                                  fill = `Prediction Interval`), 
                           alpha = 0) +
      ggplot2::scale_fill_manual(name = "Prediction Interval", values = ribbon_colors) +
      # create a transparent layer for models legend when point forecasts are not plotted
      # models legend will be covered if point forecasts are plotted
      ggplot2::geom_line(data = plot_data_forecast %>%
                           dplyr::filter(type == "quantile"),
                         mapping = ggplot2::aes(y = upper, colour = model), alpha = 0) +
      ggplot2::scale_color_manual(name = "Model", values = forecast_colors) +
      # reset alpha in legend fill
      ggplot2::guides(
        fill = ggplot2::guide_legend(override.aes = list(alpha = 1)))
  }
  
  # plot point forecasts and truth 
  graph <- graph +
    #forecast
    ggplot2::geom_line(data = plot_data_forecast %>%
                dplyr::filter(!is.na(point)),
              mapping = ggplot2::aes(x = target_end_date, 
                                     y = point, 
                                     group = interaction(model, location, forecast_date),
                                     color = model)) +
    ggplot2::geom_point(data = plot_data_forecast %>%
                 dplyr::filter(!is.na(point)),
               mapping = ggplot2::aes(x = target_end_date, 
                                      y = point, 
                                      color = model)) +
    ggplot2::scale_color_manual(name = "Model", 
                                values = forecast_colors) +
    #truth
    ggnewscale::new_scale_color() +
    ggplot2::geom_line(data = plot_data_truth %>%
                         dplyr::filter(!is.na(point)),
                       mapping = ggplot2::aes(x = target_end_date, 
                                              y = point, 
                                              color = truth_model)) +
    ggplot2::geom_point(data = plot_data_truth %>%
                          dplyr::filter(!is.na(point)),
                        mapping = ggplot2::aes(x = target_end_date, 
                                               y = point, 
                                               color = truth_model)) +
    ggplot2::scale_color_manual(name = "Truth", values = "black")
  
  # add facets
  if(!is.null(facet)){
    graph <- graph + 
      ggplot2::facet_wrap(facets = facet, scales = facet_scales,
                          nrow = facet_nrow, ncol = facet_ncol,
                          labeller = ggplot2::label_wrap_gen(multi_line=FALSE))
  
  }
  
  # add labels, title, subtitle and caption
  graph <- graph + 
    ggplot2::scale_x_date(name = NULL, date_breaks="1 month", date_labels = "%b %d") +
    ggplot2::ylab(full_target_variable) +
    ggplot2::labs(title = title,
                  subtitle = subtitle,
                  caption = caption)
  
  if (plot){
    print(graph)
  }
  
  return (invisible(graph))
}

#' Plot forecasts and optional truth for only one selected target variable.
#' Faceted plots for multiple models, locations and forecast dates are 
#' supported with specified facet formula. 
#' 
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' Please use [plot_forecasts()] instead.
#' 
#' @inheritParams plot_forecasts
#' @importFrom grDevices dev.size
#' @return invisible ggplot object
#' 
#' @export
#'
plot_forecast <- function(forecast_data,
                          truth_data = NULL, 
                          hub = hub,
                          models,
                          target_variable,
                          locations,
                          facet = NULL,
                          facet_scales = "fixed",
                          facet_nrow = NULL,
                          facet_ncol = NULL,
                          forecast_dates,
                          intervals,
                          horizon,
                          truth_source,
                          use_median_as_point = FALSE,
                          plot_truth = TRUE,
                          plot = TRUE,
                          fill_by_model = FALSE,
                          fill_transparency = 1.0,
                          truth_as_of = NULL, 
                          title = "default", 
                          subtitle = "default",
                          show_caption = TRUE){
  lifecycle::deprecate_warn("0.1.5", "plot_forecast()", "plot_forecasts()")
  plot_forecasts(forecast_data = forecast_data,
                 truth_data = truth_data, 
                 hub = hub,
                 models = models,
                 target_variable = target_variable,
                 locations = locations,
                 facet = facet,
                 facet_scales = facet_scales,
                 facet_nrow = facet_nrow,
                 facet_ncol = facet_ncol,
                 forecast_dates = forecast_dates,
                 intervals = intervals,
                 horizon = horizon,
                 truth_source = truth_source,
                 use_median_as_point = use_median_as_point,
                 plot_truth = plot_truth,
                 plot = plot,
                 fill_by_model = fill_by_model,
                 fill_transparency = fill_transparency,
                 truth_as_of = truth_as_of, 
                 title = title, 
                 subtitle = subtitle,
                 show_caption = show_caption)  
}
