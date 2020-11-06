#' Return a data frame of forecasts with predictive quantiles in columns.
#'
#' @param forecast_data forecasts data frame from load_forecast(). It has columns model, 
#' forecast_date,location, target, type, quantile, value, horizon and 
#' target_end_date.
#' @param quantiles vector of quantiles to return for plotting
#' Defaults to all available quantiles in forecast data. 
#' Note that point forecasts and median quantiles do not necessarily align. 
#' Throws an error if quantile levels are not in the forecast data.
#'
#' @return data frame with columns model, forecast_date, location,
#' horizon, temporal_resolution, target_variable, target_end_date, type, point, 
#' point_type, Prediction Interval, upper, lower.
#' 
#' @export
#' 
pivot_forecasts_wider <- function(forecast_data, quantiles){
  
  # validate quantiles
  if (!missing(quantiles)){
    quantiles <- as.character(quantiles)
    if(!all(quantiles %in% forecast_data$quantile)){
      stop("Error in pivot_forecast_wider: Not all quantile levels are available in the forecast data.")
    }
  } else (
    quantiles <- as.character(sort(unique(forecast_data$quantile)))
  )
  
  # filter to included specified quantiles and point forecasts
  forecast_data <- forecast_data %>%
    dplyr:: filter(as.character(quantile) %in% quantiles | type == "point") 
  
  # get point forecasts: point forecasts and `0.5` quantile
  points <- forecast_data %>%
    dplyr::filter(quantile == 0.5 | type == "point") %>%
    dplyr::mutate(point_type = ifelse(type !="point", "median quantile","point forecast"),
                  type = "point") %>%
    dplyr::rename(point = value) %>%
    dplyr::select(-quantile)
  
  # get quantile forecasts and generate corresponding prediction interval
  quantiles <- forecast_data %>%
    dplyr::filter(quantile != 0.5 & type != "point") %>%
   
    dplyr::mutate(endpoint_type = ifelse(quantile < 0.5, 'lower', 'upper'),
                  alpha = ifelse(endpoint_type == 'lower',
                                 format(2*quantile, digits=3, nsmall=3),
                                 format(2*(1-quantile), digits=3, nsmall=3)),
                  `Prediction Interval` = forcats::fct_rev(paste0((1-as.numeric(alpha))*100, "%"))
    ) %>%
    dplyr::select(-quantile, -alpha) %>%
    tidyr::pivot_wider(names_from='endpoint_type', values_from='value')
  
  wider_data <- dplyr::bind_rows(points, quantiles)
  
  return (wider_data)
  
}
