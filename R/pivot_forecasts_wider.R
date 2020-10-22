#' Return a data frame of forecasts with predictive quantiles in columns.
#'
#' @param data forecasts data frame from load_forecast(). It has columns model, 
#' forecast_date,location, target, type, quantile, value, horizon and 
#' target_end_date.
#' @param quantiles vector of quantiles to return for plotting
#' Defaults to c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975). 
#' Note that point forecasts and median quantiles do not necessarily align. 
#' Throws an error if quantile levels are not in the forecast data.
#'
#' @return data frame with columns model, forecast_date, location, inc_cum, death_case, 
#' horizon, type, point, target_end_date, point_forecast_type, Prediction Interval,
#' upper, lower.
#' 
#' @export
#' 
pivot_forecasts_wider <- function(data, 
                                  quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)){
  
  # validate quantiles
  quantiles = as.character(quantiles)
  
  if(!all(quantiles %in% data$quantile)){
    stop("Error in pivot_forecast_wider: Not all quantile levels are available in the forecast data.")
  }
  
  # filter to included specified quantiles and point forecasts
  data <- data %>%
    dplyr:: filter(as.character(quantile) %in% quantiles | type == "point") 
  
  # get point forecasts: point forecasts and `0.5` quantile
  points <- data %>%
    dplyr::filter(quantile == 0.5 | type == "point") %>%
    dplyr::mutate(point_type = ifelse(type !="point", "median quantile","point forecast"),
                  type = "point") %>%
    dplyr::rename(point = value) %>%
    dplyr::select(-quantile)
  
  # get quantile forecasts and generate corresponding prediction interval
  quantiles <- data %>%
    dplyr::filter(quantile != 0.5 & type != "point") %>%
   
    dplyr::mutate(endpoint_type = ifelse(quantile < 0.5, 'lower', 'upper'),
                  alpha = ifelse(endpoint_type == 'lower',
                                 format(2*quantile, digits=3, nsmall=3),
                                 format(2*(1-quantile), digits=3, nsmall=3)),
                  `Prediction Interval` = fct_rev(paste0((1-as.numeric(alpha))*100, "%"))
    ) %>%
    #dplyr::filter(alpha != "1.000") %>%
    dplyr::select(-quantile, -alpha) %>%
    tidyr::pivot_wider(names_from='endpoint_type', values_from='value')
  
  wider_data <- dplyr::bind_rows(points, quantiles)
  
  return (wider_data)
  
}
