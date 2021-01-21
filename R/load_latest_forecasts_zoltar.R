#' Load the most recent forecasts submitted in a time window from zoltar.
#' 
#' The function will throw a warning and return an empty data frame when 
#' no forecasts are submitted on any dates in forecast_dates for selected models, 
#' locations, types and target.
#' 
#' @param models Character vector of model abbreviations.
#' Default to NULL so that forecasts for all models that submitted forecasts 
#' meeting the other criteria are returned.
#' @param forecast_dates date vector to load the most recent forecast from.
#' Default to NULL which stands for all valid forecast dates in Zoltar.
#' The function will throw an error if all dates in this parameter are invalid forecast dates in Zoltar.
#' @param locations list of valid fips code. Default to NULL which stands for
#' all locations with available forecasts in Zoltar.
#' @param types character vector specifying type of forecasts to load: “quantile” 
#' or “point”. Default to NULL which stands for all valid forecast types in Zoltar.
#' @param targets character vector of targets to retrieve, for example
#' c('1 wk ahead cum death', '2 wk ahead cum death'). 
#' Default to NULL which stands for all valid targets in Zoltar.
#' 
#' @return data frame with columns model, forecast_date, location, horizon,
#' temporal_resolution, target_variable, target_end_date, type, quantile, value,
#' location_name, population, geo_type, geo_value, abbreviation
#' 

load_latest_forecasts_zoltar <- function(models = NULL,
                                         forecast_dates = NULL,
                                         locations = NULL, 
                                         types = NULL,
                                         targets = NULL){
  
  forecast <- load_forecasts(models = models,
                             forecast_dates = forecast_dates,
                             locations = locations,
                             types = types,
                             targets = targets)
  
  if(nrow(forecast) != 0) {
    # filter to get the latest forecast for each model
    forecast <- forecast %>% 
      dplyr::group_by(model) %>%
      dplyr::filter(forecast_date == max(forecast_date)) %>%
      dplyr::ungroup()
  }
  
  return(forecast)
}
