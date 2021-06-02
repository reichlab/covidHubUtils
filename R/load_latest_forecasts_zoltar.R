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
#' Default to all valid forecast dates in Zoltar.
#' The function will throw an error if all dates in this parameter are invalid forecast dates in Zoltar.
#' @param locations list of valid location codes. Default to all locations with available forecasts in Zoltar.
#' @param types character vector specifying type of forecasts to load: “quantile” 
#' or “point”. Default to all valid forecast types in Zoltar.
#' @param targets character vector of targets to retrieve, for example
#' c('1 wk ahead cum death', '2 wk ahead cum death'). 
#' Default to all valid targets in Zoltar.
#' @param as_of character for date time to load forecasts submitted as of this time. 
#' It could use the format of one of the three examples: 
#' "2021-01-01", "2020-01-01 01:01:01" and "2020-01-01 01:01:01 UTC".
#' If you would like to set a timezone, it has to be UTC now. 
#' If not, helper function will append the default timezone to your input based on hub parameter.
#' Default to NULL to load the latest version.
#' @param verbose a boolean for printing messages on zoltar job status. Default to TRUE.
#' @param hub character vector, where the first element indicates the hub
#' from which to load forecasts. Possible options are "US" and "ECDC"
#' 
#' @return data frame with columns model, forecast_date, location, horizon,
#' temporal_resolution, target_variable, target_end_date, type, quantile, value,
#' location_name, population, geo_type, geo_value, abbreviation
#' 
  
load_latest_forecasts_zoltar <- function(models = NULL,
                                         forecast_dates = NULL,
                                         locations = NULL, 
                                         types = NULL,
                                         targets = NULL,
                                         as_of = NULL,
                                         hub = c("US", "ECDC"),
                                         verbose = TRUE){
  
  forecast <- load_forecasts_zoltar(models = models,
                                    forecast_dates = forecast_dates,
                                    locations = locations,
                                    types = types,
                                    targets = targets,
                                    as_of = as_of,
                                    verbose = verbose,
                                    hub = hub)
  
  if(nrow(forecast) != 0) {
    # filter to get the latest forecast for each model
    forecast <- forecast %>% 
      dplyr::group_by(model) %>%
      dplyr::filter(forecast_date == max(forecast_date)) %>%
      dplyr::ungroup()
  }
  
  return(forecast)
}
