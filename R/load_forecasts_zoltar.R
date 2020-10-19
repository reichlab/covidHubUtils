#' Load forecasts from zoltar
#' 
#' @param models Character vector of model abbreviations.
#' If missing, forecasts for all models that submitted forecasts 
#' meeting the other criteria are returned.
#' @param forecast_dates date vector to load the most recent forecast from
#' @param locations list of valid fips code. Defaults to all locations with 
#' available forecasts.
#' @param types character vector specifying type of forecasts to load: “quantile” 
#' or “point”. Defaults to c(“quantile”, “point”)
#' @param targets character vector of targets to retrieve, for example
#' c('1 wk ahead cum death', '2 wk ahead cum death'). Defaults to all targets.
#' 
#' @return data frame with columns model, forecast_date,location, target, 
#' type, quantile, value, horizon and target_end_date.

load_forecasts_zoltar <- function(models, forecast_dates, locations, 
                                  types, targets){
  
  # Set up Zoltar
  zoltar_connection <- zoltr::new_connection()
  zoltr::zoltar_authenticate(
    zoltar_connection,
    Sys.getenv("Z_USERNAME"),
    Sys.getenv("Z_PASSWORD"))
  
  # Construct Zoltar project url
  the_projects <- projects(zoltar_connection)
  project_url <- the_projects[the_projects$name == "COVID-19 Forecasts", "url"]
  
  # If do_zoltar_query throws an error, skip that error and return
  # an empty dataframe
  zoltar_query_skip_error = purrr::possibly(zoltr::do_zoltar_query, 
                                            otherwise = data.frame())

  # Get forecasts that were submitted in the time window
  forecast <- purrr::map_dfr(
    forecast_dates,
    function (forecast_date) {
      f <- zoltar_query_skip_error(zoltar_connection = zoltar_connection,
                              project_url = project_url,
                              is_forecast_query = TRUE,
                              units= locations, 
                              timezeros = forecast_date,
                              models = models,
                              targets = targets,
                              types = types, 
                              verbose = TRUE)
      # Cast value to characters for now so that it binds
      if (nrow(f) > 0){
        f <- dplyr::mutate(f, value = as.character(value),
                           quantile = as.character(quantile))
      }
      
      return (f)
    }
  ) %>%
    dplyr::mutate(value = as.double(value),
                  quantile = as.double(quantile))

  if (nrow(forecast) ==0){
    stop("Forecasts are not available in the given time window.")
  } else {
    forecast <- forecast %>%
      # Keep only required columns
      dplyr::select(model, timezero, unit, target, class,quantile, value) %>%
      dplyr::rename(location = unit, forecast_date = timezero,
                    type = class) %>%
      # create horizon and target_end_date columns
      tidyr::separate(target, into=c("n_unit","unit","ahead","inc_cum","death_case"),
                      remove = FALSE) %>% 
      dplyr::rename(horizon = n_unit) %>%
      dplyr::mutate(
        target_end_date = as.Date(covidHubUtils::calc_target_week_end_date(
          forecast_date, as.numeric(horizon)))) %>%
      dplyr::select(model, forecast_date, location, target,horizon,
                    type, quantile, value, target_end_date)
  }
  
  return(forecast)
}
