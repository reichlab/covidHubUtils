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
  
  ## construct Zoltar query
  the_projects <- projects(zoltar_connection)
  project_url <- the_projects[the_projects$name == "COVID-19 Forecasts", "url"]
  
  # obtain the quantile forecasts for required quantiles,
  # and the filter to last submission from each model for each week
  forecasts <-
    # get forecasts from zoltar
    zoltr::do_zoltar_query(
      zoltar_connection = zoltar_connection,
      project_url = project_url,
      is_forecast_query = TRUE,
      units= locations, 
      timezeros = forecast_dates,
      models = models,
      targets = targets,
      types = types, 
      verbose = TRUE
    ) %>%
    # keep only required columns and required quantiles
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
  
  return(forecasts)
}
