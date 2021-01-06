#' Load the most recent forecasts submitted in a time window from zoltar.
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
#' @return data frame with columns model, forecast_date, location, horizon,
#' temporal_resolution, target_variable, target_end_date, type, quantile, value,
#' location_name, population, geo_type, geo_value, abbreviation
#' 

load_latest_forecasts_zoltar <- function(models, forecast_dates, locations, 
                                  types, targets){
  
  # validate models
  all_valid_models <- get_all_models(source = "zoltar")
  
  if (!missing(models)){
    models <- match.arg(models, choices = all_valid_models, several.ok = TRUE)
  } else {
    models <- all_valid_models
  }
  
  # validate locations
  all_valid_fips <- covidHubUtils::hub_locations$fips
  
  if (!missing(locations)){
    locations <- match.arg(locations, choices = all_valid_fips, several.ok = TRUE)
  } else{
    locations <- all_valid_fips
  }
  
  # validate types
  if (!missing(types)){
    types <- match.arg(types, choices = c("point", "quantile"), several.ok = TRUE)
  } else {
    types <- c("point", "quantile")
  }
  
  # set up Zoltar connection
  zoltar_connection <- zoltr::new_connection()
  if(Sys.getenv("Z_USERNAME") == "" | Sys.getenv("Z_PASSWORD") == "") {
    zoltr::zoltar_authenticate(zoltar_connection, "zoltar_demo","Dq65&aP0nIlG")
  } else {
    zoltr::zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"),Sys.getenv("Z_PASSWORD"))
  }
  
  # construct Zoltar project url
  the_projects <- zoltr::projects(zoltar_connection)
  project_url <- the_projects[the_projects$name == "COVID-19 Forecasts", "url"]
  
  # validate targets 
  all_valid_targets <- zoltr::targets(zoltar_connection, project_url)$name
  
  if (!missing(targets)){
    targets <- match.arg(targets, choices = all_valid_targets, several.ok = TRUE)
  } else {
    targets <- all_valid_targets
  }
  
  message("Large queries that span many combinations of forecast dates, models, locations, 
  and targets can take a long time to process. To reduce run-time of queries, 
  we encourage users to download a local copy of the COVID-19 Forecast Hub repository 
  so queries can be run locally: https://github.com/reichlab/covid19-forecast-hub/")
  
  # get all valid timezeros in project
  all_valid_timezeros <- zoltr::timezeros(zoltar_connection = zoltar_connection,
                                          project_url = project_url)$timezero_date
  
  if (!missing(forecast_dates)){
    # take intersection of forecast_dates and all_valid_timezeros
    valid_forecast_dates <- intersect(as.character(forecast_dates), 
                                      as.character(all_valid_timezeros))
    if (length(valid_forecast_dates) == 0) {
      stop("Error in load_forecasts: All forecast_dates are invalid.")
    }
  } else {
    valid_forecast_dates <- all_valid_timezeros
  }
  
  forecast <- zoltr::do_zoltar_query(zoltar_connection = zoltar_connection,
                                      project_url = project_url,
                                      query_type = "forecasts",
                                      units = locations, 
                                      timezeros = valid_forecast_dates,
                                      models = models,
                                      targets = targets,
                                      types = types,
                                      verbose = FALSE)

  if (nrow(forecast) == 0){
    warning("Warning in do_zotar_query: Forecasts are not available in the given time window.\n Please check your parameters.")
    # convert value column to double and select columns
    forecast <- forecast %>%
      dplyr::mutate(value = as.double(value)) %>%
      tidyr::separate(target, into=c("horizon","temporal_resolution","ahead",
                                     "target_variable"),
                      remove = FALSE, extra = "merge") %>%
      dplyr::rename(location = unit, forecast_date = timezero, type = class) %>%
      dplyr::select(model, forecast_date, location, horizon, temporal_resolution,
                    target_variable, type, quantile, value)
  } else {
    forecast <- forecast %>%
      # only include the most recent forecast submitted in the time window
      dplyr::group_by(model) %>%
      dplyr::filter(timezero == max(timezero)) %>%
      dplyr::ungroup() %>%
      # change value back to double
      dplyr::mutate(value = as.double(value),
                    timezero = as.Date(timezero)) %>%
      # keep only required columns
      dplyr::select(model, timezero, unit, target, class, quantile, value) %>%
      dplyr::rename(location = unit, forecast_date = timezero,
                    type = class) %>%
      # create horizon and target_end_date columns
      tidyr::separate(target, into=c("horizon","temporal_resolution","ahead","target_variable"),
                      remove = FALSE, extra = "merge") %>%
      dplyr::mutate(target_end_date = as.Date(
        calc_target_end_date(forecast_date, as.numeric(horizon), temporal_resolution)
        )) %>%
      dplyr::select(model, forecast_date, location, horizon, temporal_resolution,
                    target_variable, target_end_date, type, quantile, value) %>%
      dplyr::left_join(covidHubUtils::hub_locations, by=c("location" = "fips"))
  }
  
  return(forecast)
}
