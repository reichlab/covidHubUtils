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
#' @return data frame with columns model, forecast_date,location, inc_cum, death_case,
#' type, quantile, value, horizon and target_end_date.

load_forecasts_zoltar <- function(models, forecast_dates, locations, 
                                  types, targets){
  
  # validate models
  all_valid_models <- get_all_model_abbr(source = "zoltar")
  
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
    types = c("point", "quantile")
  }
  
  # set up Zoltar connection
  zoltar_connection <- zoltr::new_connection()
  zoltr::zoltar_authenticate(
    zoltar_connection,
    Sys.getenv("Z_USERNAME"),
    Sys.getenv("Z_PASSWORD"))
  
  # construct Zoltar project url
  the_projects <- zoltr::projects(zoltar_connection)
  project_url <- the_projects[the_projects$name == "COVID-19 Forecasts", "url"]
  
  # validate targets 
  all_valid_targets <- zoltr::targets(zoltar_connection, project_url)$name
  
  if (!missing(targets)){
    targets <- match.arg(targets, choices = all_valid_targets, several.ok = TRUE)
  } else {
    targets = all_valid_targets
  }
  
  
  # if do_zoltar_query throws an error, skip that error and return
  # an empty dataframe
  zoltar_query_skip_error = purrr::possibly(zoltr::do_zoltar_query, 
                                            otherwise = data.frame(),
                                            # not show error messages from zoltar
                                            quiet = TRUE)

  
  # set up parallelization
  future::plan(multiprocess)
  
  # get forecasts that were submitted in the time window
  forecast <- furrr::future_map_dfr(
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
      # cast value to characters for now so that it binds
      if (nrow(f) > 0){
        f <- dplyr::mutate(f, value = as.character(value),
                           quantile = as.character(quantile))
      }
      
      return (f)
    }, .options = furrr_options(seed = TRUE)
  )

  if (nrow(forecast) ==0){
    stop("Error in do_zotar_query: Forecasts are not available in the given time window.\n Please check your parameters.")
  } else {
    forecast <- forecast %>%
      # only include the most recent forecast submitted in the time window
      dplyr::filter(timezero == max(timezero)) %>%
      # change value and quantile back to double
      dplyr::mutate(value = as.double(value),
                    quantile = as.double(quantile)) %>%
      # keep only required columns
      dplyr::select(model, timezero, unit, target, class, quantile, value) %>%
      dplyr::rename(location = unit, forecast_date = timezero,
                    type = class) %>%
      # create horizon and target_end_date columns
      tidyr::separate(target, into=c("n_unit","unit","ahead","inc_cum","death_case"),
                      remove = FALSE) %>% 
      dplyr::rename(horizon = n_unit, target_unit = unit) %>%
      # SLOW...
      #dplyr::mutate(target_end_date = as.Date(unlist(
      #  furrr::future_pmap(list(forecast_date, as.numeric(horizon), target_unit),
      #              calc_target_end_date)))) %>%
      dplyr::mutate(
        target_end_date = as.Date(calc_target_week_end_date(forecast_date, 
                                                            as.numeric(horizon)))
      ) %>%
      dplyr::select(model, forecast_date, location, inc_cum, death_case, horizon,
                    target_unit, target_end_date, type, quantile, value)
  }
  
  return(forecast)
}
