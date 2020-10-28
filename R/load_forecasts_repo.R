#' Load forecasts from reichlab/covid19-forecast-hub repo.
#' 
#' @param file_path path to local clone of the reichlab/covid19-forecast-hub
#' repository
#' @param models character vector of model abbreviations.
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
#'
#' @export
load_forecasts_repo <- function(file_path, models, forecast_dates, locations, types, targets){
  
  # validate models
  all_valid_models <- covidHubUtils:::get_all_model_abbr(source = "remote_hub_repo")
  
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
  
  # validate targets
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
  
  
  forecasts <- purrr::map_dfr(
    models,
    function(model) {
      if (substr(file_path, nchar(file_path), nchar(file_path)) == "/") {
        file_path <- substr(file_path, 1, nchar(file_path) - 1)
      }

      results_path <- file.path(
        file_path,
        paste0(model, "/", forecast_dates, "-", model, ".csv"))
      results_path <- results_path[file.exists(results_path)]
      results_path <- tail(results_path, 1)
      
      if (length(results_path) == 0) {
        return(NULL)
      }
      
      readr::read_csv(results_path,
                      col_types = cols(
                        forecast_date = col_date(format = ""),
                        target = col_character(),
                        target_end_date = col_date(format = ""),
                        location = col_character(),
                        type = col_character(),
                        quantile = col_double(),
                        value = col_double()
                      )) %>%
        dplyr::filter(
          tolower(type) %in% types,
          location %in% locations,
          tolower(target) %in% targets) %>%
        dplyr::transmute(
          model = model,
          forecast_date = forecast_date,
          location = location,
          target = tolower(target),
          type = type,
          quantile = quantile,
          value = value
        )
    }
  ) %>%
    # only include the most recent forecast submitted in the time window
    dplyr::filter(forecast_date == max(forecast_date)) %>%
    tidyr::separate(target, into=c("n_unit","unit","ahead","inc_cum","death_case"),
                    remove = FALSE) %>% 
    dplyr::rename(horizon = n_unit, target_unit = unit) %>%
    dplyr::mutate(target_end_date = as.Date(unlist(
      purrr::pmap(list(forecast_date, as.numeric(horizon), target_unit),
                  covidHubUtils::calc_target_end_date)))) %>%
    dplyr::select(model, forecast_date, location, inc_cum, death_case, horizon,
                  target_unit, target_end_date, type, quantile, value)
  
  return(forecasts)
  
}