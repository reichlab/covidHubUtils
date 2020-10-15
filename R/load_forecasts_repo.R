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

load_forecasts_repo <- function(file_path, models, forecast_dates, locations, types, targets){
  
  forecasts <- purrr::map_dfr(
    models,
    function(model) {
      results_path <- paste0(file_path, model, '/',
                             forecast_date, '-', model, '.csv')
      results_path <- results_path[file.exists(results_path)]
      results_path <- tail(results_path, 1)
      
      if(length(results_path) == 0) {
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
    tidyr::separate(target, into=c("n_unit","unit","ahead","inc_cum","death_case"),
                    remove = FALSE) %>% 
    dplyr::rename(horizon = n_unit) %>%
    dplyr::mutate(
      target_end_date = as.Date(covidHubUtils::calc_target_week_end_date(
        forecast_date, as.numeric(horizon)))
    ) %>%
    dplyr::select(model, forecast_date, location, target, horizon,
                  type, quantile, value, target_end_date)
  
  return(forecasts)
  
}