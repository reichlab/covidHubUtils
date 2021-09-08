library(tidyverse)
library(covidHubUtils)

inc_hosp_targets <- paste(1:34, "day ahead inc hosp")
inc_case_targets <- paste(1:4, "wk ahead inc case")
combined_targets <- c(inc_hosp_targets, inc_case_targets)
forecasts_hosp <- load_forecasts(
  models = c("COVIDhub-ensemble", "LANL-GrowthRate"),
  dates = c("2021-07-12", "2021-07-19"),
  date_window_size = 6,
  locations = "US",
  types = "quantile",
  targets = combined_targets,
#  source = "zoltar",
  source = "local_hub_repo",
  hub_repo_path = "../covid19-forecast-hub",
  verbose = FALSE,
  as_of = NULL,
  hub = c("US")
)


curr_model <- models[i]
        model_url <- all_models[all_models$model_abbr == curr_model, 
            ]$url
        model_forecasts_history <- zoltr::forecasts(zoltar_connection = zoltar_connection, 
            model_url = model_url)$timezero_date
        latest_dates <- purrr::map(forecast_dates, function(a_list) {
            max(intersect(as.character(a_list), as.character(model_forecasts_history)))
        })
        latest_dates <- unique(unlist(latest_dates, use.names = FALSE))
        latest_dates <- latest_dates[!is.na(latest_dates)]
        if (length(latest_dates) != 0) {
            forecast <- zoltr::do_zoltar_query(zoltar_connection = zoltar_connection, 
                project_url = project_url, query_type = "forecasts", 
                units = locations, timezeros = latest_dates, 
                models = curr_model, targets = targets, types = types, 
                verbose = verbose, as_of = date_to_datetime(as_of, 
                  hub))
            forecast <- reformat_forecasts(forecast)
        }