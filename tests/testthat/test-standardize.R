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

#     Aug/Sep 2021
# Su Mo Tu We Th Fr Sa
#  1  2  3  4  5  6  7
#  8  9 10 11 12 13 14
# 15 16 17 18 19 20 21
# 22 23 24 25 26 27 28
# 29 30 31 1  2  3  4
#  5  6  7  8  9 10 11
# 12 13 14 15 16 17 18
# 19 20 21 22 23 24 25
# 26 27 28 29 30

original_fdf <- tibble(
    model = rep(c("a", "b", "c"), 4),
    forecast_date = rep(as.Date(
        "2021-08-12", "2021-08-14", "2021-08-16",
        "2021-08-19", "2021-08-21", "2021-08-23"
        ), 2),
    location = "US",
    horizon = ,
    temporal_resolution = rep(c(
        "wk", "wk", "day",
        "wk", "wk", "day",
        ), 2),
    target_variable = ,
    target_end_date = rep(as.Date(
        "2021-08-21", "2021-08-28", "2021-08-20",
        "2021-08-28", "2021-09-04", "2021-09-07"
        ), 2),
    type = "quantile",
    quantile = rep(c(0.25, 0.75), each = 12),
    value = ,
)

test_that("original columns preserved",

)

test_that("values in new columns are correct",

)