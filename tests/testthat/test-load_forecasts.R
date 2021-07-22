context("load_forecast")

library(covidHubUtils)
library(dplyr)
library(lubridate)

test_that("load_forecast from local repo works", {
  all_forecasts <- covidHubUtils::load_forecasts(
    models = c("COVIDhub-ensemble", "COVIDhub-baseline"),
    dates = c("2020-12-08", "2020-12-15"),
    date_window_size = 1,
    source = "local_hub_repo",
    hub_repo_path = "test-data/test-load_forecasts/")
  
  # expect correct combinations of model and forecast date
  expect_identical(
    all_forecasts %>%
      dplyr::distinct(model, forecast_date) %>%
      dplyr::arrange(model, forecast_date),
    tidyr::expand_grid(
      model = c("COVIDhub-baseline", "COVIDhub-ensemble"),
      forecast_date = lubridate::ymd(c("2020-12-07", "2020-12-14"))
    )
  )
})

test_that("load_forecast from zoltar works", {
  all_forecasts <- covidHubUtils::load_forecasts(
    models = c("COVIDhub-ensemble", "COVIDhub-baseline"),
    dates = c("2020-12-08", "2020-12-15"),
    date_window_size = 1,
    source = "zoltar")
  
  # expect correct combinations of model and forecast date
  expect_identical(
    all_forecasts %>%
      dplyr::distinct(model, forecast_date) %>%
      dplyr::arrange(model, forecast_date),
    tidyr::expand_grid(
      model = c("COVIDhub-baseline", "COVIDhub-ensemble"),
      forecast_date = lubridate::ymd(c("2020-12-07", "2020-12-14"))
    )
  )
})



