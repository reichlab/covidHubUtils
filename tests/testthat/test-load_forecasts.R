context("load_forecast")

library(covidHubUtils)
library(dplyr)
library(lubridate)

test_that("load_forecast from local repo works with window size", {
  all_forecasts <- covidHubUtils::load_forecasts(
    models = c("COVIDhub-ensemble", "COVIDhub-baseline"),
    dates = c("2020-12-08", "2020-12-15"),
    locations = c("New York", "US"),
    date_window_size = 1,
    source = "local_hub_repo",
    hub_repo_path = "test-data/test-load_forecasts/")
  
  # expect correct combinations of model and forecast date
  expect_identical(
    all_forecasts %>%
      dplyr::distinct(model, location, forecast_date) %>%
      dplyr::arrange(model, forecast_date),
    tidyr::expand_grid(
      model = c("COVIDhub-baseline", "COVIDhub-ensemble"),
      forecast_date = lubridate::ymd(c("2020-12-07", "2020-12-14")),
      location = c("36", "US")
    )
  )
})

test_that("load_forecast from local repo works without window size", {
  all_forecasts <- covidHubUtils::load_forecasts(
    models = c("COVIDhub-ensemble", "COVIDhub-baseline"),
    dates = c("2020-12-07", "2020-12-14"),
    date_window_size = 0,
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

test_that("load_forecast from local repo works with different forecast dates", {
  expect_warning(all_forecasts <- covidHubUtils::load_forecasts(
    models = c("COVIDhub-ensemble", "COVIDhub-baseline"),
    dates = c("2020-12-09", "2020-12-15", "2021-05-04", "2021-06-22"),
    date_window_size = 1,
    source = "local_hub_repo",
    hub_repo_path = "test-data/test-load_forecasts/"))
  
  # expect correct combinations of model and forecast date
  expect_identical(
    all_forecasts %>%
      dplyr::distinct(model, forecast_date) %>%
      dplyr::arrange(model, forecast_date),
    dplyr::tibble(
      model = c("COVIDhub-baseline","COVIDhub-baseline", "COVIDhub-ensemble", "COVIDhub-ensemble"),
      forecast_date = lubridate::ymd(c("2020-12-14", "2021-06-21", "2020-12-14","2021-05-03"))
    )
  )
})

test_that("load_forecast from zoltar works with window size", {
  all_forecasts <- covidHubUtils::load_forecasts(
    models = c("COVIDhub-ensemble", "COVIDhub-baseline"),
    dates = c("2020-12-08", "2020-12-15"),
    locations = c("New York", "US"),
    date_window_size = 1,
    source = "zoltar")
  
  # expect correct combinations of model and forecast date
  expect_identical(
    all_forecasts %>%
      dplyr::distinct(model, location, forecast_date) %>%
      dplyr::arrange(model, forecast_date),
    tidyr::expand_grid(
      model = c("COVIDhub-baseline", "COVIDhub-ensemble"),
      forecast_date = lubridate::ymd(c("2020-12-07", "2020-12-14")),
      location = c("US", "36"),
    )
  )
})

test_that("load_forecast from zoltar works without window size", {
  all_forecasts <- covidHubUtils::load_forecasts(
    models = c("COVIDhub-ensemble", "COVIDhub-baseline"),
    dates = c("2020-12-07", "2020-12-14"),
    date_window_size = 0,
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

test_that("load_forecast from zoltar works with different forecast dates", {
  all_forecasts <- covidHubUtils::load_forecasts(
    models = c("COVIDhub-ensemble", "MIT_CritData-GBCF", "FAIR-NRAR"),
    dates = c("2021-07-20"),
    date_window_size = 3,
    source = "zoltar")
  
  # expect correct combinations of model and forecast date
  expect_identical(
    all_forecasts %>%
      dplyr::distinct(model, forecast_date) %>%
      dplyr::arrange(model, forecast_date),
    dplyr::tibble(
      model = c("COVIDhub-ensemble", "FAIR-NRAR", "MIT_CritData-GBCF"),
      forecast_date = lubridate::ymd(c("2021-07-19 ", "2021-07-20", "2021-07-18"))
    )
  )
})



