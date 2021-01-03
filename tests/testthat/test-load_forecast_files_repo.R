context("load_forecast_files_repo")

library(covidHubUtils)
library(dplyr)
library(lubridate)

test_that("load_forecast_files_repo works", {
  forecast_files <-
    Sys.glob("test-data/test-load_forecasts/data-processed/*/*.csv")
  
  all_forecasts <- covidHubUtils::load_forecast_files_repo(forecast_files)

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

