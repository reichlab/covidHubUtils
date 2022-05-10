context("load_forecast_files_repo")

library(covidHubUtils)
library(dplyr)
library(lubridate)

test_that("load_forecast_files_repo works with data-processed folder", {
  forecast_files <-
    Sys.glob("test-data/test-load_forecasts/data-processed/*/*.csv")
  
  all_forecasts <- covidHubUtils::load_forecast_files_repo(forecast_files)

  # expect correct combinations of model and forecast date
  expect_identical(
    all_forecasts %>%
      dplyr::distinct(model, forecast_date) %>%
      dplyr::arrange(model, forecast_date),
    dplyr::tibble(
      model = c("COVIDhub-baseline", "COVIDhub-baseline", "COVIDhub-baseline",
                "COVIDhub-ensemble", "COVIDhub-ensemble", "COVIDhub-ensemble",
                "model-model1","model-model1"),
      forecast_date = lubridate::ymd(c("2020-12-07", "2020-12-14", "2021-06-21 ",
                                       "2020-12-07", "2020-12-14", "2021-05-03 ",
                                       "2020-12-07", "2020-12-14"
                                       ))
    )
  )
})

test_that("load_forecast_files_repo works with data-forecasts folder", {
  forecast_files <-
    Sys.glob("test-data/test-load_forecasts/data-forecasts/*/*.csv")
  
  all_forecasts <- covidHubUtils::load_forecast_files_repo(forecast_files)
  
  # expect correct combinations of model and forecast date
  expect_identical(
    all_forecasts %>%
      dplyr::distinct(model, forecast_date) %>%
      dplyr::arrange(model, forecast_date),
    dplyr::tibble(
      model = c("TeamA-ModelA","TeamB-ModelB"),
      forecast_date = lubridate::ymd(c("2021-12-13", "2021-12-13"))
    )
  )
})

test_that("drop rows with null in value column",{
  actual <- covidHubUtils::load_forecast_files_repo(
    "test-data/test-load_forecasts/data-processed/model-model1/2020-12-07-model-model1.csv")
  
  original <- readr::read_csv(
    "test-data/test-load_forecasts/data-processed/model-model1/2020-12-07-model-model1.csv")
  
  # number of rows match
  expect_true(nrow(original) - nrow(original[original$value == "NULL", ]) == nrow(actual))
  
  # all NULLs are at rows where location is 60.
  expect_true(!unique(original[original$value == "NULL", ]$location) %in% actual$location)
  
})

test_that("works when no need to drop rows",{
  actual <- covidHubUtils::load_forecast_files_repo(
    "test-data/test-load_forecasts/data-processed/model-model1/2020-12-14-model-model1.csv")
  
  original <- readr::read_csv(
    "test-data/test-load_forecasts/data-processed/model-model1/2020-12-14-model-model1.csv")
  
  # number of rows match
  expect_true(nrow(original) - nrow(original[original$value == "NULL", ]) == nrow(actual))
})
