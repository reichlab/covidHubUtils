library(covidHubUtils)
library(dplyr)
library(lubridate)
library(stringr)

simple_test_forecasts <- data.frame(
  model_id = c("source1", "source2"),
  forecast_date = c(ymd(20200101), ymd(20200101)),
  location = c("01", "01"),
  horizon = c("1", "1"),
  temporal_resolution = c("wk", "wk"),
  target_variable = c("inc death", "inc death"),
  target_end_date = c(ymd(20200108), ymd(20200108)),
  output_type = c("point", "point"),
  output_type_id = c(NA, NA),
  value = c(3, 4),
  stringsAsFactors = FALSE
)

test_that("Not providing the names of all mandatory columns throws an error", {
  simple_test_forecasts |>
    as_covid_hub_forecasts(horizon=NULL, target_col="target_variable", 
                          temp_res_col="temporal_resolution", 
                          target_end_date_col="target_end_date") |>
    expect_error()
})

test_that("Providing a column name not in the model_outputs throws an error", {
  simple_test_forecasts |>
    as_covid_hub_forecasts(location_col="unit", target_col="target_variable", 
                          temp_res_col="temporal_resolution", 
                          target_end_date_col="target_end_date") |>
    expect_error()
})

test_that("Not providing any date columns throws an error", {
  simple_test_forecasts |>
    as_covid_hub_forecasts(target_col="target_variable", 
                          temp_res_col="temporal_resolution", 
                          reference_date_col=NULL, target_end_date_col=NULL) |>
    expect_error()
})

test_that("Inclusion of multiple types of point forecasts throws an error", {
  simple_test_forecasts$output_type <- c("median", "mean")
  simple_test_forecasts |>
    as_covid_hub_forecasts(target_col="target_variable", 
                          temp_res_col="temporal_resolution", 
                          target_end_date_col="target_end_date") |>
    expect_error()
})

test_that("Inclusion of multiple types of point forecasts throws an error", {
  simple_test_forecasts |>
    dplyr::select(-model_id) |>
    as_covid_hub_forecasts(model_id=NULL, target_col="target_variable", 
                          temp_res_col="temporal_resolution", 
                          target_end_date_col="target_end_date") |>
    expect_warning()
})

test_that("The resulting temporal resolution and target variable columns are correctly formatted when a temporal resolution column is not provided", {
  actual_temporal_resolution = c("wk", "wk")
  actual_target_variable = c("inc death", "inc death")

  simple_test_forecasts$target_variable <- c("wk inc death", "wk ahead inc death")
  test_forecasts_hub <- simple_test_forecasts |>
    dplyr::select(-temporal_resolution) |>
    as_covid_hub_forecasts(target_col="target_variable", 
                          temp_res_col=NULL,
                          target_end_date_col="target_end_date") 

  expect_equal(test_forecasts_hub$temporal_resolution, actual_temporal_resolution)
  expect_equal(test_forecasts_hub$target_variable, actual_target_variable)
})
  
test_that("Reference dates are correctly calculated if not provided", {
  actual_reference_date = c(ymd(20200101), ymd(20200101))
  simple_test_forecasts |>
    dplyr::select(-forecast_date) |>
    as_covid_hub_forecasts(target_col="target_variable", 
                          temp_res_col="temporal_resolution", 
                          reference_date=NULL,
                          target_end_date_col="target_end_date") |>
    dplyr::pull(forecast_date) |>
    expect_equal(actual_reference_date)
})

test_that("Target end dates are correctly calculated if not provided", {
  actual_target_end_date = c(ymd(20200108), ymd(20200108))
  simple_test_forecasts |>
    dplyr::select(-target_end_date) |>
    as_covid_hub_forecasts(target_col="target_variable", 
                          temp_res_col="temporal_resolution", 
                          reference_date="forecast_date",
                          target_end_date_col=NULL) |>
    dplyr::pull(target_end_date) |>
    expect_equal(actual_target_end_date)
})

test_that("Only columns required by the Covid Hub are kept", {
  hub_cols <- c("model", "forecast_date", "location", "horizon", "target_variable", "type", "quantile", "value", "temporal_resolution", "target_end_date")
  simple_test_forecasts |>
    dplyr::mutate(abbreviation="AL", full_location_name="Alabama") |>
    as_covid_hub_forecasts(target_col="target_variable", 
                          temp_res_col="temporal_resolution", 
                          target_end_date_col="target_end_date") |>
    names() |>
    sort() |>
    expect_equal(sort(hub_cols))
})
