library(covidHubUtils)
library(dplyr)
library(lubridate)

test_that("missing forecasts should throw error", {
  test_truth <- data.frame(
    model = c("source1", "source1"),
    target_variable = c("inc death", "inc death"),
    target_end_date = c(ymd(20200101), ymd(20200108)),
    location = c("01", "01"),
    value = c(1, 2)
  )
  
  expect_error(score_forecasts(truth = test_truth))
})

test_that("null forecasts should throw error", {
  test_truth <- data.frame(
    model = c("source1", "source1"),
    target_variable = c("inc death", "inc death"),
    target_end_date = c(ymd(20200101), ymd(20200108)),
    location = c("01", "01"),
    value = c(1, 2)
  )
  
  expect_error(score_forecasts(forecasts = NULL, truth = test_truth))
})

test_that("incorrectly-formatted forecasts should throw error", {
  test_truth <- data.frame(
    model = c("source1", "source1"),
    target_variable = c("inc death", "inc death"),
    target_end_date = c(ymd(20200101), ymd(20200108)),
    location = c("01", "01"),
    value = c(1, 2),
    stringsAsFactors = FALSE
  )
  
  malformed_forecasts <- data.frame(
    wrong_column_name_1 = c(1:5),
    wrong_column_name_2 = c(2:6),
    wrong_column_name_3 = c(3:7),
    stringsAsFactors = FALSE
  )
  
  expect_error(score_forecasts(forecasts = malformed_forecasts, truth = test_truth))
})

test_that("missing truth should throw error", {
  test_forecasts <- data.frame(
    model = c("source1", "source2"),
    forecast_date = c(ymd(20200101), ymd(20200101)),
    location = c("01", "01"),
    horizon = c("1", "1"),
    temporal_resolution = c("wk", "wk"),
    target_variable = c("inc death", "inc death"),
    target_end_date = c(ymd(20200108), ymd(20200108)),
    type = c("point", "point"),
    quantile = c(NA, NA),
    value = c(3, 4),
    stringsAsFactors = FALSE
  )
  
  expect_error(score_forecasts(forecasts = test_forecasts))
})

test_that("null truth should throw error", {
  test_forecasts <- data.frame(
    model = c("source1", "source2"),
    forecast_date = c(ymd(20200101), ymd(20200101)),
    location = c("01", "01"),
    horizon = c("1", "1"),
    temporal_resolution = c("wk", "wk"),
    target_variable = c("inc death", "inc death"),
    target_end_date = c(ymd(20200108), ymd(20200108)),
    type = c("point", "point"),
    quantile = c(NA, NA),
    value = c(3, 4),
    stringsAsFactors = FALSE
  )
  
  expect_error(score_forecasts(forecasts = test_forecasts, truth = NULL))
})

test_that("incorrectly-formatted truth should throw error", {
  test_forecasts <- data.frame(
    model = c("source1", "source2"),
    forecast_date = c(ymd(20200101), ymd(20200101)),
    location = c("01", "01"),
    horizon = c("1", "1"),
    temporal_resolution = c("wk", "wk"),
    target_variable = c("inc death", "inc death"),
    target_end_date = c(ymd(20200108), ymd(20200108)),
    type = c("point", "point"),
    quantile = c(NA, NA),
    value = c(3, 4),
    stringsAsFactors = FALSE
  )
  
  malformed_truth <- data.frame(
    wrong_colname_1 = c(NA, NA),
    wrong_colname_2 = c(NA, NA)
  )
  
  expect_error(score_forecasts(forecasts = test_forecasts, truth = malformed_truth))
})

test_that("incomplete truth should throw warning", {
  
})

test_that("calculated scores that are not in scores vector should be dropped", {
  test_forecasts <- data.frame(
    model = c("source1", "source2"),
    forecast_date = c(ymd(20200101), ymd(20200101)),
    location = c("01", "01"),
    horizon = c("1", "1"),
    temporal_resolution = c("wk", "wk"),
    target_variable = c("inc death", "inc death"),
    target_end_date = c(ymd(20200108), ymd(20200108)),
    type = c("point", "point"),
    quantile = c(NA, NA),
    value = c(3, 4),
    stringsAsFactors = FALSE
  )
  
  test_truth <- data.frame(
    model = c("source1", "source1"),
    target_variable = c("inc death", "inc death"),
    target_end_date = c(ymd(20200101), ymd(20200108)),
    location = c("01", "01"),
    value = c(1, 2),
    stringsAsFactors = FALSE
  )
})

test_that("invalid scores in scores vector should have NAs", {
  
})

test_that("valid output (long version) should contain necessary columns", {
  
})

test_that("valid output (wide version) should contain necessary columns", {
  
})

test_that("(long version) calculated scores that are in scores vector should have name and value", {
  
})

test_that("(wide version) calculated scores that are in scores vector should have column and value", {
  
})
