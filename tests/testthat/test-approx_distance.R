library(covidHubUtils)
library(dplyr)
library(lubridate)
library(stringr)


test_that("missing columns should throw error", {
  forecasts <-read.csv("test-data/test-approx_distance/small_data.csv") %>%
    dplyr::select(-"location")
  expect_error(pairwise_filter(dataframe = forecasts))
})

test_that("incorrectly-specified approximation rule should throw an error", {
  forecasts <-read.csv("test-data/test-approx_distance/filtered_data.csv")
  expect_error(approx_distance(forecasts, "thumb_rule"))
})