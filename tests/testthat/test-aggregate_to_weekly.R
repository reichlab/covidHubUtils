context("aggregate_to_weekly")
library(covidHubUtils)
library(dplyr)

test_that("Aggregation works", {
  daily_data <- readr::read_csv("test-data/test-aggregate_to_weekly/test_daily_data.csv")

  actual <- aggregate_to_weekly(daily_data)

  expected <- readr::read_csv("test-data/test-aggregate_to_weekly/test_expected_weekly_data.csv")

  data <- merge(actual, expected, by = c("model", "location", "target_end_date", "target_variable"))

  expect_true(all(data$value.x - data$value.y == 0))
})
