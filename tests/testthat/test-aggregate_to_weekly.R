context("aggregate_to_weekly")
library(covidHubUtils)
library(dplyr)

test_that("Aggregation works",{
  actual <- covidData::load_jhu_data(
    temporal_resolution = 'daily',
    measure = 'deaths',
    replace_negatives = FALSE,
    adjustment_cases = 'none',
    as_of = '2021-06-02',
    geography = c("US"),
    location_code = "US") %>%
    dplyr::select(-cum) %>%
    dplyr::rename(value = inc, target_end_date = date)%>%
    dplyr::mutate(model = "Observed Data (JHU)", target_variable = "inc death")
  
  actual <- aggregate_to_weekly(actual)
  
  expected <- covidData::load_jhu_data(
    temporal_resolution = 'weekly',
    measure = 'deaths',
    replace_negatives = FALSE,
    adjustment_cases = 'none',
    as_of = '2021-06-02',
    geography = c("US"),
    location_code = "US") %>%
    dplyr::select(-cum) %>%
    dplyr::rename(value = inc, target_end_date = date)%>%
    dplyr::mutate(model = "Observed Data (JHU)", target_variable = "inc death")
  
  data <- merge(actual, expected, by = c("model", "location", "target_end_date", "target_variable"))
  
  expect_true(all(data$value.x - data$value.y == 0))
  
})

test_that("load_truth uses aggregation correctly",{ 
  actual <- load_truth(truth_source = c("JHU"),
                       target_variable = c("inc death"),
                       temporal_resolution = "weekly",
                       locations = "US") %>%
    dplyr::select(model, target_variable, location, target_end_date, value)
  
  expected <- covidData::load_jhu_data(
    temporal_resolution = 'weekly',
    measure = 'deaths',
    replace_negatives = FALSE,
    adjustment_cases = 'none',
    geography = c("US"),
    location_code = "US") %>%
    dplyr::select(-cum) %>%
    dplyr::rename(value = inc, target_end_date = date)%>%
    dplyr::mutate(model = "Observed Data (JHU)", target_variable = "inc death")
  
  data <- merge(actual, expected, by = c("model", "location", "target_end_date", "target_variable"))
  
  expect_true(all(data$value.x - data$value.y == 0))
  
  })
