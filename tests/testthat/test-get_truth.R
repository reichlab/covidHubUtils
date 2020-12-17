context("get_truth")
library(covidHubUtils)
library(dplyr)

test_that("preprocess_hospitalization files has expected combinations of location, week", {
  # Location name
  location_names <- covidData::fips_codes[, c("location", "location_name")]
  
  actual <- covidHubUtils::preprocess_hospitalization(
                            save_location = "."
                          )
  
  actual_incident_hosp <- actual$incident_hosp %>%
                          dplyr::select(date, location)
  
  actual_cumulative_hosp <- actual$cumulative_hosp %>%
                          dplyr::select(date, location)
  
  expected <- covidData::load_healthdata_data(spatial_resolution = c("national", "state"),
                                              temporal_resolution = "daily",
                                              measure = "hospitalizations")
  
  expected_incident_hosp <- expected[,c("date", "location", "inc")] %>% 
                            dplyr::left_join(location_names, by = "location") %>% 
                            tidyr::drop_na(any_of(c("location_names", "inc"))) %>% 
                            dplyr::select(date, location)
  
  expected_cumulative_hosp <- expected[,c("date", "location", "cum")] %>% 
                              dplyr::left_join(location_names, by = "location") %>% 
                              tidyr::drop_na(any_of(c("location_names", "cum"))) %>% 
                              dplyr::select(date, location)
  
  expect_equal(actual_incident_hosp, expected_incident_hosp)
  expect_equal(actual_cumulative_hosp, expected_cumulative_hosp)
})


test_that("preprocess_hospitalization can correctly reconstruct cumulative and incident values", {
  # Location name
  location_names <- covidData::fips_codes[, c("location", "location_name")]
  
  actual <- covidHubUtils::preprocess_hospitalization(
    save_location = "."
  )
  
  actual_incident_hosp <- actual$incident_hosp %>%
    dplyr::select(value)
  
  actual_cumulative_hosp <- actual$cumulative_hosp %>%
    dplyr::select(value)
  
  expected <- covidData::load_healthdata_data(spatial_resolution = c("national", "state"),
                                              temporal_resolution = "daily",
                                              measure = "hospitalizations")
  
  expected_incident_hosp <- expected[,c("date", "location", "inc")] %>% 
    dplyr::left_join(location_names, by = "location") %>% 
    tidyr::drop_na(any_of(c("location_names", "inc"))) %>% 
    dplyr::rename(value = inc) %>% 
    dplyr::select(value)
  
  expected_cumulative_hosp <- expected[,c("date", "location", "cum")] %>% 
    dplyr::left_join(location_names, by = "location") %>% 
    tidyr::drop_na(any_of(c("location_names", "cum"))) %>% 
    dplyr::rename(value = cum) %>% 
    dplyr::select(value)
  
  expect_equal(actual_incident_hosp, expected_incident_hosp)
  expect_equal(actual_cumulative_hosp, expected_cumulative_hosp)
})

test_that("preprocess_jhu files has expected combinations of location, week", {
  # Location name
  location_names <- covidData::fips_codes[, c("location", "location_name")]
  
  actual <- covidHubUtils::preprocess_jhu(
    save_location = "."
  )
  
  actual_incident_cases <- actual$incident_cases %>%
    dplyr::select(date, location)
  
  actual_cumulative_cases <- actual$cumulative_cases %>%
    dplyr::select(date, location)
  
  actual_incident_deaths <- actual$incident_deaths %>%
    dplyr::select(date, location)
  
  actual_cumulative_deaths <- actual$cumulative_deaths %>%
    dplyr::select(date, location)
  
  expected_cases <- covidData::load_jhu_data(spatial_resolution = c("national", "state", "county"),
                                              temporal_resolution = "daily",
                                              measure = "cases")
  
  expected_incident_cases <- expected_cases[,c("date", "location", "inc")] %>% 
    dplyr::left_join(location_names, by = "location") %>% 
    tidyr::drop_na(any_of(c("location_names", "inc"))) %>% 
    dplyr::select(date, location)
  
  expected_cumulative_cases <- expected_cases[,c("date", "location", "cum")] %>% 
    dplyr::left_join(location_names, by = "location") %>% 
    tidyr::drop_na(any_of(c("location_names", "cum"))) %>% 
    dplyr::select(date, location)
  
  expected_deaths <- covidData::load_jhu_data(spatial_resolution = c("national", "state", "county"),
                                             temporal_resolution = "daily",
                                             measure = "deaths")
  
  expected_incident_deaths <- expected_deaths[,c("date", "location", "inc")] %>% 
    dplyr::left_join(location_names, by = "location") %>% 
    tidyr::drop_na(any_of(c("location_names", "inc"))) %>% 
    dplyr::select(date, location)
  
  expected_cumulative_deaths <- expected_deaths[,c("date", "location", "cum")] %>% 
    dplyr::left_join(location_names, by = "location") %>% 
    tidyr::drop_na(any_of(c("location_names", "cum"))) %>% 
    dplyr::select(date, location)
  
  expect_equal(actual_incident_cases, expected_incident_cases)
  expect_equal(actual_cumulative_cases, expected_cumulative_cases)
  expect_equal(actual_incident_deaths, expected_incident_deaths)
  expect_equal(actual_cumulative_deaths, expected_cumulative_deaths)
})


test_that("preprocess_hospitalization can correctly reconstruct cumulative and incident values", {
  # Location name
  location_names <- covidData::fips_codes[, c("location", "location_name")]
  
  actual <- covidHubUtils::preprocess_jhu(
    save_location = "."
  )
  
  actual_incident_cases <- actual$incident_cases %>%
    dplyr::select(date, location)
  
  actual_cumulative_cases <- actual$cumulative_cases %>%
    dplyr::select(date, location)
  
  actual_incident_deaths <- actual$incident_deaths %>%
    dplyr::select(date, location)
  
  actual_cumulative_deaths <- actual$cumulative_deaths %>%
    dplyr::select(date, location)
  
  expected_cases <- covidData::load_jhu_data(spatial_resolution = c("national", "state", "county"),
                                             temporal_resolution = "daily",
                                             measure = "cases")
  
  expected_incident_cases <- expected_cases[,c("date", "location", "inc")] %>% 
    dplyr::left_join(location_names, by = "location") %>% 
    tidyr::drop_na(any_of(c("location_names", "inc"))) %>% 
    dplyr::rename(value = inc) %>% 
    dplyr::select(value)
  
  expected_cumulative_cases <- expected_cases[,c("date", "location", "cum")] %>% 
    dplyr::left_join(location_names, by = "location") %>% 
    tidyr::drop_na(any_of(c("location_names", "cum"))) %>% 
    dplyr::rename(value = cum) %>% 
    dplyr::select(value)
  
  expected_deaths <- covidData::load_jhu_data(spatial_resolution = c("national", "state", "county"),
                                             temporal_resolution = "daily",
                                             measure = "deaths")
  
  expected_incident_deaths <- expected_deaths[,c("date", "location", "inc")] %>% 
    dplyr::left_join(location_names, by = "location") %>% 
    tidyr::drop_na(any_of(c("location_names", "inc"))) %>% 
    dplyr::rename(value = inc) %>% 
    dplyr::select(value)
  
  expected_cumulative_deaths <- expected_deaths[,c("date", "location", "cum")] %>% 
    dplyr::left_join(location_names, by = "location") %>% 
    tidyr::drop_na(any_of(c("location_names", "cum"))) %>% 
    dplyr::rename(value = cum) %>% 
    dplyr::select(value)
  
  expect_equal(actual_incident_cases, expected_incident_cases)
  expect_equal(actual_cumulative_cases, expected_cumulative_cases)
  expect_equal(actual_incident_deaths, expected_incident_deaths)
  expect_equal(actual_cumulative_deaths, expected_cumulative_deaths)
})