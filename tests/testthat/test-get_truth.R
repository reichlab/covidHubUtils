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