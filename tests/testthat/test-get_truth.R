context("get_truth")
library(covidHubUtils)
library(covidData)
library(dplyr)
library(mockery)

test_that("preprocess_jhu files has expected columns", {
  actual <- covidHubUtils::preprocess_jhu(
    save_location = "."
  )

  actual_cumulative_deaths_column_names <- colnames(actual$cumulative_deaths)
  actual_incident_deaths_column_names <- colnames(actual$incident_deaths)
  actual_cumulative_cases_column_names <- colnames(actual$cumulative_cases)
  actual_incident_cases_column_names <- colnames(actual$incident_cases)

  expected_column_names <- c("date", "location", "location_name", "value")

  expect_equal(actual_cumulative_deaths_column_names, expected_column_names)
  expect_equal(actual_incident_deaths_column_names, expected_column_names)
  expect_equal(actual_cumulative_cases_column_names, expected_column_names)
  expect_equal(actual_incident_cases_column_names, expected_column_names)
})


test_that("preprocess_jhu files has expected combinations of location, week", {
  # Location name
  location_names <- covidData::fips_codes[, c("location", "location_name")]

  # Spatial resolutions
  spatial_resolutions <- c("national", "state", "county")

  actual <- covidHubUtils::preprocess_jhu(
    save_location = "."
  )

  actual_cumulative_deaths <- actual$cumulative_deaths %>%
    dplyr::select(date, location)

  actual_incident_deaths <- actual$incident_deaths %>%
    dplyr::select(date, location)

  expected_deaths <- covidData::load_jhu_data(
    spatial_resolution = spatial_resolutions,
    temporal_resolution = "daily",
    measure = "deaths"
  ) %>%
    dplyr::left_join(location_names, by = "location") %>%
    dplyr::filter(location != "11001")

  expected_cumulative_deaths <- expected_deaths[, c("date", "location", "location_name", "cum")] %>%
    tidyr::drop_na(any_of(c("location_name", "cum"))) %>%
    dplyr::select(date, location)

  expected_incident_deaths <- expected_deaths[, c("date", "location", "location_name", "inc")] %>%
    tidyr::drop_na(any_of(c("location_name", "inc"))) %>%
    dplyr::select(date, location)

  expect_equal(actual_cumulative_deaths, expected_cumulative_deaths)
  expect_equal(actual_incident_deaths, expected_incident_deaths)

  actual_cumulative_cases <- actual$cumulative_cases %>%
    dplyr::select(date, location)

  actual_incident_cases <- actual$incident_cases %>%
    dplyr::select(date, location)

  expected_cases <- covidData::load_jhu_data(
    spatial_resolution = spatial_resolutions,
    temporal_resolution = "daily",
    measure = "cases"
  ) %>%
    dplyr::left_join(location_names, by = "location") %>%
    dplyr::filter(location != "11001")

  expected_cumulative_cases <- expected_cases[, c("date", "location", "location_name", "cum")] %>%
    tidyr::drop_na(any_of(c("location_name", "cum"))) %>%
    dplyr::select(date, location)

  expected_incident_cases <- expected_cases[, c("date", "location", "location_name", "inc")] %>%
    tidyr::drop_na(any_of(c("location_name", "inc"))) %>%
    dplyr::select(date, location)

  expect_equal(actual_cumulative_cases, expected_cumulative_cases)
  expect_equal(actual_incident_cases, expected_incident_cases)
})


test_that("preprocess_jhu files has the same cumulative and incident values as output from covidData", {
  # Location name
  location_names <- covidData::fips_codes[, c("location", "location_name")]

  # Spatial resolutions
  spatial_resolutions <- c("national", "state", "county")

  actual <- covidHubUtils::preprocess_jhu(
    save_location = "."
  )

  actual_cumulative_deaths <- actual$cumulative_deaths %>%
    dplyr::select(value)

  actual_incident_deaths <- actual$incident_deaths %>%
    dplyr::select(value)

  expected_deaths <- covidData::load_jhu_data(
    spatial_resolution = spatial_resolutions,
    temporal_resolution = "daily",
    measure = "deaths"
  ) %>%
    dplyr::left_join(location_names, by = "location") %>%
    dplyr::filter(location != "11001")

  expected_cumulative_deaths <- expected_deaths[, c("date", "location", "location_name", "cum")] %>%
    tidyr::drop_na(any_of(c("location_name", "cum"))) %>%
    dplyr::rename(value = cum) %>%
    dplyr::select(value)

  expected_incident_deaths <- expected_deaths[, c("date", "location", "location_name", "inc")] %>%
    tidyr::drop_na(any_of(c("location_name", "inc"))) %>%
    dplyr::rename(value = inc) %>%
    dplyr::select(value)

  expect_equal(actual_cumulative_deaths, expected_cumulative_deaths)
  expect_equal(actual_incident_deaths, expected_incident_deaths)

  actual_cumulative_cases <- actual$cumulative_cases %>%
    dplyr::select(value)

  actual_incident_cases <- actual$incident_cases %>%
    dplyr::select(value)

  expected_cases <- covidData::load_jhu_data(
    spatial_resolution = spatial_resolutions,
    temporal_resolution = "daily",
    measure = "cases"
  ) %>%
    dplyr::left_join(location_names, by = "location") %>%
    dplyr::filter(location != "11001")

  expected_cumulative_cases <- expected_cases[, c("date", "location", "location_name", "cum")] %>%
    tidyr::drop_na(any_of(c("location_name", "cum"))) %>%
    dplyr::rename(value = cum) %>%
    dplyr::select(value)

  expected_incident_cases <- expected_cases[, c("date", "location", "location_name", "inc")] %>%
    tidyr::drop_na(any_of(c("location_name", "inc"))) %>%
    dplyr::rename(value = inc) %>%
    dplyr::select(value)

  expect_equal(actual_cumulative_cases, expected_cumulative_cases)
  expect_equal(actual_incident_cases, expected_incident_cases)
})


test_that("preprocess_visualization_truth files has expected columns", {
  actual <- covidHubUtils::preprocess_visualization_truth(
    save_location = "."
  )

  actual_cumulative_deaths_column_names <- colnames(actual$cumulative_deaths)
  actual_incident_deaths_column_names <- colnames(actual$incident_deaths)
  actual_cumulative_cases_column_names <- colnames(actual$cumulative_cases)
  actual_incident_cases_column_names <- colnames(actual$incident_cases)

  expected_column_names <- c("location", "epiweek", "value")

  expect_equal(actual_cumulative_deaths_column_names, expected_column_names)
  expect_equal(actual_incident_deaths_column_names, expected_column_names)
  expect_equal(actual_cumulative_cases_column_names, expected_column_names)
  expect_equal(actual_incident_cases_column_names, expected_column_names)
})


test_that("preprocess_visualization_truth files has incident values greater than 0", {
  actual <- covidHubUtils::preprocess_visualization_truth(
    save_location = "."
  )

  # Test incident deaths
  actual_incident_deaths <- actual$incident_deaths %>%
    dplyr::select(value)
  actual_incident_deaths_ge_0 <- data.frame(actual_incident_deaths > 0)

  expected_incident_deaths_ge_0 <- rep(c(TRUE), times = nrow(actual_incident_deaths))
  expected_incident_deaths_ge_0 <- data.frame("value" = expected_incident_deaths_ge_0)

  expect_equal(actual_incident_deaths_ge_0, expected_incident_deaths_ge_0)

  # Test incident cases
  actual_incident_cases <- actual$incident_cases %>%
    dplyr::select(value)
  actual_incident_cases_ge_0 <- data.frame(actual_incident_cases > 0)

  expected_incident_cases_ge_0 <- rep(c(TRUE), times = nrow(actual_incident_cases))
  expected_incident_cases_ge_0 <- data.frame("value" = expected_incident_cases_ge_0)

  expect_equal(actual_incident_cases_ge_0, expected_incident_cases_ge_0)
})


test_that("preprocess_visualization_truth files has state and national location from covidData", {
  actual <- covidHubUtils::preprocess_visualization_truth(
    save_location = "."
  )

  # Actual locations
  actual_incident_deaths_locations <- actual$incident_deaths %>%
    dplyr::select(location)
  actual_incident_cases_locations <- actual$incident_cases %>%
    dplyr::select(location)
  actual_cumulative_deaths_locations <- actual$cumulative_deaths %>%
    dplyr::select(location)
  actual_cumulative_cases_locations <- actual$cumulative_cases %>%
    dplyr::select(location)

  # Expected locations
  # Location name
  location_names <- covidData::fips_codes[, c("location", "location_name", "abbreviation")]

  # Spatial resolutions
  spatial_resolutions <- c("national", "state")

  # Expected data
  cases_dframes <- covidData::load_jhu_data(
    spatial_resolution = spatial_resolutions,
    temporal_resolution = "weekly",
    measure = "cases"
  )
  cases_dframes_locations <- dplyr::left_join(cases_dframes, location_names, by = "location") %>%
    dplyr::select(abbreviation) %>%
    dplyr::mutate(abbreviation = replace(abbreviation, abbreviation == "US", "nat")) %>%
    dplyr::rename(location = abbreviation)

  deaths_dframes <- covidData::load_jhu_data(
    spatial_resolution = spatial_resolutions,
    temporal_resolution = "weekly",
    measure = "deaths"
  )
  deaths_dframes_locations <- dplyr::left_join(deaths_dframes, location_names, by = "location") %>%
    dplyr::select(abbreviation) %>%
    dplyr::mutate(abbreviation = replace(abbreviation, abbreviation == "US", "nat")) %>%
    dplyr::rename(location = abbreviation)

  # Test equality
  expect_equal(actual_incident_deaths_locations, deaths_dframes_locations)
  expect_equal(actual_cumulative_deaths_locations, deaths_dframes_locations)
  expect_equal(actual_incident_cases_locations, cases_dframes_locations)
  expect_equal(actual_cumulative_cases_locations, cases_dframes_locations)
})


test_that("preprocess_visualization_truth files has correct date formats", {
  actual <- covidHubUtils::preprocess_visualization_truth(
    save_location = "."
  )

  # Actual dates
  actual_incident_deaths_dates <- actual$incident_deaths$epiweek
  actual_incident_cases_dates <- actual$incident_cases$epiweek
  actual_cumulative_deaths_dates <- actual$cumulative_deaths$epiweek
  actual_cumulative_cases_dates <- actual$cumulative_cases$epiweek

  # Expected dates
  # Spatial resolutions
  spatial_resolutions <- c("national", "state")
  cases_dframes <- covidData::load_jhu_data(
    spatial_resolution = spatial_resolutions,
    temporal_resolution = "weekly",
    measure = "cases"
  )
  cases_mmwr <- lapply(cases_dframes["date"], MMWRweek::MMWRweek)$date
  # shift epiweek on axis
  cases_mmwr_week <- cases_mmwr["MMWRweek"] + 1
  cases_mmwr_year <- cases_mmwr["MMWRyear"]
  cases_mmwr_year[which(cases_mmwr_week > 53), ] <- cases_mmwr_year[which(cases_mmwr_week > 53), ] + 1
  cases_mmwr_week[which(cases_mmwr_week > 53), ] <- 1
  # format date as "{epiyear}{epiweek}". Exp: "202005"
  cases_mmwr_week <- data.frame(lapply(cases_mmwr_week, sprintf, fmt = "%02d"))
  cases_dates <- paste(cases_mmwr_year$MMWRyear, cases_mmwr_week$MMWRweek, sep = "")

  deaths_dframes <- covidData::load_jhu_data(
    spatial_resolution = spatial_resolutions,
    temporal_resolution = "weekly",
    measure = "deaths"
  )
  deaths_mmwr <- lapply(deaths_dframes["date"], MMWRweek::MMWRweek)$date
  # shift epiweek on axis
  deaths_mmwr_week <- deaths_mmwr["MMWRweek"] + 1
  deaths_mmwr_year <- deaths_mmwr["MMWRyear"]
  deaths_mmwr_year[which(deaths_mmwr_week > 53), ] <- deaths_mmwr_year[which(deaths_mmwr_week > 53), ] + 1
  deaths_mmwr_week[which(deaths_mmwr_week > 53), ] <- 1
  # format date as "{epiyear}{epiweek}". Exp: "202005"
  deaths_mmwr_week <- data.frame(lapply(deaths_mmwr_week, sprintf, fmt = "%02d"))
  deaths_dates <- paste(deaths_mmwr_year$MMWRyear, deaths_mmwr_week$MMWRweek, sep = "")

  # Test equality
  expect_equal(actual_incident_deaths_dates, deaths_dates)
  expect_equal(actual_cumulative_deaths_dates, deaths_dates)
  expect_equal(actual_incident_cases_dates, cases_dates)
  expect_equal(actual_cumulative_cases_dates, cases_dates)
})


test_that("preprocess_visualization_truth files has correct values from covidData", {
  actual <- covidHubUtils::preprocess_visualization_truth(
    save_location = "."
  )

  # Actual values
  actual_incident_deaths_values <- actual$incident_deaths$value
  actual_incident_cases_values <- actual$incident_cases$value
  actual_cumulative_deaths_values <- actual$cumulative_deaths$value
  actual_cumulative_cases_values <- actual$cumulative_cases$value

  # Expected values
  # Spatial resolutions
  spatial_resolutions <- c("national", "state")

  cases_dframes <- covidData::load_jhu_data(
    spatial_resolution = spatial_resolutions,
    temporal_resolution = "weekly",
    measure = "cases"
  )
  # Threshold to 0 for incident values < 0, for visualization purpose only
  cases_dframes$inc[cases_dframes$inc <= 0] <- 0.1
  cases_dframes$cum[cases_dframes$cum == 0] <- 0.1

  deaths_dframes <- covidData::load_jhu_data(
    spatial_resolution = spatial_resolutions,
    temporal_resolution = "weekly",
    measure = "deaths"
  )
  # Threshold to 0 for incident values < 0, for visualization purpose only
  deaths_dframes$inc[deaths_dframes$inc <= 0] <- 0.1
  deaths_dframes$cum[deaths_dframes$cum == 0] <- 0.1

  # Test equality
  expect_equal(actual_incident_deaths_values, deaths_dframes$inc)
  expect_equal(actual_cumulative_deaths_values, deaths_dframes$cum)
  expect_equal(actual_incident_cases_values, cases_dframes$inc)
  expect_equal(actual_cumulative_cases_values, cases_dframes$cum)
})


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

  expected <- covidData::load_healthdata_data(
    spatial_resolution = c("national", "state"),
    temporal_resolution = "daily",
    measure = "hospitalizations"
  )

  expected_incident_hosp <- expected[, c("date", "location", "inc")] %>%
    dplyr::left_join(location_names, by = "location") %>%
    tidyr::drop_na(any_of(c("location_names", "inc"))) %>%
    dplyr::select(date, location)

  expected_cumulative_hosp <- expected[, c("date", "location", "cum")] %>%
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

  expected <- covidData::load_healthdata_data(
    spatial_resolution = c("national", "state"),
    temporal_resolution = "daily",
    measure = "hospitalizations"
  )

  expected_incident_hosp <- expected[, c("date", "location", "inc")] %>%
    dplyr::left_join(location_names, by = "location") %>%
    tidyr::drop_na(any_of(c("location_names", "inc"))) %>%
    dplyr::rename(value = inc) %>%
    dplyr::select(value)

  expected_cumulative_hosp <- expected[, c("date", "location", "cum")] %>%
    dplyr::left_join(location_names, by = "location") %>%
    tidyr::drop_na(any_of(c("location_names", "cum"))) %>%
    dplyr::rename(value = cum) %>%
    dplyr::select(value)

  expect_equal(actual_incident_hosp, expected_incident_hosp)
  expect_equal(actual_cumulative_hosp, expected_cumulative_hosp)
})
