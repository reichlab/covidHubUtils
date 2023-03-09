context("load_truth")
library(covidHubUtils)
library(covidData)
library(zoltr)
library(dplyr)

test_that("default selections from remote hub repo", {
  # US hub
  actual_us <- load_truth()
  # weekly
  expected_us_inc_case <- load_truth(
    truth_source = c("JHU"),
    target_variable = c("inc case")
  )
  # weekly
  expected_us_inc_death <- load_truth(
    truth_source = c("JHU"),
    target_variable = c("inc death")
  )
  # daily
  expected_us_inc_hosp <- load_truth(
    truth_source = c("HealthData"),
    target_variable = c("inc hosp")
  )

  expect_true(dplyr::all_equal(
    actual_us,
    dplyr::bind_rows(
      expected_us_inc_case,
      expected_us_inc_death,
      expected_us_inc_hosp
    )
  ))
  # ECDC hub
  actual_ecdc <- load_truth(hub = c("ECDC"))
  # weekly
  expected_ecdc_inc_case <- load_truth(
    truth_source = c("ECDC"),
    target_variable = c("inc case"),
    hub = c("ECDC")
  )
  # weekly
  expected_ecdc_inc_death <- load_truth(
    truth_source = c("ECDC"),
    target_variable = c("inc death"),
    hub = c("ECDC")
  )
  # daily
  expected_ecdc_inc_hosp <- load_truth(
    truth_source = c("OWID"),
    target_variable = c("inc hosp"),
    hub = c("ECDC")
  )

  expect_true(dplyr::all_equal(
    actual_ecdc,
    dplyr::bind_rows(
      expected_ecdc_inc_case,
      expected_ecdc_inc_death,
      expected_ecdc_inc_hosp
    )
  ))
  
  # Flu hub
  actual_flu <- load_truth(hub = "FluSight")
  # weekly
  expected_flu_inc_flu_hosp <- load_truth(
    truth_source = c("HealthData"),
    target_variable = c("inc flu hosp"),
    hub = c("FluSight")
  )
  
  expect_true(dplyr::all_equal(
    actual_flu,
    expected_flu_inc_flu_hosp))
})

test_that("comapre target variables and models on default selections from remote hub 
          repo and covidData", {
  # US hub from covidData
  actual_us <- load_truth(data_location = "covidData")
  expect_equal(unique(actual_us$target_variable), c(
    "inc hosp",
    "inc case",
    "inc death"
  ))
  expect_equal(
    unique(actual_us$model),
    c(
      "Observed Data (HealthData)",
      "Observed Data (JHU)"
    )
  )

  # ECDC hub from covidData
  actual_ecdc <- load_truth(
    hub = c("ECDC"), target_variable = c("inc case", "inc death"),
    truth_source = "JHU", data_location = "covidData"
  )
  expect_equal(unique(actual_ecdc$target_variable), c(
    "inc case",
    "inc death"
  ))
  expect_equal(
    unique(actual_ecdc$model),
    c("Observed Data (JHU)")
  )
})

test_that("load one target variable from multiple sources from remote hub repo", {
  # US hub
  actual_us <- load_truth(
    truth_source = c("JHU", "NYTimes"),
    target_variable = c("inc case")
  )
  expect_equal(unique(actual_us$target_variable), c("inc case"))
  expect_equal(
    unique(actual_us$model),
    c(
      "Observed Data (JHU)",
      "Observed Data (NYTimes)"
    )
  )
  expect_equal(
    unique(actual_us$target_end_date),
    seq(
      min(actual_us$target_end_date),
      max(actual_us$target_end_date), 7
    )
  )
  # ECDC hub
  # no test cases
  
  # Flu hub
  actual_flu <- load_truth(
    truth_source = c("HealthData"),
    target_variable = c("inc flu hosp"),
    hub = c("FluSight")
  )
  
  expect_equal(unique(actual_flu$target_variable), c("inc flu hosp"))
  expect_equal(unique(actual_flu$model), c("Observed Data (HealthData)"))
})

test_that("handles `inc hosp` and `HealthData` source in US hub correctly when loading
          from remote hub repo", {
  # case 1
  # add 'HealthData' internally to load "inc hosp" truth
  # when using `inc hosp` together with other target_variable,
  # inc hosp truth will be daily counts
  expect_warning(actual <- load_truth(
    truth_source = c("JHU"),
    target_variable = c("inc case", "inc hosp")
  ))
  # weekly
  expected_inc_case <- load_truth(
    truth_source = c("JHU"),
    target_variable = c("inc case")
  )
  # daily
  expected_inc_hosp <- load_truth(
    truth_source = c("HealthData"),
    target_variable = c("inc hosp")
  )

  expect_true(dplyr::all_equal(
    actual,
    rbind(
      expected_inc_case,
      expected_inc_hosp
    )
  ))

  # case 2
  # throw error when 'HealthData' is used to load other targets
  expect_error(load_truth(
    truth_source = "HealthData",
    target_variable = c(
      "inc case",
      "inc death",
      "cum death"
    )
  ))
})

test_that("handles `ECDC`source in ECDC hub correctly when loading from remote 
          hub repo", {
  # for case/death data, return warning and weekly data when temporal resolution is daily
  expect_error(actual <- load_truth(
    truth_source = c("OWID"),
    target_variable = c("inc case", "inc death"),
    temporal_resolution = "daily",
    hub = c("ECDC")
  ))
})

test_that("expects warnings when loading versioned data from hub repo", {
  expect_warning(data <- load_truth(
    truth_source = "JHU", 
    target_variable = "inc case", 
    locations = "US", 
    as_of = "2020-11-23"
  ))
})

test_that("expects error when loading flu hub data from covidData", {
  expect_error(data <- load_truth(
    hub = c("FluSight"),
    data_location = "covidData"
  ))
})

test_that("handles location name correctly", {
  actual_ny <- load_truth(locations = "New York",
                          truth_source = c("JHU"),
                          target_variable = c("inc case"))
  
  expect_equal(unique(actual_ny$location), "36")
  expect_equal(unique(actual_ny$target_variable), "inc case")
  expect_equal(unique(actual_ny$model), "Observed Data (JHU)")
  
  actual_gb <- load_truth(locations = "United Kingdom",
                          truth_source = c("JHU"),
                          target_variable = c("inc case"),
                          hub = "ECDC")
  
  expect_equal(unique(actual_gb$location), "GB")
  expect_equal(unique(actual_gb$target_variable), "inc case")
  expect_equal(unique(actual_gb$model), "Observed Data (JHU)")
  
  actual_ma <- load_truth(locations = "Massachusetts",
                          truth_source = c("HealthData"),
                          target_variable = c("inc flu hosp"),
                          hub = "FluSight")
  
  expect_equal(unique(actual_ma$location), "25")
  expect_equal(unique(actual_ma$target_variable), "inc flu hosp")
  expect_equal(unique(actual_ma$model), "Observed Data (HealthData)")
  
})

test_that("handles truth_end_date filter correctly", {
  actual_US <- load_truth(locations = "New York",
                          truth_source = c("JHU"),
                          target_variable = c("inc case"),
                          truth_end_date = "2021-12-01",
                          hub = "US")
  
  expect_true(max(actual_US$target_end_date) <= "2021-12-01")
  
  actual_ECDC <- load_truth(locations = "GB",
                          truth_source = c("JHU"),
                          target_variable = c("inc case"),
                          truth_end_date = "2021-12-01",
                          hub = "ECDC")
  
  expect_true(max(actual_ECDC$target_end_date) <= "2021-12-01")
})

