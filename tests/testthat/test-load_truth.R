context("load_truth")
library(covidHubUtils)
library(zoltr)
library(dplyr)

test_that("default selections",{
  # US hub
  actual_us <- load_truth()
  # weekly
  expected_us_inc_case <- load_truth(truth_source = c("JHU"),
                                  target_variable = c("inc case"))
  # weekly
  expected_us_inc_death <- load_truth(truth_source = c("JHU"),
                                   target_variable = c("inc death"))
  # daily
  expected_us_inc_hosp <- load_truth(truth_source = c("HealthData"),
                                  target_variable = c("inc hosp"))
  
  expect_true(dplyr::all_equal(actual_us, 
                               dplyr::bind_rows(expected_us_inc_case,
                                                expected_us_inc_death,
                                                expected_us_inc_hosp)))
  # ECDC hub
  actual_ecdc <- load_truth(hub = c("ECDC"))
  # weekly
  expected_ecdc_inc_case <- load_truth(truth_source = c("JHU"),
                                       target_variable = c("inc case"),
                                       hub = c("ECDC"))
  # weekly
  expected_ecdc_inc_death <- load_truth(truth_source = c("JHU"),
                                        target_variable = c("inc death"),
                                        hub = c("ECDC"))
  expect_true(dplyr::all_equal(actual_ecdc, 
                               dplyr::bind_rows(expected_ecdc_inc_case,
                                                expected_ecdc_inc_death)))
})

test_that("load one target variable from multiple sources", {
  # US hub
  actual_us <- load_truth(truth_source = c("JHU", "NYTimes", "USAFacts"),
                          target_variable = c("inc case"))
  expect_equal(unique(actual_us$target_variable), c("inc case"))
  expect_equal(unique(actual_us$model), 
               c("Observed Data (JHU)",
                 "Observed Data (NYTimes)", 
                 "Observed Data (USAFacts)"))
  expect_equal(unique(actual_us$target_end_date), 
               seq(min(actual_us$target_end_date), 
                   max(actual_us$target_end_date), 7))
  # ECDC hub
  actual_ecdc <- load_truth(truth_source = c("JHU", "ECDC"),
                            target_variable = c("inc case"),
                            hub = c("ECDC"))
  expect_equal(unique(actual_ecdc$target_variable), c("inc case"))
  expect_equal(unique(actual_ecdc$model), 
               c("Observed Data (ECDC)",
                 "Observed Data (JHU)"))
})

test_that("handles `inc hosp` and `HealthData` source in US hub correctly",{
  # case 1
  # add 'HealthData' internally to load "inc hosp" truth
  # when using `inc hosp` together with other target_variable,
  # inc hosp truth will be daily counts
  expect_warning(load_truth(truth_source = c("JHU"),
                            target_variable = c("inc case", "inc hosp")))
  
  actual <- load_truth(truth_source = c("JHU"),
                       target_variable = c("inc case", "inc hosp"))
  # weekly
  expected_inc_case <- load_truth(truth_source = c("JHU"),
                                  target_variable = c("inc case"))
  # daily
  expected_inc_hosp <- load_truth(truth_source = c("HealthData"),
                                  target_variable = c("inc hosp"))
  
  expect_true(dplyr::all_equal(actual, 
                               rbind(expected_inc_case, 
                                     expected_inc_hosp)))
  
  # case 2
  # throw error when 'HealthData' is used to load other targets
  expect_error(load_truth(truth_source = "HealthData",
                          target_variable = c("inc case", 
                                              "inc death",
                                              "cum death")))
})

test_that("handles `ECDC`source in ECDC hub correctly",{
  # return warning and weekly data when temporal resolution is daily
  expect_warning(load_truth(truth_source = c("ECDC"),
                            target_variable = c("inc case", "inc hosp"),
                            temporal_resolution = "daily",
                            hub = c("ECDC")))
  actual <- load_truth(truth_source = c("ECDC"),
                      target_variable = c("inc case", "inc hosp"),
                      temporal_resolution = "daily",
                      hub = c("ECDC"))
  expect_equal(unique(actual$target_end_date), 
               seq(min(actual$target_end_date), 
                   max(actual$target_end_date), 7))
  expect_equal(unique(weekdays(actual$target_end_date)),
               "Monday")
  
})

