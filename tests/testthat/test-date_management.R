context("date_management")
library(covidHubUtils)

test_that("calc_forecast_week_end_date works: on Mondays and Sundays", {
  actual <- covidHubUtils::calc_forecast_week_end_date(c(as.Date('2020-11-02'),
                                                         as.Date('2020-11-01')))
  
  expected <- c('2020-10-31', '2020-10-31')
  
  expect_equal(actual, expected)
  
})

test_that("calc_forecast_week_end_date works: on Tuesdays to Fridays", {
  actual <- covidHubUtils::calc_forecast_week_end_date(c(as.Date('2020-11-03'),
                                                         as.Date('2020-11-04')))
  
  expected <- c('2020-11-07','2020-11-07')
  
  expect_equal(actual, expected)
  
})

test_that("calc_target_week_end_date works: on Mondays and Sundays", {
  actual <- covidHubUtils::calc_target_week_end_date(forecast_date = 
                                                       c(as.Date('2020-11-02'),
                                                         as.Date('2020-11-01')),
                                                     horizon = c(1,2))
  
  expected <- c('2020-11-07','2020-11-14')
  
  expect_equal(actual, expected)
  
})

test_that("calc_target_week_end_date works: on Tuesdays to Fridays", {
  actual <- covidHubUtils::calc_target_week_end_date(forecast_date = 
                                                       c(as.Date('2020-11-03'),
                                                         as.Date('2020-11-04')),
                                                     horizon = c(1,2))
  
  expected <- c('2020-11-14','2020-11-21')
  
  expect_equal(actual, expected)
  
})

test_that("calc_target_end_date works: on Mondays and Sundays with 
          'wk' as temporal resolution", {
  actual <- covidHubUtils::calc_target_end_date(forecast_date = 
                                          c(as.Date('2020-11-02'),
                                            as.Date('2020-11-01')),
                                          horizon = c(1,2),
                                          temporal_resolution = c('wk', 'wk'))
  
  expected <- c('2020-11-07','2020-11-14')
  
  expect_equal(actual, expected)
  
})

test_that("calc_target_end_date works: on Mondays and Sundays with 
          'day' as temporal resolution", {
            actual <- covidHubUtils::calc_target_end_date(forecast_date = 
                                                            c(as.Date('2020-11-02'),
                                                              as.Date('2020-11-01')),
                                                          horizon = c(1,2),
                                                          temporal_resolution = c('day', 'day'))
            
            expected <- c('2020-11-03','2020-11-03')
            
            expect_equal(actual, expected)
            
          })

test_that("calc_target_end_date works: on Tuesdays to Fridays with 
          'wk' as temporal resolution", {
            
            actual <- covidHubUtils::calc_target_end_date(forecast_date = 
                                                            c(as.Date('2020-11-03'),
                                                              as.Date('2020-11-04')),
                                                          horizon = c(1,2),
                                                          temporal_resolution = c('wk', 'wk'))
            
            expected <- c('2020-11-14','2020-11-21')
            
            expect_equal(actual, expected)
            
          })

test_that("calc_target_end_date works: on Tuesdays to Fridays with 
          'day' as temporal resolution", {
            
            actual <- covidHubUtils::calc_target_end_date(forecast_date = 
                                                            c(as.Date('2020-11-03'),
                                                              as.Date('2020-11-04')),
                                                          horizon = c(1,2),
                                                          temporal_resolution = c('day', 'day'))
            
            expected <- c('2020-11-04','2020-11-06')
            
            expect_equal(actual, expected)
            
          })


test_that("calc_submission_due_date works", 
          {
            sample_dates <- c("2020-05-12", "2021-03-2",
                              "2022-04-09", "2021-12-04", 
                              as.character(as.Date("2021-01-01") + 0:6))
            
            actual <- calc_submission_due_date(sample_dates)
            
            expected <- c("2020-05-18", "2021-03-08", 
                          "2022-04-11", "2021-12-06", 
                          "2021-01-04", "2021-01-04", 
                          "2021-01-04", "2021-01-04", 
                          "2021-01-11", "2021-01-11", 
                          "2021-01-11")
            
            expect_equal(actual, expected)
            
          })

test_that("date_to_datetime works", {
  # case1 
  # user input is date only
  actual_us <- covidHubUtils::date_to_datetime(date = "2021-05-12", hub = c("US"))
  expect_equal(actual_us, "2021-05-13 03:59:59 UTC")
  
  actual_ecdc <- covidHubUtils::date_to_datetime(date = "2021-05-12", hub = c("ECDC"))
  expect_equal(actual_ecdc, "2021-05-12 21:59:59 UTC")
  
  # case2 
  # user input is date and time only
  # winter time
  actual_us <- covidHubUtils::date_to_datetime(date = "2021-01-01 01:30:40", hub = c("US"))
  expect_equal(actual_us, "2021-01-01 06:30:40 UTC")
  
  actual_ecdc <- covidHubUtils::date_to_datetime(date = "2021-01-01 01:30:40", hub = c("ECDC"))
  expect_equal(actual_ecdc, "2021-01-01 00:30:40 UTC")
  
  # summer time
  actual_us <- covidHubUtils::date_to_datetime(date = "2021-08-01 01:30:40", hub = c("US"))
  expect_equal(actual_us, "2021-08-01 05:30:40 UTC")
  
  actual_ecdc <- covidHubUtils::date_to_datetime(date = "2021-08-01 01:30:40", hub = c("ECDC"))
  expect_equal(actual_ecdc, "2021-07-31 23:30:40 UTC")
  
  # case3
  # no need to change but time zone is not defaults
  actual_us <- covidHubUtils::date_to_datetime(date = "2021-05-12 01:30:40 UTC", hub = c("US"))
  expect_equal(actual_us, "2021-05-12 01:30:40 UTC")
  
  actual_ecdc <- covidHubUtils::date_to_datetime(date = "2021-05-12 01:30:40 UTC", hub = c("ECDC"))
  expect_equal(actual_ecdc, "2021-05-12 01:30:40 UTC")
  
  expect_error(covidHubUtils::date_to_datetime(date = "2021-05-12 01:30:40 EST", hub = c("US")))
  expect_error(covidHubUtils::date_to_datetime(date = "2021-05-12 01:30:40 CET", hub = c("ECDC")))
})

