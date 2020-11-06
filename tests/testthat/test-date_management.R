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

