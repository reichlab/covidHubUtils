context("as_of_functioning")
library(covidHubUtils)
library(dplyr)
library(testthat)


test_that("load_forecasts uses as_of correctly in US hub",{
  #Version with as_of
  version1 <- covidHubUtils::load_forecasts(models = "Columbia_UNC-SurvCon", 
                                            forecast_dates = "2021-01-03", 
                                            as_of = "2021-01-04")
  #Version without as_of
  version2 <- covidHubUtils::load_forecasts(models = "Columbia_UNC-SurvCon", 
                                            forecast_dates = "2021-01-03")
  
  #Data processing to check the valid differences
  version2 <- left_join(version2,
                        version1,
                        by = c("model", "forecast_date", "location", "horizon", 
                               "temporal_resolution", "target_variable", "target_end_date",
                               "type", "quantile", "location_name", "population", "geo_type", 
                               "geo_value", "abbreviation"))
  version2 <- version2 %>% mutate( diff = value.x-value.y)
  
  #Assert statements to test the change
  expect_true(all(version2[version2$diff!=0,]$location == 'US'))
  expect_true(all(version2[version2$diff!=0,]$target_variable == 'inc death'))
})


test_that("load_latest_forecasts uses as_of correctly in US hub",{
  
  #Version with as_of
  version1 <- covidHubUtils::load_latest_forecasts(models = "Columbia_UNC-SurvCon",
                                                   last_forecast_date = "2021-01-03", 
                                                   source = 'zoltar',
                                                   as_of = "2021-01-04")
  #Version without as_of
  version2 <- covidHubUtils::load_latest_forecasts(models = "Columbia_UNC-SurvCon", 
                                                   last_forecast_date = "2021-01-03",
                                                   source = 'zoltar')
  
  #Data processing to check the valid differences
  version2 <- left_join(version2,
                        version1, 
                        by = c("model", "forecast_date", "location", "horizon",
                               "temporal_resolution", "target_variable", "target_end_date", 
                               "type", "quantile", "location_name", "population", "geo_type", 
                               "geo_value", "abbreviation"))
  version2 <- version2 %>% mutate( diff = value.x-value.y)
  
  #Assert statements to test the change
  expect_true(all(version2[version2$diff!=0,]$location == 'US'))
  expect_true(all(version2[version2$diff!=0,]$target_variable == 'inc death'))
  
})

test_that("load_forecasts uses as_of correctly in ECDC hub",{
  #Version with as_of
  version1 <- covidHubUtils::load_forecasts(models = "BIOCOMSC-Gompertz", 
                                            forecast_dates = "2021-04-26", 
                                            as_of = "2021-05-02 11:00:00 UTC+1", 
                                            hub = c("ECDC"))
  #Version without as_of
  version2 <- covidHubUtils::load_forecasts(models = "BIOCOMSC-Gompertz",
                                            forecast_dates = "2021-04-26", 
                                            hub = c("ECDC"))
  
  expect_true(nrow(version1) == 0)
  expect_true(nrow(version2) != 0)
})

test_that("load_latest_forecasts uses as_of correctly in ECDC hub",{
  #Version with as_of
  version1 <- covidHubUtils::load_latest_forecasts(models = "BIOCOMSC-Gompertz", 
                                                   last_forecast_date = "2021-04-26", 
                                                   as_of = "2021-05-02", 
                                                   hub = c("ECDC"),
                                                   source = 'zoltar')
  #Version without as_of
  version2 <- covidHubUtils::load_latest_forecasts(models = "BIOCOMSC-Gompertz",
                                                   last_forecast_date = "2021-04-26", 
                                                   hub = c("ECDC"),
                                                   source = 'zoltar')
  
  expect_true(nrow(version1) == 0)
  expect_true(nrow(version2) != 0)
})

