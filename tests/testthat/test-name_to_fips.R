context("name_to_fips")
library(covidHubUtils)
library(dplyr)
library(testthat)
library(lubridate)
library(stringr)

test_that("name_to_fips works as expected",{
  test_data_US <- c("Hampshire County, MA","Bullock County, AL","New Jersey","United States")
  expected_data_US <- c("US","34","01011","25015")
  function_output <- name_to_fips(test_data_US, hub = c("US"))
  #Assert statements to test the change
  expect_equal(function_output , expected_data_US)
  
  test_data_ECDC <- c("Belgium","Spain","Italy","United Kingdom")
  expected_data_ECDC <- c("BE","ES","IT","GB")
  function_output <- name_to_fips(test_data_ECDC, hub = c("ECDC"))
  #Assert statements to test the change
  expect_equal(function_output , expected_data_ECDC)
  
})

test_that("Invalid input should throw error",{
  test_data_US <- c("Hampshre, MA","Bullock County","Nw Jeey","Uned States")
  expect_error(name_to_fips(test_data_US, hub = c("US")))
  
  test_data_ECDC <- c("Begium","Sain","I-taly","Unted Kingdom")
  expect_error(name_to_fips(test_data_ECDC, hub = c("ECDC")))
}
)


test_that("NULL input should throw NULL",{
  test_data = NULL
  expect_null(name_to_fips(test_data, hub = "US"))
  
})

test_that("IF Fips codes are entered, returns fips",{
  test_data_US <- c("Hampshire County, MA","01001","New Jersey","United States")
  expected_data_US <- c("US","34","25015","01001")
  function_output <- name_to_fips(test_data_US, hub = c("US"))
  #Assert statements to test the change
  expect_equal(function_output , expected_data_US)
  
  test_data_ECDC <- c("BE","Spain","IT","United Kingdom")
  expected_data_ECDC <- c("ES","GB","BE","IT")
  function_output <- name_to_fips(test_data_ECDC, hub = c("ECDC"))
  #Assert statements to test the change
  expect_equal(function_output , expected_data_ECDC)
}
)

