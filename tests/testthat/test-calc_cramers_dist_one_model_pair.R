library(covidHubUtils)
library(dplyr)
library(lubridate)
library(stringr)

test_that("incorrectly-specified approximation rule should throw an error", {
  f_vector <- g_vector <- seq(10,225,25)
  tau_f <- tau_g <- seq(0.1,0.9,0.1)
  expect_error(calc_cramers_dist_one_model_pair(f_vector,tau_f,g_vector,tau_g,"thumb_rule"))
})

test_that("specifying more than one approximation rule should throw an error", {
  f_vector <- g_vector <- seq(10,225,25)
  tau_f <- tau_g <- seq(0.1,0.9,0.1)
  expect_error(calc_cramers_dist_one_model_pair(f_vector,tau_f,g_vector,tau_g,c("approximation1","approximation2")))
})
