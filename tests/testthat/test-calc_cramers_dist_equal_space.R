library(covidHubUtils)
library(dplyr)
library(lubridate)
library(stringr)

test_that("incorrectly-specified approximation rule should throw an error", {
  f_vector <- seq(10,250,25)
  tau_f <-  tau_g <- seq(0.1,0.9,0.1)
  g_vector <- c(seq(5,25,5),seq(30,150,30))
  expect_error(calc_cramers_dist_equal_space(f_vector,tau_f,g_vector,tau_g,"thumb_rule"))
})

test_that("non-increasing order of probability level should give a warning", {
  f_vector <- g_vector <- seq(10,225,25)
  tau_f <- seq(0.1,0.9,0.1)
  tau_g <- c(0.9,seq(0.1,0.8,0.1))
  expect_warning(calc_cramers_dist_equal_space(f_vector,tau_f,g_vector,tau_g, "approximation1"))
})

test_that("non-increasing order of quantiles should give a warning", {
  f_vector <- seq(10,225,25)
  tau_f <-tau_g <- seq(0.1,0.9,0.1)
  g_vector <- c(seq(30,150,30),seq(5,20,5))
  expect_warning(calc_cramers_dist_equal_space(f_vector,tau_f,g_vector,tau_g, "approximation1"))
})

test_that("unequal quantile lengths should throw an error", {
  f_vector <- seq(10,225,25)
  tau_f <- tau_g <- seq(0.1,0.9,0.1)
  g_vector <- c(seq(5,25,5),seq(30,150,30))
  expect_error(calc_cramers_dist_equal_space(f_vector,tau_f,g_vector,tau_g, "approximation1"))
})

test_that("unequal probability level lengths should throw an error", {
  f_vector <- seq(10,225,25)
  g_vector <- c(seq(5,25,5),seq(30,150,30))
  tau_f <- seq(0.1,0.9,0.1)
  tau_g <- seq(0.1,1,0.1)
  expect_error(calc_cramers_dist_equal_space(f_vector,tau_f,g_vector,tau_g, "approximation1"))
})

test_that("unequal lengths of a quantile vector and corresponding probability levels should throw an error", {
  f_vector <- seq(10,225,25)
  tau_f <- seq(0.1,0.8,0.1)
  g_vector <- c(seq(5,15,5),seq(30,150,30))
  tau_g <- seq(0.1,0.9,0.1)
  expect_error(calc_cramers_dist_equal_space(f_vector,tau_f,g_vector,tau_g, "approximation1"))
})

test_that("different probability level values from F and G should throw an error", {
  f_vector <- seq(10,225,25)
  g_vector <- c(seq(5,25,5),seq(30,150,30))
  tau_f <- seq(0.1,0.9,0.1)
  tau_g <- seq(0.2,1,0.1)
  expect_error(calc_cramers_dist_equal_space(f_vector,tau_f,g_vector,tau_g, "approximation1"))
})

test_that("out-of-range values of probability levels should throw an error", {
  f_vector <- seq(10,225,25)
  tau_f <- c(seq(0.1,0.8,0.1),1.2)
  g_vector <- c(seq(5,15,5),seq(30,150,30))
  tau_g <- seq(0.1,0.9,0.1)
  expect_error(calc_cramers_dist_equal_space(f_vector,tau_f,g_vector,tau_g, "approximation1"))
})

test_that("unequally-spaced probability levels should give a warning", {
  f_vector <- g_vector <- seq(10,225,25)
  tau_f <- tau_g <- c(seq(0.1,0.8,0.1),0.95)
  expect_warning(calc_cramers_dist_equal_space(f_vector,tau_f,g_vector,tau_g, "approximation1"))
})


test_that("The approximation based on approximation1 is reasonable", {
  a1 <- a2 <- 0
  b1 <- 1
  b2 <- 2
  # calculate distance from the integral
  integrand <- function(x) {(punif(x,a1,b1)-punif(x,a2,b2))^2}
  true_cd<-integrate(integrand, lower = 0, upper = 2)$value
  # quantile representation
  # probability level
  tau <- seq(0.1,0.9,0.01)
  # quantiles
  f_vector <- qunif(tau,0,1)
  g_vector <- qunif(tau,0,2)
  approx_cd <- calc_cramers_dist_equal_space(f_vector,tau,g_vector,tau, "approximation1")
  # arbitrary threshold
  expected <- 0.05
  actual <- abs(true_cd - approx_cd)
  expect_lte(actual, expected)
})

test_that("The approximation based on approximation1 gets better as k increases", {
  a1 <- a2 <- 0
  b1 <- 1
  b2 <- 2
  # calculate distance from the integral
  integrand <- function(x) {(punif(x,a1,b1)-punif(x,a2,b2))^2}
  true_cd<-integrate(integrand, lower = 0, upper = 2)$value
  # quantile representation
  # probability level
  tau1 <- seq(0.1,0.9,0.1)
  tau2 <- seq(0.1,0.9,0.001)
  # quantiles for small k
  f_vector1 <- qunif(tau1,0,1)
  g_vector1 <- qunif(tau1,0,2)
  approx_cd1 <- calc_cramers_dist_equal_space(f_vector1,tau1,g_vector1,tau1, "approximation1")
  # quantiles for large k
  f_vector2 <- qunif(tau2,0,1)
  g_vector2 <- qunif(tau2,0,2)
  approx_cd2 <- calc_cramers_dist_equal_space(f_vector2,tau2,g_vector2,tau2, "approximation1")
  # check that the approx. cd of large k is closer to true cd
  diff1 <- abs(true_cd - approx_cd1)
  diff2 <- abs(true_cd - approx_cd2)
  expect_lte(diff2,diff1)
})

test_that("The approximation based on approximation2 is reasonable", {
  a1 <- a2 <- 0
  b1 <- 1
  b2 <- 2
  # calculate distance from the integral
  integrand <- function(x) {(punif(x,a1,b1)-punif(x,a2,b2))^2}
  true_cd<-integrate(integrand, lower = 0, upper = 2)$value
  # quantile representation
  # probability level
  tau <- seq(0.1,0.9,0.01)
  # quantiles
  f_vector <- qunif(tau,0,1)
  g_vector <- qunif(tau,0,2)
  approx_cd <- calc_cramers_dist_equal_space(f_vector,tau,g_vector,tau, "approximation2")
  # arbitrary threshold
  expected <- 0.05
  actual <- abs(true_cd - approx_cd)
  expect_lte(actual, expected)
})

test_that("The approximation based on approximation2 is better than that of approximation1 for a small k", {
  a1 <- a2 <- 0
  b1 <- 1
  b2 <- 2
  # calculate distance from the integral
  integrand <- function(x) {(punif(x,a1,b1)-punif(x,a2,b2))^2}
  true_cd<-integrate(integrand, lower = 0, upper = 2)$value
  # quantile representation
  # probability level
  tau <- seq(0.1,0.9,0.1)
  # quantiles for small k
  f_vector <- qunif(tau,0,1)
  g_vector <- qunif(tau,0,2)
  approx_cd1 <- calc_cramers_dist_equal_space(f_vector,tau,g_vector,tau, "approximation1")
  approx_cd2 <- calc_cramers_dist_equal_space(f_vector,tau,g_vector,tau, "approximation2")
  # check that the approx. cd of large k is closer to true cd
  diff1 <- abs(true_cd - approx_cd1)
  diff2 <- abs(true_cd - approx_cd2)
  expect_lte(diff2,diff1)
})

test_that("approximation1 is symmetric", {
  # step function
  f_vector <- seq(2/8,10/8,1/8)
  g_vector <- seq(1/8,9/8,1/8)
  # probability level
  tau <- seq(0.1,0.9,0.1) 
  # get distance
  expected <- calc_cramers_dist_equal_space(g_vector,tau,f_vector,tau, "approximation1")
  actual <- calc_cramers_dist_equal_space(f_vector,tau,g_vector,tau, "approximation1")
  expect_equal(actual, expected)
})

test_that("approximation2 is symmetric", {
  # step function
  f_vector <- seq(2/8,10/8,1/8)
  g_vector <- seq(1/8,9/8,1/8)
  # probability level
  tau <- seq(0.1,0.9,0.1) 
  # get distance
  expected <- calc_cramers_dist_equal_space(g_vector,tau,f_vector,tau, "approximation2")
  actual <- calc_cramers_dist_equal_space(f_vector,tau,g_vector,tau, "approximation2")
  expect_equal(actual, expected)
})

test_that("approximation1 yields 0 distance from a function to itself", {
  # step function
  f_vector <- g_vector <- seq(2/8,10/8,1/8)
  # probability level
  tau <- seq(0.1,0.9,0.1) 
  # get distance
  expected <- 0
  actual <- calc_cramers_dist_equal_space(f_vector,tau,g_vector,tau, "approximation1")
  expect_equal(actual, expected)
})

test_that("approximation2 yields 0 distance from a function to itself", {
  # step function
  f_vector <- g_vector <- seq(2/8,10/8,1/8)
  # probability level
  tau <- seq(0.1,0.9,0.1) 
  # get distance
  expected <- 0
  actual <- calc_cramers_dist_equal_space(f_vector,tau,g_vector,tau, "approximation2")
  expect_equal(actual, expected)
})
