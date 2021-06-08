library(covidHubUtils)
library(dplyr)
library(lubridate)
library(stringr)

test_that("incorrectly-specified approximation rule should throw an error", {
  f_vector <- seq(10,225,25)
  tau_f <- tau_g <- seq(0.1,0.9,0.1)
  g_vector <- c(seq(5,20,5),seq(30,150,30))
  expect_error(calc_cramers_dist_unequal_space(f_vector,tau_f,g_vector,tau_g,"thumb_rule"))
})

test_that("non-increasing order of quantiles should give a warning", {
  f_vector <- seq(10,225,25)
  tau_f <- seq(0.1,0.9,0.1)
  g_vector <- c(seq(30,150,30),seq(5,20,5))
  tau_g <- seq(0.1,0.9,0.1)
  expect_warning(calc_cramers_dist_unequal_space(f_vector,tau_f,g_vector,tau_g, "left_sided_riemann"))
})

test_that("non-increasing order of probability level should give a warning", {
  f_vector <- seq(10,225,25)
  tau_f <- seq(0.1,0.9,0.1)
  g_vector <- c(seq(5,20,5),seq(30,150,30))
  tau_g <- c(0.9,seq(0.1,0.8,0.1))
  expect_warning(calc_cramers_dist_unequal_space(f_vector,tau_f,g_vector,tau_g, "left_sided_riemann"))
})

test_that("unequal lengths of a quantile vector and corresponding probability levels should throw an error", {
  f_vector <- seq(10,225,25)
  tau_f <- seq(0.1,0.8,0.1)
  g_vector <- c(seq(5,15,5),seq(30,150,30))
  tau_g <- seq(0.1,0.9,0.1)
  expect_error(calc_cramers_dist_unequal_space(f_vector,tau_f,g_vector,tau_g, "left_sided_riemann"))
})

test_that("out-of-range values of probability levels should throw an error", {
  f_vector <- seq(10,225,25)
  tau_f <- c(seq(0.1,0.8,0.1),1.2)
  g_vector <- c(seq(5,15,5),seq(30,150,30))
  tau_g <- seq(0.1,0.9,0.1)
  expect_error(calc_cramers_dist_unequal_space(f_vector,tau_f,g_vector,tau_g, "left_sided_riemann"))
})

test_that("unequal quantile lengths should give a warning", {
  f_vector <- seq(10,225,25)
  tau_f <- seq(0.1,0.9,0.1)
  g_vector <- c(seq(5,15,5),seq(30,150,30))
  tau_g <- seq(0.1,0.8,0.1)
  expect_message(calc_cramers_dist_unequal_space(f_vector,tau_f,g_vector,tau_g, "left_sided_riemann"))
})

test_that("left_sided_riemann approximation is reasonable for equally-spaced intervals", {
  a1 <- a2 <- 0
  b1 <- 1
  b2 <- 2
  # calculate distance from the integral
  integrand <- function(x) {(punif(x,a1,b1)-punif(x,a2,b2))^2}
  true_cd<-integrate(integrand, lower = 0, upper = 2)$value
  # quantile representation
  # probability level
  tau <- seq(0.1,0.9,0.1)
  # quantiles
  f_vector <- qunif(tau,0,1)
  g_vector <- qunif(tau,0,2)
  approx_cd <- calc_cramers_dist_unequal_space(f_vector,tau,g_vector,tau, "left_sided_riemann")
  # arbitrary threshold
  expected <- 0.05
  actual <- abs(true_cd - approx_cd)
  expect_lte(actual, expected)
})

test_that("left_sided_riemann approximation is reasonalble for unequally-spaced intervals", {
  a1 <- a2 <- 0
  b1 <- 1
  b2 <- 2
  # calculate distance from the integral
  integrand <- function(x) {(punif(x,a1,b1)-punif(x,a2,b2))^2}
  true_cd<-integrate(integrand, lower = 0, upper = 2)$value
  # quantile representation
  # probability level
  tau <- c(0.05,0.15,seq(0.2,0.8,0.1),0.85,0.95)
  # quantiles
  f_vector <- qunif(tau,0,1)
  g_vector <- qunif(tau,0,2)
  approx_cd <- calc_cramers_dist_unequal_space(f_vector,tau,g_vector,tau, "left_sided_riemann")
  # arbitrary threshold
  expected <- 0.05
  actual <- abs(true_cd - approx_cd)
  expect_lte(actual, expected)
})

test_that("trapezoid_riemann approximation is reasonable for equally-spaced intervals", {
  a1 <- a2 <- 0
  b1 <- 1
  b2 <- 2
  # calculate distance from the integral
  integrand <- function(x) {(punif(x,a1,b1)-punif(x,a2,b2))^2}
  true_cd<-integrate(integrand, lower = 0, upper = 2)$value
  # quantile representation
  # probability level
  tau <- seq(0.1,0.9,0.1)
  # quantiles
  f_vector <- qunif(tau,0,1)
  g_vector <- qunif(tau,0,2)
  approx_cd <- calc_cramers_dist_unequal_space(f_vector,tau,g_vector,tau, "trapezoid_riemann")
  # arbitrary threshold
  expected <- 0.05
  actual <- abs(true_cd - approx_cd)
  expect_lte(actual, expected)
})

test_that("trapezoid_riemann approximation is reasonable for unequally-spaced intervals", {
  a1 <- a2 <- 0
  b1 <- 1
  b2 <- 2
  # calculate distance from the integral
  integrand <- function(x) {(punif(x,a1,b1)-punif(x,a2,b2))^2}
  true_cd<-integrate(integrand, lower = 0, upper = 2)$value
  # quantile representation
  # probability level
  tau <- c(0.05,0.15,seq(0.2,0.8,0.1),0.85,0.95)
  # quantiles
  f_vector <- qunif(tau,0,1)
  g_vector <- qunif(tau,0,2)
  approx_cd <- calc_cramers_dist_unequal_space(f_vector,tau,g_vector,tau, "trapezoid_riemann")
  # arbitrary threshold
  expected <- 0.05
  actual <- abs(true_cd - approx_cd)
  expect_lte(actual, expected)
})

test_that("left_sided_riemann approximation behaves as expected for step functions", {
  # step function
  f_vector <- seq(2/8,10/8,1/8)
  g_vector <- seq(1/8,9/8,1/8)
  # probability level
  tau <- seq(0.1,0.9,0.1) 
  # get distance
  expected <- sum((f_vector-g_vector)*rep(0.1,9)^2)
  actual <- calc_cramers_dist_unequal_space(f_vector,tau,g_vector,tau, "left_sided_riemann")
  expect_equal(actual, expected)
})

test_that("left_sided_riemann approximation is symmetric", {
  # step function
  f_vector <- seq(2/8,10/8,1/8)
  g_vector <- seq(1/8,9/8,1/8)
  # probability level
  tau <- seq(0.1,0.9,0.1) 
  # get distance
  expected <- calc_cramers_dist_unequal_space(g_vector,tau,f_vector,tau, "left_sided_riemann")
  actual <- calc_cramers_dist_unequal_space(f_vector,tau,g_vector,tau, "left_sided_riemann")
  expect_equal(actual, expected)
})

test_that("left_sided_riemann yields 0 distance from a function to itself", {
  # step function
  f_vector <- g_vector <- seq(2/8,10/8,1/8)
  # probability level
  tau <- seq(0.1,0.9,0.1) 
  # get distance
  expected <- 0
  actual <- calc_cramers_dist_unequal_space(f_vector,tau,g_vector,tau, "left_sided_riemann")
  expect_equal(actual, expected)
})

test_that("trapezoid_riemann approximation is symmetric", {
  # step function
  f_vector <- seq(2/8,10/8,1/8)
  g_vector <- seq(1/8,9/8,1/8)
  # probability level
  tau <- seq(0.1,0.9,0.1) 
  # get distance
  expected <- calc_cramers_dist_unequal_space(g_vector,tau,f_vector,tau , "trapezoid_riemann")
  actual <- calc_cramers_dist_unequal_space(f_vector,tau,g_vector,tau, "trapezoid_riemann")
  expect_equal(actual, expected)
})

test_that("trapezoid_riemann yields 0 distance from a function to itself", {
  # step function
  f_vector <- g_vector <- seq(2/8,10/8,1/8)
  # probability level
  tau <- seq(0.1,0.9,0.1) 
  # get distance
  expected <- 0
  actual <- calc_cramers_dist_unequal_space(f_vector,tau,g_vector,tau, "trapezoid_riemann")
  expect_equal(actual, expected)
})
