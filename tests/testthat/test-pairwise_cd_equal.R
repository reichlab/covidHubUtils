library(covidHubUtils)
library(dplyr)
library(lubridate)
library(stringr)

test_that("incorrectly-specified approximation rule should throw an error", {
  f_vector <- seq(10,250,25)
  g_vector <- c(seq(5,25,5),seq(30,150,30))
  expect_error(pairwise_cd_equal(f_vector,g_vector, "thumb_rule"))
})

test_that("unequal quantile lengths should throw an error", {
  f_vector <- seq(10,225,25)
  g_vector <- c(seq(5,25,5),seq(30,150,30))
  expect_error(pairwise_cd_equal(f_vector,g_vector, "first_rule"))
})

test_that("non-increasing order of quantiles should give a warning", {
  f_vector <- seq(10,250,25)
  g_vector <- c(seq(30,150,30),seq(5,25,5))
  expect_warning(pairwise_cd_equal(f_vector,g_vector, "second_rule"))
})

test_that("first_rule approximation is reasonalble", {
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
  approx_cd <- pairwise_cd_equal(f_vector,g_vector, "first_rule")
  # arbitrary threshold
  expected <- 0.05
  actual <- abs(true_cd - approx_cd)
  expect_lte(actual, expected)
})

test_that("first_rule approximation gets better as k increases", {
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
  approx_cd1 <- pairwise_cd_equal(f_vector1,g_vector1, "first_rule")
  # quantiles for large k
  f_vector2 <- qunif(tau2,0,1)
  g_vector2 <- qunif(tau2,0,2)
  approx_cd2 <- pairwise_cd_equal(f_vector2,g_vector2, "first_rule")
  # check that the approx. cd of large k is closer to true cd
  diff1 <- abs(true_cd - approx_cd1)
  diff2 <- abs(true_cd - approx_cd2)
  expect_lte(diff2,diff1)
})

test_that("second_rule approximation is reasonalble", {
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
  approx_cd <- pairwise_cd_equal(f_vector,g_vector, "second_rule")
  # arbitrary threshold
  expected <- 0.05
  actual <- abs(true_cd - approx_cd)
  expect_lte(actual, expected)
})

test_that("second_rule approximation is better than first_rule approximation for a small k", {
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
  approx_cd1 <- pairwise_cd_equal(f_vector,g_vector, "first_rule")
  approx_cd2 <- pairwise_cd_equal(f_vector,g_vector, "second_rule")
  # check that the approx. cd of large k is closer to true cd
  diff1 <- abs(true_cd - approx_cd1)
  diff2 <- abs(true_cd - approx_cd2)
  expect_lte(diff2,diff1)
})