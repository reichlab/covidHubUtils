library(covidHubUtils)
library(dplyr)
library(lubridate)
library(stringr)

test_that("incorrectly-specified approximation rule should throw an error", {
  f_vector <- seq(10,225,25)
  tau_f <- tau_g <- seq(0.1,0.9,0.1)
  g_vector <- c(seq(5,20,5),seq(30,150,30))
  expect_error(pairwise_cd_unequal(f_vector,tau_f,g_vector,tau_g,"thumb_rule"))
})

test_that("unequal lengths of a quantile vector and corresponding probability levels should throw an error", {
  f_vector <- seq(10,225,25)
  tau_f <- seq(0.1,0.8,0.1)
  g_vector <- c(seq(5,15,5),seq(30,150,30))
  tau_g <- seq(0.1,0.9,0.1)
  expect_error(pairwise_cd_unequal(f_vector,tau_f,g_vector,tau_g, "left-sided"))
})

test_that("out-of-range values of probability levels should throw an error", {
  f_vector <- seq(10,225,25)
  tau_f <- c(seq(0.1,0.8,0.1),1.2)
  g_vector <- c(seq(5,15,5),seq(30,150,30))
  tau_g <- seq(0.1,0.9,0.1)
  expect_error(pairwise_cd_unequal(f_vector,tau_f,g_vector,tau_g, "left-sided"))
})

test_that("unequal quantile lengths should give a warning", {
  f_vector <- seq(10,225,25)
  tau_f <- seq(0.1,0.9,0.1)
  g_vector <- c(seq(5,15,5),seq(30,150,30))
  tau_g <- seq(0.1,0.8,0.1)
  expect_warning(pairwise_cd_unequal(f_vector,tau_f,g_vector,tau_g, "left-sided"))
})

test_that("non-increasing order of quantiles should give a warning", {
  f_vector <- seq(10,225,25)
  tau_f <- seq(0.1,0.9,0.1)
  g_vector <- c(seq(30,150,30),seq(5,20,5))
  tau_g <- seq(0.1,0.9,0.1)
  expect_warning(pairwise_cd_unequal(f_vector,tau_f,g_vector,tau_g, "left-sided"))
})

test_that("non-increasing order of probability level should give a warning", {
  f_vector <- seq(10,225,25)
  tau_f <- seq(0.1,0.9,0.1)
  g_vector <- c(seq(5,20,5),seq(30,150,30))
  tau_g <- c(0.9,seq(0.1,0.8,0.1))
  expect_warning(pairwise_cd_unequal(f_vector,tau_f,g_vector,tau_g, "left-sided"))
})

test_that("left-sided approximation is reasonalble for equally-spaced intervals", {
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
  approx_cd <- pairwise_cd_unequal(f_vector,tau,g_vector,tau, "left-sided")
  # arbitrary threshold
  expected <- 0.05
  actual <- abs(true_cd - approx_cd)
  expect_lte(actual, expected)
})

test_that("left-sided approximation is reasonalble for unequally-spaced intervals", {
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
  approx_cd <- pairwise_cd_unequal(f_vector,tau,g_vector,tau, "left-sided")
  # arbitrary threshold
  expected <- 0.05
  actual <- abs(true_cd - approx_cd)
  expect_lte(actual, expected)
})

test_that("trapezoid approximation is reasonalble for equally-spaced intervals", {
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
  approx_cd <- pairwise_cd_unequal(f_vector,tau,g_vector,tau, "trapezoid")
  # arbitrary threshold
  expected <- 0.05
  actual <- abs(true_cd - approx_cd)
  expect_lte(actual, expected)
})

test_that("trapezoid approximation is reasonalble for unequally-spaced intervals", {
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
  approx_cd <- pairwise_cd_unequal(f_vector,tau,g_vector,tau, "trapezoid")
  # arbitrary threshold
  expected <- 0.05
  actual <- abs(true_cd - approx_cd)
  expect_lte(actual, expected)
})