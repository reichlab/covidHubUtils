context("get_model_designations")
library(covidHubUtils)
library(dplyr)

test_that("get_model_designations works: local directory, all models", {
  actual <- covidHubUtils::get_model_designations(
    source = "local_hub_repo",
    hub_repo_path = "test-data/test-get_model_designations"
  )

  expected <- data.frame(
    model = c(
      "COVIDhub-baseline", "COVIDhub-ensemble", "teamA-modelA",
      "teamB-modelB", "teamC-modelC", "teamD-modelD", "teamE-modelE"
    ),
    designation = c(
      "proposed", "primary", "primary", "secondary", "proposed",
      "other", "other"
    ),
    stringsAsFactors = FALSE
  )

  expect_equal(actual, expected)
})

test_that("get_model_designations works: local directory, specified models", {
  actual <- covidHubUtils::get_model_designations(
    source = "local_hub_repo",
    models = c("COVIDhub-baseline", "COVIDhub-ensemble", "teamA-modelA"),
    hub_repo_path = "test-data/test-get_model_designations"
  )

  expected <- data.frame(
    model = c("COVIDhub-baseline", "COVIDhub-ensemble", "teamA-modelA"),
    designation = c("proposed", "primary", "primary"),
    stringsAsFactors = FALSE
  )

  expect_equal(actual, expected)
})


test_that("get_model_designations works: zoltar, specified models", {
  actual <- covidHubUtils::get_model_designations(
    source = "zoltar",
    models = c("COVIDhub-baseline", "COVIDhub-ensemble")
  )

  expected <- data.frame(
    model = c("COVIDhub-baseline", "COVIDhub-ensemble"),
    designation = c("secondary", "primary"),
    stringsAsFactors = FALSE
  )

  expect_true(dplyr::all_equal(actual, expected))
})

test_that("get_model_designations work: local hub repo, space in hub repo path", {
  actual <- covidHubUtils::get_model_designations(
    source = "local_hub_repo",
    hub_repo_path = "test-data/test-get_model_designations folder",
    models = c("teamA-modelA", "teamB-modelB")
  )

  expected <- data.frame(
    model = c("teamA-modelA", "teamB-modelB"),
    designation = c("primary", "secondary"),
    stringsAsFactors = FALSE
  )

  expect_true(dplyr::all_equal(actual, expected))
})
