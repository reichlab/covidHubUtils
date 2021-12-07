context("get_model_metadata")
library(covidHubUtils)
library(dplyr)

test_that("get_model_metadata works: local directory, all models", {
  actual <- covidHubUtils::get_model_metadata(
    source = "local_hub_repo",
    hub_repo_path = "test-data/test-get_model_designations"
  )

  expected <- data.frame(
    team_name = c("COVIDhub", "COVIDhub", rep("test team name", 5)),
    model_name = c("baseline", "ensemble", "A", "teamB", "C", "D", "E"),
    model = c(
      "COVIDhub-baseline", "COVIDhub-ensemble", "teamA-modelA",
      "teamB-modelB", "teamC-modelC", "teamD-modelD", "teamE-modelE"
    ),
    designation = c(
      "proposed", "primary", "primary", "secondary", "proposed",
      "other", "other"
    ),
    website_url = c(
      NA, "https://covid19forecasthub.org/", NA, NA, NA, NA, NA
    ),
    ensemble_of_hub_models = c(
      NA, TRUE, NA, NA, NA, NA, NA
      )
  )

  expect_equal(actual, expected)
})

test_that("get_model_metadata works: local directory, specified models", {
  actual <- covidHubUtils::get_model_metadata(
    source = "local_hub_repo",
    models = c("COVIDhub-baseline", "COVIDhub-ensemble", "teamA-modelA"),
    hub_repo_path = "test-data/test-get_model_designations"
  )

  expected <- data.frame(
    team_name = c("COVIDhub", "COVIDhub", "test team name"),
    model_name = c("baseline", "ensemble", "A"),
    model = c("COVIDhub-baseline", "COVIDhub-ensemble", "teamA-modelA"),
    designation = c("proposed", "primary", "primary"),
    website_url = c(NA, "https://covid19forecasthub.org/", NA),
    ensemble_of_hub_models = c(NA, TRUE, NA)
  )

  expect_equal(actual, expected)
})

test_that("get_model_metadata works: local hub repo, space in hub repo path", {
  actual <- covidHubUtils::get_model_metadata(
    source = "local_hub_repo",
    hub_repo_path = "test-data/test-get_model_designations folder",
    models = c("teamA-modelA", "teamB-modelB")
  )

  expected <- data.frame(
    team_name = c(rep("test team name", 2)),
    model_name = c("A", "teamB"),
    model = c("teamA-modelA", "teamB-modelB"),
    designation = c("primary", "secondary")
  )

  expect_true(dplyr::all_equal(actual, expected))
})
