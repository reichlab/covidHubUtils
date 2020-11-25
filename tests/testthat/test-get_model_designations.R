context("get_model_designations")
library(covidHubUtils)
library(dplyr)

# test_that("get_model_designations works: local directory, all models", {
#   actual <- covidHubUtils::get_model_designations(
#     source = "local_hub_repo",
#     hub_repo_path = "test-data/test-get_model_designations"
#   )
# 
#   expected <- data.frame(
#     model = c("COVIDhub-baseline", "COVIDhub-ensemble", "teamA-modelA",
#       "teamB-modelB", "teamC-modelC", "teamD-modelD"),
#     designation = c("proposed", "primary", "primary", "secondary", "proposed",
#       "other")
#   )
#   
#   expect_equal(actual, expected)
# })
# 
# test_that("get_model_designations works: local directory, specified models", {
#   actual <- covidHubUtils::get_model_designations(
#     source = "local_hub_repo",
#     models = c("COVIDhub-baseline", "COVIDhub-ensemble", "teamA-modelA"),
#     hub_repo_path = "test-data/test-get_model_designations"
#   )
# 
#   expected <- data.frame(
#     model = c("COVIDhub-baseline", "COVIDhub-ensemble", "teamA-modelA"),
#     designation = c("proposed", "primary", "primary")
#   )
#   
#   expect_equal(actual, expected)
# })
# 

test_that("get_model_designations works: zoltar, specified models", {
  actual <- covidHubUtils::get_model_designations(
    source = "zoltar",
    models = c("COVIDhub-baseline", "COVIDhub-ensemble")
  )
  
  expected <- data.frame(
    model = c("COVIDhub-baseline", "COVIDhub-ensemble"),
    designation = c("secondary", "primary")
  )
  
  expect_true(dplyr::all_equal(actual, expected))
})


test_that("get_model_designations work: local hub repo, specified models, versioned",{
  actual <- covidHubUtils::get_model_designations(
    source = "local_hub_repo",
    hub_repo_path = "../../../covid19-forecast-hub",
    models = c("USC-SI_kJalpha","USC-SI_kJalpha_RF"),
    as_of = as.Date('2020-11-03')
  )
  
  expected <- data.frame(
    model = c("USC-SI_kJalpha","USC-SI_kJalpha_RF"),
    designation = c("other", "primary")
  )
  
  expect_true(dplyr::all_equal(actual, expected))
})

test_that("get_model_designations work: local hub repo, specified models, versioned",{
  actual <- covidHubUtils::get_model_designations(
    source = "local_hub_repo",
    hub_repo_path = "../../../covid19-forecast-hub",
    models = c("USC-SI_kJalpha","USC-SI_kJalpha_RF"),
    as_of = Sys.Date()
  )
  
  expected <- data.frame(
    model = c("USC-SI_kJalpha","USC-SI_kJalpha_RF"),
    designation = c("primary", "other")
  )
  
  expect_true(dplyr::all_equal(actual, expected))
})
