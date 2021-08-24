context("load_forecasts")

library(covidHubUtils)
library(dplyr)
library(lubridate)

# Need to change these to valid paths
# path from your covidHubUtils folder to your local clone of zoltpy
local_zoltpy_path <- NA
# path from your local clone of zoltpy to
# covidHubUtils/tests/testthat/test-data/test-load_forecasts_local_zoltar/db.sqlite3
zoltar_module_path <- NA

skip_if_no_zoltpy_or_sqlite <- function() {
  if (!is.na(local_zoltpy_path) & !is.na(zoltar_module_path)) {
    if (!dir.exists(local_zoltpy_path) |
      !file.exists(paste(local_zoltpy_path, zoltar_module_path, sep = "/"))) {
      skip("zoltpy or sqlite data not available")
    }
  } else {
    skip("zoltpy or sqlite data not available")
  }
}

test_that("load_forecast from local zoltar works with window size", {
  skip_if_no_zoltpy_or_sqlite()
  all_forecasts <- covidHubUtils::load_forecasts(
    models = c("COVIDhub-ensemble", "COVIDhub-baseline"),
    dates = c("2021-07-27", "2021-08-03"),
    date_window_size = 1,
    locations = "US",
    source = "local_zoltar",
    local_zoltpy_path = local_zoltpy_path,
    zoltar_module_path = zoltar_module_path
  )

  expect_identical(
    all_forecasts %>%
      dplyr::distinct(model, forecast_date) %>%
      dplyr::arrange(model, forecast_date),
    tidyr::expand_grid(
      model = c("COVIDhub-baseline", "COVIDhub-ensemble"),
      forecast_date = lubridate::ymd(c("2021-07-26", "2021-08-02"))
    )
  )

  expect_true(unique(all_forecasts$location) == "US")
})

test_that("load_forecast from local zoltar works with one dates not available", {
  skip_if_no_zoltpy_or_sqlite()
  all_forecasts <- covidHubUtils::load_forecasts(
    models = c("COVIDhub-ensemble", "COVIDhub-baseline"),
    dates = c("2021-07-27", "2021-08-04"),
    date_window_size = 1,
    locations = "US",
    source = "local_zoltar",
    local_zoltpy_path = local_zoltpy_path,
    zoltar_module_path = zoltar_module_path
  )

  expect_identical(
    all_forecasts %>%
      dplyr::distinct(model, forecast_date) %>%
      dplyr::arrange(model, forecast_date),
    tidyr::expand_grid(
      model = c("COVIDhub-baseline", "COVIDhub-ensemble"),
      forecast_date = lubridate::ymd(c("2021-07-26"))
    )
  )

  expect_true(unique(all_forecasts$location) == "US")
})
