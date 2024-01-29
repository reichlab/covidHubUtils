context("get_plot_forecast_data")
library(covidHubUtils)

test_that("test-get_plot_forecast_data: usaspending retired", {
  truth_source <- c("USAFacts")
  mock_forecasts <- data.frame(
    model = "baseball",
    forecast_date = as.Date("2024-10-31"),
    location = "US",
    target = "wins",
    type = "unclear",
    quantile = 0.5,
    value = 1,
    horizon = '1 yr',
    target_end_date = as.Date("2024-10-31")
  )
  expect_error(
    get_plot_forecast_data(
      forecast_data = mock_forecasts,
      truth_source = truth_source
    ), "USAFacts can no longer be downloaded"
  )
})
