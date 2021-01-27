library(covidHubUtils)
library(dplyr)
library(lubridate)

test_that("missing forecasts should throw error", {
  test_truth <- data.frame(
    model = c("source1", "source1"),
    target_variable = c("inc death", "inc death"),
    target_end_date = c(ymd(20200101), ymd(20200108)),
    location = c("01", "01"),
    value = c(1, 2)
  )
  
  expect_error(score_forecasts(truth = test_truth))
})

test_that("null forecasts should throw error", {
  test_truth <- data.frame(
    model = c("source1", "source1"),
    target_variable = c("inc death", "inc death"),
    target_end_date = c(ymd(20200101), ymd(20200108)),
    location = c("01", "01"),
    value = c(1, 2)
  )
  
  expect_error(score_forecasts(forecasts = NULL, truth = test_truth))
})

test_that("incorrectly-formatted forecasts should throw error", {
  test_truth <- data.frame(
    model = c("source1", "source1"),
    target_variable = c("inc death", "inc death"),
    target_end_date = c(ymd(20200101), ymd(20200108)),
    location = c("01", "01"),
    value = c(1, 2),
    stringsAsFactors = FALSE
  )
  
  malformed_forecasts <- data.frame(
    wrong_column_name_1 = c(1:5),
    wrong_column_name_2 = c(2:6),
    wrong_column_name_3 = c(3:7),
    stringsAsFactors = FALSE
  )
  
  expect_error(score_forecasts(forecasts = malformed_forecasts, truth = test_truth))
})

test_that("missing truth should throw error", {
  test_forecasts <- data.frame(
    model = c("source1", "source2"),
    forecast_date = c(ymd(20200101), ymd(20200101)),
    location = c("01", "01"),
    horizon = c("1", "1"),
    temporal_resolution = c("wk", "wk"),
    target_variable = c("inc death", "inc death"),
    target_end_date = c(ymd(20200108), ymd(20200108)),
    type = c("point", "point"),
    quantile = c(NA, NA),
    value = c(3, 4),
    stringsAsFactors = FALSE
  )
  
  expect_error(score_forecasts(forecasts = test_forecasts))
})

test_that("null truth should throw error", {
  test_forecasts <- data.frame(
    model = c("source1", "source2"),
    forecast_date = c(ymd(20200101), ymd(20200101)),
    location = c("01", "01"),
    horizon = c("1", "1"),
    temporal_resolution = c("wk", "wk"),
    target_variable = c("inc death", "inc death"),
    target_end_date = c(ymd(20200108), ymd(20200108)),
    type = c("point", "point"),
    quantile = c(NA, NA),
    value = c(3, 4),
    stringsAsFactors = FALSE
  )
  
  expect_error(score_forecasts(forecasts = test_forecasts, truth = NULL))
})

test_that("incorrectly-formatted truth should throw error", {
  test_forecasts <- data.frame(
    model = c("source1", "source2"),
    forecast_date = c(ymd(20200101), ymd(20200101)),
    location = c("01", "01"),
    horizon = c("1", "1"),
    temporal_resolution = c("wk", "wk"),
    target_variable = c("inc death", "inc death"),
    target_end_date = c(ymd(20200108), ymd(20200108)),
    type = c("point", "point"),
    quantile = c(NA, NA),
    value = c(3, 4),
    stringsAsFactors = FALSE
  )
  
  malformed_truth <- data.frame(
    wrong_colname_1 = c(NA, NA),
    wrong_colname_2 = c(NA, NA)
  )
  
  expect_error(score_forecasts(forecasts = test_forecasts, truth = malformed_truth))
})

test_that("incomplete truth should throw warning", {
  
})

test_that("calculated scores that are not in scores vector should be dropped", {
  test_forecasts <- data.frame(
    model = c("source1", "source2"),
    forecast_date = c(ymd(20200101), ymd(20200101)),
    location = c("01", "01"),
    horizon = c("1", "1"),
    temporal_resolution = c("wk", "wk"),
    target_variable = c("inc death", "inc death"),
    target_end_date = c(ymd(20200108), ymd(20200108)),
    type = c("point", "point"),
    quantile = c(NA, NA),
    value = c(3, 4),
    stringsAsFactors = FALSE
  )
  
  test_truth <- data.frame(
    model = c("source1", "source1"),
    target_variable = c("inc death", "inc death"),
    target_end_date = c(ymd(20200101), ymd(20200108)),
    location = c("01", "01"),
    value = c(1, 2),
    stringsAsFactors = FALSE
  )
})

test_that("invalid scores in scores vector should have NAs", {
  
})

test_that("valid output (long version) should contain necessary columns", {
  
})

test_that("valid output (wide version) should contain necessary columns", {
  
})

test_that("(long version) calculated scores that are in scores vector should have name and value", {
  
})

test_that("(wide version) calculated scores that are in scores vector should have column and value", {
  
})

test_that("abs error is correct, point forecast only", {
  y <- c(1, -15, 22)
  
  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))
  
  forecast_target_end_dates <-
    rep(target_end_dates, times = 1)
  forecast_horizons <- rep(horizons, times = 1)
  forecast_locations <- rep(locations, times = 1)
  forecast_target_variables <-
    rep(target_variables, times = 1)
  
  point_forecast <- c(5,6,7)
  
  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )
  
  
  n_forecasts <- length(point_forecast)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("point", 3),
    quantile = NA,
    value = point_forecast,
    stringsAsFactors = FALSE
  )
  
  actual <- score_forecasts(forecasts = test_forecasts, truth = test_truth, use_median_as_point = FALSE)
  
  expected <- abs(y - point_forecast)

  expect_equal(actual$abs_error, expected)
})

test_that("abs error is correct, point and median forecasts different, use_median_as_point is FALSE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4))
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  forecast_quantiles_matrix <- forecast_quantiles_matrix[, 3, drop = FALSE]
  forecast_quantile_probs <- forecast_quantile_probs[3]
  
  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))
  
  forecast_target_end_dates <-
    rep(target_end_dates, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))
  
  point_forecast <- c(5,6,7)
  
  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )
  
  n_forecasts <- length(point_forecast) + length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = c(rep("point",length(point_forecast)),rep("quantile", length(forecast_quantiles))),
    quantile = c(rep(NA,length(point_forecast)),forecast_quantile_probs),
    value = c(point_forecast,forecast_quantiles),
    stringsAsFactors = FALSE
  )
  
  actual <- score_forecasts(forecasts = test_forecasts, truth = test_truth, use_median_as_point = FALSE)
  
  expected <- abs(y - point_forecast)
  
  expect_equal(actual$abs_error, expected)
})

test_that("abs error is correct, point and median forecasts different, use_median_as_point is TRUE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4))
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  forecast_quantiles_matrix <- forecast_quantiles_matrix[, 3, drop = FALSE]
  forecast_quantile_probs <- forecast_quantile_probs[3]
  
  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))
  
  forecast_target_end_dates <-
    rep(target_end_dates, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))
  
  point_forecast <- c(5,6,7)
  
  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )
  
  n_forecasts <- length(point_forecast) + length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = c(rep("point",length(point_forecast)),rep("quantile", length(forecast_quantiles))),
    quantile = c(rep(NA,length(point_forecast)),forecast_quantile_probs),
    value = c(point_forecast,forecast_quantiles),
    stringsAsFactors = FALSE
  )
  
  actual <- score_forecasts(forecasts = test_forecasts, truth = test_truth, use_median_as_point = TRUE)
  
  expected <- abs(y - c(1,2,3))
  
  expect_equal(actual$abs_error, expected)
})

test_that("abs error is correct, point and median forecasts same", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4))
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  forecast_quantiles_matrix <- forecast_quantiles_matrix[, 3, drop = FALSE]
  forecast_quantile_probs <- forecast_quantile_probs[3]
  
  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))
  
  forecast_target_end_dates <-
    rep(target_end_dates, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))
  
  point_forecast <- c(1,2,3)
  
  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )
  
  n_forecasts <- length(point_forecast) + length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = c(rep("point",length(point_forecast)),rep("quantile", length(forecast_quantiles))),
    quantile = c(rep(NA,length(point_forecast)),forecast_quantile_probs),
    value = c(point_forecast,forecast_quantiles),
    stringsAsFactors = FALSE
  )
  
  actual <- score_forecasts(forecasts = test_forecasts, truth = test_truth, use_median_as_point = FALSE)
  
  expected <- abs(y - point_forecast)
  
  expect_equal(actual$abs_error, expected)

})

test_that("sharpness is NaN, point forecast only, use_median_as_point FALSE", {
  y <- c(1, -15, 22)

  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))

  forecast_target_end_dates <-
    rep(target_end_dates, times = 1)
  forecast_horizons <- rep(horizons, times = 1)
  forecast_locations <- rep(locations, times = 1)
  forecast_target_variables <-
    rep(target_variables, times = 1)

  point_forecast <- c(5, 6, 7)

  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )


  n_forecasts <- length(point_forecast)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("point", 3),
    quantile = NA,
    value = point_forecast,
    stringsAsFactors = FALSE
  )

  actual <- score_forecasts(forecasts = test_forecasts, truth = test_truth, use_median_as_point = FALSE)

  expected <- rep(NaN, length(point_forecast))

  expect_equal(actual$sharpness, expected)
})

test_that("sharpness is zero, median only, use_median_as_point TRUE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4)
  )
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  forecast_quantiles_matrix <- forecast_quantiles_matrix[, 3, drop = FALSE]
  forecast_quantile_probs <- forecast_quantile_probs[3]
  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))
  forecast_target_end_dates <-
    rep(target_end_dates, times = ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))
  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )
  n_forecasts <- length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("quantile", n_forecasts),
    quantile = forecast_quantile_probs,
    value = forecast_quantiles,
    stringsAsFactors = FALSE
  )
  actual <- score_forecasts(
    forecasts = test_forecasts, truth = test_truth,
    use_median_as_point = TRUE
  )
  expected <- rep(0, length(y))
  expect_equal(actual$sharpness, expected)
})

test_that("sharpness errors, median only, use_median_as_point FALSE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4)
  )
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  forecast_quantiles_matrix <- forecast_quantiles_matrix[, 3, drop = FALSE]
  forecast_quantile_probs <- forecast_quantile_probs[3]

  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))

  forecast_target_end_dates <-
    rep(target_end_dates, times = ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))

  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )

  n_forecasts <- length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("quantile", n_forecasts),
    quantile = forecast_quantile_probs,
    value = forecast_quantiles,
    stringsAsFactors = FALSE
  )

  expect_error(actual <- score_forecasts(
    forecasts = test_forecasts, truth = test_truth,
    use_median_as_point = FALSE
  ))
})

test_that("sharpness is correct, 1 interval only, use_median_as_point TRUE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4)
  )
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  forecast_quantiles_matrix <- forecast_quantiles_matrix[, c(1, 5), drop = FALSE]
  forecast_quantile_probs <- forecast_quantile_probs[c(1, 5)]
  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))
  forecast_target_end_dates <-
    rep(target_end_dates, times = ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))
  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )
  n_forecasts <- length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("quantile", n_forecasts),
    quantile = forecast_quantile_probs,
    value = forecast_quantiles,
    stringsAsFactors = FALSE
  )
  actual <- score_forecasts(
    forecasts = test_forecasts, truth = test_truth,
    use_median_as_point = TRUE
  )
  alpha1 <- 0.2
  expected <- (alpha1 / 2) * abs(forecast_quantiles_matrix[, 2] - forecast_quantiles_matrix[, 1])
  expect_equal(actual$sharpness, expected)
})

test_that("sharpness errors, 2 intervals and median no point, use_median_as_point FALSE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4)
  )
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)

  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))

  forecast_target_end_dates <-
    rep(target_end_dates, times = ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))

  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )

  n_forecasts <- length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("quantile", n_forecasts),
    quantile = forecast_quantile_probs,
    value = forecast_quantiles,
    stringsAsFactors = FALSE
  )

  expect_error(actual <- score_forecasts(
    forecasts = test_forecasts, truth = test_truth,
    use_median_as_point = FALSE
  ))
})

test_that("sharpness is correct, 2 intervals and median no point, use_median_as_point TRUE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4)
  )
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))
  forecast_target_end_dates <-
    rep(target_end_dates, times = ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))
  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )
  n_forecasts <- length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("quantile", n_forecasts),
    quantile = forecast_quantile_probs,
    value = forecast_quantiles,
    stringsAsFactors = FALSE
  )
  actual <- score_forecasts(
    forecasts = test_forecasts, truth = test_truth,
    use_median_as_point = TRUE
  )
  alpha1 <- 0.2
  alpha2 <- 0.5
  expected <- (1 / 2.5) * (
    abs(forecast_quantiles_matrix[, 5] - forecast_quantiles_matrix[, 1]) * (alpha1 / 2) +
      abs(forecast_quantiles_matrix[, 4] - forecast_quantiles_matrix[, 2]) * (alpha2 / 2)
  )

  expect_equal(actual$sharpness, expected)
})

test_that("sharpness is correct, 2 intervals, median and point, use_median_as_point FALSE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4)
  )
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)

  point_forecast <- c(5, 6, 7)

  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))

  forecast_target_end_dates <-
    rep(target_end_dates, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))

  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )

  n_forecasts <- length(point_forecast) + length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = c(rep("point", length(point_forecast)), rep("quantile", length(forecast_quantiles))),
    quantile = c(rep(NA, length(point_forecast)), forecast_quantile_probs),
    value = c(point_forecast, forecast_quantiles),
    stringsAsFactors = FALSE
  )

  actual <- score_forecasts(
    forecasts = test_forecasts, truth = test_truth,
    use_median_as_point = FALSE
  )
  alpha1 <- 0.2
  alpha2 <- 0.5
  expected <- (1 / 2.5) * (
    abs(forecast_quantiles_matrix[, 5] - forecast_quantiles_matrix[, 1]) * (alpha1 / 2) +
      abs(forecast_quantiles_matrix[, 4] - forecast_quantiles_matrix[, 2]) * (alpha2 / 2)
  )

  expect_equal(actual$sharpness, expected)
})

test_that("sharpness is correct, 2 intervals, median and point, use_median_as_point TRUE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4)
  )
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)

  point_forecast <- c(5, 6, 7)

  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))

  forecast_target_end_dates <-
    rep(target_end_dates, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))

  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )

  n_forecasts <- length(point_forecast) + length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = c(rep("point", length(point_forecast)), rep("quantile", length(forecast_quantiles))),
    quantile = c(rep(NA, length(point_forecast)), forecast_quantile_probs),
    value = c(point_forecast, forecast_quantiles),
    stringsAsFactors = FALSE
  )

  actual <- score_forecasts(
    forecasts = test_forecasts, truth = test_truth,
    use_median_as_point = TRUE
  )
  alpha1 <- 0.2
  alpha2 <- 0.5
  expected <- (1 / 2.5) * (
    abs(forecast_quantiles_matrix[, 5] - forecast_quantiles_matrix[, 1]) * (alpha1 / 2) +
      abs(forecast_quantiles_matrix[, 4] - forecast_quantiles_matrix[, 2]) * (alpha2 / 2)
  )

  expect_equal(actual$sharpness, expected)
})

test_that("overprediction is NaN, point forecast only, use_median_as_point FALSE", {
  y <- c(1, -15, 22)

  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))

  forecast_target_end_dates <-
    rep(target_end_dates, times = 1)
  forecast_horizons <- rep(horizons, times = 1)
  forecast_locations <- rep(locations, times = 1)
  forecast_target_variables <-
    rep(target_variables, times = 1)

  point_forecast <- c(5, 6, 7)

  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )


  n_forecasts <- length(point_forecast)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("point", 3),
    quantile = NA,
    value = point_forecast,
    stringsAsFactors = FALSE
  )

  actual <- score_forecasts(forecasts = test_forecasts, truth = test_truth, use_median_as_point = FALSE)

  expected <- rep(NaN, length(point_forecast))

  expect_equal(actual$overprediction, expected)
})

test_that("overprediction is correct, median only, use_median_as_point TRUE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4)
  )
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  forecast_quantiles_matrix <- forecast_quantiles_matrix[, 3, drop = FALSE]
  forecast_quantile_probs <- forecast_quantile_probs[3]
  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))
  forecast_target_end_dates <-
    rep(target_end_dates, times = ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))
  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )
  n_forecasts <- length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("quantile", n_forecasts),
    quantile = forecast_quantile_probs,
    value = forecast_quantiles,
    stringsAsFactors = FALSE
  )
  actual <- score_forecasts(
    forecasts = test_forecasts, truth = test_truth,
    use_median_as_point = TRUE
  )
  expected <- (forecast_quantiles_matrix[, 1] - y) * (y < forecast_quantiles_matrix[, 1])
  expect_equal(actual$overprediction, expected)
})

test_that("overprediction errors, median only, use_median_as_point FALSE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4)
  )
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  forecast_quantiles_matrix <- forecast_quantiles_matrix[, 3, drop = FALSE]
  forecast_quantile_probs <- forecast_quantile_probs[3]

  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))

  forecast_target_end_dates <-
    rep(target_end_dates, times = ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))

  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )

  n_forecasts <- length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("quantile", n_forecasts),
    quantile = forecast_quantile_probs,
    value = forecast_quantiles,
    stringsAsFactors = FALSE
  )

  expect_error(actual <- score_forecasts(
    forecasts = test_forecasts, truth = test_truth,
    use_median_as_point = FALSE
  ))
})

test_that("overprediction is correct, 1 interval only, use_median_as_point TRUE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4)
  )
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  forecast_quantiles_matrix <- forecast_quantiles_matrix[, c(1, 5), drop = FALSE]
  forecast_quantile_probs <- forecast_quantile_probs[c(1, 5)]
  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))
  forecast_target_end_dates <-
    rep(target_end_dates, times = ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))
  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )
  n_forecasts <- length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("quantile", n_forecasts),
    quantile = forecast_quantile_probs,
    value = forecast_quantiles,
    stringsAsFactors = FALSE
  )
  actual <- score_forecasts(
    forecasts = test_forecasts, truth = test_truth,
    use_median_as_point = TRUE
  )
  
  expected <- (forecast_quantiles_matrix[, 1] - y) * (y < forecast_quantiles_matrix[, 1])
  expect_equal(actual$overprediction, expected)
})

test_that("overprediction errors, 2 intervals and median no point, use_median_as_point FALSE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4)
  )
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)

  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))

  forecast_target_end_dates <-
    rep(target_end_dates, times = ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))

  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )

  n_forecasts <- length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("quantile", n_forecasts),
    quantile = forecast_quantile_probs,
    value = forecast_quantiles,
    stringsAsFactors = FALSE
  )

  expect_error(actual <- score_forecasts(
    forecasts = test_forecasts, truth = test_truth,
    use_median_as_point = FALSE
  ))
})

test_that("overprediction is correct, 2 intervals and median no point, use_median_as_point TRUE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4)
  )
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))
  forecast_target_end_dates <-
    rep(target_end_dates, times = ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))
  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )
  n_forecasts <- length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("quantile", n_forecasts),
    quantile = forecast_quantile_probs,
    value = forecast_quantiles,
    stringsAsFactors = FALSE
  )
  actual <- score_forecasts(
    forecasts = test_forecasts, truth = test_truth,
    use_median_as_point = TRUE
  )

  expected <- (1 / 2.5) * (
    (1 / 2) * (forecast_quantiles_matrix[, 3] - y) * (y < forecast_quantiles_matrix[, 3]) +
      (forecast_quantiles_matrix[, 1] - y) * (y < forecast_quantiles_matrix[, 1]) +
      (forecast_quantiles_matrix[, 2] - y) * (y < forecast_quantiles_matrix[, 2])
  )

  expect_equal(actual$overprediction, expected)
})

test_that("overprediction is correct, 2 intervals, median and point, use_median_as_point FALSE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4)
  )
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)

  point_forecast <- c(5, 6, 7)

  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))

  forecast_target_end_dates <-
    rep(target_end_dates, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))

  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )

  n_forecasts <- length(point_forecast) + length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = c(rep("point", length(point_forecast)), rep("quantile", length(forecast_quantiles))),
    quantile = c(rep(NA, length(point_forecast)), forecast_quantile_probs),
    value = c(point_forecast, forecast_quantiles),
    stringsAsFactors = FALSE
  )

  actual <- score_forecasts(
    forecasts = test_forecasts, truth = test_truth,
    use_median_as_point = FALSE
  )

  expected <- (1 / 2.5) * (
    (1 / 2) * (forecast_quantiles_matrix[, 3] - y) * (y < forecast_quantiles_matrix[, 3]) +
      (forecast_quantiles_matrix[, 1] - y) * (y < forecast_quantiles_matrix[, 1]) +
      (forecast_quantiles_matrix[, 2] - y) * (y < forecast_quantiles_matrix[, 2])
  )

  expect_equal(actual$overprediction, expected)
})

test_that("overprediction is correct, 2 intervals, median and point, use_median_as_point TRUE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4)
  )
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)

  point_forecast <- c(5, 6, 7)

  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))

  forecast_target_end_dates <-
    rep(target_end_dates, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))

  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )

  n_forecasts <- length(point_forecast) + length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = c(rep("point", length(point_forecast)), rep("quantile", length(forecast_quantiles))),
    quantile = c(rep(NA, length(point_forecast)), forecast_quantile_probs),
    value = c(point_forecast, forecast_quantiles),
    stringsAsFactors = FALSE
  )

  actual <- score_forecasts(
    forecasts = test_forecasts, truth = test_truth,
    use_median_as_point = TRUE
  )

  expected <- (1 / 2.5) * (
    (1 / 2) * (forecast_quantiles_matrix[, 3] - y) * (y < forecast_quantiles_matrix[, 3]) +
      (forecast_quantiles_matrix[, 1] - y) * (y < forecast_quantiles_matrix[, 1]) +
      (forecast_quantiles_matrix[, 2] - y) * (y < forecast_quantiles_matrix[, 2])
  )

  expect_equal(actual$overprediction, expected)
})

test_that("underprediction is NaN, point forecast only, use_median_as_point FALSE", {
  y <- c(1, -15, 22)
  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))
  forecast_target_end_dates <-
    rep(target_end_dates, times = 1)
  forecast_horizons <- rep(horizons, times = 1)
  forecast_locations <- rep(locations, times = 1)
  forecast_target_variables <-
    rep(target_variables, times = 1)
  
  point_forecast <- c(5, 6, 7)
  
  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )
  
  n_forecasts <- length(point_forecast)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("point", 3),
    quantile = NA,
    value = point_forecast,
    stringsAsFactors = FALSE
  )
  
  actual <- score_forecasts(forecasts = test_forecasts, truth = test_truth, use_median_as_point = FALSE)
  
  expected <- rep(NaN, length(point_forecast))
  
  expect_equal(actual$underprediction, expected)
})

test_that("underprediction is correct, median only, use_median_as_point TRUE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4)
  )
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  forecast_quantiles_matrix <- forecast_quantiles_matrix[, 3, drop = FALSE]
  forecast_quantile_probs <- forecast_quantile_probs[3]
  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))
  forecast_target_end_dates <-
    rep(target_end_dates, times = ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))
  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )
  n_forecasts <- length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("quantile", n_forecasts),
    quantile = forecast_quantile_probs,
    value = forecast_quantiles,
    stringsAsFactors = FALSE
  )
  actual <- score_forecasts(
    forecasts = test_forecasts, truth = test_truth,
    use_median_as_point = TRUE
  )
  expected <- (y - forecast_quantiles_matrix[, 1]) * (y > forecast_quantiles_matrix[, 1])
  expect_equal(actual$underprediction, expected)
})

test_that("underprediction errors, median only, use_median_as_point FALSE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4)
  )
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  forecast_quantiles_matrix <- forecast_quantiles_matrix[, 3, drop = FALSE]
  forecast_quantile_probs <- forecast_quantile_probs[3]

  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))

  forecast_target_end_dates <-
    rep(target_end_dates, times = ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))

  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )

  n_forecasts <- length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("quantile", n_forecasts),
    quantile = forecast_quantile_probs,
    value = forecast_quantiles,
    stringsAsFactors = FALSE
  )

  expect_error(actual <- score_forecasts(
    forecasts = test_forecasts, truth = test_truth,
    use_median_as_point = FALSE
  ))
})

test_that("underprediction is correct, 1 interval only, use_median_as_point TRUE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4)
  )
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  forecast_quantiles_matrix <- forecast_quantiles_matrix[, c(1, 5), drop = FALSE]
  forecast_quantile_probs <- forecast_quantile_probs[c(1, 5)]
  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))
  forecast_target_end_dates <-
    rep(target_end_dates, times = ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))
  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )
  n_forecasts <- length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("quantile", n_forecasts),
    quantile = forecast_quantile_probs,
    value = forecast_quantiles,
    stringsAsFactors = FALSE
  )
  actual <- score_forecasts(
    forecasts = test_forecasts, truth = test_truth,
    use_median_as_point = TRUE
  )

  expected <- (y - forecast_quantiles_matrix[, 2]) * (y > forecast_quantiles_matrix[, 2])
  expect_equal(actual$underprediction, expected)
})

test_that("underprediction errors, 2 intervals and median no point, use_median_as_point FALSE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4)
  )
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)

  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))

  forecast_target_end_dates <-
    rep(target_end_dates, times = ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))

  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )

  n_forecasts <- length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("quantile", n_forecasts),
    quantile = forecast_quantile_probs,
    value = forecast_quantiles,
    stringsAsFactors = FALSE
  )

  expect_error(actual <- score_forecasts(
    forecasts = test_forecasts, truth = test_truth,
    use_median_as_point = FALSE
  ))
})

test_that("underprediction is correct, 2 intervals and median no point, use_median_as_point TRUE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4)
  )
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))
  forecast_target_end_dates <-
    rep(target_end_dates, times = ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))
  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )
  n_forecasts <- length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("quantile", n_forecasts),
    quantile = forecast_quantile_probs,
    value = forecast_quantiles,
    stringsAsFactors = FALSE
  )
  actual <- score_forecasts(
    forecasts = test_forecasts, truth = test_truth,
    use_median_as_point = TRUE
  )

  expected <- (1 / 2.5) * (
    (1 / 2) * (y - forecast_quantiles_matrix[, 3]) * (y > forecast_quantiles_matrix[, 3]) +
      (y - forecast_quantiles_matrix[, 5]) * (y > forecast_quantiles_matrix[, 5]) +
      (y - forecast_quantiles_matrix[, 4]) * (y > forecast_quantiles_matrix[, 4])
  )

  expect_equal(actual$underprediction, expected)
})

test_that("underprediction is correct, 2 intervals, median and point, use_median_as_point FALSE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4)
  )
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)

  point_forecast <- c(5, 6, 7)

  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))

  forecast_target_end_dates <-
    rep(target_end_dates, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))

  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )

  n_forecasts <- length(point_forecast) + length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = c(rep("point", length(point_forecast)), rep("quantile", length(forecast_quantiles))),
    quantile = c(rep(NA, length(point_forecast)), forecast_quantile_probs),
    value = c(point_forecast, forecast_quantiles),
    stringsAsFactors = FALSE
  )

  actual <- score_forecasts(
    forecasts = test_forecasts, truth = test_truth,
    use_median_as_point = FALSE
  )

  expected <- (1 / 2.5) * (
    (1 / 2) * (y - forecast_quantiles_matrix[, 3]) * (y > forecast_quantiles_matrix[, 3]) +
      (y - forecast_quantiles_matrix[, 5]) * (y > forecast_quantiles_matrix[, 5]) +
      (y - forecast_quantiles_matrix[, 4]) * (y > forecast_quantiles_matrix[, 4])
  )

  expect_equal(actual$underprediction, expected)
})

test_that("underprediction is correct, 2 intervals, median and point, use_median_as_point TRUE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4)
  )
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)

  point_forecast <- c(5, 6, 7)

  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))

  forecast_target_end_dates <-
    rep(target_end_dates, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))

  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )

  n_forecasts <- length(point_forecast) + length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = c(rep("point", length(point_forecast)), rep("quantile", length(forecast_quantiles))),
    quantile = c(rep(NA, length(point_forecast)), forecast_quantile_probs),
    value = c(point_forecast, forecast_quantiles),
    stringsAsFactors = FALSE
  )

  actual <- score_forecasts(
    forecasts = test_forecasts, truth = test_truth,
    use_median_as_point = TRUE
  )

  expected <- (1 / 2.5) * (
    (1 / 2) * (y - forecast_quantiles_matrix[, 3]) * (y > forecast_quantiles_matrix[, 3]) +
      (y - forecast_quantiles_matrix[, 5]) * (y > forecast_quantiles_matrix[, 5]) +
      (y - forecast_quantiles_matrix[, 4]) * (y > forecast_quantiles_matrix[, 4])
  )

  expect_equal(actual$underprediction, expected)
})

test_that("wis is NaN, point forecast only, use_median_as_point FALSE",{
  y <- c(1, -15, 22)
  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))
  forecast_target_end_dates <-
    rep(target_end_dates, times = 1)
  forecast_horizons <- rep(horizons, times = 1)
  forecast_locations <- rep(locations, times = 1)
  forecast_target_variables <-
    rep(target_variables, times = 1)
  
  point_forecast <- c(5, 6, 7)
  
  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )
  
  n_forecasts <- length(point_forecast)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("point", 3),
    quantile = NA,
    value = point_forecast,
    stringsAsFactors = FALSE
  )
  actual <- score_forecasts(forecasts = test_forecasts, truth = test_truth, use_median_as_point = FALSE)
  
  expected <- rep(NaN,length(point_forecast))
  
  expect_equal(actual$wis, expected)
})

test_that("wis is correct, median only, use_median_as_point TRUE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4))
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  forecast_quantiles_matrix <- forecast_quantiles_matrix[, 3, drop = FALSE]
  forecast_quantile_probs <- forecast_quantile_probs[3]

  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))

  forecast_target_end_dates <-
    rep(target_end_dates, times = ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))

  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )

  n_forecasts <- length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("quantile", n_forecasts),
    quantile = forecast_quantile_probs,
    value = forecast_quantiles,
    stringsAsFactors = FALSE
  )

  actual <- score_forecasts(forecasts = test_forecasts, truth = test_truth, 
                            use_median_as_point = TRUE)

  expected <- abs(y - forecast_quantiles_matrix[, 1])

  expect_equal(actual$wis, expected)
})

test_that("wis errors, median only, use_median_as_point FALSE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4))
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  forecast_quantiles_matrix <- forecast_quantiles_matrix[, 3, drop = FALSE]
  forecast_quantile_probs <- forecast_quantile_probs[3]
  
  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))
  
  forecast_target_end_dates <-
    rep(target_end_dates, times = ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))
  
  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )
  
  n_forecasts <- length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("quantile", n_forecasts),
    quantile = forecast_quantile_probs,
    value = forecast_quantiles,
    stringsAsFactors = FALSE
  )
  
  expect_error(actual <- score_forecasts(forecasts = test_forecasts, truth = test_truth, 
                            use_median_as_point = FALSE))
})

test_that("wis is correct, 1 interval only, use_median_as_point TRUE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4))
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  forecast_quantiles_matrix <- forecast_quantiles_matrix[, c(1, 5), drop = FALSE]
  forecast_quantile_probs <- forecast_quantile_probs[c(1, 5)]

  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))

  forecast_target_end_dates <-
    rep(target_end_dates, times = ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))

  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )

  n_forecasts <- length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("quantile", n_forecasts),
    quantile = forecast_quantile_probs,
    value = forecast_quantiles,
    stringsAsFactors = FALSE
  )

  actual <- score_forecasts(forecasts = test_forecasts, truth = test_truth, 
                            use_median_as_point = TRUE)

  alpha1 <- 0.2
  expected <- (forecast_quantiles_matrix[, 2] - forecast_quantiles_matrix[, 1]) * (alpha1 / 2) +
    c(0, (-2) - (-15), 22 - 4)

  expect_equal(actual$wis, expected)
})

test_that("wis is correct, 2 intervals and median no point, use_median_as_point TRUE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4))
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)

  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))

  forecast_target_end_dates <-
    rep(target_end_dates, times = ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))

  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )

  n_forecasts <- length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("quantile", n_forecasts),
    quantile = forecast_quantile_probs,
    value = forecast_quantiles,
    stringsAsFactors = FALSE
  )

  actual <- score_forecasts(forecasts = test_forecasts, truth = test_truth, 
                            use_median_as_point = TRUE)
  alpha1 <- 0.2
  alpha2 <- 0.5
  expected <- (1 / 2.5) * (
    0.5 * abs(y - forecast_quantiles_matrix[, 3]) +
    (forecast_quantiles_matrix[, 5] - forecast_quantiles_matrix[, 1])*(alpha1/2) + c(0, (-2)-(-15), 22-4) +
    (forecast_quantiles_matrix[, 4] - forecast_quantiles_matrix[, 2])*(alpha2/2) + c(0, 1-(-15), 22-3)
  )

  expect_equal(actual$wis, expected)
})

test_that("wis errors, 2 intervals and median no point, use_median_as_point FALSE",{
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4))
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  
  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))
  
  forecast_target_end_dates <-
    rep(target_end_dates, times = ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))
  
  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )
  
  n_forecasts <- length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("quantile", n_forecasts),
    quantile = forecast_quantile_probs,
    value = forecast_quantiles,
    stringsAsFactors = FALSE
  )
  
  expect_error(actual <- score_forecasts(forecasts = test_forecasts, truth = test_truth, 
                            use_median_as_point = FALSE))
})

test_that("wis is correct, 2 intervals, median and point, use_median_as_point TRUE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4))
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  
  point_forecast <- c(5,6,7)
  
  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))
  
  forecast_target_end_dates <-
    rep(target_end_dates, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))
  
  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )
  
  n_forecasts <- length(point_forecast) + length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = c(rep("point",length(point_forecast)),rep("quantile", length(forecast_quantiles))),
    quantile = c(rep(NA,length(point_forecast)),forecast_quantile_probs),
    value = c(point_forecast,forecast_quantiles),
    stringsAsFactors = FALSE
  )
  
  actual <- score_forecasts(forecasts = test_forecasts, truth = test_truth, 
                            use_median_as_point = TRUE)
  alpha1 <- 0.2
  alpha2 <- 0.5
  expected <- (1 / 2.5) * (
    0.5 * abs(y - forecast_quantiles_matrix[, 3]) +
      (forecast_quantiles_matrix[, 5] - forecast_quantiles_matrix[, 1])*(alpha1/2) + c(0, (-2)-(-15), 22-4) +
      (forecast_quantiles_matrix[, 4] - forecast_quantiles_matrix[, 2])*(alpha2/2) + c(0, 1-(-15), 22-3)
  )
  
  expect_equal(actual$wis, expected)
})

test_that("wis is correct, 2 intervals, median and point, use_median_as_point FALSE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4))
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  
  point_forecast <- c(5,6,7)
  
  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))
  
  forecast_target_end_dates <-
    rep(target_end_dates, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))
  
  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )
  
  n_forecasts <- length(point_forecast) + length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = c(rep("point",length(point_forecast)),rep("quantile", length(forecast_quantiles))),
    quantile = c(rep(NA,length(point_forecast)),forecast_quantile_probs),
    value = c(point_forecast,forecast_quantiles),
    stringsAsFactors = FALSE
  )
  
  actual <- score_forecasts(forecasts = test_forecasts, truth = test_truth, 
                            use_median_as_point = FALSE)
  alpha1 <- 0.2
  alpha2 <- 0.5
  expected <- (1 / 2.5) * (
    0.5 * abs(y - forecast_quantiles_matrix[, 3]) +
      (forecast_quantiles_matrix[, 5] - forecast_quantiles_matrix[, 1])*(alpha1/2) + c(0, (-2)-(-15), 22-4) +
      (forecast_quantiles_matrix[, 4] - forecast_quantiles_matrix[, 2])*(alpha2/2) + c(0, 1-(-15), 22-3)
  )
  
  expect_equal(actual$wis, expected)
})

test_that("wis is sum of components, median only, use_median_as_point TRUE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4))
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  forecast_quantiles_matrix <- forecast_quantiles_matrix[, 3, drop = FALSE]
  forecast_quantile_probs <- forecast_quantile_probs[3]
  
  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))
  
  forecast_target_end_dates <-
    rep(target_end_dates, times = ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))
  
  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )
  
  n_forecasts <- length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("quantile", n_forecasts),
    quantile = forecast_quantile_probs,
    value = forecast_quantiles,
    stringsAsFactors = FALSE
  )
  
  actual <- score_forecasts(forecasts = test_forecasts, truth = test_truth, 
                            use_median_as_point = TRUE)
  
  expected <- rowSums(actual[,c("sharpness","overprediction","underprediction")])
  
  expect_equal(actual$wis, expected)
})

test_that("wis is sum of components, 1 interval only, use_median_as_point TRUE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4))
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  forecast_quantiles_matrix <- forecast_quantiles_matrix[, c(1, 5), drop = FALSE]
  forecast_quantile_probs <- forecast_quantile_probs[c(1, 5)]
  
  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))
  
  forecast_target_end_dates <-
    rep(target_end_dates, times = ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))
  
  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )
  
  n_forecasts <- length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("quantile", n_forecasts),
    quantile = forecast_quantile_probs,
    value = forecast_quantiles,
    stringsAsFactors = FALSE
  )
  
  actual <- score_forecasts(forecasts = test_forecasts, truth = test_truth, 
                            use_median_as_point = TRUE)
  
  expected <- rowSums(actual[,c("sharpness","overprediction","underprediction")])
  
  expect_equal(actual$wis, expected)
})

test_that("wis is sum of components, 2 intervals and median no point, use_median_as_point TRUE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4))
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  
  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))
  
  forecast_target_end_dates <-
    rep(target_end_dates, times = ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))
  
  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )
  
  n_forecasts <- length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = rep("quantile", n_forecasts),
    quantile = forecast_quantile_probs,
    value = forecast_quantiles,
    stringsAsFactors = FALSE
  )
  
  actual <- score_forecasts(forecasts = test_forecasts, truth = test_truth, 
                            use_median_as_point = TRUE)
  expected <- rowSums(actual[,c("sharpness","overprediction","underprediction")])
  
  expect_equal(actual$wis, expected)
})

test_that("wis is sum of components, 2 intervals, median and point, use_median_as_point TRUE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4))
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  
  point_forecast <- c(5,6,7)
  
  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))
  
  forecast_target_end_dates <-
    rep(target_end_dates, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))
  
  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )
  
  n_forecasts <- length(point_forecast) + length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = c(rep("point",length(point_forecast)),rep("quantile", length(forecast_quantiles))),
    quantile = c(rep(NA,length(point_forecast)),forecast_quantile_probs),
    value = c(point_forecast,forecast_quantiles),
    stringsAsFactors = FALSE
  )
  
  actual <- score_forecasts(forecasts = test_forecasts, truth = test_truth, 
                            use_median_as_point = TRUE)
  expected <- rowSums(actual[,c("sharpness","overprediction","underprediction")])
  
  expect_equal(actual$wis, expected)
})

test_that("wis is sum of components, 2 intervals, median and point, use_median_as_point FALSE", {
  y <- c(1, -15, 22)
  forecast_quantiles_matrix <- rbind(
    c(-1, 0, 1, 2, 3),
    c(-2, 1, 2, 2, 4),
    c(-2, 0, 3, 3, 4))
  forecast_quantile_probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  
  point_forecast <- c(5,6,7)
  
  target_end_dates <- lubridate::ymd(20200101) + c(7, 14, 7)
  horizons <- c("1", "2", "1")
  locations <- c("01", "01", "02")
  target_variables <- rep("inc death", length(y))
  
  forecast_target_end_dates <-
    rep(target_end_dates, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_horizons <- rep(horizons, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_locations <- rep(locations, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_target_variables <-
    rep(target_variables, times = 1 + ncol(forecast_quantiles_matrix))
  forecast_quantile_probs <- rep(forecast_quantile_probs, each = length(y))
  forecast_quantiles <- forecast_quantiles_matrix
  dim(forecast_quantiles) <- prod(dim(forecast_quantiles))
  
  test_truth <- data.frame(
    model = rep("truth_source", length(y)),
    target_variable = target_variables,
    target_end_date = target_end_dates,
    location = locations,
    value = y,
    stringsAsFactors = FALSE
  )
  
  n_forecasts <- length(point_forecast) + length(forecast_quantiles)
  test_forecasts <- data.frame(
    model = rep("m1", n_forecasts),
    forecast_date = rep(lubridate::ymd("20200101"), n_forecasts),
    location = forecast_locations,
    horizon = forecast_horizons,
    temporal_resolution = rep("wk", n_forecasts),
    target_variable = forecast_target_variables,
    target_end_date = forecast_target_end_dates,
    type = c(rep("point",length(point_forecast)),rep("quantile", length(forecast_quantiles))),
    quantile = c(rep(NA,length(point_forecast)),forecast_quantile_probs),
    value = c(point_forecast,forecast_quantiles),
    stringsAsFactors = FALSE
  )
  
  actual <- score_forecasts(forecasts = test_forecasts, truth = test_truth, 
                            use_median_as_point = FALSE)
  expected <- rowSums(actual[,c("sharpness","overprediction","underprediction")])
  
  expect_equal(actual$wis, expected)
})
