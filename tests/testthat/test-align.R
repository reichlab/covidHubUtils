library(tidyverse)
library(covidHubUtils)
library(testthat)

# inc_hosp_targets <- paste(1:34, "day ahead inc hosp")
# inc_case_targets <- paste(1:4, "wk ahead inc case")
# combined_targets <- c(inc_hosp_targets, inc_case_targets)
# forecasts_hosp <- load_forecasts(
#   models = c("COVIDhub-ensemble", "LANL-GrowthRate"),
#   dates = c("2021-07-12", "2021-07-19"),
#   date_window_size = 6,
#   locations = "US",
#   types = "quantile",
#   targets = combined_targets,
# #  source = "zoltar",
#   source = "local_hub_repo",
#   hub_repo_path = "../covid19-forecast-hub",
#   verbose = FALSE,
#   as_of = NULL,
#   hub = c("US")
# )


#     Aug/Sep 2021
# Su Mo Tu We Th Fr Sa
#  1  2  3  4  5  6  7
#  8  9 10 11 12 13 14
# 15 16 17 18 19 20 21
# 22 23 24 25 26 27 28
# 29 30 31 1  2  3  4
#  5  6  7  8  9 10 11
# 12 13 14 15 16 17 18
# 19 20 21 22 23 24 25
# 26 27 28 29 30

test_that("original columns preserved and new columns are correct", {

    original_fdf <- tibble(
        model = rep(c("a", "b", "c"), 4),
        forecast_date = as.Date(rep(c(
            "2021-08-12", "2021-08-14", "2021-08-16",
            "2021-08-19", "2021-08-21", "2021-08-22"
        ), 2)),
        location = "US",
        horizon = as.character(c(
            1, 2, 4,
            1, 2, 16,
            1, 2, 4,
            1, 2, 16
        )),
        temporal_resolution = rep(c(
            "wk", "wk", "day",
            "wk", "wk", "day"
        ), 2),
        target_variable = rep(c(
            "inc case", "inc case", "inc hosp"
        ), 4),
        target_end_date = as.Date(rep(c(
            "2021-08-21", "2021-08-28", "2021-08-20",
            "2021-08-28", "2021-09-04", "2021-09-07"
        ), 2)),
        type = "quantile",
        quantile = rep(c(0.25, 0.75), each = 6),
        value = c(
            100, 90, 30,
            110, 95, 32,
            200, 190, 70,
            220, 200, 75
        )
    )
    reference_dates <- as.Date(c(
        "2021-08-14", 
        "2021-08-14",
        "2021-08-16",
        "2021-08-21",
        "2021-08-21",
        "2021-08-23",
        "2021-08-14",
        "2021-08-14",
        "2021-08-16",
        "2021-08-21",
        "2021-08-21",
        "2021-08-23"
    ))
    relative_horizons <- c(1, 2, 4, 1, 2, 15, 1, 2, 4, 1, 2, 15)

    new_df <- align_forecasts(original_fdf)
    expect_equal(
        new_df %>% dplyr::select(-reference_date, -relative_horizon),
        original_fdf
    )
    expect_equal(new_df$reference_date, reference_dates)
    expect_equal(new_df$relative_horizon, relative_horizons)
})

