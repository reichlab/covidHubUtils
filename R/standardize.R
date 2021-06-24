#' Add reference dates and relative horizons to a dataframe of forecasts
#' 
#' @param forecasts dataframe returned by \code{load_forecats}
#' @param reference_dates
#' @param reference_weekday capitalized weekday
#' @param reference_windows
#' @param drop_nonpos_horizons
#' @param horizon_time_scale 
#'
#' @return forecast dataframe augmented by standardization columns

standardize_forecasts <- function(
  forecasts,
  reference_dates,
  reference_weekday = "Saturday",
  reference_windows,
  drop_nonpos_horizons = TRUE,
  horizon_time_scale = "wk"
) {
  if (horizon_time_scale == "wk") {
    ts_days <- 7
  } else if (horizon_time_scale == "days") {
    ts_days <- 1
  } else {
    stop("Horizon time scale undefined")
  }
  if (missing(reference_dates)) {
    all_dates <- seq(
      min(forecasts$forecast_date) - ts_days,
      max(forecasts$forecast_date) + ts_days,
      by=1
    )
    reference_dates <- all_dates[weekdays(all_dates) == reference_weekday]
  }
  if (missing(reference_windows)) {
    if (reference_weekday == "Saturday") {
      reference_windows <- -4:2
    } else if (reference_weekday == "Monday") {
      reference_windows <- -6:0
    } else {
      stop("Reference windows undefined")
    }
  }
  if (!is.list(reference_windows)) {
    reference_windows <- list(reference_windows) 
  }

  ref_df <- tibble(
    reference_date = reference_dates,
    forecast_date = purrr::map2(
      reference_date, 
      reference_windows, 
      ~.x+.y
    )
  ) %>% unnest(cols = forecast_date)

  reps <- group_by(ref_df, forecast_date) %>% 
    dplyr::tally() %>% 
    dplyr::filter(n>1)
  if (nrow(reps > 0)) {
    print(reps)
    stop("The above forecast dates are associated with multiple reference dates")
  }

  forecasts <- forecasts %>% 
    dplyr::left_join(ref_df) %>% 
    dplyr::mutate(
      relative_horizon = 
        ceiling(as.numeric((target_end_date - reference_date)/ts_days))
    )

  if (drop_nonpos_horizons) {
    forecasts <- forecasts %>% 
      dplyr::filter(relative_horizon > 0)
  }

  return(forecasts)
}