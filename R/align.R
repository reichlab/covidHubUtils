#' Add reference dates and relative horizons to a dataframe of forecasts
#' 
#' @param forecasts dataframe in format returned by \code{load_forecasts}
#' @param reference_dates a named list of vectors of reference dates for
#' forecasts, grouping forecasts that were made during the same week. The list
#' should have two components: "wk" providing reference dates for forecasts at
#' a weekly temporal resolution, and "day" providing reference dates for
#' forecasts at a daily temporal resolution. Dates may be a character vector in
#' the format "YYYY-MM-DD" or a vector of Dates. For example, if a forecast of
#' daily hospitalizations is issued with a forecast date that is Saturday,
#' 2021-09-04, and we want to align analyses around a Monday, the reference
#' date would be Monday, 2021-09-06. This can be accomplished more easily by
#' providing the `reference_weekday` argument below.
#' @param reference_weekday a named list or named character vector of weekdays
#' to use as reference dates. The list should contain entries named "wk" and
#' "day". The default uses "Saturday" as the reference weekday for forecasts
#' of weekly targets and "Monday" as the reference weekday for forecasts of
#' daily targets.
#' @param reference_windows a named list of integer vectors with offset values
#' giving the set of forecast dates that should be assigned to a particular
#' reference date, in units of number of days. The function defaults to using
#' `-4:2` for forecasts at a weekly temporal resolution so that a forecast
#' issued on a Monday will have a reference date of the previous Saturday, and
#' `-6:0` for forecasts at a daily temporal resolution so that all forecasts
#' with a `forecast_date` in the week leading up to a particular Monday will be
#' assigned a reference date of that Monday.
#' @param drop_nonpos_relative_horizons boolean indicating whether forecasts that have a
#' non-positive horizon relative to the reference date should be dropped.
#' Defaults to `TRUE`
#'
#' @return forecast dataframe augmented by columns reference_date and
#' relative_horizon
#' 
#' @examples
#' \dontrun{
#' library(tidyverse)
#' library(covidHubUtils)
#' hub_repo_path <- "../covid19-forecast-hub"
#' dates <- seq.Date(as.Date("2021-05-01"), as.Date("2021-06-01"), by = 1)
#' forecasts <- load_forecasts(
#'   models = get_all_models(source = "local_hub_repo", hub_repo_path = hub_repo_path),
#'   dates = dates[!(weekdays(dates) %in% c("Monday", "Sunday"))],
#'   date_window_size = 0,
#'   locations = "US",
#'   types = "quantile",
#'   source = "local_hub_repo",
#'   hub_repo_path = hub_repo_path,
#'   verbose = FALSE,
#'   as_of = NULL,
#'   hub = c("US")
#' ) %>% filter(quantile == .5)
#' forecasts
#' forecasts %>% distinct(model)
#' forecasts %>% align_forecasts()
#' forecasts %>% align_forecasts(drop_nonpos_relative_horizons = FALSE) %>% 
#'   filter(relative_horizon <= 0)
#'}
#'
#' @export
align_forecasts <- function(
  forecasts,
  reference_dates = list("wk" = NULL, "day" = NULL),
  reference_weekday = list("wk" = "Saturday", "day" = "Monday"),
  reference_windows = list("wk" = -4:2, "day" = -6:0),
  drop_nonpos_relative_horizons = TRUE
) {
  if (!(
    c("wk", "day") %in% (names(reference_dates)) &&
    c("wk", "day") %in% (names(reference_weekday)) &&
    c("wk", "day") %in% (names(reference_windows))
    )) {
    stop(paste0("reference_dates, reference_weekday, and reference_windows ",
                "must be named lists with entries for 'wk' and 'day'."))
  }
  
  aligned_forecasts <- purrr::map_dfr(
    unique(forecasts$temporal_resolution),
    function(temporal_res) {
      align_forecasts_one_temporal_resolution(
        forecasts = forecasts %>%
          dplyr::filter(temporal_resolution == temporal_res),
        reference_dates = reference_dates[[temporal_res]],
        reference_weekday = reference_weekday[[temporal_res]],
        reference_windows = reference_windows[[temporal_res]],
        drop_nonpos_relative_horizons = drop_nonpos_relative_horizons
      )
    }
  )
  return(
    # use join to retain order of original dataframe
    dplyr::right_join(forecasts, aligned_forecasts, by = names(forecasts)) %>% 
    # place columns in a convenient order
    dplyr::relocate(reference_date, .after = forecast_date) %>% 
    dplyr::relocate(relative_horizon, .after = horizon)
  )
}

#' Internal function that add reference dates and relative horizons
#' to a dataframe of forecasts. This function requires that the forecasts
#' data frame contains only forecasts one temporal_resolution, i.e., only
#' at the daily resolution or the weekly resolution.
#' 
#' @param forecasts dataframe in format returned by \code{load_forecasts}
#' @param reference_dates a vector of reference dates for forecasts, grouping
#' forecasts that were made during the same week. May be a character vector in
#' the format "YYYY-MM-DD" or a vector of Dates. For example, if a forecast of
#' daily hospitalizations is issued with a forecast date that is Saturday,
#' 2021-09-04, and we want to align analyses around a Monday, the reference
#' date would be Monday, 2021-09-06. This can be accomplished more easily by
#' providing the `reference_weekday` argument below.
#' @param reference_weekday capitalized weekday, e.g. "Monday". The day of week
#' to use for reference dates, if the `reference_dates` argument is missing or
#' `NULL`
#' @param reference_windows an integer vector of offset values indicating the
#' set of forecast dates should be assigned to a particular reference date,
#' in units of number of days. If the `reference_weekday` is "Monday", the
#' function defaults to `-6:0`, so that all forecasts with a `forecast_date`
#' in the week leading up to a particular Monday will be assigned a reference
#' date of that Monday. By default, if the `reference_weekday` is "Saturday",
#' the function defaults to `-4:2`. This is appropriate for forecasts of
#' weekly targets, in which case we might like to assign a forecast issued on
#' a Monday to have a reference date of the previous Saturday.
#' @param drop_nonpos_relative_horizons boolean indicating whether forecasts that have a
#' non-positive horizon relative to the reference date should be dropped.
#' Defaults to `TRUE`
#'
#' @return forecast dataframe augmented by columns reference_date and
#' relative_horizon
align_forecasts_one_temporal_resolution <- function(
  forecasts,
  reference_dates,
  reference_weekday,
  reference_windows,
  drop_nonpos_relative_horizons
) {
  if (length(unique(forecasts$temporal_resolution)) > 1) {
    stop("standardize_forecasts_one_temporal_resolution only supports forecasts at a single temporal resolution.")
  }

  if (is.null(reference_windows)) {
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

  if (!is.null(reference_dates)) {
    # ensure we have dates
    reference_dates <- as.Date(reference_dates)
  } else {
    # every date from that of first forecast - diameter of first window
    # to that of last forecast + diameter of last window
    all_dates <- seq(
      min(forecasts$forecast_date) - (
        max(sort(reference_windows[[1]])) -
        min(sort(reference_windows[[1]]))
      ),
      max(forecasts$forecast_date) + (
        max(sort(reference_windows[[length(reference_windows)]])) -
        min(sort(reference_windows[[length(reference_windows)]])) 
      ),
      by = 1
    )

    # keep the dates identified above that are the specified reference_weekday
    reference_dates <- all_dates[weekdays(all_dates) == reference_weekday]
  }

  # create a tibble where each row contains:
  # - a possible forecast date
  # - a reference date to which that forecast date should be assigned
  ref_df <- tibble(
    reference_date = reference_dates,
    forecast_date = purrr::map2(
      reference_date, 
      reference_windows, 
            ~.x+.y
    )
  ) %>% unnest(cols = forecast_date)

  # ensure that in the tibble constructed above, each forecast date is
  # associated with at most one reference date
  # this could be violated if some windows are overlapping
  reps <- ref_df %>%
    dplyr::group_by(forecast_date) %>%
    dplyr::tally() %>% 
    dplyr::filter(n > 1)
  if (nrow(reps) > 0) {
    stop(paste0(
      "The following forecast dates are associated with multiple reference dates: ",
      paste(reps %>% dplyr::pull(forecast_date), collapse = ", ")
    ))
  }

  # join with the reference date lookup table above
  # and calculate the relative horizon
  forecasts <- forecasts %>% 
    dplyr::left_join(ref_df, by = "forecast_date") %>% 
    dplyr::mutate(
      ts_days = ifelse(temporal_resolution == "wk", 7, 1),
      relative_horizon = 
        ceiling(as.numeric((target_end_date - reference_date) / ts_days))
    ) %>%
    dplyr::select(-ts_days)

  if (drop_nonpos_relative_horizons) {
    forecasts <- forecasts %>%
      dplyr::filter(relative_horizon > 0)
  }

  return(forecasts)
}