#' Calculate week end date (i.e., Saturday) for the MMWR epidemic week that is
#' offset a specified number of epidemic week from a specified date
#'
#' @param timezero character vector of dates in 'yyyy-mm-dd' format
#' @param integer vector of week offsets.  must be either length 1 or the same
#'     length as timezero
#'
#' @return character vector of dates in 'yyyy-mm-dd' format
date_to_week_end_date <- function(timezero, week_offset = 0) {
  if(!(length(week_offset) %in% c(1, length(timezero)))) {
    stop("week_offset must be either length 1 or the same length as timezero")
  }

  result <- purrr::pmap_chr(
    MMWRweek::MMWRweek(lubridate::ymd(timezero) + week_offset*7),
    function(MMWRyear, MMWRweek, MMWRday) {
      as.character(MMWRweek::MMWRweek2Date(MMWRyear, MMWRweek, 7))
    }
  )

  return(result)
}


#' Calculate end date for the week a forecast was made. Following
#' https://github.com/reichlab/covid19-forecast-hub/blob/master/data-processed/README.md,
#' "For week-ahead forecasts with forecast_date of Sunday or Monday of EW12, a
#' 1 week ahead forecast corresponds to EW12 and should have target_end_date of
#' the Saturday of EW12."  This means that the forecast week end date is set to
#' Saturday of EW11 (the previous week) for Sunday and Monday of EW12, and
#' Saturday of EW12 (the current week) for Tuesday through Saturday of EW12.
#'
#' @param timezero character vector of dates in 'yyyy-mm-dd' format
#'
#' @return character vector of dates in 'yyyy-mm-dd' format
#'
#' @export
calc_forecast_week_end_date <- function(timezero) {
  result <- ifelse(
    lubridate::wday(lubridate::ymd(timezero), label=TRUE) %in% c('Sun', 'Mon'),
    date_to_week_end_date(timezero, week_offset=-1),
    date_to_week_end_date(timezero, week_offset=0)
  )

  return(result)
}


#' Calculate end date for the week a forecast is targeting. Following
#' https://github.com/reichlab/covid19-forecast-hub/blob/master/data-processed/README.md,
#' "For week-ahead forecasts with forecast_date of Sunday or Monday of EW12, a
#' 1 week ahead forecast corresponds to EW12 and should have target_end_date of
#' the Saturday of EW12."  This means that if horizon is 1, the forecast week
#' end date is set to Saturday of EW12 (the current week) for Sunday and Monday
#' of EW12, and Saturday of EW13 (the next week) for Tuesday through Saturday of
#' EW12.
#'
#' @param timezero character vector of dates in 'yyyy-mm-dd' format
#' @param horizon number of weeks ahead a prediction is targeting
#'
#' @return character vector of dates in 'yyyy-mm-dd' format
#'
#' @export
calc_target_week_end_date <- function(timezero, horizon) {
  result <- ifelse(
    lubridate::wday(lubridate::ymd(timezero), label=TRUE) %in% c('Sun', 'Mon'),
    date_to_week_end_date(timezero, week_offset=horizon-1),
    date_to_week_end_date(timezero, week_offset=horizon)
  )

  return(result)
}
