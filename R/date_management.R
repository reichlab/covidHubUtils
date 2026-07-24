#' Calculate the date of the Saturday that ends the
#' MMWR epidemic week for the given date,offset a specified number
#' of epidemic week from a specified date
#'
#' @param forecast_date vector of dates as Date objects
#' @param week_offset vector of week offsets.  must be either length 1 or the same
#' length as `forecast_date`
#'
#' @return character vector of dates in 'yyyy-mm-dd' format
date_to_week_end_date <- function(forecast_date, week_offset = 0) {
  if (!(length(week_offset) %in% c(1, length(forecast_date)))) {
    stop("week_offset must be either length 1 or the same length as forecast_date")
  }

  result <- as.character(lubridate::ceiling_date(
    forecast_date + week_offset * 7,
    unit = "week"
  ) - 1)

  return(result)
}

#' Calculate end date for the week a forecast was made.
#'
#' Following
#' https://github.com/reichlab/covid19-forecast-hub/blob/master/data-processed/README.md,
#' "For week-ahead forecasts with forecast_date of Sunday or Monday of EW12, a
#' 1 week ahead forecast corresponds to EW12 and should have target_end_date of
#' the Saturday of EW12."  This means that the forecast week end date is set to
#' Saturday of EW11 (the previous week) for Sunday and Monday of EW12, and
#' Saturday of EW12 (the current week) for Tuesday through Saturday of EW12.
#'
#' @param forecast_date character vector of dates in 'yyyy-mm-dd' format
#'
#' @return character vector of dates in 'yyyy-mm-dd' format
#'
#' @export
calc_forecast_week_end_date <- function(forecast_date) {
  forecast_date <- lubridate::ymd(forecast_date)
  result <- rep(NA_character_, length(forecast_date))
  inds <- lubridate::wday(forecast_date) %in% 1:2
  result[inds] <- date_to_week_end_date(forecast_date[inds], week_offset = -1)
  result[!inds] <- date_to_week_end_date(forecast_date[!inds], week_offset = 0)

  return(result)
}


#' Calculate end date for the week a forecast is targeting.
#'
#' Following
#' https://github.com/reichlab/covid19-forecast-hub/blob/master/data-processed/README.md,
#' "For week-ahead forecasts with forecast_date of Sunday or Monday of EW12, a
#' 1 week ahead forecast corresponds to EW12 and should have target_end_date of
#' the Saturday of EW12."  This means that if horizon is 1, the forecast week
#' end date is set to Saturday of EW12 (the current week) for Sunday and Monday
#' of EW12, and Saturday of EW13 (the next week) for Tuesday through Saturday of
#' EW12.
#'
#' @param forecast_date character vector of dates in 'yyyy-mm-dd' format
#' @param horizon number of weeks ahead a prediction is targeting
#'
#' @return character vector of dates in 'yyyy-mm-dd' format
#'
#' @export
calc_target_week_end_date <- function(forecast_date, horizon) {
  forecast_date <- lubridate::ymd(forecast_date)
  result <- rep(NA_character_, length(forecast_date))
  inds <- lubridate::wday(forecast_date) %in% 1:2

  if (length(horizon) == 1) {
    result[inds] <- date_to_week_end_date(forecast_date[inds],
      week_offset = horizon - 1
    )
    result[!inds] <- date_to_week_end_date(forecast_date[!inds],
      week_offset = horizon
    )
  } else if (length(horizon) == length(forecast_date)) {
    result[inds] <- date_to_week_end_date(forecast_date[inds],
      week_offset = horizon[inds] - 1
    )
    result[!inds] <- date_to_week_end_date(forecast_date[!inds],
      week_offset = horizon[!inds]
    )
  }

  return(result)
}


#' Calculate end date for a forecast is targeting.
#'
#' @param forecast_date character vector of dates in 'yyyy-mm-dd' format
#' @param horizon number of weeks ahead a prediction is targeting
#' @param temporal_resolution string of target unit. Currently only 'wk' and 'day' are supported.
#'
#' @return character vector of dates in 'yyyy-mm-dd' format
#'
#' @export
calc_target_end_date <- function(forecast_date, horizon, temporal_resolution) {
  if (!all(temporal_resolution %in% c("wk", "day"))) {
    stop("Error in calc_target_end_date: Only 'wk' and 'day' are supported for temporal_resolution.")
  }
  result <- rep(NA_character_)
  inds <- (temporal_resolution == "wk")
  if (length(horizon) == 1) {
    result[inds] <- calc_target_week_end_date(forecast_date[inds], horizon)
    result[!inds] <-
      as.character(lubridate::ymd(forecast_date[!inds]) + horizon)
  } else if (length(horizon) == length(forecast_date)) {
    result[inds] <-
      calc_target_week_end_date(forecast_date[inds], horizon[inds])
    result[!inds] <-
      as.character(lubridate::ymd(forecast_date[!inds]) + horizon[!inds])
  }

  return(result)
}



#' Calculate the submission date that corresponds to a given forecast date
#'
#' Assuming that submissions are made on Mondays, this helper function
#' finds the nearest Monday for which a submission could have been made
#'
#' @param forecast_date character vector of dates in 'yyyy-mm-dd' format
#' @param submission_day day when forecasts have to be submitted. Default is
#' Monday.
#'
#' @return return character vecor with corresponding submission date
#' @importFrom lubridate ceiling_date
#'
#' @examples
#' \dontrun{
#' calc_submission_due_date(Sys.Date() + 0:7)
#' }
#' @export
calc_submission_due_date <- function(forecast_date, submission_day = "Monday") {
  dates <- as.character(forecast_date)

  dates <- ifelse(
    weekdays(as.Date(dates)) == submission_day,
    dates,
    as.character(
      lubridate::ceiling_date(as.Date(dates), unit = "weeks", week_start = 1)
    )
  )

  return(dates)
}

#' Create a datetime by appending time and/or UTC timezone
#' to the given date string, if necessary
#'
#' If you would like to set a timezone in date parameter, it has to be UTC now.
#' If not, this function will first convert the input to the default timezone based on hub parameter.
#' It is using Eastern Time for \code{hub = "US"} and CET for \code{hub = "ECDC"}.
#' Then, return the converted date time in UTC timezone.
#'
#' @param date date in character.
#' It could be a YYYY-MM-DD date,
#' or a YYYY-MM-DD date format with HH:MM:SS time,
#' or a YYYY-MM-DD date with HH:MM:SS time and UTC timezone.
#' Default to NULL
#' @param hub character vector, where the first element indicates the hub
#' to set default timezone. Possible options are `"US"`, `"ECDC"` and `"FluSight"`.
#'
#' @return NULL or datetime characters with date, time in UTC timezone
#'
#' @export
date_to_datetime <- function(date = NULL, hub = c("US", "ECDC", "FluSight")) {
  # default for `as_of` in zoltr
  if (is.null(date)) {
    return(NULL)
  }

  if (hub[1] == "US" | hub[1] == "FluSight") {
    default_timezone_location <- "America/New_York"
  } else if (hub[1] == "ECDC") {
    default_timezone_location <- "Europe/Stockholm"
  }

  # case 1: date only
  # append time and default timezone
  if (date == strftime(date, format = "%Y-%m-%d")) {
    date <- format(as.POSIXct(paste(date, "23:59:59", sep = " "),
      format = "%Y-%m-%d %H:%M:%S",
      tz = default_timezone_location
    ), tz = "UTC", usetz = TRUE)
    message("Warning in date_to_datetime: appending default time to the given input and converting it to default timezone.")
  }
  # case 2: date and time, no timezone
  # append default timezone
  else if (date == strftime(date, format = "%Y-%m-%d %H:%M:%S")) {
    date <- format(as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = default_timezone_location),
      tz = "UTC", usetz = TRUE
    )
    message("Warning in date_to_datetime: converting the given date to default timezone.")
  }
  # case 3: full date time in UTC timezone
  else if (grepl("UTC", date, fixed = TRUE)) {
    date <- strftime(date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = TRUE)
  }
  # case 4: none above, mostly for full date time in other timezones
  else {
    stop("Error in date_to_datetime: Please make sure date parameter is in the right format.")
  }

  return(date)
}
