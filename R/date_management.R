#' Calculate the date of the Saturday that ends the
#' MMWR epidemic week for the given date,offset a specified number 
#' of epidemic week from a specified date
#'
#' @param forecast_date vector of dates as Date objects
#' @param integer vector of week offsets.  must be either length 1 or the same
#'     length as forecast_date
#'
#' @return character vector of dates in 'yyyy-mm-dd' format
date_to_week_end_date <- function(forecast_date, week_offset = 0) {
  if(!(length(week_offset) %in% c(1, length(forecast_date)))) {
    stop("week_offset must be either length 1 or the same length as forecast_date")
  }
  
  result <- as.character(lubridate::ceiling_date(
    forecast_date + week_offset*7, unit = 'week') - 1)

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
#' @param forecast_date character vector of dates in 'yyyy-mm-dd' format
#'
#' @return character vector of dates in 'yyyy-mm-dd' format
#'
#' @export
calc_forecast_week_end_date <- function(forecast_date) {
  forecast_date <- lubridate::ymd(forecast_date)
  result <- rep(NA_character_, length(forecast_date))
  inds <- (lubridate::wday(forecast_date, label = TRUE) %in% c("Sun", "Mon"))
  result[inds] <- date_to_week_end_date(forecast_date[inds], week_offset = -1)
  result[!inds] <- date_to_week_end_date(forecast_date[!inds], week_offset = 0)

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
#' @param forecast_date character vector of dates in 'yyyy-mm-dd' format
#' @param horizon number of weeks ahead a prediction is targeting
#'
#' @return character vector of dates in 'yyyy-mm-dd' format
#'
#' @export
calc_target_week_end_date <- function(forecast_date, horizon) {
  forecast_date <- lubridate::ymd(forecast_date)
  result <- rep(NA_character_, length(forecast_date))
  inds <- (lubridate::wday(forecast_date, label = TRUE) %in% c("Sun", "Mon"))

  if (length(horizon) == 1) {
    result[inds] <- date_to_week_end_date(forecast_date[inds],
      week_offset = horizon - 1)
    result[!inds] <- date_to_week_end_date(forecast_date[!inds],
      week_offset = horizon)
  } else if (length(horizon) == length(forecast_date)) {
    result[inds] <- date_to_week_end_date(forecast_date[inds],
      week_offset = horizon[inds] - 1)
    result[!inds] <- date_to_week_end_date(forecast_date[!inds],
      week_offset = horizon[!inds])
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
#' @param submision_day day when forecasts have to be submitted. Default is 
#' Monday. 
#' 
#' @return return character vecor with corresponding submission date
#' @importFrom lubridate ceiling_date
#'
#' @examples 
#' calc_submission_due_date(Sys.Date() + 0:7) 
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

#' Create a datetime by appending time and/or timezone 
#' to the given date string, if necessary
#' 
#' @param date date in character. 
#' It could be a YYYY-MM-DD date, 
#' or a YYYY-MM-DD date format with HH:MM:SS time, 
#' or a YYYY-MM-DD date with HH:MM:SS time and timezone.
#' @param hub character vector, where the first element indicates the hub
#' to set default timezone. Possible options are "US" and "ECDC".
#' 
#' @return datetime characters with date, time and timezone
#' 
#' @export
date_to_datetime <- function(date, hub = c("US", "ECDC")){
  if (hub[1] == "US"){
    default_timezone <- "America/New_York" 
  } else if (hub[1] == "ECDC"){
    default_timezone <- "Europe/Berlin"
  }
 
  # case 1: date only
  # append time and default timezone
  if (date == strftime(date, format = "%Y-%m-%d")){
    date <- strftime(paste(date, "23:59:59", sep = " "),
                      format = "%Y-%m-%d %H:%M:%S", 
                     tz = default_timezone, 
                     usetz = TRUE)
    #date <- strftime(date, tz = default_timezone, usetz = TRUE)
    warning("Warning in date_to_datetime: appending default time and timezone to the given date.")
  } 
  # case 2: date and time, no timezone
  # append default timezone
  else if (date == strftime(date, format = "%Y-%m-%d %H:%M:%S")){
    date <- strftime(date, format = "%Y-%m-%d %H:%M:%S", tz = default_timezone, usetz = TRUE)
    warning("Warning in date_to_datetime: appending default timezone to the given date.")
  } else {
    date <- format(x = date, format = "%Y-%m-%d %H:%M:%S", usetz = TRUE)
  }
  
  if (nchar(date) < 22){
    stop("Error in date_to_datetime: Please make sure date parameter is in the right format.")
  }
  
  return (date)
}
