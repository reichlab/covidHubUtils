#' Aggregate daily data to weekly data
#'
#' If the last week is not complete, drop all observations from the
#' previous Saturday in that week
#'
#' @param data data.frame with columns `model`, `location`, 
#' `target_variable`, `target_end_date` and `value`
#'
#' @return data.frame with the same set of input columns and weekly 
#' aggregated data in `value` column
#'
#' @export
aggregate_to_weekly <- function(data) {
  # check if data has all needed columns
  columns_check <- all(c(
    "model", "target_variable",
    "target_end_date", "location", "value"
  )
  %in% colnames(data))
  if (!columns_check) {
    stop("Error in aggregate_to_weekly: Please provide columns model, 
           target_variable, target_end_date, location and value in data.")
  }

  data <- data %>%
    dplyr::mutate(
      sat_date = lubridate::ceiling_date(
        lubridate::ymd(target_end_date),
        unit = "week"
      ) - 1
    ) %>%
    dplyr::group_by(model, location, target_variable) %>%
    # if the last week is not complete, drop all observations from the
    # previous Saturday in that week
    dplyr::filter(
      if (max(target_end_date) < max(sat_date)) target_end_date <= max(sat_date) - 7 else TRUE
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-target_end_date) %>%
    dplyr::rename(target_end_date = sat_date) %>%
    dplyr::group_by(model, location, target_end_date, target_variable) %>%
    dplyr::summarize(value = sum(value, na.rm = FALSE)) %>%
    dplyr::ungroup()

  return(data)
}
