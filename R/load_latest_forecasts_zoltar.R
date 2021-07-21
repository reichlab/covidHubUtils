#' Load the most recent forecasts of all that were submitted
#' on `forecast_dates` from Zoltar.
#'
#' This function will throw a warning and return an empty data frame when
#' no forecasts are submitted on any dates in `forecast_dates` for selected `models`,
#' `locations`, `types` and `target`.
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#' Please use [load_forecasts_zoltar()] instead.
#'
#' @inheritParams load_forecasts_zoltar
#'
#' @export
load_latest_forecasts_zoltar <- function(models = NULL,
                                         forecast_dates = NULL,
                                         locations = NULL,
                                         types = NULL,
                                         targets = NULL,
                                         as_of = NULL,
                                         hub = c("US", "ECDC"),
                                         verbose = TRUE) {
  lifecycle::deprecate_warn("0.1.5",
    "load_latest_forecasts_zoltar()",
    details =
      "This function has been superseded by the latest load_forecasts_zoltar(). Please switch your code to using the new function."
  )


  forecast <- load_forecasts_zoltar(
    models = models,
    forecast_dates = forecast_dates,
    locations = locations,
    types = types,
    targets = targets,
    as_of = as_of,
    verbose = verbose,
    hub = hub
  )

  if (nrow(forecast) != 0) {
    # filter to get the latest forecast for each model
    forecast <- forecast %>%
      dplyr::group_by(model) %>%
      dplyr::filter(forecast_date == max(forecast_date)) %>%
      dplyr::ungroup()
  }

  return(forecast)
}
