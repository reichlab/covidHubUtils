#' load covid forecasts from local files or Zoltar
#'
#' @param models Character vector of model abbreviations
#' @param last_timezero The last timezero date of forecasts to retrieve
#' @param timezero_window_size The number of days back to go.  A window size of
#' 0 will retrieve only forecasts submitted on the `last_timezero` date.
#' @param targets character vector of targets to retrieve, for example
#' c('1 wk ahead cum death', '2 wk ahead cum death')
#' @param source string specifying where forecasts will be loaded from:
#' currently only "hub_repo" is supported; eventually, either "hub_repo" or
#' "zoltar"
#' @param hub_repo_path path to local clone of the reichlab/covid19-forecast-hub
#' repository
#'
#' @return data frame with columns model, timezero, location, target, quantile,
#' value
#'
#' @details One of `hub_repo_path` or `zoltar_connection` must be provided. If
#' both are provided, the `zoltar_connection` is ignored and forecasts are read
#' in from the local clone of the repository.
#'
#' @export
load_forecasts <- function (
  models,
  last_timezero,
  timezero_window_size,
  types,
  targets,
  source = "hub_repo",
  hub_repo_path) {
  # validate source
  source <- match.arg(source, choices = c("hub_repo", "zoltar"))

  # path to data-processed folder in hub repo
  data_processed <- file.path(hub_repo_path, "data-processed")

  # dates of forecasts to load
  forecast_dates <- as.character(last_timezero +
    seq(from = -timezero_window_size, to = 0, by = 1))

  if (source == "hub_repo") {
    forecasts <- purrr::map_dfr(
      models,
      function(model) {
        results_path <- paste0(submissions_root, model, '/',
                              submission_dates, '-', model, '.csv')
        results_path <- results_path[file.exists(results_path)]
        results_path <- tail(results_path, 1)

        if(length(results_path) == 0) {
          return(NULL)
        }

        readr::read_csv(results_path,
          col_types = cols(
            forecast_date = col_date(format = ""),
            target = col_character(),
            target_end_date = col_date(format = ""),
            location = col_character(),
            type = col_character(),
            quantile = col_double(),
            value = col_double()
          )) %>%
          dplyr::filter(
            tolower(type) %in% types,
            tolower(target) %in% targets) %>%
          dplyr::transmute(
            model = model,
            timezero = forecast_date,
            location = location,
            target = tolower(target),
            quantile = quantile,
            value = value
          )
      }
    )
  } else {
    stop('Only source = "hub_repo" is currently supported.')

    # Below is some old code that may not be correct.

    # Set up Zoltar
    zoltar_connection <- new_connection()
    zoltar_authenticate(
      zoltar_connection,
      Sys.getenv("Z_USERNAME"),
      Sys.getenv("Z_PASSWORD"))

    # obtain the quantile forecasts for required quantiles,
    # and the filter to last submission from each model for each week
    forecasts <-
      # get forecasts from zoltar
      purrr::map_dfr(
        monday_dates,
        zoltr::do_zoltar_query,
        models = models,
        timezero_window_size = timezero_window_size,
        targets = targets,
        zoltar_connection = zoltar_connection,
        project_url = project_url,
        verbose = TRUE
      ) %>%
      # keep only required columns and required quantiles
      dplyr::select(model, timezero, unit, target, quantile, value) %>%
      dplyr::filter(
        format(quantile, digits = 3, nsmall = 3) %in%
          format(required_quantiles, digits = 3, nsmall = 3)) %>%
      # put quantiles in columns
      tidyr::pivot_wider(names_from = quantile, values_from = value) %>%
      # create columns for horizon, forecast week end date of the forecast, and
      # target end date of the forecast
      dplyr::mutate(
        horizon = as.integer(substr(target, 1, 1)),
        forecast_week_end_date = calc_forecast_week_end_date(timezero),
        target_end_date = calc_target_week_end_date(timezero, horizon)
      ) %>%
      # keep only the last submission for each model for a given unit, target,
      # and forecast week end date
      dplyr::group_by(
        unit, target, forecast_week_end_date, model
      ) %>%
      dplyr::top_n(1, timezero) %>%
      # pivot longer; quantiles are in rows again
      tidyr::pivot_longer(
        cols = all_of(as.character(required_quantiles)),
        names_to = "quantile",
        values_to = "value") %>%
      ungroup()
  }

  return(forecasts)
}
