#' Filter covid forecasts for pairwise Cramér distance approximation.
#'
#' @param dataframe required data.frame with forecasts in the format returned
#' by load_forecasts
#'
#' @return data frame with columns model, location, horizon,
#' target_variable, target_end_date, type, quantile, value,
#' abbreviation
#'
#' @export
#'
pairwise_filter <- function(dataframe) {
  required_cols <- c(
    "location",
    "target_variable",
    "target_end_date",
    "type",
    "quantile",
    "model",
    "value",
    "horizon",
    "abbreviation"
  )
  check <- required_cols %in% colnames(dataframe)
  if (sum(check) != 9) {
    missing_cols <- required_cols[!check]
    stop(paste0(" Missing column: ", missing_cols), call. = TRUE)
  }
  # get unique locations in the data
  n_locs <- length(unique(dataframe$location))
  # filtering process
  filtered_frame <- dataframe %>%
    # filter out NA or NULL values and keep only quantile
    dplyr::filter(!any(is.na(value)), !any(is.null(value)),
                  type == "quantile") %>%
    # filter out forecasts with less quantile levels than those provided by the model(s)
    # with the maximum number of quantile levels within group
    dplyr::group_by(location, horizon,  target_end_date, model, target_variable) %>%
    dplyr::mutate(n_q = n_distinct(quantile)) %>%
    ungroup() %>%
    dplyr::group_by(location, horizon,  target_end_date, target_variable) %>%
    dplyr::filter(n_q == max(n_q)) %>%
    ungroup() %>%
    dplyr::select(-"n_q") %>%
    # convert to wide format
    dplyr::select(
      "location",
      "target_variable",
      "target_end_date",
      "type",
      "quantile",
      "model",
      "value",
      "horizon",
      "abbreviation"
    ) %>%
    dplyr::arrange(
      target_variable,
      location,
      horizon,
      target_variable,
      target_end_date,
      model,
      quantile
    ) %>%
    tidyr::pivot_wider(names_from = model, values_from = value)
  return(filtered_frame)
}
