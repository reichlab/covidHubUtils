#' Score forecasts
#'
#' @param forecasts required data.frame with forecasts in the format returned
#' by load_forecasts
#' @param truths required data.frame with forecasts in the format returned
#' by load_truth
#' @param scores character vector of scores to calculate
#' @param return_format string: "long" returns long format with a column for
#' "score_name" and a column for "score_value"; "wide" returns wide format with
#' a separate column for each score.
#'
#' @return data.frame with columns model, forecast_date, location,
#' target_variable, horizon, target_end_date
#' If return_format is "long", also contains columns score_name and score_value
#' where score_name is one of wis, wis_width, wis_penalty_low, wis_penalty_high,
#' interval_coverage_ where level is 0.95 for a 95% confidence interval, or
#' quantile_coverage_ where level is 0.975 for a quantile level 0.975
#' score_value has the numeric value of the score.
#' If return_format is "wide", each calculated score is in its own column.
#'
#' @export
score_forecasts <- function(
  forecasts,
  truth,
  scores = vector(),
  return_format = "wide"
) {
  
  # forecasts data.frame format
  # columns: model, forecast_date, location, horizon, temporal_resolution,
  #          target_variable, target_end_date, type, quantile, value
  forecasts_colnames <- c(
    "model", "forecast_date", "location", "horizon", "temporal_resolution",
    "target_variable", "target_end_date", "type", "quantile", "value"
  )
  
  # validate forecasts
  if (missing(forecasts) || is.null(forecasts)) {
    stop("Forecast dataframe missing", call. = TRUE)
  } else if (!setequal(colnames(forecasts), forecasts_colnames)) {
    stop("Forecast dataframe columns malformed", call. = TRUE)
  }
  
  # truth data.frame format
  # columns: model, target_variable, target_end_date, location and value
  truth_colnames <- c(
    "model", "target_variable", "target_end_date", "location", "value"
  )
  
  # validate truth
  if (missing(truth) || is.null(truth)) {
    stop("Truth dataframe missing", call. = TRUE)
  } else if (!setequal(colnames(truth), truth_colnames)) {
    stop("Truth dataframe columns malformed", call. = TRUE)
  }
  
  # validate return_format
  if (return_format != "long" || return_format != "wide") {
    return_format <- "wide"
  }
  
  # make sure forecast_data has the truth_data it needs to score properly
  # score the minimally-viable subset of forecast_data
  
  # get dataframe into scoringutil format
  joint_df <- dplyr::left_join(x = forecasts, y = truth, 
                               by = c("location", "target_variable", "target_end_date")) %>%
    dplyr::select(-c("model.y")) %>%
    dplyr::rename(model = model.x, prediction = value.x, true_value = value.y) %>%
    dplyr::filter(!is.na(true_value))
  
  # score using scoringutil
  observation_cols <- c(
    "model", "location",
    "horizon", "temporal_resolution", "target_variable",
    "forecast_date", "target_end_date"
  )
  scores_df <- tibble::tibble(scoringutils::eval_forecasts(data = joint_df, by = observation_cols))
  
  # mangle the data for output
  if (length(scores) != 0) {
    extra_na_colnames <- setdiff(scores, colnames(scores_df))
    extra_na_cols <- matrix(
      NA_real_,
      nrow = nrow(scores_df),
      ncol = length(extra_na_colnames),
      dimnames = list(NULL, extra_na_colnames)
    )
    
    scores_df <- scores_df %>%
      dplyr::select(c(observation_cols, scores)) %>%
      dplyr::bind_cols(as.data.frame(extra_na_cols))
  }
  
  scores_df
  
}