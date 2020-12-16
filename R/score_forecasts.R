#' Score forecasts
#'
#' @param forecasts required data.frame with forecasts in the format returned
#' by load_forecasts
#' @param truths required data.frame with forecasts in the format returned
#' by load_truth
#' @param desired_score_types character vector of scores to calculate; defaults to returning
#' all available scores
#' @param return_format string: "long" returns long format with a column for
#' "score_name" and a column for "score_value"; "wide" returns wide format with
#' a separate column for each score. Defaults to "wide".
#'
#' @return data.frame with columns model, forecast_date, location, horizon,
#' temporal_resolution, target_variable, horizon, target_end_date, and scores:
#' If return_format is "long", also contains columns score_name and score_value
#' where score_name is the type of score calculated and score_value has the numeric
#' value of the score.
#' If return_format is "wide", each calculated score is in its own column.
#'
#' @export
score_forecasts <- function(
  forecasts,
  truth,
  desired_score_types,
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
  # as long as forecasts contains the columns above, it will pass the check
  if (missing(forecasts) || is.null(forecasts)) {
    stop("Forecast dataframe missing", call. = TRUE)
  } else if (!any(is.element(colnames(forecasts), forecasts_colnames))) {
    stop("Forecast dataframe columns malformed", call. = TRUE)
  }
  
  # truth data.frame format
  # columns: model, target_variable, target_end_date, location and value
  truth_colnames <- c(
    "model", "target_variable", "target_end_date", "location", "value"
  )
  
  # validate truth
  # as long as forecasts contains the columns above, it will pass the check
  if (missing(truth) || is.null(truth)) {
    stop("Truth dataframe missing", call. = TRUE)
  } else if (!any(is.element(colnames(truth), truth_colnames))) {
    stop("Truth dataframe columns malformed", call. = TRUE)
  }
  
  # validate return_format
  # match.arg returns error if arg does not match choice
  # which is a bit more complicated to deal with
  if (!is.element(return_format, c("long", "wide"))) {
    return_format <- "wide"
  }
  
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
  scores <- tibble::tibble(scoringutils::eval_forecasts(data = joint_df, by = observation_cols))
  
  # mangle the data for output
  # if users didn't provide score types
  if (!missing(desired_score_types)) {
    extra_na_colnames <- setdiff(desired_score_types, colnames(scores))
    extra_na_cols <- matrix(
      NA_real_,
      nrow = nrow(scores),
      ncol = length(extra_na_colnames),
      dimnames = list(NULL, extra_na_colnames)
    )
    
    scores <- scores %>%
      dplyr::select(c(observation_cols, desired_score_types)) %>%
      dplyr::bind_cols(as.data.frame(extra_na_cols))
  }
  
  # manipulate return format:
  #   eval_forecasts(), by default, returns in wide format
  #   only change if user specifies long return format
  if (return_format == "long") {
    scores <- scores %>% 
      tidyr::pivot_longer(
        cols = !any_of(observation_cols),
        names_to = "score_name",
        values_to = "score_value"
      )
  }
  
  
  scores
}