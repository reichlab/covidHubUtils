#' Score forecasts
#'
#' @param forecasts required data.frame with forecasts in the format returned
#' by load_forecasts
#' @param truth required data.frame with forecasts in the format returned
#' by load_truth
#' @param return_format string: "long" returns long format with a column for
#' "score_name" and a column for "score_value"; "wide" returns wide format with
#' a separate column for each score. Defaults to "wide".
#'
#' @return data.frame with scores. The result will have some columns that
#' define the observation, namely, `model`, `forecast_date`, `location`, 
#' `horizon`, `temporal_resolution`, `target_variable`, `horizon`, and 
#' `target_end_date`.
#' Other columns will contain scores: 
#'  - `abs_error` is the absolute error based on median estimate,
#'  - `wis` is the weighted interval score
#'  - `sharpness` the component of WIS made up of interval widths
#'  - `overprediction` the component of WIS made up of overprediction of intervals
#'  - `underprediction` the component of WIS made up of underprediction of intervals
#'  - `coverage_X` are prediction interval coverage at alpha level X
#' If return_format is "long", also contains columns score_name and score_value
#' where score_name is the type of score calculated and score_value has the numeric
#' value of the score.
#' If return_format is "wide", each calculated score is in its own column.
#'
#' @references 
#' Bracher J, Ray EL, Gneiting T, Reich NG. (2020) Evaluating epidemic forecasts 
#' in an interval format. arXiv:2005.12881. 
#' \url{https://arxiv.org/abs/2005.12881}.
#'
#' @examples
#' \dontrun{
#' forecasts <- load_latest_forecasts(models=c("COVIDhub-ensemble", "UMass-MechBayes"), 
#'   last_forecast_date = "2020-12-14", 
#'   forecast_date_window_size = 7, 
#'   locations = c("US"), 
#'   targets = paste(1:4, "wk ahead inc death"), 
#'   source = "zoltar")
#' truth <- load_truth("JHU", target_variable = "inc death", locations = "US")
#' scores <- score_forecasts(forecasts, truth)
#' }
#'
#' @export
score_forecasts <- function(
  forecasts,
  truth,
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
    "model", 
    "location", 
    "horizon", "temporal_resolution", "target_variable",
    "forecast_date", "target_end_date"
  )

  scores <- tibble::tibble(scoringutils::eval_forecasts(data = joint_df, 
    by = observation_cols,
    summarise_by = c(observation_cols, "range"),
    ## the below interval_score_arguments should ensure that WIS is computed correctly
    interval_score_arguments = list(weigh = TRUE, count_median_twice=FALSE))) %>%
    tidyr::pivot_wider(id_cols = observation_cols,
      names_from = c("range"), 
      values_from = c("coverage", "interval_score", "aem", "sharpness", "overprediction", "underprediction")) %>%
    purrr::set_names(~sub("aem_0", "abs_error", .x)) %>% 
    ## before next lines: do we need to check to ensure interval_score columns exist?
    ## the following lines ensure that we use denominator for the wis of 
    ## (# of interval_scores)-0.5
    ## which is written in the paper and elsewhere as
    ## (# of interval_scores at level >0 ) + 0.5 or (K + 1/2)
    dplyr::mutate(
      n_interval_scores = rowSums(!is.na(dplyr::select(., dplyr::starts_with("interval_score")))),
      interval_score_0_exists = "interval_score_0" %in% names(.),
      wis = rowSums(dplyr::select(., dplyr::starts_with("interval_score")))/(n_interval_scores-0.5*(interval_score_0_exists)),
    ) %>%
    dplyr::mutate(
      sharpness = rowMeans(dplyr::select(., dplyr::starts_with("sharpness"))),
      overprediction = rowMeans(dplyr::select(., dplyr::starts_with("overprediction"))),
      underprediction = rowMeans(dplyr::select(., dplyr::starts_with("underprediction")))
    ) %>%
    dplyr::select(
      -dplyr::starts_with("aem_"), 
      -dplyr::starts_with("interval_score"), 
      -dplyr::starts_with("sharpness_"), 
      -dplyr::starts_with("underprediction_"), 
      -dplyr::starts_with("overprediction_")
    ) 
  
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
