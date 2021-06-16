#' Score forecasts
#'
#' @param forecasts required data.frame with forecasts in the format returned
#' by load_forecasts
#' @param truth required data.frame with forecasts in the format returned
#' by load_truth
#' @param return_format string: "long" returns long format with a column for
#' "score_name" and a column for "score_value"; "wide" returns wide format with
#' a separate column for each score. Defaults to "wide".
#' @param use_median_as_point logical: "TRUE" uses the median as the point
#' forecast when scoring; "FALSE" uses the point forecasts from the data when
#' scoring. Defaults to "FALSE"
#' 
#' @importFrom dplyr any_of
#' @return data.frame with scores. The result will have some columns that
#' define the observation, namely, `model`, `forecast_date`, `location`,
#' `horizon`, `temporal_resolution`, `target_variable`, `horizon`, and
#' `target_end_date`.
#' Other columns will contain scores:
#'  - `true_value` is the observed truth at that `location` and `target_end_date`
#'  - `abs_error` is the absolute error based on median estimate if
#'  use_median_as_point is TRUE, and absolute error based on point forecast
#'  if use_median_as_point is FALSE
#'  - `wis` is the weighted interval score
#'  - `sharpness` the component of WIS made up of interval widths
#'  - `overprediction` the component of WIS made up of overprediction of intervals
#'  - `underprediction` the component of WIS made up of underprediction of intervals
#'  - `coverage_X` are prediction interval coverage at alpha level X
#'  - `quantile_coverage_0.X` are one-sided quantile coverage at quantile X
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
#' \dontrun{
#' forecasts <- load_latest_forecasts(models=c("ILM-EKF"),
#'   hub = c("ECDC","US"), last_forecast_date = "2021-03-08",
#'   forecast_date_window_size = 0,
#'   locations = c("GB"),
#'   targets = paste(1:4, "wk ahead inc death"),
#'   source = "zoltar")
#' truth <- load_truth("JHU",hub = c("ECDC","US"), 
#'   target_variable = "inc death", locations = "GB")
#' scores <- score_forecasts(forecasts, truth)
#' }
#'
#' @export
score_forecasts <- function(
  forecasts,
  truth,
  return_format = "wide",
  use_median_as_point = FALSE
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

  # validate use_median_as_point
  if (is.null(use_median_as_point)) {
    stop("use_median_as_point is NULL and should be one of (TRUE,FALSE)")
  }

  # match.arg does not like logical input
  if (!(use_median_as_point %in% c(FALSE,TRUE))) {
    stop("use_median_as_point should be one of (TRUE,FALSE)")
  }

  if (length(use_median_as_point) != 1) {
    stop("use_median_as_point should only have a length of 1")
  }

  if (use_median_as_point==FALSE && !("point" %in% unique(forecasts$type))){
    stop("Want to use point forecast when scoring but no point forecast in forecast data")
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
    "forecast_date", "target_end_date","true_value"
  )

  # creates placeholder variables to store the name of the column from scoringutils::eval_forecasts() to
  # take values from (`abs_var`) and the column name to rename as "abs_error" (`abs_var_rename`)
  if (use_median_as_point) {
    abs_var <- "aem"
    abs_var_rename <- "aem_0"
  } else {
    abs_var <- "ae_point"
    abs_var_rename <- "ae_point_NA"
  }
  
  
  # two sided
  scores <- purrr::map_dfr(
    unique(joint_df$target_variable),
    function(var) {
      joint_df_target <- suppressMessages(joint_df %>% 
                                            dplyr::filter(target_variable==var))
      scoringutils::eval_forecasts(data = joint_df_target,
                                   by = observation_cols,
                                   summarise_by = c(observation_cols, "range"),
                                   ## the below interval_score_arguments should ensure that WIS is computed correctly
                                   interval_score_arguments = list(weigh = TRUE, count_median_twice=FALSE)) %>%
        tidyr::pivot_wider(id_cols = observation_cols,
                           names_from = c("range"),
                           values_from = c("coverage", "interval_score", abs_var, "sharpness", "overprediction", "underprediction")) %>%
        purrr::set_names(~sub(abs_var_rename, "abs_error", .x)) %>%
        ## need to remove all columns ending with NA to not affect WIS calculations
        dplyr::select(
          -dplyr::ends_with("_NA")
        ) %>%
        ## before next lines: do we need to check to ensure interval_score columns exist?
        ## the following lines ensure that we use denominator for the wis of
        ## (# of interval_scores)-0.5
        ## which is written in the paper and elsewhere as
        ## (# of interval_scores at level >0 ) + 0.5 or (K + 1/2)
        ## to make sure that the median only gets half the weight of the other
        ## intervals, multiply its value by 0.5
        dplyr::mutate(
          n_interval_scores = rowSums(!is.na(dplyr::select(., dplyr::starts_with("interval_score")))),
          exists_interval_score_0 = "interval_score_0" %in% names(.),
          interval_score_0 = ifelse(exists_interval_score_0, 0.5 * interval_score_0, NA_real_),
          sharpness_0 = ifelse(exists_interval_score_0, 0.5 * sharpness_0, NA_real_),
          underprediction_0 = ifelse(exists_interval_score_0, 0.5 * underprediction_0, NA_real_),
          overprediction_0 = ifelse(exists_interval_score_0, 0.5 * overprediction_0, NA_real_)) %>%
        dplyr::mutate(
          wis = rowSums(dplyr::select(., dplyr::starts_with("interval_score")), na.rm = FALSE)/(n_interval_scores-0.5*(exists_interval_score_0)),
        ) %>%
        dplyr::mutate(
          sharpness = rowSums(dplyr::select(., dplyr::starts_with("sharpness")), na.rm = FALSE)/(n_interval_scores-0.5*(exists_interval_score_0)),
          overprediction = rowSums(dplyr::select(., dplyr::starts_with("overprediction")), na.rm = FALSE)/(n_interval_scores-0.5*(exists_interval_score_0)),
          underprediction = rowSums(dplyr::select(., dplyr::starts_with("underprediction")), na.rm = FALSE)/(n_interval_scores-0.5*(exists_interval_score_0))
        ) %>%
        dplyr::select(
          -dplyr::starts_with("aem_"),
          -dplyr::starts_with("ae_point_"),
          -dplyr::starts_with("interval_score"),
          -dplyr::starts_with("sharpness_"),
          -dplyr::starts_with("underprediction_"),
          -dplyr::starts_with("overprediction_")
        ) %>% 
        dplyr::select(1:8, dplyr::starts_with("coverage_"),
                      dplyr::starts_with("abs_error"),
                  "n_interval_scores", "exists_interval_score_0", "wis",
                  "sharpness", "overprediction", "underprediction")
    })
     
  # one sided     
  sq <- scoringutils::eval_forecasts(data = joint_df,
                                     by = observation_cols,
                                     summarise_by = c(observation_cols, "quantile"),
                                     interval_score_arguments = list(weigh = TRUE, count_median_twice=FALSE))%>%
    tidyr::pivot_wider(id_cols = observation_cols,
                       names_from = c("quantile"),
                       values_from = c("quantile_coverage", "interval_score", abs_var, "sharpness", "overprediction", "underprediction")) %>%
    purrr::set_names(~sub(abs_var_rename, "abs_error", .x))%>%
    dplyr::select(
      -dplyr::ends_with("_NA")
    ) %>% 
    dplyr::select(
      -dplyr::starts_with("abs_error."),
      -dplyr::starts_with("aem_"),
      -dplyr::starts_with("ae_point_"),
      -dplyr::starts_with("interval_score"),
      -dplyr::starts_with("sharpness_"),
      -dplyr::starts_with("underprediction_"),
      -dplyr::starts_with("overprediction_")
    )  
  #order one-sided quantiles to ascending order
  quantile_coverage_columns <-
    sort(colnames(sq %>% 
                    dplyr::select(dplyr::starts_with("quantile_coverage_"))))
  
  #select necessary columns and the one-sided quantiles in ascending order
  scores_one_sided <- sq %>% 
    dplyr::select(1:8, dplyr::all_of(quantile_coverage_columns))
  
  #combine one and two sided
  scores <- suppressMessages(dplyr::full_join(scores_one_sided, 
                                              scores))
  
                    
  
  
  if ("coverage_0" %in% names(scores)) {
    scores <- scores %>% 
      dplyr::select(-c("coverage_0"))
    }


  # manipulate return format:
  #   eval_forecasts(), by default, returns in wide format
  #   only change if user specifies long return format
  if (return_format == "long") {
    scores <- scores %>%
      tidyr::pivot_longer(
        cols = !dplyr::any_of(observation_cols[observation_cols != "true_value"]),
        names_to = "score_name",
        values_to = "score_value"
      )
  }


  scores
}
