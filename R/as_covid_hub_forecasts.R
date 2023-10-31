#' Reformat model outputs stored as a `model_output_tbl` class (or similar) to 
#' that of a `data.frame` formatted according to standards of the COVID-19 
#' Forecasting Hub which can be processed by functions from the `covidHubUtils` 
#' package such as `score_forecasts()` or `plot_forecasts()`. The supplied 
#' `model_output_tbl` should have columns defining properties akin to 
#' reference dates, locations, horizons, and targets.
#'
#' @param model_outputs an object of class `model_output_tbl` with component
#'   model outputs (e.g., predictions). Should have columns containing the
#'   following information: model name, reference date or target end date, 
#'   location, horizon, target, temporal resolution*, output type, output 
#'   type id, and value. Note that the temporal resolution may be included 
#'   in the target column.
#' @param model_id_col `character` string of the name of the column 
#'   containing the model name(s) for the forecasts. Defaults to "model_id".
#'   Should be set to NULL if no such column exists, in which case a model_id
#'   column will be created populated with the value "model_id".
#' @param location_col `character` string of the name of the column 
#'   containing the locations for the forecasts. Defaults to "location".
#' @param horizon_col `character` string of the name of the column 
#'   containing the horizons for the forecasts. Defaults to "horizon".
#' @param target_col `character` string of the name of the column 
#'   containing the targets for the forecasts. Defaults to "target". If 
#'   `temp_res_col` is NULL, the target column in `model_outputs` is assumed
#'   to contain targets of the form "[temporal resolution] [target]" or 
#'   "[temporal resolution] ahead [target]", such as "wk ahead inc flu hosp"
#'   "wk inc flu hosp".
#' @param reference_date_col `character` string of the name of the column
#'   containing the reference dates for the forecasts. Defaults to 
#'   "forecast_date". Should be set to NULL if no such column exists, in which 
#'   case the column will be created using the following information: 
#'   horizon, target end date, and temporal resolution.
#' @param target_end_date_col `character` string of the name of the column 
#'   containing the target end dates for the forecasts. Defaults to 
#'   "target_end_date". Should be set to NULL if no such column exists, in
#'   which case the column will be created using the following information:
#'   horizon, forecast date, and temporal resolution.
#' @param output_type_col `character` string of the name of the column 
#'   containing the output types for the forecasts. Defaults to "output_type".
#' @param output_type_id_col `character` string of the name of the column 
#'   containing the output type ids for the forecasts. Defaults to 
#'   "output_type_id".
#' @param value_col `character` string of the name of the column 
#'   containing the values for the forecasts. Defaults to "value".
#' @param temp_res_col `character` string of the name of the column 
#'   containing the temporal resolutions for the forecasts. Defaults to 
#'   "temporal_resolution". Should be set to NULL if no such column exists,
#'   in which case the column will be created from the existing target column.
#'
#' @return a `data.frame` of reformatted model outputs that may be fed into 
#'   any of the `covidHubUtils` functions with 10 total columns: model,
#'   forecast_date, location, horizon, temporal_resolution, target_variable,
#'   target_end_date, type, quantile, value. Other columns are removed.
#' @export
#'
#' @examples
# ' forecasts <- load_forecasts(
#'   models = c("COVIDhub-ensemble", "UMass-MechBayes"),
#'   dates = "2020-12-14",
#'   date_window_size = 7,
#'   locations = c("US"),
#'   targets = paste(1:4, "wk ahead inc death"),
#'   source = "zoltar"
#' ) 
#' altered_forecasts <- forecasts |> # Alter forecasts to not be CovidHub format
# '   rename(model_id=model, output_type=type, output_type_id=quantile) |>
#'   mutate(target_variable = "wk ahead inc death", horizon=as.numeric(horizon)) |>
#'   select(-temporal_resolution)
#' formatted_forecasts <- as_covid_hub_forecasts(
#'    altered_forecasts, 
#'    target_col="target_variable", 
#'    temp_res_col=NULL 
#' ) |>
#' mutate(horizon=as.character(horizon))
#' expect_equal(formatted_forecasts, dplyr::select(forecasts, model:value)) 

as_covid_hub_forecasts <- function(model_outputs, model_id_col = "model_id",
                                  reference_date_col="forecast_date", 
                                  location_col="location", 
                                  horizon_col="horizon", target_col="target", 
                                  output_type_col="output_type",
                                  output_type_id_col="output_type_id",
                                  value_col="value",
                                  temp_res_col="temporal_resolution", 
                                  target_end_date_col="target_end_date") {

  provided_names <- c(model_id_col, reference_date_col, location_col, horizon_col, target_col, output_type_col, output_type_id_col, value_col, temp_res_col, target_end_date_col)
  
  if (isFALSE(all(order(provided_names)) %in% order(names(model_outputs)))) {
    stop("Not all provided column names exist in the provided model_outputs.")
  }

  if (is.null(reference_date_col) & is.null(target_end_date_col)) {
    stop("You must provide at least one date column")
  }
  
  if (all(c("mean", "median") %in% unique(model_outputs[[output_type_col]]))){
    stop("You may only have one type of point forecast.")
  }
  
  if (is.null(model_id_col)) {
    warning("No model_id_col provided, creating one automatically.")
    model_outputs <- dplyr::mutate(model_outputs, model_id = "model_id", .before = 1)
  }
  
  model_outputs <- model_outputs |> 
    dplyr::rename(model = model_id_col, 
                  type = output_type_col, quantile = output_type_id_col,
                  forecast_date = reference_date_col, location = location_col, 
                  value = value_col, target_variable = target_col) 
  
  if (is.null(temp_res_col)) {
    model_outputs <- model_outputs |>
      dplyr::rename(target = target_variable) |>
      mutate(target = ifelse(
        stringr::str_detect(target, "ahead"), 
        stringr::str_replace(target, "ahead", "") |> stringr::str_squish(), 
        target)) |>
      tidyr::separate(target, sep=" ", convert=TRUE, into=c("temporal_resolution", "target_variable"), extra="merge")
  }
  
  if (is.null(reference_date_col)) {
    outputs <- outputs |>
      dplyr::mutate(forecast_date=case_when(
        temporal_resolution %in% c("d", "day") ~ target_end_date - lubridate::days(horizon),
        temporal_resolution %in% c("w", "wk", "week") ~ target_end_date - lubridate::weeks(horizon),
        temporal_resolution %in% c("m", "mth", "mnth", "month") ~ target_end_date %m-% months(horizon),
        temporal_resolution %in% c("y", "yr", "year") ~ target_end_date - lubridate::years(horizon),
        .default = target_end_date), 
      .before = type) 
  }

  if (is.null(target_end_date_col)) {
    model_outputs <- model_outputs |>
      dplyr::mutate(target_end_date=case_when(
        temporal_resolution %in% c("d", "day") ~ forecast_date + lubridate::days(horizon),
        temporal_resolution %in% c("w", "wk", "week") ~ forecast_date + lubridate::weeks(horizon),
        temporal_resolution %in% c("m", "mth", "mnth", "month") ~ forecast_date %m+% months(horizon),
        temporal_resolution %in% c("y", "yr", "year") ~ forecast_date + lubridate::years(horizon),
        .default = forecast_date), 
      .before = type) 
  }
  
  covid_hub_outputs <- model_outputs |>                
    dplyr::mutate(type = ifelse(type %in% c("mean", "median"), "point", type)) |>
    dplyr::select(model, forecast_date, location, horizon, temporal_resolution, 
                  target_variable, target_end_date, type, quantile, value)

  return (covid_hub_outputs)
}
