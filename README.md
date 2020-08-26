# covidHubUtils
Utility functions for the COVID-19 forecast hub

We have discussed including the following functions in this repository:

**Reading Forecast Data**
 * `get_model_designations(model_abbrs, hub_repo_path, zoltar_connection)`: Assemble a data frame with columns model and designation
 * `load_forecasts(model_abbrs, forecast_dates, targets, <other qualifiers here>, hub_repo_path, zoltar_connection)`: load forecasts either from csv files within a local clone of the covid19-forecast-hub repository or calling `zoltr::do_zoltar_query`.  The two sources differ in format, so standardize return results around the format used by Zoltar.

**Reading Observed "Truth" Data**
* `load_truth(truth_source, truth_date, hub_repo_path, hub_zoltar_connection, project_url)`: load truth data either by calling `zoltr::truth` or by calling `covidData::load_jhu_data` and converting to format used by zoltr, or by reading in a csv file.

**Plotting Forecasts**
 * `plot_forecasts(forecasts, truth_source, truth_date, <options for plots here>, plot_path)`: plot by assembling truth and then calling `zoltr::plot_forecasts`

**Scoring Forecasts**
 * `score_forecasts(forecasts, truth, scores = c("WIS", "PIT", "interval coverage", "quantile coverage", ...))` Calculate specified scores for each combination of `timezero`, `unit`, and `target` in the `forecasts` data frame.
