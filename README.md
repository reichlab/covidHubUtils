# covidHubUtils
Utility functions for the COVID-19 forecast hub

Currently available:

**Reading Forecast Data**
 * `get_model_designations(models, source, hub_repo_path)`: Assemble a data frame with columns model and designation.
 * `load_latest_forecasts(models, last_forecast_date, forecast_date_window_size = 1, locations, types, targets, source, hub_repo_path)`: Load the most recent forecasts in a specified time window either from a local clone of the covid19-forecast-hub repository or Zoltar.
 * `load_forecasts(models, forecast_dates, locations, types, targets)`: Load all available forecasts from Zoltar.
 
**Reading Observed "Truth" Data**
* `load_truth(truth_source, target_variable, truth_end_date, temporal_resolution, locations, data_location, local_repo_path)`: Load truth data for specified target variable and locations from covid19-forecast-hub repository.

**Plotting Forecasts**
 * `plot_forecast(forecast_data, truth_data, model, target_variable, location, intervals, horizon, truth_source, plot, truth_as_of)`: Plot forecast with optional truth data for only one combination of model, target and location.

Coming next: 

**Scoring Forecasts**
 * `score_forecasts(forecasts, truth, scores = c("WIS", "PIT", "interval coverage", "quantile coverage", ...))` Calculate specified scores for each combination of `timezero`, `unit`, and `target` in the `forecasts` data frame.
