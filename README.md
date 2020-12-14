# covidHubUtils
Utility functions for the COVID-19 forecast hub

# Installation
``` r
# install.packages("devtools")
devtools::install_github("reichlab/covidHubUtils")
```
# Features

## Currently available:

**Reading Forecast Data**
 * `get_model_designations(models, source, hub_repo_path)`: Assemble a data frame with columns model and designation.
 * `load_latest_forecasts(models, last_forecast_date, forecast_date_window_size = 1, locations, types, targets, source, hub_repo_path)`: Load the most recent forecasts in a specified time window either from a local clone of the covid19-forecast-hub repository or Zoltar.
 * `load_forecasts(models, forecast_dates, locations, types, targets)`: Load all available forecasts from Zoltar.
 
**Reading Observed "Truth" Data**
* `load_truth(truth_source, target_variable, truth_end_date, temporal_resolution, locations, data_location, local_repo_path)`: Load truth data for specified target variable and locations from covid19-forecast-hub repository. **Note:** Truth data for `"inc hosp"` is not available now.

**Plotting Forecasts**
 * `plot_forecast(forecast_data, truth_data, models, target_variable, locations, facet, facet_scales, forecast_dates, intervals, horizon, truth_source, plot_truth, plot, fill_by_model, truth_as_of, title, subtitle, show_caption)`: Plot forecasts with optional truth data for multiple models, locations and forecast dates. **Note:** If `target_variable` is `"inc hosp"`, please provide `truth_data`and the corresponding `truth_source`. To see more example plots, please to go [vignettes/demo](https://htmlpreview.github.io/?https://github.com/reichlab/covidHubUtils/blob/master/vignettes/demo.html).

**Download and pre-process "Truth" Data**
 * `download_raw_nytimes(save_location)`: Download raw truth data from NYTimes and write to CSV files.
 * `download_raw_usafacts(save_location)`: Download raw truth data from USAFacts and write to CSV files.
 * `preprocess_nytimes(save_location)`: Preprocess raw truth data from NYTimes into Cumulative/Incident - Deaths/Cases and write to CSVs
 * `preprocess_usafacts(save_location)`: Preprocess raw truth data from USAFacts into Cumulative/Incident - Deaths/Cases and write to CSVs
 * `preprocess_jhu(save_location)`: Preprocess raw truth data from JHU CSSE into Cumulative/Incident - Deaths/Cases and write to CSVs. **Note:** To use this method, the [covidData](https://github.com/reichlab/covidData) package needs to be installed. 

## Coming next: 

**Scoring Forecasts**
 * `score_forecasts(forecasts, truth, scores = c("WIS", "PIT", "interval coverage", "quantile coverage", ...))` Calculate specified scores for each combination of `timezero`, `unit`, and `target` in the `forecasts` data frame.
