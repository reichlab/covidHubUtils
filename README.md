# covidHubUtils
Utility functions for the COVID-19 forecast hub

# Installation

The `covidHubUtils` package relies on a small number of packages, including many from the `tidyverse` and, importantly, [the `zoltr` package](http://reichlab.io/zoltr/) that is used to access the Zoltar API for downloading forecasts. Please install `zoltr` from GitHub, as this development version often has important features not yet on the CRAN version:
```r
devtools::install_github("reichlab/zoltr")
```
The `covidHubUtils` package currently is only available on GitHub, and it may be installed using the `devtools` package:
``` r
devtools::install_github("reichlab/covidHubUtils")
```

# Getting Started

For those starting out we recommend you begin with the [Getting Started vignette](https://htmlpreview.github.io/?https://github.com/reichlab/covidHubUtils/blob/master/vignettes/covidHubUtils-overview.html).

# Features

## Currently available:

**Reading Forecast Data**
 * `get_model_designations(models, source, hub_repo_path, as_of)`: Assemble a data frame with columns model and designation. **Note:** Currently only support versioned model designations in a local clone of the covid19-forecast-hub repository.
 * `load_latest_forecasts(models, last_forecast_date, forecast_date_window_size, locations, types, targets, source, hub_repo_path, as_of, verbose, hub)`: Load the most recent forecasts in a specified time window either from a local clone of the covid19-forecast-hub repository or Zoltar.
 * `load_forecasts(models, forecast_dates, locations, types, targets, source, hub_repo_path, as_of, verbose, hub)`: Load all available forecasts either from a local clone of the covid19-forecast-hub repository or Zoltar.
 
**Reading Observed "Truth" Data**
* `load_truth(truth_source, target_variable, truth_end_date, temporal_resolution, locations, data_location, local_repo_path, hub)`: Load truth data for specified target variable and locations from covid19-forecast-hub repository. **Note:** Only support national level and state level truth data for `"inc hosp"` from `"HealthData"` source.

**Plotting Forecasts**
 * `plot_forecasts(forecast_data, truth_data, hub, models, target_variable, locations, facet, facet_scales, forecast_dates, intervals, horizon, truth_source, use_median_as_point, plot_truth, plot, fill_by_model, truth_as_of, title, subtitle, show_caption)`: Plot forecasts with optional truth data for multiple models, locations and forecast dates. To see more example plots, please to go [vignettes/demo](https://htmlpreview.github.io/?https://github.com/reichlab/covidHubUtils/blob/master/vignettes/demo.html).
 
**Scoring Forecasts**
 * `score_forecasts(forecasts, truth, desired_score_types = c(...), return_format = c("long", "wide"))` Calculate specified scores for each combination of `model`, `forecast_date`, `location`, `horizon`, `temporal_resolution`, `target_variable`, and `target_end_date` in the `forecasts` data frame. Please see [this reference](https://epiforecasts.io/scoringutils/reference/eval_forecasts.html#details) for valid scores in the `desired_score_types` vector.

**Download and pre-process "Truth" Data**
 * `download_raw_nytimes(save_location)`: Download raw truth data from NYTimes and write to CSV files.
 * `download_raw_usafacts(save_location)`: Download raw truth data from USAFacts and write to CSV files.
 * `preprocess_nytimes(save_location)`: Preprocess raw truth data from NYTimes into Cumulative/Incident - Deaths/Cases and write to CSVs
 * `preprocess_usafacts(save_location)`: Preprocess raw truth data from USAFacts into Cumulative/Incident - Deaths/Cases and write to CSVs
 * `preprocess_jhu(save_location)`: Preprocess raw truth data from JHU CSSE into Cumulative/Incident - Deaths/Cases and write to CSVs. **Note:** To use this method, the [covidData](https://github.com/reichlab/covidData) package needs to be installed. 
 * `preprocess_hospitalization(save_location)`: Preprocess raw hospitalization data into Cumulative/Incident hospitalizations and write to CSVs. **Note:** To use this method, the [covidData](https://github.com/reichlab/covidData) package needs to be installed. 
 * `preprocess_truth_for_zoltar(target, issue_date)`: Preprocess raw truth data from JHU CSSE into Cumulative/Incident - Deaths/Cases for Zoltar. **Note:** To use this method, the [covidData](https://github.com/reichlab/covidData) package needs to be installed. 
 * `save_truth_for_zoltar(save_location)`: Write results from `preprocess_truth_for_zoltar()` to CSVs. **Note:** To use this method, the [covidData](https://github.com/reichlab/covidData) package needs to be installed.
 
 **Calculating Forecast Similarities**
 * `calc_cramers_dist_equal_space(q_F, tau_F, q_G, tau_G, approx_rule)`: Calculating approximated Cramer's 
distance between a pair of distributions F and G that are represented by a collection of equally-spaced quantiles.
 * `calc_cramers_dist_unequal_space(q_F, tau_F, q_G, tau_G, approx_rule)`: Calculating approximated Cramer's 
distance between a pair of distributions F and G that are represented by a collection of unequally-spaced quantiles.
*`calc_cramers_dist_one_model_pair(q_F, tau_F, q_G, tau_G, approx_rule)`: A wrapper function for `calc_cramers_dist_equal_space()` and `calc_cramers_dist_unequal_space()`.
 
# Contributing Guidelines
If you would like to contribute your work, please follow this list to create a pull request:
* New functions should come with unit tests, or a promise of a new unit test in the form of an issue if getting the functionality merged in is urgent.
* If you added a new .R file with unit tests, add the tests to `.github/workflows/pr_unittest.yaml`.
* Small/quick fixes don't need to be tested, necessarily.
* Update `NEWS.md` by adding a short summary of your changes under “Changes since last release.”
* Update `README.md` if you created a new function or add a new parameter to existing functions.
* Update `DESCRIPTION` when you are using a new dependency in your script.


