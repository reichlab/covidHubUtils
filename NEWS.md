## Changes since last release
- Update `load_forecasts_repo()` and `load_latest_forecasts_repo()` to use local data objects to validate `targets` parameter.
- Update `score_forecasts()` to return the `true_value` in function output
- Add `hub` parameter in `plot_forecasts()` and `get_plot_forecast_data()`.
- Update validation for `locations`, `truth_source` and `target_variable` in `plot_forecasts()` and `get_plot_forecast_data()`.
- Add a column that appends state abbreviation to county names in US hub locations data object.
- `load_truth()` now supports multiple target variables and has a new set of default values for `target_variable` and `truth_source` based on `hub` parameter.
- `load_forecasts_repo()`, `load_latest_forecasts_repo()` and `load_forecast_files_repo()` drop rows with NULLs in `value` column in forecast files.
- Add `date_to_datetime()` that converts a date to a date time in the corresponding timezone based on `hub` and returns that date time in UTC timezone. This function is used when the user is using `as_of` parameter to load forecasts from zoltar only.
- Add `preprocess_visualization_truth()` to generate JSON truth file for covid19 hub visualization, and its corresponding unit tests
- Add `calc_cramers_dist_equal_space()`, `calc_cramers_dist_equal_space()`, and `calc_cramers_dist_one_model_pair()` to calculate forecast similarities based on the approximation of Cramer's distance.

## covidHubUtils 0.1.5

This is a release for renaming `plot_forecast()` to `plot_forecasts()`. `plot_forecast()` is still available to use but will return deprecation warnings to the user.

### Package updates
- There is backwards compatibility.


## covidHubUtils 0.1.4

This is a release focusing on updates that provide better interface with Zoltar and European COVID-19 Forecast Hub. The release also contains new feature updates and bug fixes in other util functions.

### Feature updates

- Update `load_forecasts()` and `load_latest_forecasts()`
  
  + Add `hub` parameter to specify the forecast hub for which data should be loaded. 

  + Add `as_of` parameter to improve interface with the Zoltar query.
  
  + Add `verbose` parameter to specify whether to print out diagnostic messages.
  
  + Support `source = local_hub_repo` in `load_forecasts()`. However, loading versioned forecast files is only available through zoltar.
  
  + All models inputed into `load_latest_forecasts()` must be available in the selected `source`.
  
  + Refactor to improve efficiency.
  
- Update `plot_forecast()`

  + Load `"inc hosp"` truth data from remote hub repository. The user does not need to provide `truth_data` parameter to plot daily incident hospitalization forecasts.
  
  + `target_variable` is now optional when `forecast_data` only has one target variable.
  
  + Add a new parameter `use_median_as_point` that defaults to FALSE. "TRUE" uses the median quantile and "FALSE" uses the point forecasts.
  
  + The function now errors when trying to plot multiple locations without a facet formula.
  
  + Fix bug that model legend is missing when the user is only plotting quantile forecasts.
  
  + Update quantile forecast color so that color transparency will not be overwritten by `fill_transparency` when plotting more than five models.
  
- Update `load_truth()`

  + Add `hub` parameter to specify the forecast hub for which data should be loaded. 
  
  + Add `"inc hosp"` target variable and `"HealthData"` source.

- `score_forecasts()` has new parameter `use_median_as_point` that defaults to FALSE. "TRUE" uses the median quantile when calculating absolute error and "FALSE" uses the point forecasts for absolute error.

- Add optional `as_of` parameter in `get_model_designations()`. Currently only support versioned model designation in local hub repo.

### Package updates
- There is no backwards compatibility.
- Add Nikos I. Bosse and Ariane Stark to author/contributor list
- Add hub_locations_ecde.rda to data folder


## covidHubUtils 0.1.3

This is a release focusing on new scoring function and truth-processing functions. The release also contains new feature updates and bug fixes in other util functions. 

covidHubUtils now requires the `scoringutils` package version to be at least 0.1.5.

### Breaking changes
- `score_forecasts()` is now implemented for quantile-format forecasts to compute absolute error, weighted interval score, sharpness, overprediction, underprediction, and prediction interval coverage at any specified quantile.  Minimally one should have the `forecasts` dataframe produced by `load_forecasts()` and the truth dataframe produced by `load_truth()` to calculate scores. If one desires to specify a subset of all available scores, one should consult [this reference](https://epiforecasts.io/scoringutils/reference/eval_forecasts.html#details) for valid scores in the `desired_score_types` vector.

  + wis calculation changed to reflect preferred weighting scheme for interval scores.

 
- `preprocess_truth_for_zoltar()` and `save_truth_for_zoltar()` are now implemented to create standard cumulative and incident death truth csv files for Zoltar.
  
- `preprocess_hospitalization()` is now implemented to create standard cumulative and incident hospitalization truth csv files.

### Feature updates

- Update `load_forecasts()` and `load_latest_forecasts()`

  + Update default value of `forecast_date_window_size` to 0 in`load_latest_forecasts()` so that it looks for forecasts on the `latest_forecast_date` only.
  
  + Refactor `load_latest_forecasts_repo()`, splitting out functionality for reading in forecasts into a new exported function `load_forecast_files_repo()` that loads specific forecast files.
  
  + Standardize data format and columns types of the output.
  
  + Fix validation bug for `forecast_dates` when loading forecasts from zoltar. Loading functions will throw an error if all dates in `forecast_dates` are invalid forecast dates in Zoltar.
  
- Update `plot_forecast()` to use more user-friendly color palettes when plotting a small number of intervals.

- Update `get_model_designations()` to return `NA` when model designations for outdated models are not available on Zoltar.

### Package updates
- There is no backwards compatibility.
- Minor updates to overview vignette.


## covidHubUtils 0.1.2
  
This is a release focusing on new features in scoring functions and plotting functions. 

### Feature updates

- Update `plot_forecast()`
  
  + Set `truth_source` to be optional when the user provides `truth_data`. However, it is still needed when `show_caption = TRUE`.
  
  + Remove format validation for `model` column in user-provided `truth_data`.
  
  + Support daily hospitalization plot. When `target_variable = "inc hosp"`, the user needs to provided `truth_data`. Otherwise, an error will be thrown. 
  
  + Add `facet_nrow`, `facet_ncol`, `fill_transparency`, `title` and `subtitle`.

- Update `get_plot_forecast_data()`
  
  + Remove format validation for `model` column in user-provided `truth_data`.
  
  + When `target_variable = "inc hosp"`, the user needs to provided `truth_data`. Otherwise, an error will be thrown. 

### Package updates
- There is no backwards compatibility.
- Add Khoa Le and Yuxin David Huang to author/contributor list
- Create covidHubUtils-overview vignette


## covidHubUtils 0.1.1

This is a release focusing on new features in plotting functions. 

### Feature updates
- `plot_forecast()` now supports faceted plots of multiple models, locations and forecast dates for one target variable. 
  
  + In `plot_forecast()`, `facet` and `facet_scales` are equivalent to `facets` and `scales` in `ggplot2::facet_wrap()`. `facet` takes facet formula, for example `facet = ~ model`. `facet_scales` are expecting the same values for `scales` in `ggplot2::facet_wrap()`, such as `"fixed"`, `"free_y"`, `"free_x"` or `"free"`.
  
  + If `fill_by_model = TRUE`, each model will be represented by a unique color. If `fill_by model = FALSE`, all models and selected prediction intervals will be represented by blue colors.
  
  + For simplicity, prediction interval legends will be grey in faceted plots. Morever, when the user selects more than 5 models, only 95% predicition interval is included. Otherwise, all selected prediction intervals will be plotted. 
  
### Package updates
- There is no backwards compatibility due to argument changes in `plot_forecast()`.
  
## covidHubUtils 0.1

This is the first version of the package with a 0.x release.

### Feature updates
- details on new features will be listed here for future updates
- current key features include loading and plotting forecast and truth data

### Package updates
- details on other changes will be listed here for future updates
- added initial author/contributor list
