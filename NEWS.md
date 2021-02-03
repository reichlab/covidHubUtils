## Changes since last release

- `plot_forecast()` now errors when trying to plot multiple locations without location in the facet formula.
- Fix bug that model legend is missing when the user is only plotting quantile forecasts in `plot_forecast()`. If `forecast_data` doesn't contain point forecasts, the function will look for all available medians in `forecast_data` and plot them as point forecasts. 
- Update quantile forecast color so that color transparency will not be overwritten by `fill_transparency` in `plot_forecast()` when plotting more than five models.
- target_variable in `plot_forecast()` now optional when forecast_data only has one target variable
- all models inputed into `load_latest_forecasts()` must be available in the source
- `plot_forecast()` has new parameter `use_median_as_point` that defaults to FALSE. "TRUE" uses the median quantile and "FALSE" uses the point forecasts.
- added the `as_of` parameter to `load_forecasts()`, `load_latest_forecasts()` and `load_latest_forecasts_zoltar()` to improve interface with the Zoltar query.
- `score_forecasts()` has new parameter `use_median_as_point` that defaults to FALSE. "TRUE" uses the median quantile when calculating absolute error and "FALSE" uses the point forecasts for absolute error.

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
