## Changes since the last version

- `score_forecasts()` is now implemented.

  Minimally one should have the `forecasts` dataframe produced by `load_forecasts()` and the truth dataframe produced by `load_truth()` to calculate scores. If one desires to specify a subset of all available scores, one should consult [this reference](https://epiforecasts.io/scoringutils/reference/eval_forecasts.html#details) for valid scores in the `desired_score_types` vector.

- Update `plot_forecast()`
  
  Set `truth_source` to be optional when the user provides `truth_data`. However, it is still needed when `show_caption = TRUE`.
  
  Remove format validation for `model` column in user-provided `truth_data`.

## covidHubUtils 0.1.1

This is a release focusing on new features in plotting functions. 

### Feature updates
- `plot_forecast()` now supports faceted plots of multiple models, locations and forecast dates for one target variable. 
  
  In `plot_forecast()`, `facet` and `facet_scales` are equivalent to `facets` and `scales` in `ggplot2::facet_wrap()`. `facet` takes facet formula, for example `facet = ~ model`. `facet_scales` are expecting the same values for `scales` in `ggplot2::facet_wrap()`, such as `"fixed"`, `"free_y"`, `"free_x"` or `"free"`.
  
  If `fill_by_model = TRUE`, each model will be represented by a unique color. If `fill_by model = FALSE`, all models and selected prediction intervals will be represented by blue colors.
  
  For simplicity, prediction interval legends will be grey in faceted plots. Morever, when the user selects more than 5 models, only 95% predicition interval is included. Otherwise, all selected prediction intervals will be plotted. 
  
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

### v 0.1
 - initial release: big plotting changes
 - v0.1.1:
    - in load_latest_forecasts, use provided source for calls to lower level functions
 - v0.1.2:
    - support daily hospitalization plot in plot_forecast
    - add `facet_nrow`, `facet_ncol`, and `fill_transparency` to `plot_forecast` function
    
