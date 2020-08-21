#' Get models and their designations
#' 
#' @param models optional character vector of model abbreviations for
#' which to retrieve designations. If not provided, the function returns model
#' designations for all models.
#' @param source string specifying where forecasts will be loaded from:
#' currently only "hub_repo" is supported; eventually, either "hub_repo" or
#' "zoltar"
#' @param hub_repo_path path to local clone of the reichlab/covid19-forecast-hub
#' repository
#' 
#' @return data frame with columns `model` and `designation`
#' 
#' @export
get_model_designations <- function(models, source, hub_repo_path) {
  source <- match.arg(source, choices = c("hub_repo", "zoltar"))

  data_processed <- file.path(hub_repo_path, "data-processed")

  if(!missing(hub_repo_path)) {
    # List of directories within the data_processed directory
    model_dirs <- list.dirs(data_processed)

    # drop first result, which is the data-processed directory itself
    model_dirs <- model_dirs[-1]

    # Data frame with model abbreviation and designation for each model
    model_info <- purrr::map_dfr(
      model_dirs,
      function(model_dir) {
        metadata_path <- Sys.glob(paste0(model_dir, "/metadata*"))
        return(as.data.frame(
          yaml::read_yaml(metadata_path)[
            c("model_abbr", "team_model_designation")],
          stringsAsFactors = FALSE
        ))
      }
    ) %>%
      dplyr::select(model = model_abbr, designation = team_model_designation)

    # filter to requested models
    if(!missing(models)) {
      model_info <- model_info %>%
        dplyr::filter(model %in% models)
    }
  } else {
    # In the future, put logic to get this information from zoltar here
    stop("Argument hub_repo_path must be provided; an interface via Zoltar is not yet supported.")
  }

  return(model_info)
}
