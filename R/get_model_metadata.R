#' Get Hub model metadata
#'
#' @param models optional character vector of model abbreviations for
#' which to retrieve designations. If not provided, the function returns model
#' designations for all models.
#' @param source string specifying where forecasts will be loaded from:
#'  currently only supports `"local_hub_repo"`
#' @param hub_repo_path path to local clone of the `reichlab/covid19-forecast-hub`
#' repository
#'
#' @return data.frame with columns corresponding to any fieldnames in the 
#' metadata files from the queried models. NA represents an absence of data
#' in that corresponding field. Two field names are changed from the raw
#' metadata field names to align with `get_model_designations()` implementation: 
#'    `model_abbr` is changed to `model`
#'    `team_model_designation` is changed to `designation`
#' @importFrom stringr str_split_fixed
#' @importFrom yaml yaml.load
#' @export

get_model_metadata <- function(models =  NULL, 
                               source = "local_hub_repo", 
                               hub_repo_path) {
  source <- match.arg(source, choices = c("local_hub_repo"))
  
  if (missing(hub_repo_path)) {
    stop("Error in get_model_designations: Please provide a hub_repo_path")
  } else {
    
    # validate models
    all_valid_models <- get_all_models(
      source = "local_hub_repo",
      hub_repo_path = hub_repo_path
    )
    
    if (!missing(models)) {
      models <- match.arg(models, choices = all_valid_models, several.ok = TRUE)
    } else {
      models <- all_valid_models
    }
    
    # construct path to metadata file from the root of hub repo
    model_metadata_paths <- paste0("data-processed/", models, "/metadata-", models, ".txt")
    
    # replace space in hub repo path with a backslash and a space
    hub_repo_path <- gsub(" ", "\ ", hub_repo_path, fixed = TRUE)
    
    model_info <- purrr::map_dfr(
      model_metadata_paths,
      function(model_metadata_path) {
        model_metadata_path <- paste0(hub_repo_path, "/", model_metadata_path)
        metadata_list <- yaml::read_yaml(model_metadata_path)
        metadata_list <- metadata_list[!sapply(metadata_list, is.null)]
        tmp <- as.data.frame(metadata_list, stringsAsFactors = FALSE)
        return(tmp)
      }
    )
    model_info <- model_info %>%
      dplyr::rename(
        model = model_abbr,
        designation = team_model_designation
      )
  }
  return(model_info)
}
