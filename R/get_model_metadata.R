#' Get Hub model metadata
#'
#' @param models optional character vector of model abbreviations for
#' which to retrieve designations. If not provided, the function returns model
#' designations for all models.
#' @param source string specifying where forecasts will be loaded from:
#'  currently only supports `"local_hub_repo"`
#' @param hub_repo_path path to local clone of the `reichlab/covid19-forecast-hub`
#' repository
#' @param as_of optional date specifying the version. defaults to current date.
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
                               hub_repo_path, 
                               as_of = Sys.Date()) {
  source <- match.arg(source, choices = c("local_hub_repo"))
  
  if (as_of != Sys.Date() & source != "local_hub_repo") {
    stop("Error in get_model_designations: Currently only support versioned model designation in local hub repo.")
  }
  
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
        # create search time in EST based on as_of date
        search_time <- paste0(as.Date(as_of) + 1, " 00:00:00")
        search_time <- as.POSIXct(search_time, format = "%Y-%m-%d %H:%M:%S", tz = "EST")
        # convert search time to UTC timestamp
        attr(search_time, "tzone") <- "UTC"
        search_time_timestamp <- as.numeric(search_time)
        
        # find git commits related to a specified metadata file before search_time
        commits_command <- paste0(
          "cd ", hub_repo_path,
          "; git log --date=unix --pretty=format:'%H %ad' --before='",
          as.character(search_time), "' --follow -- ",
          model_metadata_path
        )
        
        # invoke command and parse result
        all_commits <- system(commits_command, intern = TRUE) %>%
          stringr::str_split_fixed(" ", 2) %>%
          as.data.frame() %>%
          dplyr::rename(sha = V1, date = V2)
        
        if (nrow(all_commits) == 0) {
          print(paste0("Currently checking commits for: ", model_metadata_path, " before ", search_time))
          stop("Error in get_model_designations: Commits to model metadata are not available by as_of date.\n Please check your parameters.")
        }
        
        recent_commit_sha <- all_commits$sha[1]
        
        # construct git command to read metadata file
        read_command <- paste0(
          "cd ", hub_repo_path, "; git show ",
          recent_commit_sha, ":./", model_metadata_path
        )
        
        tmp <- as.data.frame(yaml::yaml.load(system(read_command, intern = TRUE)),
                      stringsAsFactors = FALSE
        )
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
