#' Get all valid model names
#'
#' @param source string specifying where to get all valid model names
#' Currently support `"local_hub_repo"`, `"remote_hub_repo"` and `"zoltar"`.
#' `"FluSight"`data is not supported on `"zoltar"` for now.
#' @param hub character vector, where the first element indicates the hub
#' from which to load forecasts. Possible options are `"US"`, `"ECDC"` and `"FluSight"`.
#' @param  hub_repo_path path to local clone of the `reichlab/covid19-forecast-hub`
#' repository
#' @importFrom httr GET stop_for_status content
#' @return a list of valid model names
#'
#' @export
get_all_models <- function(source = "zoltar", hub = c("US", "ECDC", "FluSight"), hub_repo_path) {
  # validate source
  source <- match.arg(source,
    choices = c("remote_hub_repo", "local_hub_repo", "zoltar"),
    several.ok = FALSE
  )
  
  if(hub[1] == "FluSight" & source == "zoltar") {
    stop("Error in get_all_models: FluSight data is not supported on zoltar for now.")
  }
  

  if (source == "local_hub_repo") {
    if (missing(hub_repo_path)) {
      stop("Error in get_all_models: Please provide a hub_repo_path")
    }
    
    if (hub[1] == "US") {
      forecast_foldername <- "data-processed"
    } else if (hub[1] == "ECDC") {
      forecast_foldername <- "data-processed"
    } else if (hub[1] == "FluSight") {
      forecast_foldername <- "data-forecasts"
    }

    forecast_folder <- file.path(hub_repo_path, forecast_foldername)
    if (!dir.exists( forecast_folder)) {
      stop("Error in get_all_models: forecast folder subdirectory does not exist.")
    }
    models <- list.dirs(forecast_folder, full.names = FALSE)
    models <- unique(models[nchar(models) > 0])
    models <- sort(models, method = "radix")
  } else if (source == "remote_hub_repo") {
    if (hub[1] == "US") {
      req_url <- "https://api.github.com/repos/reichlab/covid19-forecast-hub/git/trees/master?recursive=1"
      forecast_foldername <- "data-processed/"
    } else if (hub[1] == "ECDC") {
      req_url <- "https://api.github.com/repos/epiforecasts/covid19-forecast-hub-europe/git/trees/main?recursive=1"
      forecast_foldername <- "data-processed/"
    } else if (hub[1] == "FluSight") {
      req_url <- "https://api.github.com/repos/cdcepi/Flusight-forecast-data/git/trees/master?recursive=1"
      forecast_foldername <- "data-forecasts/"
    }
    # set up remote hub repo request
    req <- httr::GET(req_url)
    httr::stop_for_status(req)

    # get all files in data-processed/ from tree structure
    filelist <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = FALSE)
    filenames <- grep(forecast_foldername, filelist, value = TRUE, fixed = TRUE)

    # filter to get subfolder directories for each model
    folders <- filenames %>%
      grep(pattern = "^[^/]+/?[^/]+$", value = TRUE) %>%
      grep(pattern = "\\.", invert = TRUE, value = TRUE)

    # subtract model_abbr from folder directories
    models <- sapply(folders, function(filename) {
      unlist(strsplit(filename, "/"))[2]
    })
    models <- sort(unique(models), method = "radix")
  } else if (source == "zoltar") {
    # set up Zoltar connection
    zoltar_connection <- setup_zoltar_connection(staging = FALSE)

    # construct Zoltar project url
    project_url <- get_zoltar_project_url(
      hub = hub,
      zoltar_connection = zoltar_connection
    )

    models <- zoltr::models(zoltar_connection, project_url)$model_abbr
    models <- sort(unique(models), method = "radix")
  }

  return(models)
}
