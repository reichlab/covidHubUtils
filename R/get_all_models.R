#' Get all valid model names
#'
#' @param source string specifying where to get all valid model names
#' Currently support "local_hub_repo", "remote_hub_repo" and "zoltar".
#' @param  hub_repo_path path to local clone of the reichlab/covid19-forecast-hub
#' repository
#' @importFrom httr GET stop_for_status content
#' @return a list of valid model names
#'
#' @export
get_all_models <- function(source = "zoltar", hub_repo_path) {

  # validate source
  source <- match.arg(source,
                      choices = c("remote_hub_repo","local_hub_repo", "zoltar"),
                      several.ok = FALSE)

  if (source == "local_hub_repo") {
    if (missing(hub_repo_path)) {
      stop ("Error in get_all_models: Please provide a hub_repo_path")
    }

    data_processed <- file.path(hub_repo_path, "data-processed")
    if (!dir.exists(data_processed)){
      stop ("Error in get_all_models: data-processed subdirectory does not exist.")
    }
    models <- list.dirs(data_processed, full.names = FALSE)
    models <- models[nchar(models) > 0]

  } else if (source == "remote_hub_repo") {
    # set up remote hub repo request
    req <- httr::GET("https://api.github.com/repos/reichlab/covid19-forecast-hub/git/trees/master?recursive=1")
    httr::stop_for_status(req)

    # get all files in data-processed/ from tree structure
    filelist <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = FALSE)
    filenames <- grep("data-processed/", filelist, value = TRUE, fixed = TRUE)

    # filter to get subfolder directories for each model
    folders <- filenames %>%
      grep(pattern = "^[^/]+/?[^/]+$", value = TRUE) %>%
      grep(pattern = "\\.", invert = TRUE, value = TRUE)

    # subtract model_abbr from folder directories
    models <- sapply(folders, function (filename) {
      unlist(strsplit(filename,"/"))[2]
    })

  } else if (source == "zoltar"){
    # set up Zoltar connection
    zoltar_connection <- zoltr::new_connection()
    if(Sys.getenv("Z_USERNAME") == "" | Sys.getenv("Z_PASSWORD") == "") {
      zoltr::zoltar_authenticate(zoltar_connection, "zoltar_demo","Dq65&aP0nIlG")
    } else {
      zoltr::zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"),Sys.getenv("Z_PASSWORD"))
    }

    # construct Zoltar project url
    the_projects <- zoltr::projects(zoltar_connection)
    project_url <- the_projects[the_projects$name == "COVID-19 Forecasts", "url"]

    models <- zoltr::models(zoltar_connection, project_url)$model_abbr
  }

  return (unique(models))
}

