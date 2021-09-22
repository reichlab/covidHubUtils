#' Get model designations from metadata
#'
#' @param models optional character vector of model abbreviations for
#' which to retrieve designations. If not provided, the function returns model
#' designations for all models.
#' @param source string specifying where forecasts will be loaded from:
#'  either `"local_hub_repo"` or `"zoltar"`
#' @param hub_repo_path path to local clone of the `reichlab/covid19-forecast-hub`
#' repository
#' @param as_of optional date specifying the version. Only support versioned
#' model designations in `"local_hub_repo"`
#'
#' @return data.frame with columns `model` and `designation`
#' @importFrom stringr str_split_fixed
#' @importFrom yaml yaml.load
#' @export
get_model_designations <- function(models = NULL, 
                                   source, 
                                   hub_repo_path, 
                                   as_of = Sys.Date()) {
  source <- match.arg(source, choices = c("local_hub_repo", "zoltar"))

  warning("get_model_designations() will be deprecated soon. please use get_model_metadata() instead.")
  
  if (as_of != Sys.Date() & source != "local_hub_repo") {
    stop("Error in get_model_designations: Currently only support versioned model designation in local hub repo.")
  }

  if (source == "local_hub_repo") {
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

          # find regular git commits related to a specified metadata file before search_time
          regular_commits_command <- paste0(
            "cd ", hub_repo_path,
            "; git log --date=unix --pretty=format:'%H %ad' --before='",
            as.character(search_time), "' --follow -- ",
            model_metadata_path
          )
          
          # find merge git commits related to a specified metadata file before search_time
          pr_commits_command <- paste0(
            "cd ", hub_repo_path,
            "; git log --first-parent master --date=unix --pretty=format:'%H %ad' --before='",
            as.character(search_time), "' --follow -- ",
            model_metadata_path
          )
          
          # invoke command and parse result
          all_regular_commits <- system(regular_commits_command, intern = TRUE) %>%
            stringr::str_split_fixed(" ", 2) %>%
            as.data.frame() %>%
            dplyr::rename(sha = V1, date = V2)
          
          # invoke command and parse result
          all_pr_commits <- system(pr_commits_command, intern = TRUE) %>%
            stringr::str_split_fixed(" ", 2) %>%
            as.data.frame() %>%
            dplyr::rename(sha = V1, date = V2)
          
          # combine all regular commits and merge commits
          all_commits <- rbind(all_regular_commits, all_pr_commits) %>%
            dplyr::arrange(desc(date))
          
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

          as.data.frame(yaml::yaml.load(system(read_command, intern = TRUE))[
            c("model_abbr", "team_model_designation")
          ],
          stringsAsFactors = FALSE
          )
        }
      ) %>%
        dplyr::select(model = model_abbr, designation = team_model_designation)
    }
  } else if (source == "zoltar") {
    # set up Zoltar connection
    zoltar_connection <- zoltr::new_connection()

    if (Sys.getenv("Z_USERNAME") == "" | Sys.getenv("Z_PASSWORD") == "") {
      zoltr::zoltar_authenticate(zoltar_connection, "zoltar_demo", "Dq65&aP0nIlG")
    } else {
      zoltr::zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"), Sys.getenv("Z_PASSWORD"))
    }

    the_projects <- zoltr::projects(zoltar_connection)
    project_url <- the_projects[the_projects$name == "COVID-19 Forecasts", "url"]
    primary_models <- zoltr::models(zoltar_connection, project_url)

    # filter to requested models
    if (!missing(models)) {
      primary_models <- primary_models %>%
        dplyr::filter(model_abbr %in% models)
    }

    model_info <- primary_models %>%
      dplyr::mutate(
        model = model_abbr,
        designation = ifelse(notes == "", "NA", notes)
      ) %>%
      dplyr::select(model, designation)
  }

  return(model_info)
}
