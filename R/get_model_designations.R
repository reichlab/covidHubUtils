#' Get model designations from metadata
#'
#' @param models optional character vector of model abbreviations for
#' which to retrieve designations. If not provided, the function returns model
#' designations for all models.
#' @param source string specifying where forecasts will be loaded from:
#'  either `"local_hub_repo"` or `"zoltar"`
#' @param hub_repo_path path to local clone of the `reichlab/covid19-forecast-hub`
#' repository
#'
#' @return data.frame with columns `model` and `designation`
#' @importFrom stringr str_split_fixed
#' @importFrom yaml yaml.load
#' @export
get_model_designations <- function(models = NULL, 
                                   source, 
                                   hub_repo_path) {
  source <- match.arg(source, choices = c("local_hub_repo", "zoltar"))

  warning("get_model_designations() will be deprecated soon. please use get_model_metadata() instead.")
  
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
          model_metadata_path <- paste0(hub_repo_path, "/", model_metadata_path)
          metadata_list <- yaml::read_yaml(model_metadata_path)
          metadata_list <- metadata_list[!sapply(metadata_list, is.null)]
          tmp <- as.data.frame(metadata_list, stringsAsFactors = FALSE)
          return(tmp)
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
