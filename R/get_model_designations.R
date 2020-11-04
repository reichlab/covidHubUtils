#' Get models and their designations
#' 
#' @param models optional character vector of model abbreviations for
#' which to retrieve designations. If not provided, the function returns model
#' designations for all models.
#' @param source string specifying where forecasts will be loaded from:
#'  either "local_hub_repo" or "zoltar"
#' @param hub_repo_path path to local clone of the reichlab/covid19-forecast-hub
#' repository
#' 
#' @return data frame with columns `model` and `designation`
#' 
#' @export
get_model_designations <- function(models, source, hub_repo_path) {
  source <- match.arg(source, choices = c("local_hub_repo", "zoltar"))

  if(source == "local_hub_repo") {
    if (missing(hub_repo_path)){
      stop ("Error in get_model_designations: Please provide a hub_repo_path")
    } else {
 
      data_processed <- file.path(hub_repo_path, "data-processed")
      # list of directories within the data_processed directory
      model_dirs <- list.dirs(data_processed)
  
      # drop first result, which is the data-processed directory itself
      model_dirs <- model_dirs[-1]
  
      # data frame with model abbreviation and designation for each model
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
      
    }
  } else {
    # set up Zoltar connection
    zoltar_connection <- zoltr::new_connection()
    
    zoltr::zoltar_authenticate(zoltar_connection, 
                        Sys.getenv("Z_USERNAME"),
                        Sys.getenv("Z_PASSWORD"))
    
    the_projects <- zoltr::projects(zoltar_connection)
    project_url <- the_projects[the_projects$name == "COVID-19 Forecasts", "url"]
    primary_models <- zoltr::models(zoltar_connection, project_url) 
    
    # filter to requested models
    if(!missing(models)) {
      primary_models <- primary_models %>%
        dplyr::filter(model_abbr %in% models)
    }
    
    model_info <- as.data.frame(purrr::map_dfr(
      primary_models$url,
      zoltr::model_info,
      zoltar_connection = zoltar_connection)  %>%
      dplyr::select(abbreviation,notes) %>%
      dplyr::rename(model = abbreviation, designation = notes))
  }

  return(model_info)
}
