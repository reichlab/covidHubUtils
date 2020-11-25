#' Get models and their designations
#' 
#' @param models optional character vector of model abbreviations for
#' which to retrieve designations. If not provided, the function returns model
#' designations for all models.
#' @param source string specifying where forecasts will be loaded from:
#'  either "local_hub_repo" or "zoltar"
#' @param hub_repo_path path to local clone of the reichlab/covid19-forecast-hub
#' repository
#' @param as_of optional date specifying the version. Only support versioned
#' model designations in "local_hub_repo"
#' 
#' @return data frame with columns `model` and `designation`
#' 
#' @export
get_model_designations <- function(models, source, hub_repo_path, as_of = Sys.Date()) {
  source <- match.arg(source, choices = c("local_hub_repo", "zoltar"))

  
  if (as_of!= Sys.Date() & source != "local_hub_repo"){
    stop("Error in get_model_designations: Currently only support versioned model designation in local hub repo.")
  }

  if(source == "local_hub_repo") {
    if (missing(hub_repo_path)){
      stop ("Error in get_model_designations: Please provide a hub_repo_path")
    } else {
      
      # validate models
      all_valid_models <- get_all_models(source = "local_hub_repo", 
                                         hub_repo_path = hub_repo_path)
      
      if (!missing(models)){
        models <- match.arg(models, choices = all_valid_models, several.ok = TRUE)
      } else {
        models <- all_valid_models
      }
      
      # construct path to metadata file from the root of hub repo
      model_metadata_paths = paste0('data-processed/',models,'/metadata-',models,'.txt')
        
      model_info <- purrr::map_dfr(
        model_metadata_paths,
        function (model_metadata_path){
          # search day is one day later than as_of date
          search_day <- as.Date(as_of) + 1 
          
          # find git commits related to a specified metadata file before search_day
          commits_command <- paste0("cd ",hub_repo_path,
                                    "; git log --date=short --pretty=format:'%H %ad' --before='",as.character(search_day),"' --follow -- ",
                                    model_metadata_path
                                    ) 
          # invoke command and parse result
          all_commits <- system(commits_command,intern = TRUE) %>% 
            stringr::str_split_fixed(" ", 2) %>%
            as.data.frame()%>%
            dplyr::rename(sha = V1, date = V2)
          
          recent_commit_sha <- all_commits$sha[1]
          
          # construct git command to read metadata file
          read_command <- paste0("cd ",hub_repo_path,"; git show ", 
                            recent_commit_sha,":",model_metadata_path)
          
          as.data.frame(yaml::yaml.load(system(read_command, intern = TRUE))[
            c("model_abbr", "team_model_designation")],
            stringsAsFactors = FALSE
          )
        }) %>%
        dplyr::select(model = model_abbr, designation = team_model_designation)
    }
  } else if (source == "zoltar") {
    # set up Zoltar connection
    zoltar_connection <- zoltr::new_connection()
    
    if(Sys.getenv("Z_USERNAME") == "" | Sys.getenv("Z_PASSWORD") == "") {
      zoltr::zoltar_authenticate(zoltar_connection, "zoltar_demo","Dq65&aP0nIlG")
    } else {
      zoltr::zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"),Sys.getenv("Z_PASSWORD"))
    }
    
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
