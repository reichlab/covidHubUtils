#' Get all valid model_abbrs
#' 
#' @param source string specifying where to get all valid model abbreviations.
#' Currently support "remote_hub_repo" and "zoltar".
#' 
#' @return a list of valid model abbreviations
#' 
get_all_model_abbr <- function(source) {
  
  # validate source
  source <- match.arg(source, choices = c("remote_hub_repo", "zoltar"), several.ok = FALSE)
  
  if (source == "remote_hub_repo") {
    # set up remote hub repo request
    req <- httr::GET("https://api.github.com/repos/reichlab/covid19-forecast-hub/git/trees/master?recursive=1")
    httr::stop_for_status(req)
    
    # get all files in data-processed/ from tree structure 
    filelist <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = F)
    filenames<-grep("data-processed/", filelist, value = TRUE, fixed = TRUE)
    
    # filter to get subfolder directories for each model
    folders <- filenames %>% 
      grep(pattern = "^[^/]+/?[^/]+$", value = TRUE) %>%
      grep(pattern = "\\.", invert = TRUE, value = TRUE)
    
    # subtract model_abbr from folder directories
    model_abbrs <- sapply(folders, function (filename) {
      unlist(strsplit(filename,"/"))[2]
    })
    
  } else if (source == "zoltar"){
    # set up Zoltar connection
    zoltar_connection <- zoltr::new_connection()
    zoltr::zoltar_authenticate(
      zoltar_connection,
      Sys.getenv("Z_USERNAME"),
      Sys.getenv("Z_PASSWORD"))
    
    # construct Zoltar project url
    the_projects <- zoltr::projects(zoltar_connection)
    project_url <- the_projects[the_projects$name == "COVID-19 Forecasts", "url"]
    
    model_abbrs <- zoltr::models(zoltar_connection, project_url)$model_abbr
  }
  
  return (unique(model_abbrs))
}
  
