#' Get all valid model_abbrs from remote hub repo
#' 
#' @return a list of valid model names
#' 
get_all_model_abbr <- function() {
  req <- httr::GET("https://api.github.com/repos/reichlab/covid19-forecast-hub/git/trees/master?recursive=1")
  httr::stop_for_status(req)
  filelist <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = F)
  filenames<-grep("data-processed/", filelist, value = TRUE, fixed = TRUE)
  folders <-filenames %>% 
    str_subset(pattern = "^[^/]+/?[^/]+$") %>%
    str_subset(pattern = "\\.", negate = TRUE)
    
  model_abbrs <- sapply(folders, function (filename) {
    unlist(strsplit(filename,"/"))[2]
  })
  
  return (unique(model_abbrs))
}
