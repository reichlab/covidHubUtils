#' Load truth data for specified target variable and locations
#' from one or more truth sources using files in reichlab/covid19-forecast-hub.
#' 
#' @param truth_source character vector specifying where the truths will
#' be loaded from: currently support "JHU","USAFacts", "NYTimes"
#' @param target_variable string specifying target type It should be one of 
#' "Cumulative Cases","Cumulative Deaths","Incident Cases" and "Incident Deaths"
#' @param locations vector of valid fips code. Defaults to all locations with available forecasts.
#' @param data_location character specifying the location of truth data.
#' Currently only supports "local_hub_repo" and "remote_hub_repo". Default to "remote_hub_repo".
#' @param truth_end_date date to include the last available truth point. Default to system date.
#' @param temporal_resolution character specifying temporal resolution
#' to include: currently support "weekly" and "daily". Default to 'weekly'.
#' @param local_repo_path path to local clone of the reichlab/covid19-forecast-hub
#' repository. Only used when data_location is "local_hub_repo"
#' 
#' @return data frame with columns model, inc_cum, death_case, target_end_date, location and value
#' 
#' @export
load_truth <- function (truth_source,
                        target_variable, 
                        truth_end_date = Sys.Date(),
                        temporal_resolution = 'weekly',
                        locations,
                        data_location = "remote_hub_repo",
                        local_repo_path = NULL){
  
  # validate truth source
  truth_source <- match.arg(truth_source, 
                            choices = c("JHU","USAFacts", "NYTimes"), 
                            several.ok = TRUE)
  # validate target variable 
  target_variable <- match.arg(target_variable, 
                               choices = c("Cumulative Cases",
                                           "Cumulative Deaths",
                                           "Incident Cases",
                                           "Incident Deaths"), 
                               several.ok = FALSE)
  
  # validate temporal resolution
  temporal_resolution <- match.arg(temporal_resolution, 
                            choices = c("daily","weekly"), 
                            several.ok = FALSE)
  
  # validate data location
  # COVIDcast is not available now
  if(!missing(data_location)){
    data_location <- match.arg(data_location, 
                               choices = c("remote_hub_repo",
                                           "local_hub_repo"),
                                           #"COVIDcast"), 
                               several.ok = FALSE)
  } else {
    data_location = "remote_hub_repo"
  }
  
  # validate locations
  all_valid_fips <- covidHubUtils::hub_locations %>%
    pull(fips)
  
  if (!missing(locations)){
    locations <- match.arg(locations, 
                           choices = all_valid_fips, 
                           several.ok = TRUE)
  }
  
  # create file path and file name based on data_location
  if (data_location == "remote_hub_repo"){
    # create path to the truth folder
    repo_path = "https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master"
    file_name<- paste0(gsub(" ","%20", target_variable), ".csv")
  } else if (data_location == "local_hub_repo") {
    if (missing(local_repo_path) | is.na(local_repo_path) | is.null(local_repo_path)) {
      stop ("Error in local_repo_path : Please provide a valid local_repo_path.")
    } else {
      repo_path = local_repo_path
      file_name<- paste0(target_variable, ".csv")
    }
  }
 
  # load truth data from source
  truth <- purrr::map_dfr(
    truth_source,
    function (source) {
      # read file from path
      truth_folder_path <- ifelse(tolower(source) =="jhu", "/data-truth/truth-", paste0("/data-truth/",tolower(source),"/truth_",tolower(source),"-"))
      file_path <- paste0(repo_path, truth_folder_path, file_name)
      
      truth <- readr::read_csv(file_path) 
      
      # add inc_cum and death_case columns and rename date column
      truth <- truth %>%
        dplyr::mutate(model = paste0("Observed Data (",source,")"), 
                      inc_cum = ifelse(
                        unlist(strsplit(target_variable, " "))[1] == "Cumulative",
                        "cum", 
                        "inc"),
                      death_case = ifelse(
                        unlist(strsplit(target_variable, " "))[2] == "Cases",
                        "case",
                        "death")) %>%
        dplyr::rename(target_end_date = date)
    }
  ) %>%
    dplyr::select(model, inc_cum, death_case, target_end_date, location, value)
  
  # filter to only include specified locations
  if (!missing(locations)){
    truth<- dplyr::filter(truth,location %in% locations) 
  }
    
  
  if (temporal_resolution == "weekly"){
    if (unlist(strsplit(target_variable, " "))[1] == "Cumulative") {
      # only keep weekly data
      truth <- dplyr::filter(truth, 
                             target_end_date %in% seq.Date(
                               as.Date("2020-01-25"), 
                               to = truth_end_date, 
                               by="1 week")) 
    } else {
      # aggregate to weekly 
      truth <- truth %>%
        dplyr::group_by(model, location) %>%
        arrange(target_end_date) %>%
        # generate weekly counts
        dplyr:: mutate(value = RcppRoll::roll_sum(
          value, 7, align = "right", fill = NA)) %>%
        ungroup() %>%
        dplyr:: filter(target_end_date %in% seq.Date(
          as.Date("2020-01-25"), to = truth_end_date, by="1 week")) 
  
    }
  }
  
  return (truth)
}
