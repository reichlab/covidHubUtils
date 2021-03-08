#' Load truth data for a specified target variable, locations and temporal resolution
#' from one or more truth sources using files in reichlab/covid19-forecast-hub.
#' 
#' "inc hosp" is only available from "HeatlthData" and this function is not loading
#' data for other target variables from "HealthData".
#' 
#' When loading weekly data, if there are not enough observations for a week, the corresponding
#' weekly count would be NA in resulting data frame.
#' 
#' @param truth_source character vector specifying where the truths will
#' be loaded from: currently support "JHU","USAFacts", "NYTimes" and "HealthData".
#' @param target_variable string specifying target type It should be one of 
#' "cum death", "inc case", "inc death", "inc hosp". 
#' @param locations vector of valid fips code. Defaults to all locations with available forecasts.
#' @param data_location character specifying the location of truth data.
#' Currently only supports "local_hub_repo" and "remote_hub_repo". Default to "remote_hub_repo".
#' @param truth_end_date date to include the last available truth point in 'yyyy-mm-dd' format. 
#' Default to system date.
#' @param temporal_resolution character specifying temporal resolution
#' to include: currently support "weekly" and "daily". 
#' Default to 'weekly' for cases and deaths, 'daily' for hospitalizations.
#' @param local_repo_path path to local clone of the reichlab/covid19-forecast-hub
#' repository. Only used when data_location is "local_hub_repo"
#' @param hub character, which hub to use. Default is "US", other option is
#' "ECDC"
#' 
#' @return data frame with columns model, inc_cum, death_case, target_end_date, 
#' location, value, location_name, population, geo_type, geo_value, abbreviation
#' 
#' @examples 
#' library(covidHubUtils)
#' 
#' # load for US
#' load_truth(truth_source = "JHU", target_variable = "inc case")
#' 
#' # load for ECDC
#' load_truth(truth_source = "JHU", target_variable = "inc case", hub = "ECDC")
#' 
#' @export
load_truth <- function (truth_source,
                        target_variable, 
                        truth_end_date = Sys.Date(),
                        temporal_resolution,
                        locations,
                        data_location = "remote_hub_repo",
                        local_repo_path = NULL, 
                        hub = c("US", "ECDC")){
  
  
  # preparations and validation checks that are different for US and ECDC hub
  if (hub[1] == "US") {
    # validate target variable 
    target_variable <- match.arg(target_variable, 
                                 choices = c("cum death",
                                             "inc case",
                                             "inc death",
                                             "inc hosp"), 
                                 several.ok = FALSE)
    
    # validate truth source
    truth_source <- match.arg(truth_source, 
                              choices = c("JHU","USAFacts", "NYTimes", "HealthData"), 
                              several.ok = TRUE)
    
    # extra checks for truth source if target is inc hosp
    if(target_variable == "inc hosp"){
      if (any(truth_source != "HealthData")){
        warning("Warning in load_truth: Incident hopsitalization truth data is only available from HealthData.gov now.
              Will be loading data from HealthData instead.")
        truth_source <- "HealthData"
      }
    } else {
      if ("HealthData" %in% truth_source){
        stop("Error in load_truth: This function does not support selected target_variable from HealthData.")
      }
    }
    
    # get list of all valid locations and codes
    valid_locations <- covidHubUtils::hub_locations
    valid_location_codes <- covidHubUtils::hub_locations$fips
    
    # store path of remote repo
    remote_repo_path <- "https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master"
    
    
  } else if (hub[1] == "ECDC") {
    # validate target variable 
    target_variable <- match.arg(target_variable, 
                                 choices = c("inc case",
                                             "inc death"), 
                                 several.ok = FALSE)
    
    # validate truth source
    truth_source <- match.arg(truth_source, 
                              choices = c("JHU", "jhu", "ECDC", "ecdc"), 
                              several.ok = FALSE)
    
    # get list of all valid locations and codes
    valid_locations <- covidHubUtils::hub_locations_ecdc
    valid_location_codes <- covidHubUtils::hub_locations_ecdc$location
    
    # store path of remote repo
    remote_repo_path <- "https://raw.githubusercontent.com/epiforecasts/covid19-forecast-hub-europe/main"
  }
  
  # validate truth end date
  truth_end_date <- tryCatch({
    as.Date(truth_end_date)}, 
    error = function(err) {
      stop("Error in load_truth: Please provide a valid date object or
    string in format YYYY-MM-DD in truth_end_date")}
  )
  
  # validate temporal resolution
  if (missing(temporal_resolution)){
    # only relevant for US hub currently where inc hosp is available
    if (target_variable == "inc hosp"){
      temporal_resolution = "daily"
    } else {
      temporal_resolution = "weekly"
    }
  } else {
    temporal_resolution <- match.arg(temporal_resolution, 
                                     choices = c("daily","weekly"), 
                                     several.ok = FALSE)
  }
  
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
  if (!missing(locations)){
    locations <- match.arg(locations, 
                           choices = valid_location_codes, 
                           several.ok = TRUE)
  } 
  
  # create file path and file name based on data_location
  if (data_location == "remote_hub_repo"){
    repo_path <- remote_repo_path
  } else if (data_location == "local_hub_repo") {
    if (missing(local_repo_path) | is.na(local_repo_path) | is.null(local_repo_path)) {
      stop ("Error in local_repo_path : Please provide a valid local_repo_path.")
    } else {
      repo_path = local_repo_path
    }
  }
 
  # load truth data from source
  truth <- purrr::map_dfr(
    truth_source,
    function (source) {
      # construct file path and read file from path
      file_path <- get_truth_path(source = source, 
                                  repo_path = repo_path,
                                  target_variable = target_variable, 
                                  data_location = data_location,
                                  hub = hub)
      truth <- readr::read_csv(file_path) 
      
      # add inc_cum and death_case columns and rename date column
      truth <- truth %>%
        dplyr::mutate(model = paste0("Observed Data (",source,")"), 
                      target_variable = target_variable,
                      date = as.Date(date)) %>%
        dplyr::rename(target_end_date = date)
    }
  ) %>%
    dplyr::select(model, target_variable, target_end_date, location, value)
  
  # filter to only include specified locations
  if (!missing(locations)){
    truth <- dplyr::filter(truth, location %in% locations) 
    if (nrow(truth) == 0){
      warning("Warning in load_truth: Truth for selected locations are not available.\n Please check your parameters.")
      if (target_variable == "inc hosp"){
        warning("Warning in load_truth: Only national and state level of incident hospitalization data is available.")
      }
    }
  }
  
  if (temporal_resolution == "weekly"){
    if (unlist(strsplit(target_variable, " "))[1] == "cum") {
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
        dplyr::arrange(target_end_date) %>%
        # generate weekly counts
        dplyr:: mutate(value = RcppRoll::roll_sum(
          value, 7, align = "right", fill = NA)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(target_end_date %in% seq.Date(
          as.Date("2020-01-25"), to = truth_end_date, by = "1 week")) 
    }
  }
  
  # merge with location data to get populations and location names
  # for US the location codes are stored in a column called 'fips'
  if (hub[1] == "US") {
    truth <- truth %>%
      dplyr::left_join(valid_locations, by = c("location" = "fips"))
  } else {
    truth <- truth %>%
      dplyr::left_join(valid_locations, by = c("location"))
  }
  return (truth)
}



# helper function to construct the file path based on the target_variable, 
# the source and the hub
get_truth_path <- function(source, 
                           repo_path,
                           target_variable,
                           data_location,
                           hub) {
  # generate full target variable (this works for both hubs as previous 
  # checks already made sure only applicable target_variables are used)
  if (target_variable == "cum death"){
    full_target_variable = "Cumulative Deaths"
  } else if (target_variable == "inc case"){
    full_target_variable = "Incident Cases"
  } else if (target_variable == "inc death"){
    full_target_variable = "Incident Deaths"
  } else if (target_variable == "inc hosp"){
    full_target_variable = "Incident Hospitalizations"
  }
  
  if (data_location == "remote_hub_repo") {
    file_name <- paste0(gsub(" ","%20", full_target_variable), ".csv")
  } else if (data_location == "local_hub_repo") {
    file_name <- paste0(full_target_variable, ".csv")
  }
  
  if (hub[1] == "US") {
    truth_folder_path <- ifelse((tolower(source) == "jhu" || tolower(source) == "healthdata"), 
                                "/data-truth/truth-", 
                                paste0("/data-truth/", tolower(source),"/truth_",
                                       tolower(source),"-"))
    file_path <- paste0(repo_path, truth_folder_path, file_name)
  } else if (hub[1] == "ECDC") {
    truth_folder_path <- paste0("/data-truth/", toupper(source), "/truth_", toupper(source), "-")
    file_path <- paste0(repo_path, truth_folder_path, file_name)
  }
  return(file_path)
}



