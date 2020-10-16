#' Load truth data for specified target variable and locations
#' from one or more truth sources using files in reichlab/covid19-forecast-hub.
#' 
#' @param truth_source character vector specifying where the truths will
#' be loaded from: currently support "jhu","usafacts", "nytimes"
#' @param target_variable string specifying target type It should be one of 
#' "Cumulative Cases","Cumulative Deaths","Incident Cases" and "Incident Deaths"
#' @param locations vector of valid fips code. Defaults to all locations with available forecasts.
#' @param truth_end_date date to include the last available truth point. Default to system date.
#' @param temporal_resolution character specifying temporal resolution
#' to include: currently support "weekly" and "daily". Default to 'weekly'
#' @param remote_repo boolean specifying whether repo is remote or local
#' @param local_repo_path path to local clone of the reichlab/covid19-forecast-hub
#' repository. Only used when remote_repo is FALSE
#' 
#' @return data frame with columns model, target, target_end_date, location and value
#' 
#' @export
load_truth <- function (truth_source,
                         target_variable, 
                         truth_end_date = Sys.Date(),
                         temporal_resolution = 'weekly',
                         locations,
                         remote_repo = TRUE,
                         local_repo_path = NULL){

  truth_source <- match.arg(truth_source, 
                            choices = c("jhu","usafacts", "nytimes"), 
                            several.ok = TRUE)
  
  target_variable <- match.arg(target_variable, 
                               choices = c("Cumulative Cases",
                                           "Cumulative Deaths",
                                           "Incident Cases",
                                           "Incident Deaths"), 
                               several.ok = FALSE)
  
  temporal_resolution <- match.arg(temporal_resolution, 
                            choices = c("daily","weekly"), 
                            several.ok = FALSE)
  
  file_name<- paste0(gsub(" ","%20", target_variable), ".csv")
  
  all_valid_fips <- covidHubUtils::hub_locations %>%
    pull(fips)
  
  if (length(locations) == 0){
    locations <- all_valid_fips
  } else {
    locations <- match.arg(locations, 
                           choices = all_valid_fips, 
                           several.ok = TRUE)
  }
  
  if (remote_repo == TRUE){
    # path to the truth folder
    repo_path = "https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master"
  } else {
    repo_path = local_repo_path
  }
 
  truth <- purrr::map_dfr(
    truth_source,
    function (source) {
      # create file path
      truth_folder_path <- ifelse(source =="jhu", "/data-truth/truth-", paste0("/data-truth/",source,"/truth_",source,"-"))
      file_path <- paste0(repo_path,
                          truth_folder_path, file_name)
      
      truth <- readr::read_csv(file_path) %>%
        dplyr::filter(location %in% locations) %>%
        dplyr::mutate(model = paste0("observed data (",source,")"), 
                      inc_cum = ifelse(unlist(strsplit(target_variable, " "))[1] == "Cumulative",
                                        "cum", "inc"),
                      death_case = ifelse(unlist(strsplit(target_variable, " "))[2] == "Cases",
                                          "case","death")) %>%
        dplyr::rename(target_end_date = date)
    }
  ) %>%
    dplyr::select(model, inc_cum, death_case, target_end_date, location, value)
    
    
  if (temporal_resolution == "weekly"){
  #tidyr::separate(target, into = c("inc_cum","death_case"),remove = FALSE) %>%
    if (unlist(strsplit(target_variable, " "))[1] == "Cumulative") {
      truth <- dplyr:: filter(truth, target_end_date %in% seq.Date(as.Date("2020-01-25"), to = truth_end_date, by="1 week")) 
    } else {
      # aggregate to weekly 
      truth <- truth %>%
        dplyr::group_by(model, location) %>%
        arrange(target_end_date) %>%
        # generate weekly counts
        dplyr:: mutate(value = RcppRoll::roll_sum(value, 7, align = "right", fill = NA)) %>%
        ungroup() %>%
        dplyr:: filter(target_end_date %in% seq.Date(as.Date("2020-01-25"), to = truth_end_date, by="1 week")) 
  
    }
  }
  # truth <- suppressMessages(
  #   covidcast::covidcast_signal(data_source = truth_source,
  #                               signal = target,
  #                               start_day = as.Date("2020-01-25"),
  #                               end_day = end_day,
  #                               geo_type = location_types,
  #                               geo_values = locations
  #                               )
  # )
  return (truth)
}
