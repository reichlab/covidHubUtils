#' Create full_locations.csv in /data-locations for loading data from 
#' zoltar and covidcast
#'
#' @return data frame with columns fips, location_name, population, geo_type,
#' geo_value, abbreviation
#' 
get_locations <- function(){
  locations <- readr::read_csv("data-locations/locations.csv") %>%
    # add columns used for reading files in covidcast package 
    # US has "state" as geo_type
    dplyr::mutate (geo_type = ifelse(!is.na(abbreviation), "state", "county"),
                   geo_value = ifelse(!is.na(abbreviation), tolower(abbreviation),location)) %>%
    # add a column for states/'US' that each location belongs to
    dplyr::mutate(state_abbreviation = substr(location, start = 1, stop = 2))%>%
    dplyr::mutate(state_abbreviation = unlist(lapply(state_abbreviation, 
                                                     function (x) {
                                                       ifelse(x %in% location,abbreviation[location ==x],"NA")
                                                     })))%>%
    dplyr::rename(abbreviation = state_abbreviation, 
                  state_abbreviation = abbreviation,
                  fips = location) %>%
    dplyr::select(-c("state_abbreviation")) 
  
  write.csv(locations, file = "data-locations/full_locations.csv")
    
}