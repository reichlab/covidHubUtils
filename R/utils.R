#' Set up Zoltar connection
#' 
#' @param staging boolean to change Zoltar server for staging
#' 
#' @return a Zoltar connection
#' @export
setup_zoltar_connection <- function(staging = FALSE) {
  if(staging){
    host = "https://rl-zoltar-staging.herokuapp.com"
  } else {
    host = "https://zoltardata.com"
  }
  
  zoltar_connection <- zoltr::new_connection(host)
  if(Sys.getenv("Z_USERNAME") == "" | Sys.getenv("Z_PASSWORD") == "") {
    zoltr::zoltar_authenticate(zoltar_connection, "zoltar_demo","Dq65&aP0nIlG")
  } else {
    zoltr::zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"),Sys.getenv("Z_PASSWORD"))
  }
  return(zoltar_connection)
}


#' Get Zoltar project URL
#' 
#' @param hub character vector, where the first element indicates the hub
#' from which to load forecasts. Possible options are "US" and "ECDC"
#' @param zoltar_connection an authenticated Zoltar connecton
#' 
#' @return project url
#' @export
get_zoltar_project_url <- function(hub = c("US", "ECDC"), 
                                   zoltar_connection = NULL) {
  # if no zoltar connection was provided, set up a new one
  if (is.null(zoltar_connection)) {
    zoltar_connection <- setup_zoltar_connection()
  }
  
  # get all existing projects
  the_projects <- zoltr::projects(zoltar_connection)
  
  # get the URL to the right forecast hub project
  if (hub[1] == "US") {
    project_url <- the_projects[the_projects$name == "COVID-19 Forecasts", "url"]
  } else if (hub[1] == "ECDC") {
    project_url <- the_projects[the_projects$name == "ECDC European COVID-19 Forecast Hub", "url"]
  }
  
  return(project_url)
}

#' Add stored information about location name and population by joining the 
#' data with the correct stored location data
#'
#' @param data data frame to append location data
#' @param hub character vector, where the first element indicates the hub
#' from which to load forecasts. Possible options are "US" and "ECDC"
#' 
#' @return original data with corresponding location information
#' @export
join_with_hub_locations <- function(data, 
                                    hub = c("US", "ECDC")) {
  if (hub[1] == "US") {
    data <- dplyr::left_join(data, 
                             covidHubUtils::hub_locations, 
                             by=c("location" = "fips"))
  } else if (hub[1] == "ECDC") {
    data <- dplyr::left_join(data, 
                             covidHubUtils::hub_locations_ecdc, 
                             by=c("location"))
  }
  return(data)
}

globalVariables(
  c(".", "County", "Name", "Prediction", "Interval", "State", "StateFIPS", 
    "V1", "V2", "abbreviation", "alpha", "cases", "cases_deaths", "county", 
    "countyFIPS", "cum", "deaths", "designation", "endpoint_type", 
    "exists_interval_score_0", "fips", "forecast_date", "full_location_name", 
    "geo_type", "head", "horizon", "inc", "inc_cases", "inc_deaths", 
    "interval_score_0", "location", "location_name", "lower", "model", 
    "model.x", "model_abbr", "n_interval_scores", "notes", "overprediction_0", 
    "point", "quantile", "sharpness_0", "state", "state_abbreviation", 
    "tail", "target", "target_end_date", "target_variable", 
    "team_model_designation", "temporal_resolution", "timezero", "true_value", 
    "truth_forecast", "truth_model", "type", "underprediction_0", 
    "unit", "upper", "value", "value.x", "value.y", 
    "County", "Name", "Prediction", "Interval"
  )
)
