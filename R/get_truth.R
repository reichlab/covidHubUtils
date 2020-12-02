#' Download raw truth data from NYTimes and write to CSV files
#' 
#' @param save_location character specifying the location of to save raw truth data.
#' Default to "./data-truth/nytimes/raw/"
#' 
#' @return data frame of national, state and county level raw truth data
#' 
#' @export
download_raw_nytimes <- function (save_location="./data-truth/nytimes/raw/"){
  us_url     <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv"
  states_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
  counties_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

  us <- readr::read_csv(us_url,
                        col_types = readr::cols(
                          date   = readr::col_date(format = "%Y-%m-%d"),
                          cases  = readr::col_integer(),
                          deaths = readr::col_integer()
                        )) 
  
  states <- readr::read_csv(states_url,
                            col_types = readr::cols(
                              date   = readr::col_date(format = "%Y-%m-%d"),
                              state  = readr::col_character(),
                              fips   = readr::col_character(),
                              cases  = readr::col_integer(),
                              deaths = readr::col_integer()
                            )) 
  
  counties <- readr::read_csv(counties_url,
                              col_types = readr::cols(
                                date   = readr::col_date(format = "%Y-%m-%d"),
                                county = readr::col_character(),
                                state  = readr::col_character(),
                                fips   = readr::col_character(),
                                cases  = readr::col_integer(),
                                deaths = readr::col_integer()
                              )) 
  
  readr::write_csv(us,       path = paste0(save_location,"us.csv"))
  readr::write_csv(states,   path = paste0(save_location,"us-states.csv"))
  readr::write_csv(counties, path = paste0(save_location,"us-counties.csv"))
  return(list("us" = us, "states" = states, "counties" = counties))
}


#' Preprocess raw truth data from NYTimes into Cumulative/Incident - Deaths/Cases and write to CSVs
#' 
#' @param save_location character specifying the location of to save raw truth data.
#' Default to "./data-truth/nytimes/raw/"
#' 
#' @export
preprocess_nytimes <- function (save_location="./data-truth/nytimes/"){
  raw_dframes <- download_raw_nytimes(paste0(save_location,"raw/"))
  us <- raw_dframes$us
  states <- raw_dframes$states
  counties <- raw_dframes$counties
  
  d <- us %>%
    dplyr::mutate(location = "US") %>%
    dplyr::bind_rows(states %>% 
                       dplyr::rename(location = fips) %>%
                       dplyr::select(-state)
    ) %>%
    dplyr::bind_rows(counties %>%
                       dplyr::rename(location = fips) %>%
                       dplyr::select(-state, -county)
    ) %>%
    dplyr::group_by(location) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      inc_deaths = as.integer(diff(c(0, deaths))),
      inc_cases  = as.integer(diff(c(0, cases )))) %>%
    dplyr::arrange(location, date) 
  
  
  readr::write_csv(
    d %>% dplyr::select(date, location, deaths) %>% dplyr::rename(value = deaths),
    path = paste0(save_location,"truth_nytimes-Cumulative Deaths.csv"))
  
  readr::write_csv(
    d %>% dplyr::select(date, location, cases) %>% dplyr::rename(value = cases),
    path = paste0(save_location,"truth_nytimes-Cumulative Cases.csv"))
  
  readr::write_csv(
    d %>% dplyr::select(date, location, inc_deaths) %>% dplyr::rename(value = inc_deaths),
    path = paste0(save_location,"truth_nytimes-Incident Deaths.csv"))
  
  readr::write_csv(
    d %>% dplyr::select(date, location, inc_cases) %>% dplyr::rename(value = inc_cases),
    path = paste0(save_location,"truth_nytimes-Incident Cases.csv"))
}


#' Download raw truth data from USAFacts
#' 
#' @param save_location character specifying the location of to save raw truth data.
#' Default to "./data-truth/usafacts/raw/"
#' 
#' @return data frame of cases and deaths raw truth data
#' 
#' @export
download_raw_usafacts <- function (save_location="./data-truth/usafacts/raw/"){
  confirmed_url <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"
  deaths_url    <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv"

  cases <- readr::read_csv(confirmed_url,
                           col_types = readr::cols(
                             countyFIPS    = readr::col_integer(),
                             `County Name` = readr::col_character(),
                             State         = readr::col_character(),
                             stateFIPS     = readr::col_integer(),
                             .default      = readr::col_integer()
                           )) 
  
  deaths <- readr::read_csv(deaths_url,
                            col_types = readr::cols(
                              countyFIPS    = readr::col_integer(),
                              `County Name` = readr::col_character(),
                              State         = readr::col_character(),
                              stateFIPS     = readr::col_integer(),
                              .default      = readr::col_integer()
                            ))
  
  
  readr::write_csv(cases,  path = paste0(save_location,"covid_confirmed_usafacts.csv"))
  readr::write_csv(deaths, path = paste0(save_location,"covid_deaths_usafacts.csv"))
  return(list("cases" = cases, "deaths" = deaths))
}


#' Preprocess raw truth data from USAFacts into Cumulative/Incident - Deaths/Cases and write to CSVs
#' 
#' @param save_location character specifying the location of to save raw truth data.
#' Default to "./data-truth/usafacts/raw/"
#' 
#' @export
preprocess_usafacts <- function (save_location="./data-truth/usafacts/"){
  raw_dframes <- download_raw_usafacts(paste0(save_location,"raw/"))
  cases <- raw_dframes$cases
  deaths <- raw_dframes$deaths

  counties <- cases %>% dplyr::mutate(cases_deaths = "case") %>%
    dplyr::bind_rows(deaths %>% dplyr::mutate(cases_deaths = "death")) %>%
    dplyr::select(-`County Name`, -State, -stateFIPS) %>%
    dplyr::rename(location = countyFIPS) %>%
    dplyr::filter(location >= 1000) %>%
    dplyr::mutate(location = sprintf("%05d", location)) %>%
    tidyr::pivot_longer(
      -c(location, cases_deaths),
      names_to = "date",
      values_to = "cum"
    ) %>%
    dplyr::mutate(date = as.Date(date, format = "%m/%d/%y")) %>%
    
    dplyr::group_by(location, cases_deaths) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(inc = diff(c(0,cum))) %>%
    dplyr::ungroup()
  
  
  states <- cases %>% dplyr::mutate(cases_deaths = "case") %>%
    dplyr::bind_rows(deaths %>% dplyr::mutate(cases_deaths = "death")) %>%
    
    dplyr::select(-countyFIPS, -`County Name`, -State) %>%
    dplyr::rename(location = stateFIPS) %>%
    dplyr::mutate(location = sprintf("%02d", location)) %>%
    tidyr::pivot_longer(
      -c(location, cases_deaths),
      names_to = "date",
      values_to = "cum"
    ) %>%
    dplyr::mutate(date = as.Date(date, format = "%m/%d/%y")) %>%
    
    # Calculate incident cases and deaths 
    # aggregated across counties within a state
    dplyr::group_by(location, cases_deaths, date) %>%
    dplyr::summarize(cum = sum(cum)) %>%
    dplyr::group_by(location, cases_deaths) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(inc = diff(c(0,cum))) %>%
    dplyr::ungroup()
  
  us <- states %>% 
    dplyr::group_by(cases_deaths, date) %>%
    dplyr::summarize(cum = sum(cum)) %>%
    dplyr::group_by(cases_deaths) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(inc = diff(c(0,cum))) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(location = "US")
  
  d <- bind_rows(counties, states, us)
  
  
  
  readr::write_csv(
    d %>% 
      dplyr::filter(cases_deaths == "death") %>% 
      dplyr::rename(value = cum) %>%
      dplyr::select(date, location, value),
    path = paste0(save_location,"truth_usafacts-Cumulative Deaths.csv"))
  
  readr::write_csv(
    d %>% 
      dplyr::filter(cases_deaths == "death") %>% 
      dplyr::rename(value = inc) %>%
      dplyr::select(date, location, value),
    path = paste0(save_location,"truth_usafacts-Incident Deaths.csv"))
  
  readr::write_csv(
    d %>% 
      dplyr::filter(cases_deaths == "case") %>% 
      dplyr::rename(value = cum) %>%
      dplyr::select(date, location, value),
    path = paste0(save_location,"truth_usafacts-Cumulative Cases.csv"))
  
  readr::write_csv(
    d %>% 
      dplyr::filter(cases_deaths == "case") %>% 
      dplyr::rename(value = inc) %>%
      dplyr::select(date, location, value),
    path = paste0(save_location,"truth_usafacts-Incident Cases.csv"))
}


#' Preprocess raw truth data from JHU CSSE into Cumulative/Incident - Deaths/Cases and write to CSVs
#' 
#' @param save_location character specifying the location of to save raw truth data.
#' Default to "./data-truth/"
#' 
#' @export
preprocess_jhu <- function (save_location="./data-truth/"){
  # Location name
  location_names <- covidData::fips_codes[, c("location", "location_name")]
  
  # National data
  national_cases_dframes <- covidData::load_jhu_data(spatial_resolution = "national",
                                                     temporal_resolution = "daily",
                                                     measure = "cases")
  national_deaths_dframes <- covidData::load_jhu_data(spatial_resolution = "national",
                                                      temporal_resolution = "daily",
                                                      measure = "deaths")

  # State data
  state_cases_dframes <- covidData::load_jhu_data(spatial_resolution = "state",
                                                     temporal_resolution = "daily",
                                                     measure = "cases")
  state_deaths_dframes <- covidData::load_jhu_data(spatial_resolution = "state",
                                                      temporal_resolution = "daily",
                                                      measure = "deaths")
  
  # County data
  county_cases_dframes <- covidData::load_jhu_data(spatial_resolution = "county",
                                                     temporal_resolution = "daily",
                                                     measure = "cases")
  county_deaths_dframes <- covidData::load_jhu_data(spatial_resolution = "county",
                                                      temporal_resolution = "daily",
                                                      measure = "deaths")
  
  # Combine to get cases data
  cases_dframes <- dplyr::bind_rows(national_cases_dframes, state_cases_dframes, county_cases_dframes)
  cases_dframes <- dplyr::left_join(cases_dframes, location_names, by = "location")
  cases_dframes$inc[cases_dframes$inc < 0] <- 0
  cases_dframes<-cases_dframes[!(cases_dframes$location==11001),]
  
  # Combine to get death data
  deaths_dframes <- dplyr::bind_rows(national_deaths_dframes, state_deaths_dframes, county_deaths_dframes)
  deaths_dframes <- dplyr::left_join(deaths_dframes, location_names, by = "location")
  deaths_dframes$inc[deaths_dframes$inc < 0] <- 0
  deaths_dframes<-deaths_dframes[!(deaths_dframes$location==11001),]
  
  # Get cumulative deaths
  cumulative_deaths <- deaths_dframes[, c("date", "location", "location_name", "cum")]
  colnames(cumulative_deaths)[colnames(cumulative_deaths) == 'cum'] <- 'value'
  cumulative_deaths <- cumulative_deaths[!rowSums(is.na(cumulative_deaths[c("location_name","value")])), ]
  readr::write_csv(cumulative_deaths, path = paste0(save_location,"truth-Cumulative Deaths.csv"))
  
  # Get incident deaths
  incident_deaths <- deaths_dframes[, c("date", "location", "location_name", "inc")]
  colnames(incident_deaths)[colnames(incident_deaths) == 'inc'] <- 'value'
  incident_deaths <- incident_deaths[!rowSums(is.na(incident_deaths[c("location_name","value")])), ]
  readr::write_csv(incident_deaths, path = paste0(save_location,"truth-Incident Deaths.csv"))
  
  # Get cumulative cases
  cumulative_cases <- cases_dframes[, c("date", "location", "location_name", "cum")]
  colnames(cumulative_cases)[colnames(cumulative_cases) == 'cum'] <- 'value'
  cumulative_cases <- cumulative_cases[!rowSums(is.na(cumulative_cases[c("location_name","value")])), ]
  readr::write_csv(cumulative_cases, path = paste0(save_location,"truth-Cumulative Cases.csv"))
  
  # Get incident cases
  incident_cases <- cases_dframes[, c("date", "location", "location_name", "inc")]
  colnames(incident_cases)[colnames(incident_cases) == 'inc'] <- 'value'
  incident_cases <- incident_cases[!rowSums(is.na(incident_cases[c("location_name","value")])), ]
  readr::write_csv(incident_cases, path = paste0(save_location,"truth-Incident Cases.csv"))
}