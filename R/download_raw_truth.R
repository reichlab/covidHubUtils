#' Download raw truth data from NYTimes
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
  return(list(us, states, counties))
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
  return(list(cases, deaths))
}