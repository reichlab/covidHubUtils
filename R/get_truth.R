#' Download raw truth data from NYTimes and write to CSV files
#'
#' @param save_location character specifying the location of to save raw truth data.
#' Default to `"./data-truth/nytimes/raw/"`
#' @importFrom readr cols col_date col_integer col_character write_csv
#' @importFrom purrr map_dfr
#' @return data.frame of national, state and county level raw truth data
#'
#' @export
download_raw_nytimes <- function(save_location = "./data-truth/nytimes/raw/") {
  us_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv"
  states_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
  counties_urls <- c("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-2020.csv",
                    "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-2021.csv",
                    "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-2022.csv")

  us <- readr::read_csv(us_url,
    col_types = readr::cols(
      date = readr::col_date(format = "%Y-%m-%d"),
      cases = readr::col_integer(),
      deaths = readr::col_integer()
    )
  )

  states <- readr::read_csv(states_url,
    col_types = readr::cols(
      date = readr::col_date(format = "%Y-%m-%d"),
      state = readr::col_character(),
      fips = readr::col_character(),
      cases = readr::col_integer(),
      deaths = readr::col_integer()
    )
  )

  counties <- counties_urls %>%
    purrr::map_dfr(
      readr::read_csv,
      col_types = readr::cols(
        date = readr::col_date(format = "%Y-%m-%d"),
        state = readr::col_character(),
        fips = readr::col_character(),
        cases = readr::col_integer(),
        deaths = readr::col_integer()
      )
    )

  readr::write_csv(us, file = paste0(save_location, "us.csv"))
  readr::write_csv(states, file = paste0(save_location, "us-states.csv"))
  readr::write_csv(counties, file = paste0(save_location, "us-counties.csv"))
  return(list("us" = us, "states" = states, "counties" = counties))
}


#' Preprocess raw truth data from NYTimes into Cumulative/Incident - Deaths/Cases and write to CSVs
#'
#' @param save_location character specifying the location of to save raw truth data.
#' Default to `"./data-truth/nytimes/raw/"`
#'
#' @importFrom readr write_csv
#' @export
preprocess_nytimes <- function(save_location = "./data-truth/nytimes/") {
  raw_dframes <- download_raw_nytimes(paste0(save_location, "raw/"))
  us <- raw_dframes$us
  states <- raw_dframes$states
  counties <- raw_dframes$counties

  d <- us %>%
    dplyr::mutate(location = "US") %>%
    dplyr::bind_rows(states %>%
      dplyr::rename(location = fips) %>%
      dplyr::select(-state)) %>%
    dplyr::bind_rows(counties %>%
      dplyr::rename(location = fips) %>%
      dplyr::select(-state, -county)) %>%
    dplyr::group_by(location) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      inc_deaths = as.integer(diff(c(0, deaths))),
      inc_cases  = as.integer(diff(c(0, cases)))
    ) %>%
    dplyr::arrange(location, date)


  readr::write_csv(
    d %>% dplyr::select(date, location, deaths) %>% dplyr::rename(value = deaths),
    file = paste0(save_location, "truth_nytimes-Cumulative Deaths.csv")
  )

  readr::write_csv(
    d %>% dplyr::select(date, location, cases) %>% dplyr::rename(value = cases),
    file = paste0(save_location, "truth_nytimes-Cumulative Cases.csv")
  )

  readr::write_csv(
    d %>% dplyr::select(date, location, inc_deaths) %>% dplyr::rename(value = inc_deaths),
    file = paste0(save_location, "truth_nytimes-Incident Deaths.csv")
  )

  readr::write_csv(
    d %>% dplyr::select(date, location, inc_cases) %>% dplyr::rename(value = inc_cases),
    file = paste0(save_location, "truth_nytimes-Incident Cases.csv")
  )
}


#' Preprocess raw truth data from JHU CSSE into Cumulative/Incident - Deaths/Cases and write to CSVs
#'
#' @param save_location character specifying the location of to save raw truth data.
#' Default to `"./data-truth"`
#'
#' @importFrom readr write_csv
#' @export
preprocess_jhu <- function(save_location = "./data-truth") {
  # Location name
  location_names <- covidData::fips_codes[, c("location", "location_name")]

  # Spatial resolutions
  spatial_resolutions <- c("national", "state", "county")

  # Combine to get cases data
  cases_dframes <- covidData::load_jhu_data(
    spatial_resolution = spatial_resolutions,
    temporal_resolution = "daily",
    measure = "cases"
  )
  cases_dframes <- dplyr::left_join(cases_dframes, location_names, by = "location")
  cases_dframes <- cases_dframes[!(cases_dframes$location == "11001"), ]

  # Combine to get death data
  deaths_dframes <- covidData::load_jhu_data(
    spatial_resolution = spatial_resolutions,
    temporal_resolution = "daily",
    measure = "deaths"
  )
  deaths_dframes <- dplyr::left_join(deaths_dframes, location_names, by = "location")
  deaths_dframes <- deaths_dframes[!(deaths_dframes$location == "11001"), ]

  # Get cumulative deaths
  cumulative_deaths <- deaths_dframes[, c("date", "location", "location_name", "cum")]
  colnames(cumulative_deaths)[colnames(cumulative_deaths) == "cum"] <- "value"
  cumulative_deaths <- cumulative_deaths[!rowSums(is.na(cumulative_deaths[c("location_name", "value")])), ]
  readr::write_csv(cumulative_deaths, file = file.path(save_location, "truth-Cumulative Deaths.csv"))

  # Get incident deaths
  incident_deaths <- deaths_dframes[, c("date", "location", "location_name", "inc")]
  colnames(incident_deaths)[colnames(incident_deaths) == "inc"] <- "value"
  incident_deaths <- incident_deaths[!rowSums(is.na(incident_deaths[c("location_name", "value")])), ]
  readr::write_csv(incident_deaths, file = file.path(save_location, "truth-Incident Deaths.csv"))

  # Get cumulative cases
  cumulative_cases <- cases_dframes[, c("date", "location", "location_name", "cum")]
  colnames(cumulative_cases)[colnames(cumulative_cases) == "cum"] <- "value"
  cumulative_cases <- cumulative_cases[!rowSums(is.na(cumulative_cases[c("location_name", "value")])), ]
  readr::write_csv(cumulative_cases, file = file.path(save_location, "truth-Cumulative Cases.csv"))

  # Get incident cases
  incident_cases <- cases_dframes[, c("date", "location", "location_name", "inc")]
  colnames(incident_cases)[colnames(incident_cases) == "inc"] <- "value"
  incident_cases <- incident_cases[!rowSums(is.na(incident_cases[c("location_name", "value")])), ]
  readr::write_csv(incident_cases, file = file.path(save_location, "truth-Incident Cases.csv"))

  return(list(
    "cumulative_deaths" = cumulative_deaths,
    "incident_deaths" = incident_deaths,
    "cumulative_cases" = cumulative_cases,
    "incident_cases" = incident_cases
  ))
}


#' Preprocess raw truth data from JHU CSSE into Cumulative/Incident - Deaths/Cases for visualization
#' purpose and write to JSON files
#'
#' @param save_location character specifying the location of to save raw truth data.
#' Default to `"./visualization/vis-master/covid-csv-tools/dist/truth"`
#'
#' @importFrom readr write_csv
#' @importFrom jsonlite write_json
#' @export
preprocess_visualization_truth <- function(save_location = "./visualization/vis-master/covid-csv-tools/dist/truth") {
  # Location name
  location_names <- covidData::fips_codes[, c("location", "location_name", "abbreviation")]

  # Spatial resolutions
  spatial_resolutions <- c("national", "state")

  # Combine to get cases data
  cases_dframes <- covidData::load_jhu_data(
    spatial_resolution = spatial_resolutions,
    temporal_resolution = "weekly",
    measure = "cases"
  )
  cases_dframes <- dplyr::left_join(cases_dframes, location_names, by = "location")
  cases_mmwr <- lapply(cases_dframes["date"], MMWRweek::MMWRweek)$date
  # shift epiweek on axis
  cases_mmwr_week <- cases_mmwr["MMWRweek"] + 1
  cases_mmwr_year <- cases_mmwr["MMWRyear"]
  cases_mmwr_year[which(cases_mmwr_week > 53), ] <- cases_mmwr_year[which(cases_mmwr_week > 53), ] + 1
  cases_mmwr_week[which(cases_mmwr_week > 53), ] <- 1
  # format date as "{epiyear}{epiweek}". Exp: "202005"
  cases_mmwr_week <- data.frame(lapply(cases_mmwr_week, sprintf, fmt = "%02d"))
  cases_dframes["date"] <- paste(cases_mmwr_year$MMWRyear, cases_mmwr_week$MMWRweek, sep = "")
  # Replace US with "nat" this is NECESSARY for visualization code!
  cases_dframes <- cases_dframes %>%
    dplyr::mutate(abbreviation = replace(abbreviation, abbreviation == "US", "nat"))
  # Threshold to 0 for incident values < 0, for visualization purpose only
  cases_dframes$inc[cases_dframes$inc < 0] <- 0

  # Combine to get death data
  deaths_dframes <- covidData::load_jhu_data(
    spatial_resolution = spatial_resolutions,
    temporal_resolution = "weekly",
    measure = "deaths"
  )
  deaths_dframes <- dplyr::left_join(deaths_dframes, location_names, by = "location")
  deaths_mmwr <- lapply(deaths_dframes["date"], MMWRweek::MMWRweek)$date
  # shift epiweek on axis
  deaths_mmwr_week <- deaths_mmwr["MMWRweek"] + 1
  deaths_mmwr_year <- deaths_mmwr["MMWRyear"]
  deaths_mmwr_year[which(deaths_mmwr_week > 53), ] <- deaths_mmwr_year[which(deaths_mmwr_week > 53), ] + 1
  deaths_mmwr_week[which(deaths_mmwr_week > 53), ] <- 1
  # format date as "{epiyear}{epiweek}". Exp: "202005"
  deaths_mmwr_week <- data.frame(lapply(deaths_mmwr_week, sprintf, fmt = "%02d"))
  deaths_dframes["date"] <- paste(deaths_mmwr_year$MMWRyear, deaths_mmwr_week$MMWRweek, sep = "")
  # Replace US with "nat" this is NECESSARY for visualization code!
  deaths_dframes <- deaths_dframes %>%
    dplyr::mutate(abbreviation = replace(abbreviation, abbreviation == "US", "nat"))
  # Threshold to 0 for incident values < 0, for visualization purpose only
  deaths_dframes$inc[deaths_dframes$inc < 0] <- 0

  # Get cumulative deaths
  cumulative_deaths <- deaths_dframes[, c("abbreviation", "date", "cum")]
  names(cumulative_deaths)[1:3] <- c("location", "epiweek", "value")
  cumulative_deaths <- cumulative_deaths[!rowSums(is.na(cumulative_deaths[c("location", "value")])), ]
  cumulative_deaths$value[cumulative_deaths$value == 0] <- 0.1
  jsonlite::write_json(cumulative_deaths, path = file.path(save_location, "Cumulative Deaths.json"))

  # Get incident deaths
  incident_deaths <- deaths_dframes[, c("abbreviation", "date", "inc")]
  names(incident_deaths)[1:3] <- c("location", "epiweek", "value")
  incident_deaths <- incident_deaths[!rowSums(is.na(incident_deaths[c("location", "value")])), ]
  incident_deaths$value[incident_deaths$value == 0] <- 0.1
  jsonlite::write_json(incident_deaths, path = file.path(save_location, "Incident Deaths.json"))

  # Get cumulative cases
  cumulative_cases <- cases_dframes[, c("abbreviation", "date", "cum")]
  names(cumulative_cases)[1:3] <- c("location", "epiweek", "value")
  cumulative_cases <- cumulative_cases[!rowSums(is.na(cumulative_cases[c("location", "value")])), ]
  cumulative_cases$value[cumulative_cases$value == 0] <- 0.1
  jsonlite::write_json(cumulative_cases, path = file.path(save_location, "Cumulative Cases.json"))

  # Get incident cases
  incident_cases <- cases_dframes[, c("abbreviation", "date", "inc")]
  names(incident_cases)[1:3] <- c("location", "epiweek", "value")
  incident_cases <- incident_cases[!rowSums(is.na(incident_cases[c("location", "value")])), ]
  incident_cases$value[incident_cases$value == 0] <- 0.1
  jsonlite::write_json(incident_cases, path = file.path(save_location, "Incident Cases.json"))

  return(list(
    "cumulative_deaths" = cumulative_deaths,
    "incident_deaths" = incident_deaths,
    "cumulative_cases" = cumulative_cases,
    "incident_cases" = incident_cases
  ))
}


#' Preprocess raw hospitalization data into Cumulative/Incident hospitalizations and write to CSVs
#'
#' @param save_location character specifying the location of to save raw truth data.
#' Default to `"./data-truth"`
#'
#' @importFrom readr write_csv
#' @return data.frame of cumulative and incident hospitalization data
#' @export
preprocess_hospitalization <- function(save_location = "./data-truth") {
  # Location name
  location_names <- covidData::fips_codes[, c("location", "location_name")]

  # Spatial resolutions
  spatial_resolutions <- c("national", "state")

  # Combine to get cases data
  hospitalization_dframes <- covidData::load_healthdata_data(
    spatial_resolution = spatial_resolutions,
    temporal_resolution = "daily",
    measure = "hospitalizations"
  )
  hospitalization_dframes <- dplyr::left_join(hospitalization_dframes, location_names, by = "location")

  # Get cumulative hospitalizations
  cumulative_hosp <- hospitalization_dframes[, c("date", "location", "location_name", "cum")]
  colnames(cumulative_hosp)[colnames(cumulative_hosp) == "cum"] <- "value"

  # Filter out rows with NA values in 'location_name' and 'value' columns, else the visualization might break
  cumulative_hosp <- cumulative_hosp[!rowSums(is.na(cumulative_hosp[c("location_name", "value")])), ]
  readr::write_csv(cumulative_hosp, file = file.path(save_location, "truth-Cumulative Hospitalizations.csv"))

  # Get incident hospitalizations
  incident_hosp <- hospitalization_dframes[, c("date", "location", "location_name", "inc")]
  colnames(incident_hosp)[colnames(incident_hosp) == "inc"] <- "value"

  # Filter out rows with NA values in 'location_name' and 'value' columns, else the visualization might break
  incident_hosp <- incident_hosp[!rowSums(is.na(incident_hosp[c("location_name", "value")])), ]
  readr::write_csv(incident_hosp, file = file.path(save_location, "truth-Incident Hospitalizations.csv"))

  return(list("cumulative_hosp" = cumulative_hosp, "incident_hosp" = incident_hosp))
}
