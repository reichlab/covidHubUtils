#' Download raw truth data from NYTimes and write to CSV files
#' 
#' @param save_location character specifying the location of to save raw truth data.
#' Default to "./data-truth/nytimes/raw/"
#' @importFrom readr cols col_date col_integer col_character write_csv
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
  
  readr::write_csv(us,       file = paste0(save_location,"us.csv"))
  readr::write_csv(states,   file = paste0(save_location,"us-states.csv"))
  readr::write_csv(counties, file = paste0(save_location,"us-counties.csv"))
  return(list("us" = us, "states" = states, "counties" = counties))
}


#' Preprocess raw truth data from NYTimes into Cumulative/Incident - Deaths/Cases and write to CSVs
#' 
#' @param save_location character specifying the location of to save raw truth data.
#' Default to "./data-truth/nytimes/raw/"
#' 
#' @importFrom readr write_csv
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
    file = paste0(save_location,"truth_nytimes-Cumulative Deaths.csv"))
  
  readr::write_csv(
    d %>% dplyr::select(date, location, cases) %>% dplyr::rename(value = cases),
    file = paste0(save_location,"truth_nytimes-Cumulative Cases.csv"))
  
  readr::write_csv(
    d %>% dplyr::select(date, location, inc_deaths) %>% dplyr::rename(value = inc_deaths),
    file = paste0(save_location,"truth_nytimes-Incident Deaths.csv"))
  
  readr::write_csv(
    d %>% dplyr::select(date, location, inc_cases) %>% dplyr::rename(value = inc_cases),
    file = paste0(save_location,"truth_nytimes-Incident Cases.csv"))
}


#' Download raw truth data from USAFacts
#' 
#' @param save_location character specifying the location of to save raw truth data.
#' Default to "./data-truth/usafacts/raw/"
#' 
#' @importFrom readr read_csv write_csv
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
                             StateFIPS     = readr::col_integer(),
                             .default      = readr::col_integer()
                           )) 
  
  deaths <- readr::read_csv(deaths_url,
                            col_types = readr::cols(
                              countyFIPS    = readr::col_integer(),
                              `County Name` = readr::col_character(),
                              State         = readr::col_character(),
                              StateFIPS     = readr::col_integer(),
                              .default      = readr::col_integer()
                            ))
  
  
  readr::write_csv(cases,  file = paste0(save_location,"covid_confirmed_usafacts.csv"))
  readr::write_csv(deaths, file = paste0(save_location,"covid_deaths_usafacts.csv"))
  return(list("cases" = cases, "deaths" = deaths))
}


#' Preprocess raw truth data from USAFacts into Cumulative/Incident - Deaths/Cases and write to CSVs
#' 
#' @param save_location character specifying the location of to save raw truth data.
#' Default to "./data-truth/usafacts/raw/"
#' 
#' @importFrom readr write_csv
#' @export
preprocess_usafacts <- function (save_location="./data-truth/usafacts/"){
  raw_dframes <- download_raw_usafacts(paste0(save_location,"raw/"))
  cases <- raw_dframes$cases
  deaths <- raw_dframes$deaths

  counties <- cases %>% dplyr::mutate(cases_deaths = "case") %>%
    dplyr::bind_rows(deaths %>% dplyr::mutate(cases_deaths = "death")) %>%
    dplyr::select(-`County Name`, -State, -StateFIPS) %>%
    dplyr::rename(location = countyFIPS) %>%
    dplyr::filter(location >= 1000) %>%
    dplyr::mutate(location = sprintf("%05d", location)) %>%
    tidyr::pivot_longer(
      -c(location, cases_deaths),
      names_to = "date",
      values_to = "cum"
    ) %>%
    dplyr::mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    
    dplyr::group_by(location, cases_deaths) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(inc = diff(c(0,cum))) %>%
    dplyr::ungroup()
  
  
  states <- cases %>% dplyr::mutate(cases_deaths = "case") %>%
    dplyr::bind_rows(deaths %>% dplyr::mutate(cases_deaths = "death")) %>%
    
    dplyr::select(-countyFIPS, -`County Name`, -State) %>%
    dplyr::rename(location = StateFIPS) %>%
    dplyr::mutate(location = sprintf("%02d", location)) %>%
    tidyr::pivot_longer(
      -c(location, cases_deaths),
      names_to = "date",
      values_to = "cum"
    ) %>%
    dplyr::mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    
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
  
  d <- dplyr::bind_rows(counties, states, us)
  
  
  
  readr::write_csv(
    d %>% 
      dplyr::filter(cases_deaths == "death") %>% 
      dplyr::rename(value = cum) %>%
      dplyr::select(date, location, value),
    file = paste0(save_location,"truth_usafacts-Cumulative Deaths.csv"))
  
  readr::write_csv(
    d %>% 
      dplyr::filter(cases_deaths == "death") %>% 
      dplyr::rename(value = inc) %>%
      dplyr::select(date, location, value),
    file = paste0(save_location,"truth_usafacts-Incident Deaths.csv"))
  
  readr::write_csv(
    d %>% 
      dplyr::filter(cases_deaths == "case") %>% 
      dplyr::rename(value = cum) %>%
      dplyr::select(date, location, value),
    file = paste0(save_location,"truth_usafacts-Cumulative Cases.csv"))
  
  readr::write_csv(
    d %>% 
      dplyr::filter(cases_deaths == "case") %>% 
      dplyr::rename(value = inc) %>%
      dplyr::select(date, location, value),
    file = paste0(save_location,"truth_usafacts-Incident Cases.csv"))
}


#' Preprocess raw truth data from JHU CSSE into Cumulative/Incident - Deaths/Cases and write to CSVs
#' 
#' @param save_location character specifying the location of to save raw truth data.
#' Default to "./data-truth"
#' 
#' @importFrom readr write_csv
#' @export
preprocess_jhu <- function (save_location="./data-truth"){
  # Location name
  location_names <- covidData::fips_codes[, c("location", "location_name")]
  
  # Spatial resolutions
  spatial_resolutions <- c("national", "state", "county")
  
  # Combine to get cases data
  cases_dframes <- covidData::load_jhu_data(spatial_resolution = spatial_resolutions,
                                            temporal_resolution = "daily",
                                            measure = "cases")
  cases_dframes <- dplyr::left_join(cases_dframes, location_names, by = "location")
  cases_dframes<-cases_dframes[!(cases_dframes$location=="11001"),]
  
  # Combine to get death data
  deaths_dframes <- covidData::load_jhu_data(spatial_resolution = spatial_resolutions,
                                             temporal_resolution = "daily",
                                             measure = "deaths")
  deaths_dframes <- dplyr::left_join(deaths_dframes, location_names, by = "location")
  deaths_dframes<-deaths_dframes[!(deaths_dframes$location=="11001"),]
  
  # Get cumulative deaths
  cumulative_deaths <- deaths_dframes[, c("date", "location", "location_name", "cum")]
  colnames(cumulative_deaths)[colnames(cumulative_deaths) == 'cum'] <- 'value'
  cumulative_deaths <- cumulative_deaths[!rowSums(is.na(cumulative_deaths[c("location_name","value")])), ]
  readr::write_csv(cumulative_deaths, file = file.path(save_location,"truth-Cumulative Deaths.csv"))
  
  # Get incident deaths
  incident_deaths <- deaths_dframes[, c("date", "location", "location_name", "inc")]
  colnames(incident_deaths)[colnames(incident_deaths) == 'inc'] <- 'value'
  incident_deaths <- incident_deaths[!rowSums(is.na(incident_deaths[c("location_name","value")])), ]
  readr::write_csv(incident_deaths, file = file.path(save_location,"truth-Incident Deaths.csv"))
  
  # Get cumulative cases
  cumulative_cases <- cases_dframes[, c("date", "location", "location_name", "cum")]
  colnames(cumulative_cases)[colnames(cumulative_cases) == 'cum'] <- 'value'
  cumulative_cases <- cumulative_cases[!rowSums(is.na(cumulative_cases[c("location_name","value")])), ]
  readr::write_csv(cumulative_cases, file = file.path(save_location,"truth-Cumulative Cases.csv"))
  
  # Get incident cases
  incident_cases <- cases_dframes[, c("date", "location", "location_name", "inc")]
  colnames(incident_cases)[colnames(incident_cases) == 'inc'] <- 'value'
  incident_cases <- incident_cases[!rowSums(is.na(incident_cases[c("location_name","value")])), ]
  readr::write_csv(incident_cases, file = file.path(save_location,"truth-Incident Cases.csv"))
  
  return(list("cumulative_deaths" = cumulative_deaths, 
              "incident_deaths" = incident_deaths,
              "cumulative_cases" = cumulative_cases, 
              "incident_cases" = incident_cases))
}


#' Preprocess raw truth data from JHU CSSE into Cumulative/Incident - Deaths/Cases for visualization 
#' purpose and write to JSON files
#' 
#' @param save_location character specifying the location of to save raw truth data.
#' Default to "./visualization/vis-master/covid-csv-tools/dist/truth"
#' 
#' @importFrom readr write_csv
#' @importFrom jsonlite write_json
#' @export
preprocess_visualization_truth <- function (save_location="./visualization/vis-master/covid-csv-tools/dist/truth"){
  # Location name
  location_names <- covidData::fips_codes[, c("location", "location_name", "abbreviation")]
  
  # Spatial resolutions
  spatial_resolutions <- c("national", "state")
  
  # Combine to get cases data
  cases_dframes <- covidData::load_jhu_data(spatial_resolution = spatial_resolutions,
                                            temporal_resolution = "weekly",
                                            measure = "cases")
  cases_dframes <- dplyr::left_join(cases_dframes, location_names, by = "location")
  cases_mmwr <- lapply(cases_dframes['date'], MMWRweek::MMWRweek)$date
  # shift epiweek on axis
  cases_mmwr_week <- cases_mmwr['MMWRweek'] + 1
  cases_mmwr_year <- cases_mmwr['MMWRyear']
  cases_mmwr_year[which(cases_mmwr_week>53),] <- cases_mmwr_year[which(cases_mmwr_week>53),] + 1
  cases_mmwr_week[which(cases_mmwr_week>53),] <- 1
  # format date as "{epiyear}{epiweek}". Exp: "202005"
  cases_mmwr_week <- data.frame(lapply(cases_mmwr_week, sprintf, fmt = "%02d"))
  cases_dframes['date'] <- paste(cases_mmwr_year$MMWRyear, cases_mmwr_week$MMWRweek, sep = "")
  # Replace US with "nat" this is NECESSARY for visualization code!
  cases_dframes <- cases_dframes %>%
    dplyr::mutate(abbreviation = replace(abbreviation, abbreviation == "US", "nat"))
  # Threshold to 0 for incident values < 0, for visualization purpose only
  cases_dframes$inc[cases_dframes$inc < 0] <- 0
  
  # Combine to get death data
  deaths_dframes <- covidData::load_jhu_data(spatial_resolution = spatial_resolutions,
                                             temporal_resolution = "weekly",
                                             measure = "deaths")
  deaths_dframes <- dplyr::left_join(deaths_dframes, location_names, by = "location")
  deaths_mmwr <- lapply(deaths_dframes['date'], MMWRweek::MMWRweek)$date
  # shift epiweek on axis
  deaths_mmwr_week <- deaths_mmwr['MMWRweek'] + 1
  deaths_mmwr_year <- deaths_mmwr['MMWRyear']
  deaths_mmwr_year[which(deaths_mmwr_week>53),] <- deaths_mmwr_year[which(deaths_mmwr_week>53),] + 1
  deaths_mmwr_week[which(deaths_mmwr_week>53),] <- 1
  # format date as "{epiyear}{epiweek}". Exp: "202005"
  deaths_mmwr_week <- data.frame(lapply(deaths_mmwr_week, sprintf, fmt = "%02d"))
  deaths_dframes['date'] <- paste(deaths_mmwr_year$MMWRyear, deaths_mmwr_week$MMWRweek, sep = "")
  # Replace US with "nat" this is NECESSARY for visualization code!
  deaths_dframes <- deaths_dframes %>%
    dplyr::mutate(abbreviation = replace(abbreviation, abbreviation == "US", "nat"))
  # Threshold to 0 for incident values < 0, for visualization purpose only
  deaths_dframes$inc[deaths_dframes$inc < 0] <- 0
  
  # Get cumulative deaths
  cumulative_deaths <- deaths_dframes[, c("abbreviation", "date", "cum")]
  names(cumulative_deaths)[1:3] <- c("location","epiweek","value")
  cumulative_deaths <- cumulative_deaths[!rowSums(is.na(cumulative_deaths[c("location","value")])), ]
  cumulative_deaths$value[cumulative_deaths$value == 0] <- 0.1
  jsonlite::write_json(cumulative_deaths, path = file.path(save_location,"Cumulative Deaths.json"))
  
  # Get incident deaths
  incident_deaths <- deaths_dframes[, c("abbreviation", "date", "inc")]
  names(incident_deaths)[1:3] <- c("location","epiweek","value")
  incident_deaths <- incident_deaths[!rowSums(is.na(incident_deaths[c("location","value")])), ]
  incident_deaths$value[incident_deaths$value == 0] <- 0.1
  jsonlite::write_json(incident_deaths, path = file.path(save_location,"Incident Deaths.json"))
  
  # Get cumulative cases
  cumulative_cases <- cases_dframes[, c("abbreviation", "date", "cum")]
  names(cumulative_cases)[1:3] <- c("location","epiweek","value")
  cumulative_cases <- cumulative_cases[!rowSums(is.na(cumulative_cases[c("location","value")])), ]
  cumulative_cases$value[cumulative_cases$value == 0] <- 0.1
  jsonlite::write_json(cumulative_cases, path = file.path(save_location,"Cumulative Cases.json"))
  
  # Get incident cases
  incident_cases <- cases_dframes[, c("abbreviation", "date", "inc")]
  names(incident_cases)[1:3] <- c("location","epiweek","value")
  incident_cases <- incident_cases[!rowSums(is.na(incident_cases[c("location","value")])), ]
  incident_cases$value[incident_cases$value == 0] <- 0.1
  jsonlite::write_json(incident_cases, path = file.path(save_location,"Incident Cases.json"))
  
  return(list("cumulative_deaths" = cumulative_deaths, 
              "incident_deaths" = incident_deaths,
              "cumulative_cases" = cumulative_cases, 
              "incident_cases" = incident_cases))
}


#' Preprocess raw hospitalization data into Cumulative/Incident hospitalizations and write to CSVs
#' 
#' @param save_location character specifying the location of to save raw truth data.
#' Default to "./data-truth"
#' 
#' @importFrom readr write_csv
#' @return data frame of cumulative and incident hospitalization data
#' @export
preprocess_hospitalization <- function (save_location="./data-truth"){
  # Location name
  location_names <- covidData::fips_codes[, c("location", "location_name")]
  
  # Spatial resolutions
  spatial_resolutions <- c("national", "state")
  
  # Combine to get cases data
  hospitalization_dframes <- covidData::load_healthdata_data(spatial_resolution = spatial_resolutions,
                                                             temporal_resolution = "daily",
                                                             measure = "hospitalizations")
  hospitalization_dframes <- dplyr::left_join(hospitalization_dframes, location_names, by = "location")

  # Get cumulative hospitalizations
  cumulative_hosp <- hospitalization_dframes[, c("date", "location", "location_name", "cum")]
  colnames(cumulative_hosp)[colnames(cumulative_hosp) == 'cum'] <- 'value'
  
  # Filter out rows with NA values in 'location_name' and 'value' columns, else the visualization might break
  cumulative_hosp <- cumulative_hosp[!rowSums(is.na(cumulative_hosp[c("location_name","value")])), ]
  readr::write_csv(cumulative_hosp, file = file.path(save_location,"truth-Cumulative Hospitalizations.csv"))
  
  # Get incident hospitalizations
  incident_hosp <- hospitalization_dframes[, c("date", "location", "location_name", "inc")]
  colnames(incident_hosp)[colnames(incident_hosp) == 'inc'] <- 'value'
  
  # Filter out rows with NA values in 'location_name' and 'value' columns, else the visualization might break
  incident_hosp <- incident_hosp[!rowSums(is.na(incident_hosp[c("location_name","value")])), ]
  readr::write_csv(incident_hosp, file = file.path(save_location,"truth-Incident Hospitalizations.csv"))
  
  return(list("cumulative_hosp" = cumulative_hosp, "incident_hosp" = incident_hosp))
}

#' Generate versioned truth data for zoltar for a specified target
#' It only includes national and state-level truth data.
#' 
#' @param target string specifying target. 
#' Currently only support "Cumulative Deaths" and "Incident Deaths".
#' @param issue_date optional date specifying issue date of truth data.
#' Default to NULL which will load the latest truth data from covidData. 
#' 
#' @return data frame with timezero, unit, target and value
#'
#' @export
preprocess_truth_for_zoltar <- function(target, issue_date = NULL){
  
  # validate target 
  target <- match.arg(target, 
                      choices = c("Cumulative Deaths", "Incident Deaths"), 
                      several.ok = FALSE)
  
  # load the most up to date weeky truth data from JHU CSSE
  df <- covidData::load_jhu_data(issue_date = issue_date, 
                                 spatial_resolution = c('national', 'state'),
                                 temporal_resolution = 'weekly', 
                                 measure = 'deaths',
                                 replace_negatives = FALSE, 
                                 adjustment_cases = 'none',
                                 adjustment_method = 'none')
  
  # select columns needed for target
  if (target == "Cumulative Deaths"){
    target_var <- " wk ahead cum death"
    df <- df %>%
      dplyr::select(-inc) %>%
      dplyr::rename(value = cum, unit = location)
  } else {
    target_var <- " wk ahead inc death"
    df <- df %>%
      dplyr::select(-cum) %>%
      dplyr::rename(value = inc, unit = location)
  }
  
  # expand the data frame where for each observed value at time t, 
  # that observation is the observed value at each horizon 1 through 20 
  # relative to the corresponding past forecast timezeros from times t-1 through t-20
  df <- tidyr::expand_grid(df, horizon = seq_len(20)) %>%
    dplyr::mutate(target = paste0(horizon,target_var))
  
  # set up Zoltar connection
  zoltar_connection <- zoltr::new_connection()
  if(Sys.getenv("Z_USERNAME") == "" | Sys.getenv("Z_PASSWORD") == "") {
    zoltr::zoltar_authenticate(zoltar_connection, "zoltar_demo","Dq65&aP0nIlG")
  } else {
    zoltr::zoltar_authenticate(zoltar_connection, Sys.getenv("Z_USERNAME"),Sys.getenv("Z_PASSWORD"))
  }
  
  # construct Zoltar project url
  the_projects <- zoltr::projects(zoltar_connection)
  project_url <- the_projects[the_projects$name == "COVID-19 Forecasts", "url"]
  
  # get all valid timezeros from zoltar
  zoltar_timezeros<- zoltr::timezeros(zoltar_connection, project_url)$timezero_date
  
  # generate target end date with horzion 1 to 20 for all zoltar_timezeros
  df_zoltar <- tidyr::expand_grid(
    zoltar_timezeros = zoltar_timezeros, horizon = seq_len(20)) %>%
    dplyr::mutate(target_end_date = 
                    covidHubUtils::calc_target_week_end_date(
                      zoltar_timezeros, horizon))
  
  # merge zoltar timezeros with truth values and targets
  df_final <- merge(x = df, 
                    y = df_zoltar, 
                    by.x = c('date','horizon'),
                    by.y = c('target_end_date','horizon'),
                    all.y = TRUE)
  
  # select columns and drop empty rows
  df_final <- df_final %>%
    dplyr::select(zoltar_timezeros, unit, target, value) %>%
    dplyr::rename(timezero = zoltar_timezeros) %>%
    tidyr::drop_na()
  
  return (df_final)
  
}


#' Generate the most up to date truth data for zoltar
#' It only includes national and state-level truth data.
#' 
#' @param save_location character specifying the location of to save zoltar truth data.
#' Default to "./data-truth"
#' 
#' @importFrom readr write_csv
#' @export
#'
save_truth_for_zoltar <- function(save_location = "./data-truth"){
  
  df_cum_death <- preprocess_truth_for_zoltar("Cumulative Deaths")
  df_inc_death <- preprocess_truth_for_zoltar("Incident Deaths")
  
  zoltar_truth <- rbind(df_cum_death, df_inc_death)
  
  file_path <- file.path(save_location,"zoltar-truth.csv")
  
  readr::write_csv(zoltar_truth, file_path)
  
}
