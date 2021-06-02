context("get_truth")
library(covidHubUtils)
library(covidData)
library(zoltr)
library(dplyr)

test_that("preprocess_jhu files has expected columns", {
  actual <- covidHubUtils::preprocess_jhu(
    save_location = "."
  )
  
  actual_cumulative_deaths_column_names <- colnames(actual$cumulative_deaths)
  actual_incident_deaths_column_names <- colnames(actual$incident_deaths)
  actual_cumulative_cases_column_names <- colnames(actual$cumulative_cases)
  actual_incident_cases_column_names <- colnames(actual$incident_cases)
  
  expected_column_names <- c('date','location','location_name','value')
  
  expect_equal(actual_cumulative_deaths_column_names, expected_column_names)
  expect_equal(actual_incident_deaths_column_names, expected_column_names)
  expect_equal(actual_cumulative_cases_column_names, expected_column_names)
  expect_equal(actual_incident_cases_column_names, expected_column_names)
})


test_that("preprocess_jhu files has expected combinations of location, week", {
  # Location name
  location_names <- covidData::fips_codes[, c("location", "location_name")]
  
  # Spatial resolutions
  spatial_resolutions <- c("national", "state", "county")
  
  actual <- covidHubUtils::preprocess_jhu(
    save_location = "."
  )
  
  actual_cumulative_deaths <- actual$cumulative_deaths %>%
    dplyr::select(date, location)
  
  actual_incident_deaths <- actual$incident_deaths %>%
    dplyr::select(date, location)
  
  expected_deaths <- covidData::load_jhu_data(spatial_resolution = spatial_resolutions,
                                              temporal_resolution = "daily",
                                              measure = "deaths") %>% 
                      dplyr::left_join(location_names, by = "location") %>% 
                      dplyr::filter(location != "11001") 
  
  expected_cumulative_deaths <- expected_deaths[,c("date", "location", "location_name", "cum")] %>% 
    tidyr::drop_na(any_of(c("location_name", "cum"))) %>% 
    dplyr::select(date, location)
  
  expected_incident_deaths <- expected_deaths[,c("date", "location", "location_name", "inc")] %>% 
    tidyr::drop_na(any_of(c("location_name", "inc"))) %>% 
    dplyr::select(date, location)
  
  expect_equal(actual_cumulative_deaths, expected_cumulative_deaths)
  expect_equal(actual_incident_deaths, expected_incident_deaths)
  
  actual_cumulative_cases <- actual$cumulative_cases %>%
    dplyr::select(date, location)
  
  actual_incident_cases <- actual$incident_cases %>%
    dplyr::select(date, location)
  
  expected_cases <- covidData::load_jhu_data(spatial_resolution = spatial_resolutions,
                                              temporal_resolution = "daily",
                                              measure = "cases") %>% 
                      dplyr::left_join(location_names, by = "location") %>% 
                      dplyr::filter(location != "11001") 
  
  expected_cumulative_cases <- expected_cases[,c("date", "location", "location_name", "cum")] %>% 
    tidyr::drop_na(any_of(c("location_name", "cum"))) %>% 
    dplyr::select(date, location)
  
  expected_incident_cases <- expected_cases[,c("date", "location", "location_name", "inc")] %>% 
    tidyr::drop_na(any_of(c("location_name", "inc"))) %>% 
    dplyr::select(date, location)
  
  expect_equal(actual_cumulative_cases, expected_cumulative_cases)
  expect_equal(actual_incident_cases, expected_incident_cases)
})


test_that("preprocess_jhu files has the same cumulative and incident values as output from covidData", {
  # Location name
  location_names <- covidData::fips_codes[, c("location", "location_name")]
  
  # Spatial resolutions
  spatial_resolutions <- c("national", "state", "county")
  
  actual <- covidHubUtils::preprocess_jhu(
    save_location = "."
  )
  
  actual_cumulative_deaths <- actual$cumulative_deaths %>%
    dplyr::select(value)
  
  actual_incident_deaths <- actual$incident_deaths %>%
    dplyr::select(value)
  
  expected_deaths <- covidData::load_jhu_data(spatial_resolution = spatial_resolutions,
                                              temporal_resolution = "daily",
                                              measure = "deaths") %>% 
                      dplyr::left_join(location_names, by = "location") %>% 
                      dplyr::filter(location != "11001") 
  
  expected_cumulative_deaths <- expected_deaths[,c("date", "location", "location_name", "cum")] %>% 
    tidyr::drop_na(any_of(c("location_name", "cum"))) %>% 
    dplyr::rename(value = cum) %>% 
    dplyr::select(value)
  
  expected_incident_deaths <- expected_deaths[,c("date", "location", "location_name", "inc")] %>% 
    tidyr::drop_na(any_of(c("location_name", "inc"))) %>% 
    dplyr::rename(value = inc) %>% 
    dplyr::select(value)
  
  expect_equal(actual_cumulative_deaths, expected_cumulative_deaths)
  expect_equal(actual_incident_deaths, expected_incident_deaths)
  
  actual_cumulative_cases <- actual$cumulative_cases %>%
    dplyr::select(value)
  
  actual_incident_cases <- actual$incident_cases %>%
    dplyr::select(value)
  
  expected_cases <- covidData::load_jhu_data(spatial_resolution = spatial_resolutions,
                                              temporal_resolution = "daily",
                                              measure = "cases") %>% 
                    dplyr::left_join(location_names, by = "location") %>% 
                    dplyr::filter(location != "11001") 
  
  expected_cumulative_cases <- expected_cases[,c("date", "location", "location_name", "cum")] %>% 
    tidyr::drop_na(any_of(c("location_name", "cum"))) %>% 
    dplyr::rename(value = cum) %>% 
    dplyr::select(value)
  
  expected_incident_cases <- expected_cases[,c("date", "location", "location_name", "inc")] %>% 
    tidyr::drop_na(any_of(c("location_name", "inc"))) %>% 
    dplyr::rename(value = inc) %>% 
    dplyr::select(value)
  
  expect_equal(actual_cumulative_cases, expected_cumulative_cases)
  expect_equal(actual_incident_cases, expected_incident_cases)
})


test_that("preprocess_visualization_truth files has expected columns", {
  actual <- covidHubUtils::preprocess_visualization_truth(
    save_location = "."
  )
  
  actual_cumulative_deaths_column_names <- colnames(actual$cumulative_deaths)
  actual_incident_deaths_column_names <- colnames(actual$incident_deaths)
  actual_cumulative_cases_column_names <- colnames(actual$cumulative_cases)
  actual_incident_cases_column_names <- colnames(actual$incident_cases)
  
  expected_column_names <- c("location","epiweek","value")
  
  expect_equal(actual_cumulative_deaths_column_names, expected_column_names)
  expect_equal(actual_incident_deaths_column_names, expected_column_names)
  expect_equal(actual_cumulative_cases_column_names, expected_column_names)
  expect_equal(actual_incident_cases_column_names, expected_column_names)
})


test_that("preprocess_visualization_truth files has incident values greater than 0", {
  actual <- covidHubUtils::preprocess_visualization_truth(
    save_location = "."
  )
  
  # Test incident deaths
  actual_incident_deaths <- actual$incident_deaths %>%
    dplyr::select(value)
  actual_incident_deaths_ge_0 <- data.frame(actual_incident_deaths > 0)
  
  expected_incident_deaths_ge_0 <-rep(c(TRUE),times=nrow(actual_incident_deaths))
  expected_incident_deaths_ge_0 <-data.frame("value" = expected_incident_deaths_ge_0)
  
  expect_equal(actual_incident_deaths_ge_0, expected_incident_deaths_ge_0)
  
  # Test incident cases
  actual_incident_cases <- actual$incident_cases %>%
    dplyr::select(value)
  actual_incident_cases_ge_0 <- data.frame(actual_incident_cases > 0)
  
  expected_incident_cases_ge_0 <-rep(c(TRUE),times=nrow(actual_incident_cases))
  expected_incident_cases_ge_0 <-data.frame("value" = expected_incident_cases_ge_0)
  
  expect_equal(actual_incident_cases_ge_0, expected_incident_cases_ge_0)
})


test_that("preprocess_visualization_truth files has state and national location from covidData", {
  actual <- covidHubUtils::preprocess_visualization_truth(
    save_location = "."
  )
  
  # Actual locations
  actual_incident_deaths_locations <- actual$incident_deaths %>%
    dplyr::select(location)
  actual_incident_cases_locations <- actual$incident_cases %>%
    dplyr::select(location)
  actual_cumulative_deaths_locations <- actual$cumulative_deaths %>%
    dplyr::select(location)
  actual_cumulative_cases_locations <- actual$cumulative_cases %>%
    dplyr::select(location)

  # Expected locations
  # Location name
  location_names <- covidData::fips_codes[, c("location", "location_name", "abbreviation")]
  
  # Spatial resolutions
  spatial_resolutions <- c("national", "state")
  
  # Expected data
  cases_dframes <- covidData::load_jhu_data(spatial_resolution = spatial_resolutions,
                                            temporal_resolution = "weekly",
                                            measure = "cases")
  cases_dframes_locations <- dplyr::left_join(cases_dframes, location_names, by = "location") %>%
    dplyr::select(abbreviation) %>%
    dplyr::mutate(abbreviation = replace(abbreviation, abbreviation == "US", "nat")) %>% 
    dplyr::rename(location = abbreviation)
  
  deaths_dframes <- covidData::load_jhu_data(spatial_resolution = spatial_resolutions,
                                            temporal_resolution = "weekly",
                                            measure = "deaths")
  deaths_dframes_locations <- dplyr::left_join(deaths_dframes, location_names, by = "location") %>%
    dplyr::select(abbreviation) %>%
    dplyr::mutate(abbreviation = replace(abbreviation, abbreviation == "US", "nat")) %>% 
    dplyr::rename(location = abbreviation)
  
  # Test equality
  expect_equal(actual_incident_deaths_locations, deaths_dframes_locations)
  expect_equal(actual_cumulative_deaths_locations, deaths_dframes_locations)
  expect_equal(actual_incident_cases_locations, cases_dframes_locations)
  expect_equal(actual_cumulative_cases_locations, cases_dframes_locations)
})


test_that("preprocess_visualization_truth files has correct date formats", {
  actual <- covidHubUtils::preprocess_visualization_truth(
    save_location = "."
  )
  
  # Actual dates
  actual_incident_deaths_dates <- actual$incident_deaths$epiweek
  actual_incident_cases_dates <- actual$incident_cases$epiweek
  actual_cumulative_deaths_dates <- actual$cumulative_deaths$epiweek
  actual_cumulative_cases_dates <- actual$cumulative_cases$epiweek
  
  # Expected dates
  # Spatial resolutions
  spatial_resolutions <- c("national", "state")
  cases_dframes <- covidData::load_jhu_data(spatial_resolution = spatial_resolutions,
                                            temporal_resolution = "weekly",
                                            measure = "cases")
  cases_mmwr <- lapply(cases_dframes['date'], MMWRweek::MMWRweek)$date
  # shift epiweek on axis
  cases_mmwr_week <- cases_mmwr['MMWRweek'] + 1
  cases_mmwr_year <- cases_mmwr['MMWRyear']
  cases_mmwr_year[which(cases_mmwr_week>53),] <- cases_mmwr_year[which(cases_mmwr_week>53),] + 1
  cases_mmwr_week[which(cases_mmwr_week>53),] <- 1
  # format date as "{epiyear}{epiweek}". Exp: "202005"
  cases_mmwr_week <- data.frame(lapply(cases_mmwr_week, sprintf, fmt = "%02d"))
  cases_dates <- paste(cases_mmwr_year$MMWRyear, cases_mmwr_week$MMWRweek, sep = "")

  deaths_dframes <- covidData::load_jhu_data(spatial_resolution = spatial_resolutions,
                                             temporal_resolution = "weekly",
                                             measure = "deaths")
  deaths_mmwr <- lapply(deaths_dframes['date'], MMWRweek::MMWRweek)$date
  # shift epiweek on axis
  deaths_mmwr_week <- deaths_mmwr['MMWRweek'] + 1
  deaths_mmwr_year <- deaths_mmwr['MMWRyear']
  deaths_mmwr_year[which(deaths_mmwr_week>53),] <- deaths_mmwr_year[which(deaths_mmwr_week>53),] + 1
  deaths_mmwr_week[which(deaths_mmwr_week>53),] <- 1
  # format date as "{epiyear}{epiweek}". Exp: "202005"
  deaths_mmwr_week <- data.frame(lapply(deaths_mmwr_week, sprintf, fmt = "%02d"))
  deaths_dates <- paste(deaths_mmwr_year$MMWRyear, deaths_mmwr_week$MMWRweek, sep = "")
  
  # Test equality
  expect_equal(actual_incident_deaths_dates, deaths_dates)
  expect_equal(actual_cumulative_deaths_dates, deaths_dates)
  expect_equal(actual_incident_cases_dates, cases_dates)
  expect_equal(actual_cumulative_cases_dates, cases_dates)
})


test_that("preprocess_visualization_truth files has correct values from covidData", {
  actual <- covidHubUtils::preprocess_visualization_truth(
    save_location = "."
  )
  
  # Actual values
  actual_incident_deaths_values <- actual$incident_deaths$value
  actual_incident_cases_values <- actual$incident_cases$value
  actual_cumulative_deaths_values <- actual$cumulative_deaths$value
  actual_cumulative_cases_values <- actual$cumulative_cases$value
  
  # Expected values
  # Spatial resolutions
  spatial_resolutions <- c("national", "state")
  
  cases_dframes <- covidData::load_jhu_data(spatial_resolution = spatial_resolutions,
                                            temporal_resolution = "weekly",
                                            measure = "cases")
  # Threshold to 0 for incident values < 0, for visualization purpose only
  cases_dframes$inc[cases_dframes$inc <= 0] <- 0.1
  cases_dframes$cum[cases_dframes$cum == 0] <- 0.1
  
  deaths_dframes <- covidData::load_jhu_data(spatial_resolution = spatial_resolutions,
                                             temporal_resolution = "weekly",
                                             measure = "deaths")
  # Threshold to 0 for incident values < 0, for visualization purpose only
  deaths_dframes$inc[deaths_dframes$inc <= 0] <- 0.1
  deaths_dframes$cum[deaths_dframes$cum == 0] <- 0.1
  
  # Test equality
  expect_equal(actual_incident_deaths_values, deaths_dframes$inc)
  expect_equal(actual_cumulative_deaths_values, deaths_dframes$cum)
  expect_equal(actual_incident_cases_values, cases_dframes$inc)
  expect_equal(actual_cumulative_cases_values, cases_dframes$cum)
})


test_that("preprocess_hospitalization files has expected combinations of location, week", {
  # Location name
  location_names <- covidData::fips_codes[, c("location", "location_name")]
  
  actual <- covidHubUtils::preprocess_hospitalization(
                            save_location = "."
                          )
  
  actual_incident_hosp <- actual$incident_hosp %>%
                          dplyr::select(date, location)
  
  actual_cumulative_hosp <- actual$cumulative_hosp %>%
                          dplyr::select(date, location)
  
  expected <- covidData::load_healthdata_data(spatial_resolution = c("national", "state"),
                                              temporal_resolution = "daily",
                                              measure = "hospitalizations")
  
  expected_incident_hosp <- expected[,c("date", "location", "inc")] %>% 
                            dplyr::left_join(location_names, by = "location") %>% 
                            tidyr::drop_na(any_of(c("location_names", "inc"))) %>% 
                            dplyr::select(date, location)
  
  expected_cumulative_hosp <- expected[,c("date", "location", "cum")] %>% 
                              dplyr::left_join(location_names, by = "location") %>% 
                              tidyr::drop_na(any_of(c("location_names", "cum"))) %>% 
                              dplyr::select(date, location)
  
  expect_equal(actual_incident_hosp, expected_incident_hosp)
  expect_equal(actual_cumulative_hosp, expected_cumulative_hosp)
})


test_that("preprocess_hospitalization can correctly reconstruct cumulative and incident values", {
  # Location name
  location_names <- covidData::fips_codes[, c("location", "location_name")]
  
  actual <- covidHubUtils::preprocess_hospitalization(
    save_location = "."
  )
  
  actual_incident_hosp <- actual$incident_hosp %>%
    dplyr::select(value)
  
  actual_cumulative_hosp <- actual$cumulative_hosp %>%
    dplyr::select(value)
  
  expected <- covidData::load_healthdata_data(spatial_resolution = c("national", "state"),
                                              temporal_resolution = "daily",
                                              measure = "hospitalizations")
  
  expected_incident_hosp <- expected[,c("date", "location", "inc")] %>% 
    dplyr::left_join(location_names, by = "location") %>% 
    tidyr::drop_na(any_of(c("location_names", "inc"))) %>% 
    dplyr::rename(value = inc) %>% 
    dplyr::select(value)
  
  expected_cumulative_hosp <- expected[,c("date", "location", "cum")] %>% 
    dplyr::left_join(location_names, by = "location") %>% 
    tidyr::drop_na(any_of(c("location_names", "cum"))) %>% 
    dplyr::rename(value = cum) %>% 
    dplyr::select(value)
  
  expect_equal(actual_incident_hosp, expected_incident_hosp)
  expect_equal(actual_cumulative_hosp, expected_cumulative_hosp)
})

test_that("preprocess_truth_for_zoltar works: get exactly all combinations of locations, 
          targets and timezeros", {
            
            cum_deaths <- covidHubUtils::preprocess_truth_for_zoltar(
              target = "Cumulative Deaths")  %>%
              # get horizon number
              tidyr::separate(target, into=c("horizon","other"), remove = FALSE, extra = "merge") %>%
              dplyr::mutate(horizon = as.integer(horizon)) %>%
              dplyr::select(timezero, unit, horizon)
            
            inc_deaths <- covidHubUtils::preprocess_truth_for_zoltar(
              target = "Incident Deaths") %>%
              # get horizon number
              tidyr::separate(target, into=c("horizon","other"), remove = FALSE, extra = "merge") %>%
              dplyr::mutate(horizon = as.integer(horizon)) %>%
              dplyr::select(timezero, unit, horizon)
            
            # set up Zoltar connection
            zoltar_connection <- zoltr::new_connection()
            if(Sys.getenv("Z_USERNAME") == "" | Sys.getenv("Z_PASSWORD") == "") {
              zoltr::zoltar_authenticate(zoltar_connection, "zoltar_demo", "Dq65&aP0nIlG")
            } else {
              zoltr::zoltar_authenticate(zoltar_connection, 
                                         Sys.getenv("Z_USERNAME"), Sys.getenv("Z_PASSWORD"))
            }
            
            # construct Zoltar project url
            the_projects <- zoltr::projects(zoltar_connection)
            project_url <- the_projects[the_projects$name == "COVID-19 Forecasts", "url"]
            
            # get all valid timezeros from zoltar
            zoltar_timezeros<- zoltr::timezeros(zoltar_connection, project_url)$timezero_date
            
            all_valid_fips <- covidHubUtils::hub_locations %>%
              dplyr::filter(geo_type == 'state', fips != 74) %>%
              dplyr::pull(fips)
            
            # create a data frame with all combination of timezeros, units and horizons
            expected <- expand.grid(timezero = zoltar_timezeros, 
                                    unit = all_valid_fips, horizon = 1:20) %>%
              # calculate corresponding target end date
              dplyr::mutate(unit = as.character(unit), 
                            timezero = as.Date(timezero),
                            target_end_date = 
                              covidHubUtils::calc_target_week_end_date(timezero, horizon)) %>%
              # filter dates
              dplyr::filter(timezero <= Sys.Date(), 
                            target_end_date <= Sys.Date(), 
                            target_end_date >= as.Date('2020-01-25')) %>%
              dplyr::select(-target_end_date)
            
            
            expect_true(dplyr::all_equal(cum_deaths, expected))
            expect_true(dplyr::all_equal(inc_deaths, expected))
          })

test_that("preprocess_truth_for_zoltar works: truth values for all duplicated locations 
          and targets are identical for all timezeros in the same week span", {
            
            cum_deaths <- covidHubUtils::preprocess_truth_for_zoltar(
              target = "Cumulative Deaths") %>%
              # get horizon number
              tidyr::separate(target, into = c("horizon","other"), 
                              remove = FALSE, extra = "merge") %>%
              # calculate target_end_date
              dplyr::mutate(target_end_date = 
                              covidHubUtils::calc_target_week_end_date(
                                timezero, as.numeric(horizon))) %>%
              # get number of possible unique values
              # Note: conditioning on a forecast horizon, timezeros in the same week span 
              # will have the same target end date
              dplyr::group_by(target_end_date, unit, target) %>%
              dplyr::summarise(num_unique_values = length(unique(value)))
            
            inc_deaths <- covidHubUtils::preprocess_truth_for_zoltar(
              target = "Incident Deaths") %>%
              # get horizon number
              tidyr::separate(target, into = c("horizon", "other"), 
                              remove = FALSE, extra = "merge") %>%
              # calculate target_end_date
              dplyr::mutate(target_end_date = 
                              covidHubUtils::calc_target_week_end_date(
                                timezero, as.numeric(horizon))) %>%
              # get number of possible unique values
              # Note: conditioning on a forecast horizon, timezeros in the same week span 
              # will have the same target end date
              dplyr::group_by(target_end_date, unit, target) %>%
              dplyr::summarise(num_unique_values = length(unique(value)))
            
            # expect only one unique value for each combination of target_end_date, unit and target
            expect_true(all(cum_deaths$num_unique_values == 1))
            expect_true(all(inc_deaths$num_unique_values == 1))
          })

test_that("preprocess_truth_for_zoltar works: could construct cumulative values in JHU time
          series data from cumulative values in function output 
          with a date in an earlier week as issue date", {
            
            issue_date <- as.Date("2020-12-12")
            
            # load function output from configure zoltar_truth 
            # and calculate target end date 
            cum_deaths <- covidHubUtils::preprocess_truth_for_zoltar(
              target = "Cumulative Deaths",
              issue_date = issue_date) %>% 
              # get horizon number
              tidyr::separate(target, into = c("horizon", "other"),
                              remove = FALSE, extra = "merge") %>%
              # calculate target_end_date
              dplyr::mutate(target_end_date = covidHubUtils::calc_target_week_end_date(
                timezero, as.numeric(horizon))) %>%
              dplyr::select(timezero, target_end_date, unit, horizon, value)
            
            # read in JHU time series data
            # aggregate county-level counts to get cumulative deaths for each state
            expected_state_cum_deaths <- readr::read_csv(
              paste0("test-data/test-preprocess_truth_for_zoltar/",
                     as.character(issue_date), "/", as.character(issue_date), 
                     "_time_series_covid19_deaths_US.csv"))%>%
              tidyr::pivot_longer(
                matches("^\\d{1,2}\\/\\d{1,2}\\/\\d{2,4}$"),
                names_to = "date",
                values_to = "cum") %>%
              dplyr::mutate(
                date = as.character(lubridate::mdy(date))) %>%
              dplyr::group_by(Province_State, date) %>%
              dplyr::summarize(cum = sum(cum)) %>%
              dplyr::ungroup() %>%
              dplyr::left_join(covidHubUtils::hub_locations, 
                               by = c("Province_State" = "location_name")) %>%
              dplyr::filter(geo_type == 'state', fips != 74) %>%
              dplyr::select(-population, -geo_type, -geo_value, -abbreviation)
            
            # aggregate state-level counts to get national cumulative deaths
            expected_national_cum_deaths <- expected_state_cum_deaths %>%
              dplyr::group_by(date) %>%
              dplyr::summarize(cum = sum(cum)) %>%
              dplyr::ungroup() %>%
              dplyr::mutate(fips = 'US')
            
            # merge JHU cumulative counts with function output and calculate count difference
            cum_deaths <- cum_deaths %>%
              dplyr::left_join(
                dplyr::bind_rows(expected_state_cum_deaths, expected_national_cum_deaths), 
                by = c("unit" = "fips", "target_end_date" = "date")) %>%
              #Note from covidData: we are off by a total of 3 deaths attributed to Diamond Princess.
              dplyr::mutate(diff = value - cum)
            
            # the only possible values for difference should be  0, 1, 3
            expect_true(all(unique(cum_deaths$diff) == c(0,1,3)))
            # differences should only occur at national level
            expect_true(all(cum_deaths[cum_deaths$diff==3, ]$unit == 'US'))
            expect_true(all(cum_deaths[cum_deaths$diff==1, ]$unit == 'US'))
          })

test_that("preprocess_truth_for_zoltar works: could construct cumulative values in JHU time
          series data from cumulative values in function output 
          with a date in a later week as issue date", {
            
            issue_date <- as.Date("2020-12-14")
            
            # load function output from configure zoltar_truth 
            # and calculate target end date 
            cum_deaths <- covidHubUtils::preprocess_truth_for_zoltar(
              target = "Cumulative Deaths",
              issue_date = issue_date) %>% 
              # get horizon number
              tidyr::separate(target, into = c("horizon", "other"),
                              remove = FALSE, extra = "merge") %>%
              # calculate target_end_date
              dplyr::mutate(target_end_date = covidHubUtils::calc_target_week_end_date(
                timezero, as.numeric(horizon))) %>%
              dplyr::select(timezero, target_end_date, unit, horizon, value)
            
            # read in JHU time series data
            # aggregate county-level counts to get cumulative deaths for each state
            expected_state_cum_deaths <- readr::read_csv(
              paste0("test-data/test-preprocess_truth_for_zoltar/",
                     as.character(issue_date), "/", as.character(issue_date), 
                     "_time_series_covid19_deaths_US.csv"))%>%
              tidyr::pivot_longer(
                matches("^\\d{1,2}\\/\\d{1,2}\\/\\d{2,4}$"),
                names_to = "date",
                values_to = "cum") %>%
              dplyr::mutate(
                date = as.character(lubridate::mdy(date))) %>%
              dplyr::group_by(Province_State, date) %>%
              dplyr::summarize(cum = sum(cum)) %>%
              dplyr::ungroup() %>%
              dplyr::left_join(covidHubUtils::hub_locations, 
                               by = c("Province_State" = "location_name")) %>%
              dplyr::filter(geo_type == 'state', fips != 74) %>%
              dplyr::select(-population, -geo_type, -geo_value, -abbreviation)
            
            # aggregate state-level counts to get national cumulative deaths
            expected_national_cum_deaths <- expected_state_cum_deaths %>%
              dplyr::group_by(date) %>%
              dplyr::summarize(cum = sum(cum)) %>%
              dplyr::ungroup() %>%
              dplyr::mutate(fips = 'US')
            
            # merge JHU cumulative counts with function output and calculate count difference
            cum_deaths <- cum_deaths %>%
              dplyr::left_join(
                dplyr::bind_rows(expected_state_cum_deaths, expected_national_cum_deaths), 
                by = c("unit" = "fips", "target_end_date" = "date")) %>%
              #Note from covidData: we are off by a total of 3 deaths attributed to Diamond Princess.
              dplyr::mutate(diff = value - cum)
            
            # the only possible values for difference should be  0, 1, 3
            expect_true(all(unique(cum_deaths$diff) == c(0,1,3)))
            # differences should only occur at national level
            expect_true(all(cum_deaths[cum_deaths$diff==3,]$unit == 'US'))
            expect_true(all(cum_deaths[cum_deaths$diff==1,]$unit == 'US'))
            
          })


test_that("preprocess_truth_for_zoltar works: could construct cumulative values in JHU time
          series data from incident values in function output 
          with a date in an earlier week as issue date", {
            
            issue_date<- as.Date("2020-12-12")
            
            inc_to_cum_deaths <- covidHubUtils::preprocess_truth_for_zoltar(
              target = "Incident Deaths",
              issue_date = issue_date) %>% 
              # get horizon number
              tidyr::separate(target, into = c("horizon","other"),
                              remove = FALSE, extra = "merge") %>%
              # calculate target_end_date
              dplyr::mutate(target_end_date = covidHubUtils::calc_target_week_end_date(
                timezero, as.numeric(horizon))) %>%
              dplyr::select(target_end_date, unit, value) %>%
              # rows with timezero in the same week span are duplicate
              dplyr::distinct(target_end_date, unit, .keep_all = TRUE) %>%
              dplyr::mutate(inc = value) %>%
              # calculate cumulative counts from incident counts
              dplyr::group_by(unit) %>%
              dplyr::mutate(tentative_cum = cumsum(inc)) %>%
              dplyr::ungroup() 
            
            # read in JHU time series data
            # aggregate county-level counts to get cumulative deaths for each state
            expected_state_cum_deaths <- readr::read_csv(
              paste0("test-data/test-preprocess_truth_for_zoltar/",
                     as.character(issue_date), "/", as.character(issue_date), 
                     "_time_series_covid19_deaths_US.csv"))%>%
              tidyr::pivot_longer(
                matches("^\\d{1,2}\\/\\d{1,2}\\/\\d{2,4}$"),
                names_to = "date",
                values_to = "cum") %>%
              dplyr::mutate(
                date = as.character(lubridate::mdy(date))) %>%
              dplyr::group_by(Province_State, date) %>%
              dplyr::summarize(cum = sum(cum)) %>%
              dplyr::ungroup() %>%
              dplyr::left_join(covidHubUtils::hub_locations, 
                               by = c("Province_State" = "location_name")) %>%
              dplyr::filter(geo_type == 'state', fips != 74) %>%
              dplyr::select(-population, -geo_type, -geo_value, -abbreviation)
            
            # aggregate state-level counts to get national cumulative deaths
            expected_national_cum_deaths <- expected_state_cum_deaths %>%
              dplyr::group_by(date) %>%
              dplyr::summarize(cum = sum(cum)) %>%
              dplyr::ungroup() %>%
              dplyr::mutate(fips = 'US')
            
            
            inc_to_cum_deaths <- inc_to_cum_deaths %>%
              dplyr::left_join(
                dplyr::bind_rows(expected_state_cum_deaths, expected_national_cum_deaths), 
                by = c("unit" = "fips", "target_end_date" = "date")) %>%
              #Note from covidData: we are off by a total of 3 deaths attributed to Diamond Princess.
              dplyr::mutate(diff = tentative_cum - cum)
            
            # the only possible values for difference should be  0, 1, 3
            expect_true(all(unique(inc_to_cum_deaths$diff) == c(0,1,3)))
            # differences should only occur at national level
            expect_true(all(inc_to_cum_deaths[inc_to_cum_deaths$diff==3, ]$unit == 'US'))
            expect_true(all(inc_to_cum_deaths[inc_to_cum_deaths$diff==1, ]$unit == 'US'))
          })


test_that("preprocess_truth_for_zoltar works: could construct cumulative values in JHU time
          series data from incident values in function output 
          with a date in an later week as issue date", {
            
            issue_date<- as.Date("2020-12-14")
            
            inc_to_cum_deaths <- covidHubUtils::preprocess_truth_for_zoltar(
              target = "Incident Deaths",
              issue_date = issue_date) %>% 
              # get horizon number
              tidyr::separate(target, into = c("horizon","other"),
                              remove = FALSE, extra = "merge") %>%
              # calculate target_end_date
              dplyr::mutate(target_end_date = covidHubUtils::calc_target_week_end_date(
                timezero, as.numeric(horizon))) %>%
              dplyr::select(target_end_date, unit, value) %>%
              # rows with timezero in the same week span are duplicate
              dplyr::distinct(target_end_date, unit, .keep_all = TRUE) %>%
              dplyr::mutate(inc = value) %>%
              # calculate cumulative counts from incident counts
              dplyr::group_by(unit) %>%
              dplyr::mutate(tentative_cum = cumsum(inc)) %>%
              dplyr::ungroup() 
            
            # read in JHU time series data
            # aggregate county-level counts to get cumulative deaths for each state
            expected_state_cum_deaths <- readr::read_csv(
              paste0("test-data/test-preprocess_truth_for_zoltar/",
                     as.character(issue_date), "/", as.character(issue_date), 
                     "_time_series_covid19_deaths_US.csv"))%>%
              tidyr::pivot_longer(
                matches("^\\d{1,2}\\/\\d{1,2}\\/\\d{2,4}$"),
                names_to = "date",
                values_to = "cum"
              ) %>%
              dplyr::mutate(
                date = as.character(lubridate::mdy(date)),
              ) %>%
              dplyr::group_by(Province_State, date) %>%
              dplyr::summarize(cum = sum(cum)) %>%
              dplyr::ungroup() %>%
              dplyr::left_join(covidHubUtils::hub_locations, 
                               by = c("Province_State" = "location_name")) %>%
              dplyr::filter(geo_type == 'state', fips!=74) %>%
              dplyr::select(-population, -geo_type, -geo_value, -abbreviation)
            
            # aggregate state-level counts to get national cumulative deaths
            expected_national_cum_deaths <- expected_state_cum_deaths %>%
              dplyr::group_by(date) %>%
              dplyr::summarize(cum = sum(cum)) %>%
              dplyr::ungroup() %>%
              dplyr::mutate(fips = 'US')
            
            
            inc_to_cum_deaths <- inc_to_cum_deaths %>%
              dplyr::left_join(
                dplyr::bind_rows(expected_state_cum_deaths, expected_national_cum_deaths), 
                by = c("unit" = "fips", "target_end_date" = "date")) %>%
              #Note from covidData: we are off by a total of 3 deaths attributed to Diamond Princess.
              dplyr::mutate(diff = tentative_cum - cum)
            
            # the only possible values for difference should be  0, 1, 3
            expect_true(all(unique(inc_to_cum_deaths$diff) == c(0,1,3)))
            # differences should only occur at national level
            expect_true(all(inc_to_cum_deaths[inc_to_cum_deaths$diff==3,]$unit == 'US'))
            expect_true(all(inc_to_cum_deaths[inc_to_cum_deaths$diff==1,]$unit == 'US'))
            
          })


