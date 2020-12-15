context("configure_zoltar_truth")
library(covidHubUtils)
library(covidData)
library(dplyr)

test_that("configure_zoltar_truth works: get exactly all combinations of locations, 
          target and week", {
  cum_deaths <- covidHubUtils::configure_zoltar_truth(
    target = "Cumulative Deaths"
  )
  
  inc_deaths <- covidHubUtils::configure_zoltar_truth(
    target = "Incident Deaths"
  )
  
  cum_deaths <- cum_deaths %>%
    tidyr::separate(target, into=c("horizon","other"), remove = FALSE, extra = "merge") %>%
    dplyr::mutate(horizon = as.integer(horizon)) %>%
    dplyr::select(timezero, unit, horizon)
  
  inc_deaths <- inc_deaths %>%
    tidyr::separate(target, into=c("horizon","other"), remove = FALSE, extra = "merge") %>%
    dplyr::mutate(horizon = as.integer(horizon)) %>%
    dplyr::select(timezero, unit, horizon)
  
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
  
  all_valid_fips <- covidHubUtils::hub_locations %>%
    dplyr::filter(geo_type == 'state', fips!=74) %>%
    dplyr::pull(fips)
  
  expected <- expand.grid(timezero = zoltar_timezeros, 
                          unit = all_valid_fips, horizon = 1:20) %>%
    dplyr::mutate(unit = as.character(unit), 
                  timezero = as.Date(timezero),
                  target_end_date = 
                    covidHubUtils::calc_target_week_end_date(
                      timezero, horizon)) %>%
    dplyr::filter(timezero <= Sys.Date(), 
                  target_end_date <= Sys.Date(), 
                  target_end_date >= as.Date('2020-01-25')) %>%
    dplyr::select(-target_end_date)

  
  expect_true(dplyr::all_equal(cum_deaths, expected))
  expect_true(dplyr::all_equal(inc_deaths, expected))
})

test_that("configure_zoltar_truth works: truth values for all duplicated locations 
          and targets are identical in the same week span", {
  cum_deaths <- covidHubUtils::configure_zoltar_truth(
    target = "Cumulative Deaths"
  )
  
  cum_deaths <- cum_deaths %>%
    tidyr::separate(target, into = c("horizon","other"), 
                    remove = FALSE, extra = "merge") %>%
    dplyr::mutate(target_end_date = 
                    covidHubUtils::calc_target_week_end_date(
                      timezero, as.numeric(horizon))) %>%
    dplyr::group_by(target_end_date, unit, target) %>%
    dplyr::summarise(num_unique_values = length(unique(value)))
  
  inc_deaths <- covidHubUtils::configure_zoltar_truth(
    target = "Incident Deaths"
  )
  
  inc_deaths <- inc_deaths %>%
    tidyr::separate(target, into = c("horizon","other"), 
                    remove = FALSE, extra = "merge") %>%
    dplyr::mutate(target_end_date = 
                    covidHubUtils::calc_target_week_end_date(
                      timezero, as.numeric(horizon))) %>%
    dplyr::group_by(target_end_date, unit, target) %>%
    dplyr::summarise(num_unique_values = length(unique(value)))
  
  
  
  expect_true(all(cum_deaths$num_unique_values == 1))
  expect_true(all(inc_deaths$num_unique_values == 1))
})


