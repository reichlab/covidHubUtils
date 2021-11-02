#' Load truth data under multiple target variables
#' from multiple truth sources
#' using files in `reichlab/covid19-forecast-hub`.
#'
#' `"inc hosp"` is only available from `"HeatlthData"` and this function is not loading
#' data for other target variables from `"HealthData"`.
#'
#' When loading data for multiple `target_variables`, `temporal_resolution` will be applied
#' to all target variables but `"inc hosp"`. In that case, this function will return
#' daily incident hospitalization counts along with other data.
#'
#' Weekly temporal resolution will be applied to `"inc hosp"` if the user specifies `"inc hosp"`
#' as the only `target_variable`.
#'
#' When loading weekly data, if there are not enough observations for a week, the corresponding
#' weekly count would be `NA` in resulting data frame.
#'
#' @param truth_source character vector specifying where the truths will
#' be loaded from: currently support `"JHU"`, `"USAFacts"`, `"NYTimes"`, `"HealthData`" and `"ECDC"`.
#' If `NULL`, default for US hub is `c("JHU", "HealthData")`.
#' If `NULL`, default for ECDC hub is `c("JHU")`.
#' @param target_variable string specifying target type It should be one or more of
#' `"cum death"`, `"inc case"`, `"inc death"`, `"inc hosp"`.
#' If `NULL`, default for US hub is `c("inc case", "inc death", "inc hosp")`.
#' If `NULL`, default for ECDC hub is `c("inc case", "inc death")`.
#' @param as_of character vector of "as of" dates to use for querying truths in
#' format 'yyyy-mm-dd'. For each spatial unit and temporal reporting unit, the last
#' available data with an issue date on or before the given `as_of` date are returned.
#' This is only available for `covidData` and `zoltar`now.
#' @param locations vector of valid location code.
#' If `NULL`, default to all locations with available forecasts.
#' US hub is using FIPS code and ECDC hub is using country name abbreviation.
#' @param data_location character specifying the location of truth data.
#' Currently only supports `"local_hub_repo"`, `"remote_hub_repo"`, `"covidData"` and `"zoltar"`.
#' If `NULL`, default to `"remote_hub_repo"`.
#' @param truth_end_date date to include the last available truth point in 'yyyy-mm-dd' format.
#' If `NULL`,default to system date.
#' @param temporal_resolution character specifying temporal resolution
#' to include: currently support `"weekly"` and `"daily"`.
#' If `NULL`, default to `"weekly"` for cases and deaths, `"daily"` for hospitalizations.
#' Weekly `temporal_resolution` will not be applied to `"inc hosp"` when
#' multiple target variables are specified.
#' `"ECDC"` truth data is weekly by default. Daily level data is not available.
#' @param local_repo_path path to local clone of the `reichlab/covid19-forecast-hub`
#' repository. Only used when data_location is `"local_hub_repo"`
#' @param hub character, which hub to use. Default is `"US"`, other option is
#' `"ECDC"`. `"ECDC"` is not available when `data_location = "zoltar"`.
#'
#' @return data.frame with columns `model`, `target_variable`, `target_end_date`,
#' `location`, `value`, `location_name`, `population`, `geo_type`, `geo_value`, `abbreviation`
#' and `full_location_name`
#'
#' @examples
#' library(covidHubUtils)
#'
#' # load for US
#' load_truth(
#'   truth_source = c("JHU", "HealthData"),
#'   target_variable = c("inc case", "inc death", "inc hosp")
#' )
#'
#' # load for ECDC
#' load_truth(
#'   truth_source = c("JHU"),
#'   target_variable = c("inc case", "inc death"),
#'   hub = "ECDC"
#' )
#' @export
load_truth <- function(truth_source = NULL,
                       target_variable = NULL,
                       as_of = NULL,
                       truth_end_date = NULL,
                       temporal_resolution = NULL,
                       locations = NULL,
                       data_location = NULL,
                       local_repo_path = NULL,
                       hub = c("US", "ECDC")) {


  # preparations and validation checks that are different for US and ECDC hub
  if (hub[1] == "US") {
    if (is.null(target_variable)) {
      target_variable <- c("inc case", "inc death", "inc hosp")
    } else {
      # validate target variable
      target_variable <- match.arg(target_variable,
        choices = c(
          "cum death",
          "inc case",
          "inc death",
          "inc hosp"
        ),
        several.ok = TRUE
      )
    }

    # NEED TO CHANGE WHEN THERE IS A TARGET VAR
    if (is.null(truth_source)) {
      truth_source <- c("JHU", "HealthData")
    } else {
      # validate truth source
      truth_source <- match.arg(truth_source,
        choices = c("JHU", "USAFacts", "NYTimes", "HealthData"),
        several.ok = TRUE
      )
    }

    # extra checks for truth source if target is inc hosp
    if ("inc hosp" %in% target_variable) {
      if (!"HealthData" %in% truth_source) {
        warning("Warning in load_truth: Incident hopsitalization truth data is only available from HealthData.gov now.
              Will be loading data from HealthData instead.")
        truth_source <- c(truth_source, "HealthData")
      }
    } else {
      if ("HealthData" %in% truth_source) {
        stop("Error in load_truth: This function does not support selected target_variable from HealthData.")
      }
    }

    # get list of all valid locations and codes
    valid_locations <- covidHubUtils::hub_locations
    valid_location_codes <- covidHubUtils::hub_locations$fips

    # store path of remote repo
    remote_repo_path <- "https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master"
  } else if (hub[1] == "ECDC") {
    if (is.null(target_variable)) {
      target_variable <- c("inc case", "inc death")
    } else {
      # validate target variable
      target_variable <- match.arg(target_variable,
        choices = c(
          "inc case",
          "inc death"
        ),
        several.ok = TRUE
      )
    }

    if (is.null(truth_source)) {
      truth_source <- c("JHU")
    } else {
      # validate truth source
      truth_source <- match.arg(truth_source,
        choices = c("JHU", "ECDC"),
        several.ok = TRUE
      )
    }

    # get list of all valid locations and codes
    valid_locations <- covidHubUtils::hub_locations_ecdc
    valid_location_codes <- covidHubUtils::hub_locations_ecdc$location

    # store path of remote repo
    remote_repo_path <- "https://raw.githubusercontent.com/epiforecasts/covid19-forecast-hub-europe/main"
  }

  # validate truth end date
  if (is.null(truth_end_date)) {
    truth_end_date <- Sys.Date()
  }

  truth_end_date <- tryCatch(
    {
      as.Date(truth_end_date)
    },
    error = function(err) {
      stop("Error in load_truth: Please provide a valid date object or
    string in format YYYY-MM-DD in truth_end_date")
    }
  )

  # validate temporal resolution
  if (is.null(temporal_resolution)) {
    # only relevant for US hub currently where inc hosp is available
    if (length(target_variable) == 1) {
      if (target_variable == "inc hosp") {
        temporal_resolution <- "daily"
      } else {
        temporal_resolution <- "weekly"
      }
    } else {
      temporal_resolution <- "weekly"
    }
  } else {
    temporal_resolution <- match.arg(temporal_resolution,
      choices = c("daily", "weekly"),
      several.ok = FALSE
    )
    if ("ECDC" %in% truth_source & temporal_resolution == "daily") {
      warning("Warning in load_truth: ECDC truth data will be weekly.")
    }
  }

  # validate data location
  if (!is.null(data_location)) {
    data_location <- match.arg(data_location,
      choices = c(
        "remote_hub_repo",
        "local_hub_repo",
        "covidData"
      ),
      several.ok = FALSE
    )
  } else {
    data_location <- "remote_hub_repo"
  }

  # validate truth source for covidData
  if (data_location == "covidData") {
    if (hub[1] == "US") {
      if ("USAFacts" %in% truth_source || "NYTimes" %in% truth_source) {
        stop("Error in load_truth: The truth source you selected is not supported in for US data in covidData.")
      }
    } else if (hub[1] == "ECDC") {
      if ("ECDC" %in% truth_source) {
        stop("Error in load_truth: The truth source you selected is not supported in for ECDC data in covidData.")
      }
    }
  }


  # validate locations
  if (!is.null(locations)) {
    locations <- match.arg(locations,
      choices = valid_location_codes,
      several.ok = TRUE
    )
  }

  # create file path and file name based on data_location
  if (data_location == "remote_hub_repo") {
    repo_path <- remote_repo_path
  } else if (data_location == "local_hub_repo") {
    if (is.na(local_repo_path) | is.null(local_repo_path)) {
      stop("Error in local_repo_path : Please provide a valid local_repo_path.")
    } else {
      repo_path <- local_repo_path
    }
  }

  # get all combinations of elements in tsruth_source and target_variable
  all_combinations <- tidyr::crossing(truth_source, target_variable)

  # load truth data for each combination of truth_source and target_variable
  truth <- purrr::map2_dfr(
    all_combinations$truth_source, all_combinations$target_variable,
    function(source, target) {
      if ((source == "HealthData" & target == "inc hosp") |
        (source != "HealthData" & target != "inc hosp")) {
        if (data_location == "covidData") {
          if (target == "inc hosp") {
            temporal_resolution <- "daily"
          }
          if (is.null(locations)) {
            selected_locations <- valid_location_codes
          } else {
            selected_locations <- locations
          }

          data <- load_from_coviddata(
            target_variable = target,
            truth_source = source,
            locations = selected_locations,
            as_of = as_of,
            temporal_resolution = temporal_resolution,
            truth_end_date = truth_end_date,
            hub = hub
          )
          return(data)
        } else {
          data <- load_from_hub_repo(
            target_variable = target,
            truth_source = source,
            repo_path = repo_path,
            temporal_resolution = temporal_resolution,
            truth_end_date = truth_end_date,
            data_location = data_location,
            hub = hub
          )
          return(data)
        }
      }
    }
  ) %>%
    dplyr::select(model, target_variable, target_end_date, location, value)

  # filter to only include specified locations
  if (!is.null(locations)) {
    truth <- dplyr::filter(truth, location %in% locations)
    if (nrow(truth) == 0) {
      warning("Warning in load_truth: Truth for selected locations are not available.\n Please check your parameters.")
      if ("inc hosp" %in% target_variable) {
        warning("Warning in load_truth: Only national and state level of incident hospitalization data is available.")
      }
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
  return(truth)
}



#' Construct the file path to a truth file
#'
#' @param source character vector specifying where the truths will
#' be loaded from: currently support `"JHU"`, `"USAFacts"`, `"NYTimes"`, `"HealthData`" and `"ECDC"`.
#' @param repo_path path to local clone or remote of the corresponding prediction hub repository.
#' @param target_variable string specifying target type It should be one or more of
#' `"cum death"`, `"inc case"`, `"inc death"`, `"inc hosp"`.
#' @param data_location character specifying the location of truth data.
#' Currently only supports `"local_hub_repo"` and `"remote_hub_repo"`.
#' @param hub character, which hub to use. Default is `"US"`, other option is
#' `"ECDC"`
#'
#' @return character of file path

get_truth_path <- function(source,
                           repo_path,
                           target_variable,
                           data_location,
                           hub = c("US", "ECDC")) {
  # generate full target variable (this works for both hubs as previous
  # checks already made sure only applicable target_variables are used)
  if (target_variable == "cum death") {
    full_target_variable <- "Cumulative Deaths"
  } else if (target_variable == "inc case") {
    full_target_variable <- "Incident Cases"
  } else if (target_variable == "inc death") {
    full_target_variable <- "Incident Deaths"
  } else if (target_variable == "inc hosp") {
    full_target_variable <- "Incident Hospitalizations"
  }

  if (data_location == "remote_hub_repo") {
    file_name <- paste0(gsub(" ", "%20", full_target_variable), ".csv")
  } else if (data_location == "local_hub_repo") {
    file_name <- paste0(full_target_variable, ".csv")
  }

  if (hub[1] == "US") {
    truth_folder_path <- ifelse((tolower(source) == "jhu" || tolower(source) == "healthdata"),
      "/data-truth/truth-",
      paste0(
        "/data-truth/", tolower(source), "/truth_",
        tolower(source), "-"
      )
    )
    file_path <- paste0(repo_path, truth_folder_path, file_name)
  } else if (hub[1] == "ECDC") {
    truth_folder_path <- paste0("/data-truth/", toupper(source), "/truth_", toupper(source), "-")
    file_path <- paste0(repo_path, truth_folder_path, file_name)
  }
  return(file_path)
}

#' load truth data from covidData
#'
#' @param target_variable string specifying target type It should be one or more of
#' `"cum death"`, `"inc case"`, `"inc death"`, `"inc hosp"`.
#' @param locations vector of valid location code.
#' US hub is using FIPS code and ECDC hub is using country name abbreviation.
#' @param as_of character vector of "as of" dates to use for querying truths in
#' format 'yyyy-mm-dd'. For each spatial unit and temporal reporting unit, the last
#' available data with an issue date on or before the given `as_of` date are returned.
#' @param temporal_resolution string specifying temporal resolution
#' to include: one of `'daily'` or `'weekly'`
#' @param truth_end_date date to include the last available truth point in 'yyyy-mm-dd' format.
#' @param hub character, which hub to use. Default is `"US"`, other option is
#' `"ECDC"`
#'
#' @return data.frame with columns `model`, `location`, `target_end_date`, `target_variable` 
#' and `value`
#'
load_from_coviddata <- function(target_variable,
                                truth_source,
                                locations = NULL,
                                as_of = NULL,
                                temporal_resolution,
                                truth_end_date,
                                hub = c("US", "ECDC")) {

  # create geography variable
  if (hub[1] == "ECDC") {
    geography <- "global"
  } else {
    geography <- hub
  }

  inc_cum <- unlist(strsplit(target_variable, " "))[1]
  # create measure variable
  measure <- unlist(strsplit(target_variable, " "))[2]
  if (measure == "death") {
    measure <- "deaths"
  } else if (measure == "case") {
    measure <- "cases"
  } else if (measure == "hosp") {
    measure <- "hospitalizations"
  }

  truth_data <- covidData::load_data(
    location_code = locations,
    as_of = as_of,
    temporal_resolution = temporal_resolution,
    geography = geography,
    measure = measure,
    source = tolower(truth_source)
  ) %>%
    dplyr::rename(target_end_date = date) %>%
    dplyr::mutate(model = paste0("Observed Data (", truth_source, ")"), target_variable = target_variable) %>%
    # take out DC county and filter target_end_date
    dplyr::filter(
      location != "11001",
      target_end_date >= as.Date("2020-01-25"),
      target_end_date <= truth_end_date
    )

  if (inc_cum == "inc") {
    truth_data <- truth_data %>%
      dplyr::rename(value = inc) %>%
      dplyr::select(-cum)
  } else if (inc_cum == "cum") {
    truth_data <- truth_data %>%
      dplyr::rename(value = cum) %>%
      dplyr::select(-inc)
  }
  # drop na
  if (target_variable == "inc hosp") {
    truth_data <- truth_data %>%
      tidyr::drop_na()
  }
  return(truth_data)
}

#' load truth data from covidData
#'
#' @param target_variable string specifying target type It should be one or more of
#' `"cum death"`, `"inc case"`, `"inc death"`, `"inc hosp"`.
#' @param truth_source character vector specifying where the truths will
#' be loaded from: currently support `"JHU"`, `"USAFacts"`, `"NYTimes"`, `"HealthData`" and `"ECDC"`.
#' @param repo_path path to local clone or remote of the corresponding prediction hub repository.
#' @param temporal_resolution string specifying temporal resolution
#' to include: one of `'daily'` or `'weekly'`
#' @param truth_end_date date to include the last available truth point in 'yyyy-mm-dd' format.
#' @param data_location character specifying the location of truth data.
#' Currently only supports `"local_hub_repo"` or `"remote_hub_repo"`.
#' @param hub character, which hub to use. Default is `"US"`, other option is
#' `"ECDC"`
#' @return a data frame with columns `model`, `location`, `target_end_date`, `target_variable` 
#' and `value`
#'
load_from_hub_repo <- function(target_variable,
                               truth_source,
                               repo_path,
                               temporal_resolution,
                               truth_end_date,
                               data_location,
                               hub = c("US", "ECDC")) {

  # construct file path and read file from path
  file_path <- get_truth_path(
    source = truth_source,
    repo_path = repo_path,
    target_variable = target_variable,
    data_location = data_location,
    hub = hub
  )
  # load data from file path
  truth_data <- readr::read_csv(file_path)

  if (truth_source == "ECDC") {
    truth_data <- truth_data %>% dplyr::rename(date = week_start)
  }

  truth_data <- truth_data %>%
    # add inc_cum and death_case columns and rename date column
    dplyr::mutate(
      model = paste0("Observed Data (", truth_source, ")"),
      target_variable = target_variable,
      date = as.Date(date)
    ) %>%
    dplyr::rename(target_end_date = date) %>%
    dplyr::filter(target_end_date >= as.Date("2020-01-25"))

  # optional aggregation step based on temporal resolution
  # only loading daily incident hospitalization truth data
  if ((target_variable != "inc hosp" & temporal_resolution == "weekly") &
    # ECDC is weekly data by default. No need to aggregate.
    (truth_source != "ECDC")) {
    if (unlist(strsplit(target_variable, " "))[1] == "cum") {
      # only keep weekly data
      truth_data <- dplyr::filter(
        truth_data,
        target_end_date %in% seq.Date(
          as.Date("2020-01-25"),
          to = truth_end_date,
          by = "1 week"
        )
      )
    } else {
      # aggregate daily inc counts to weekly counts
      truth_data <- aggregate_to_weekly(truth_data)
    }
  }
  return(truth_data)
}


#' load truth data from zoltar server
#'
#' @param target_variable string specifying target type It should be one or more of
#' `"cum death"` and `"inc death"`.
#' @param locations vector of valid location code.
#' If `NULL`, default to all locations with available forecasts.
#' US hub is using FIPS code and ECDC hub is using country name abbreviation.
#' @param as_of character vector of "as of" dates to use for querying truths in
#' format 'yyyy-mm-dd'. For each spatial unit and temporal reporting unit, the last
#' available data with an issue date on or before the given `as_of` date are returned.
#' @param hub character, which hub to use. Default is `"US"`. 
#' `"ECDC"` is not available now.
#' @return data.frame with columns `model`, `location`, `target_end_date`, `target_variable` 
#' and `value`
#'
load_from_zoltar <- function(target_variable,
                             locations,
                             as_of,
                             hub = c("US", "ECDC")) {
  
  # set up Zoltar connection
  zoltar_connection <- setup_zoltar_connection(staging = FALSE)
  
  # construct Zoltar project url
  project_url <- get_zoltar_project_url(
    hub = hub,
    zoltar_connection = zoltar_connection
  )
  
  targets <- c()
  if (hub[1] == "US"){
    if ("inc death" %in% target_variable) {
      targets <- c(targets, paste(1:20, "wk ahead inc death"))
    } else if ("cum death" %in% target_variable) {
      targets <- c(targets, paste(1:20, "wk ahead cum death"))
    } else {
      warning("Warning in load_from_zoltar: Some of the specified target variables are not available in Zoltar.")
    }
  } else if (hub[1] == "ECDC") {
    stop("Error in load_from_zoltar: Truth data for ECDC hub is not available on zoltar.")
    
  }
  
  truth <- zoltr::do_zoltar_query(zoltar_connection = zoltar_connection, 
                                  project_url = project_url, 
                                  units = locations, 
                                  query_type = "truth",
                                  as_of = date_to_datetime(as_of, hub))
  
  # reformat 
  truth <- truth %>%
    dplyr::mutate(model = "Observed Data (Zoltar)") %>%
    # keep only required columns
    dplyr::select(model, timezero, unit, target, value) %>%
    dplyr::rename(location = unit, 
                  forecast_date = timezero) %>%
    # create horizon and target_end_date columns
    tidyr::separate(target, into=c("horizon","temporal_resolution","ahead","target_variable"),
                    remove = FALSE, extra = "merge") %>%
    dplyr::mutate(target_end_date = as.Date(
      calc_target_end_date(forecast_date, as.numeric(horizon), temporal_resolution)
    )) %>%
    # drop duplicates
    # for each target_end_date, keep the entry with the most recent forecast_date
    dplyr::select(model, forecast_date, target_variable, target_end_date, location, value) %>%
    dplyr::group_by(model, target_end_date, target_variable, location) %>%
    dplyr::arrange(value, desc(forecast_date)) %>%
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>%
    dplyr::select(-forecast_date)
  
  
  return (truth)
    
}
  
