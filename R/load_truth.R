#' Load truth data under multiple target variables from multiple truth sources
#'
#' @description
#' By default, for the US hub, the resulting data.frame contains data for weekly incident cases (JHU),
#' weekly incident deaths (JHU) and daily incident hospitalization (HealthData) at all county, state and
#' national level. For the ECDC hub, the default resulting data.frame contains data for weekly incident cases (JHU),
#' weekly incident deaths (JHU) and daily incident hospitalization (ECDC) for all European countries.
#' For the FluSight hub, the default resulting data.frame contains data for weekly incident hospitalization (HealthData)
#' for all US locations.
#'
#' @details
#' \itemize{
#'   \item`"inc hosp"` is only available from `"HealthData"`, `"ECDC"` and `"OWID"`.`"inc flu hosp"` is only available from `"HealthData"`.
#'   
#'   \item This function is not loading data for other target variables from `"HealthData"`.
#'
#'   \item When loading data for multiple target variables for the US hub, `temporal_resolution` will be applied
#' to all target variables but `"inc hosp"` and `"inc flu hosp"`. In that case, the function will return
#' daily incident COVID hospitalization counts and weekly incident Influenza hospitalization.
#'
#'   \item For the US hub, weekly temporal resolution will be applied to `"inc hosp"` if the user specifies `"inc hosp"`
#' as the only `target_variable`. On the other hand, `temporal_resolution` will
#' be applied to `"inc hosp"` in all cases for the ECDC hub.
#'
#'   \item When aggregating daily data, if there are not enough observations for a week, the corresponding
#' weekly count would be `NA` in resulting data frame.
#'
#'   \item `as_of` is only supported when `data_location = "covidData"`. Otherwise, this function
#' will return a warning.
#' }
#'
#' @param truth_source character vector specifying where the truths will
#' be loaded from: currently support `"JHU"`, `"NYTimes"`, `"HealthData`", `"ECDC"` and `"OWID"`
#' If `NULL`, default for US hub is `c("JHU", "HealthData")`.
#' If `NULL`, default for ECDC hub is `c("OWID")`.
#' If `NULL`, default for FluSight hub is `c("HealthData")`.
#' @param target_variable string specifying target type It should be one or more of
#' `"cum death"`, `"inc case"`, `"inc death"`, `"inc hosp"`.
#' If `NULL`, default for US hub is `c("inc case", "inc death", "inc hosp")`.
#' If `NULL`, default for ECDC hub is `c("inc hosp")`.
#' If `NULL`, default for FluSight hub is `c("inc flu hosp")`.
#' @param as_of character vector of "as of" dates to use for querying truths in
#' format 'yyyy-mm-dd'. For each spatial unit and temporal reporting unit, the last
#' available data with an issue date on or before the given `as_of` date are returned.
#' This is only available for `covidData` now.
#' @param locations a vector of strings of fips code or CBSA codes or location names,
#' such as "Hampshire County, MA", "Alabama", "United Kingdom".
#' A US county location names must include state abbreviation. 
#' Default to `NULL` which would include all locations with available forecasts.
#' @param data_location character specifying the location of truth data.
#' Currently only supports `"local_hub_repo"`, `"remote_hub_repo"` and `"covidData"`.
#' If `NULL`, default to `"remote_hub_repo"`.
#' @param truth_end_date date to include the last available truth point in 'yyyy-mm-dd' format.
#' If `NULL`,default to system date.
#' @param temporal_resolution character specifying temporal resolution
#' to include: currently support `"weekly"` and `"daily"`.
#' If `NULL`, default to `"weekly"` for cases and deaths, `"daily"` for hospitalizations.
#' Weekly `temporal_resolution` will not be applied to `"inc hosp"` and `"inc flu hosp"`when
#' multiple target variables are specified.
#' `"ECDC"` truth data is weekly by default. Daily level data is not available.
#' @param local_repo_path path to local clone of the hub repository. 
#' Only used when data_location is `"local_hub_repo"`
#' @param hub character, which hub to use. Default is "US". 
#' Other options are "ECDC" and "FluSight".
#'
#' @return data.frame with columns `model`, `target_variable`, `target_end_date`,
#' `location`, `value`, `location_name`, `population` and extra information in these cases
#' \itemize{
#'   \item If `hub = "US"`, it returns extra columns `geo_type`, `geo_value`, `abbreviation` and `full_location_name`.
#'   \item If `truth_source = "ECDC"`, this function returns extra columns `week_start`. However, when `target_variable` is only
#' `inc hosp`, there are no extra columns appended to the resulting data frame.
#' }
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
                       hub = c("US", "ECDC", "FluSight")) {

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

  # warning for as_of
  if (data_location != "covidData" & !is.null(as_of)) {
    if (as_of != Sys.Date()) {
      warning("Warning in load_truth: as_of parameter is only supported when data_location is covidData.
              Will be loading the latest data as of today.")
    }
  }
  
  hub <- match.arg(hub,
                   choices = c("US", "ECDC", "FluSight"),
                   several.ok = TRUE
  )

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

    if (is.null(truth_source)) {
      truth_source <- c("JHU", "HealthData")
    } else {
      # validate truth source
      truth_source <- match.arg(truth_source,
        choices = c("JHU", "NYTimes", "HealthData"),
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
    remote_repo_path <- "https://media.githubusercontent.com/media/reichlab/covid19-forecast-hub/master/"
  } else if (hub[1] == "ECDC") {
    ecdc_default <- FALSE
    if (is.null(target_variable)) {
      ecdc_default <- TRUE
      target_variable <- c("inc case", "inc death", "inc hosp")
    } else {
      # validate target variable
      target_variable <- match.arg(target_variable,
        choices = c(
          "inc case",
          "inc death",
          "inc hosp"
        ),
        several.ok = TRUE
      )
    }

    if (is.null(truth_source)) {
      ecdc_default <- TRUE & ecdc_default
      truth_source <- c("JHU")
    } else {
      # validate truth source
      truth_source <- match.arg(truth_source,
        choices = c("JHU", "ECDC", "OWID"),
        several.ok = TRUE
      )
    }
    
    if (any(c("ECDC", "OWID") %in% truth_source) &
        (any(target_variable %in% c("inc case", "inc death")))) {
      stop("Error in load_truth: ECDC/OWID case and death data are not available.")
    }
    
    # extra checks for truth source if target is inc hosp
    if ("inc hosp" %in% target_variable) {
      if (!any(c("ECDC", "OWID") %in% truth_source) & data_location != "covidData") {
        warning("Warning in load_truth: Incident hospitalization truth data is only available from ECDC or OWID.
              Will be loading data from OWID instead.")
        truth_source <- c(truth_source, "OWID")
        if (!ecdc_default) {
          warning("Warning in load_truth: OWID is added to truth_source.
              Will also be loading data for all selected target_variable from OWID.")
        }
      }
    }

    # get list of all valid locations and codes
    valid_locations <- covidHubUtils::hub_locations_ecdc
    valid_location_codes <- covidHubUtils::hub_locations_ecdc$location

    # store path of remote repo
    remote_repo_path <- "https://raw.githubusercontent.com/epiforecasts/covid19-forecast-hub-europe/main"
  } else if (hub[1] == "FluSight"){
    if (is.null(target_variable)) {
      target_variable <- "inc flu hosp"
    } else {
      # validate target variable
      target_variable <- match.arg(
        target_variable,
        choices = c("inc flu hosp"),
        several.ok = FALSE
      )
    }
    
    if (is.null(truth_source)) {
      truth_source <- "HealthData"
    } else {
      # validate truth source
      truth_source <- match.arg(
        truth_source,
        choices = c("HealthData"),
        several.ok = FALSE
      )
    }
    
    # get list of all valid locations and codes
    valid_locations <- covidHubUtils::hub_locations_flusight
    valid_location_codes <- covidHubUtils::hub_locations_flusight$fips
    
    # store path of remote repo
    remote_repo_path <- "https://raw.githubusercontent.com/cdcepi/Flusight-forecast-data/master"
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
  if (is.null(temporal_resolution) & hub[1] == "US") {
    # only relevant for US hub
    if (length(target_variable) == 1) {
      if (target_variable == "inc hosp") {
        temporal_resolution <- "daily"
      } else {
        temporal_resolution <- "weekly"
      }
    } else {
      temporal_resolution <- "weekly"
    }
  } else if(hub[1] == "FluSight") {
    temporal_resolution <- match.arg(temporal_resolution,
                                     choices = c("weekly"),
                                     several.ok = FALSE
    )
  } else {
    temporal_resolution <- match.arg(temporal_resolution,
      choices = c("weekly", "daily"),
      several.ok = FALSE
    )
  }
  
  # validate truth source for covidData
  if (data_location == "covidData") {
    if (hub[1] == "US") {
      if ("NYTimes" %in% truth_source) {
        stop("Error in load_truth: The truth source you selected is not supported for US data in covidData.")
      }
    } else if (hub[1] == "ECDC") {
      if (any(c("ECDC", "OWID") %in% truth_source)) {
        if ("inc hosp" %in% target_variable) {
          target_variable <- target_variable[target_variable != "inc hosp"]
          truth_source <- truth_source[!truth_source %in% c("ECDC", "OWID")]
          warning("Warning in load_truth: inc hosp data from ECDC/OWID is not available in covidData.
                  Will skip the query for this specific truth data.")
        } else {
          stop("Error in load_truth: The truth source you selected is not supported for ECDC/OWID data in covidData.")
        }
      }
    } else if (hub[1] == "FluSight"){
      stop("Error in load_truth: FluSight truth data is not available in covidData.")
    }
  }


  # validate locations
  if (!is.null(locations)) {
    # Convert location names to fips codes or country abbreviations
    locations <- name_to_fips(locations, hub)
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

  # get all combinations of elements in truth_source and target_variable
  all_combinations <- tidyr::crossing(truth_source, target_variable)
  if (hub[1] == "ECDC") {
    if (ecdc_default) {
      # take out OWID - inc case and OWID - inc death
      all_combinations <- all_combinations %>%
        dplyr::filter(
          (truth_source == "OWID" & target_variable == "inc hosp") |
          (truth_source == "ECDC" & target_variable == "inc hosp") |
          !(truth_source %in% c("OWID", "ECDC")) & target_variable != "inc hosp"
        )
    }
  }

  if (nrow(all_combinations) == 0) {
    return(data.frame())
  }

  # load truth data for each combination of truth_source and target_variable
  truth <- purrr::map2_dfr(
    all_combinations$truth_source, all_combinations$target_variable,
    function(source, target) {
      if ((source %in% c("HealthData", "ECDC", "OWID") & target == "inc hosp") |
        (source != "HealthData" & target != "inc hosp")|
        (target == "inc flu hosp")) {
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
        }
        if (nrow(data) == 0) data <- NULL
        return(data)
      }
    }
  )

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
  if (hub[1] == "US" | hub[1] == "FluSight") {
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
#' be loaded from: currently support "JHU", "NYTimes", "HealthData" and "ECDC".
#' @param repo_path path to local clone or remote of the corresponding prediction hub repository.
#' @param target_variable string specifying target type It should be one or more of
#' "cum death", "inc case", "inc death", "inc hosp".
#' @param data_location character specifying the location of truth data.
#' Currently only supports "local_hub_repo" and "remote_hub_repo".
#' @param hub character, which hub to use. Default is "US". 
#' Other options are "ECDC" and "FluSight".
#'
#' @return character of file path

get_truth_path <- function(source,
                           repo_path,
                           target_variable,
                           data_location,
                           hub = c("US", "ECDC", "FluSight")) {
  # generate full target variable (this works for both hubs as previous
  # checks already made sure only applicable target_variables are used)
  if (target_variable == "cum death") {
    full_target_variable <- "Cumulative Deaths"
  } else if (target_variable == "inc case") {
    full_target_variable <- "Incident Cases"
  } else if (target_variable == "inc death") {
    full_target_variable <- "Incident Deaths"
  } else if (target_variable == "inc hosp" | 
             target_variable == "inc flu hosp") {
    full_target_variable <- "Incident Hospitalizations"
  } 

  if (data_location == "remote_hub_repo") {
    file_name <- paste0(gsub(" ", "%20", full_target_variable), ".csv")
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
    } else if (hub[1] == "FluSight") {
      truth_folder_path <- "/data-truth/truth-"
      file_path <- paste0(repo_path, truth_folder_path, file_name)
    }
  } else if (data_location == "local_hub_repo") {
    file_name <- paste0(full_target_variable, ".csv")
    if (hub[1] == "US") {
      truth_folder_path <- ifelse((tolower(source) == "jhu" || tolower(source) == "healthdata"),
                                  file.path("data-truth","truth-"),
                                  file.path(
                                    "data-truth", tolower(source), "truth_",
                                    tolower(source), "-"
                                  )
      )
      file_path <- file.path(repo_path, truth_folder_path, file_name)
    } else if (hub[1] == "ECDC") {
      truth_folder_path <- file.path("data-truth", toupper(source), paste0("truth_", toupper(source), "-"))
      file_path <- file.path(repo_path, paste0(truth_folder_path, file_name))
    } else if (hub[1] == "FluSight") {
      truth_folder_path <- file.path("data-truth","truth-")
      file_path <- file.path(repo_path, truth_folder_path, file_name)
    }
  }
  return(file_path)
}

#' load truth data from covidData
#'
#' @param target_variable string specifying target type It should be one or more of
#' "cum death", "inc case", "inc death", "inc hosp".
#' @param truth_source character vector specifying where the truths will
#' be loaded from: currently support `"JHU"` and `"HealthData`".
#' @param locations vector of valid location code.
#' The US hub is using FIPS code and the ECDC hub is using country name abbreviation.
#' @param as_of character vector of "as of" dates to use for querying truths in
#' format `'yyyy-mm-dd'`. For each spatial unit and temporal reporting unit, the last
#' available data with an issue date on or before the given `as_of` date are returned.
#' @param temporal_resolution string specifying temporal resolution
#' to include: one of `'daily'` or `'weekly'`
#' @param truth_end_date date to include the last available truth point in `'yyyy-mm-dd'` format.
#' @param hub character, which hub to use. Default is `"US"`, other option is
#' `"ECDC"`
#'
#' @return a data.frame with columns `location`, `target_end_date`, `target_variable` and `value`
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
      target_end_date <= as.Date(truth_end_date)
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

#' load truth data from a local clone of forecast hub repo
#'
#' @param target_variable string specifying target type It should be one or more of
#' `"cum death"`, `"inc case"`, `"inc death"`, `"inc hosp"`.
#' @param truth_source character vector specifying where the truths will
#' be loaded from: currently support `"JHU"`, `"NYTimes"`, `"HealthData"` and `"ECDC"`.
#' @param repo_path path to local clone or remote of the corresponding prediction hub repository.
#' @param temporal_resolution string specifying temporal resolution
#' to include: one of `'daily'` or `'weekly'`
#' @param truth_end_date date to include the last available truth point in `'yyyy-mm-dd'` format.
#' @param data_location character specifying the location of truth data.
#' Currently only supports `"local_hub_repo"` or `"remote_hub_repo"`.
#' @param hub character, which hub to use. Default is "US". 
#' Other options are "ECDC" and "FluSight".
#' @return a data.frame with columns `location`, `target_end_date`, `target_variable` and `value`
#'
load_from_hub_repo <- function(target_variable,
                               truth_source,
                               repo_path,
                               temporal_resolution,
                               truth_end_date,
                               data_location,
                               hub = c("US", "ECDC", "FluSight")) {

  # construct file path and read file from path
  file_path <- get_truth_path(
    source = truth_source,
    repo_path = repo_path,
    target_variable = target_variable,
    data_location = data_location,
    hub = hub
  )
  # load data from file path
  truth_data <- readr::read_csv(file_path, show_col_types = FALSE)

  truth_data <- truth_data %>%
    # add inc_cum and death_case columns
    dplyr::mutate(
      model = paste0("Observed Data (", truth_source, ")"),
      target_variable = target_variable
    )

  # Date columns: ECDC case/death is in ISO weeks
  if ("week_start" %in% names(truth_data)) {
    truth_data <- truth_data %>%
      dplyr::mutate(
        week_start = as.Date(week_start),
        target_end_date = week_start + 5
      ) %>% # to match epiweek
      dplyr::select(
        model, target_variable,
        week_start, target_end_date, # include ISO week start
        location, value
      )
  } else {
    # for daily data, rename date column
    truth_data <- truth_data %>%
      dplyr::mutate(date = as.Date(date,"%Y-%m-%d")) %>%
      dplyr::rename(target_end_date = date) %>%
      dplyr::select(model, target_variable, target_end_date, location, value)
  }

  truth_data <- truth_data %>%
    dplyr::filter(location != "11001",
                  target_end_date >= as.Date("2020-01-25"),
                  target_end_date <= as.Date(truth_end_date))

  # optional aggregation step based on temporal resolution
  # ECDC case/death already in weeks
  if (!"week_start" %in% names(truth_data)) {
    if (temporal_resolution == "weekly" &
      ((hub[1] == "ECDC") |
        # in the US, only loading daily incident hospitalization truth data
        (hub[1] == "US" & target_variable != "inc hosp"))) {
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
  }
  return(truth_data)
}
