#' Available locations in the US Hub
#'
#' Data set with available locations for the US hub
#'
#' @format A data frame with 3200 rows and 6 columns:
#' \describe{
#'   \item{fips}{FIPS code}
#'   \item{location_name}{Location name}
#'   \item{population}{Location population}
#'   \item{geo_type}{Type of location}
#'   \item{geo_value}{Location abbreviation or FIPS code}
#'   \item{abbreviation}{Corresponding state abbrevaition}
#'   \item{full_location_name}{Full location name with correponding state}
#' }
"hub_locations"

#' Available locations in the ECDC Hub
#'
#' Data set with available locations for the ECDC hub
#'
#' @format A data frame with 32 rows and 3 columns:
#' \describe{
#'   \item{location_name}{Name of the location}
#'   \item{location}{Location abbreviation}
#'   \item{population}{Location population}
#' }
"hub_locations_ecdc"

#' Available targets in the US Hub
#'
#' List with 441 elements
"hub_targets_us"

#' Available targets in the ECDC Hub
#'
#' List with 441 elements
"hub_targets_ecdc"
