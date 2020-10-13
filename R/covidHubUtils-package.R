#' covidHubUtils - Utility functions for the COVID-19 forecast hub
#'
#' @author Evan L. Ray <elray@umass.edu>
#' @docType package
#' @importFrom dplyr `%>%`
#' @name covidHubUtils
#' @aliases covidHubUtils covidHubUtils-package
NULL


##' @name hub_locations
##' @title Information on locations in the COVID-19 Forecast Hub
##' @docType data
##' @format A data frame with information and metadata about national, state, and county locations for the US. 3,200 observations on the following 6 variables. 
##' \describe{
##'   \item{\code{fips}}{the FIPS code for each location}
##'   \item{\code{location_name}}{A non-abbreviated name for each location}
##'   \item{\code{population}}{the population of each location, taken from the JHU CSSE dataset}
##'   \item{\code{geo_type}}{geographical level, either "state" or "county", should match covidcast API definition}
##'   \item{\code{geo_value}}{two-character lowercase abbreviation for each state or a five-character fips code, to match covidcast API definition}
##'   \item{\code{abbreviation}}{two-character uppercase state abbreviation (i.e. the postal abbreviations) for each location}
##' }
##' @usage data(hub_locations)
##' @source JHU CSSE COVID Dashboard (https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data), covidcast API (https://github.com/cmu-delphi/covidcast), COVID-19 Forecast Hub (https://covid19forecasthub.org/).
##' @keywords datasets
NULL