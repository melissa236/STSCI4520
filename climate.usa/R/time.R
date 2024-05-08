#' Extract the time series
#'
#' Eextract the time series for a specific station by station id.
#'
#' @param name station name contains the city name, station number, and station direction
#' @param starting_date date in date format of range you want to start with
#' @param ending_date date in date format of range you want to end with
#'
#' @return a data frame with the following components:
#' \itemize{
#' \item \code{type} data for the dates in selected ranges
#' }
#'@examples
#' data for Asheville 13 S since January 3 2005
#' print(get_time_series("Asheville 13 S", "2005-01-03"))
#' data for Asheville 13 S since January 3 2006 until January 3 2009
#' print(get_time_series("Asheville 13 S", "2006-01-03", "2009-01-03"))
#' @export
library(dplyr)
get_time_series <- function(name, starting_date = NULL, ending_date = NULL) {
  station_data <- dat %>%
    filter(STATION_NAME == name)

  if (!is.null(starting_date)) {
    station_data <- station_data %>%
      filter(LST_DATE >= as.Date(starting_date))
  }

  if (!is.null(ending_date)) {
    station_data <- station_data %>%
      filter(LST_DATE <= as.Date(ending_date))
  }

  return(station_data)
}
#print(get_time_series("Asheville 13 S"))
#print(get_time_series("Asheville 13 S", "2005-01-03"))
#print(get_time_series("Asheville 13 S", "2006-01-03", "2009-01-03"))
#print(get_time_series("Asheville 8 SSW"))

