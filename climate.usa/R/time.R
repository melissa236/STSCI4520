#'
#'
#'
#'
#'
#'
#'
#'
#'
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

