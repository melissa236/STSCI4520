#' Estimate the yearly cycle for one weather station
#'
#' Estimate the expected temperature on each day of the year
#'for one USCRN/USRCRN weather station. 
#'
#' @param station station name contains the city name, station number, and station direction
#' @param year year in which the expected temperature on each day will be estimated
#' @return a data frame with the following components:
#' \itemize{
#' \item \code{type} data frame with column for day number (1-365), and a column for the expected average temperature on each day. 
#' }
#'@examples
#' #get yearly cycle for weather station Asheville 13 S
#' station_info<- yearly_cycle("Asheville 13 S")
#' print(station_info)
#' @export

yearly_cycle<- function(station, year){
  df<- dat |>
    #filter by station name
    filter(STATION_NAME == station)|>
    #extract year from lst_date and calculate day of year 
    mutate(
      YEAR = substr(as.character(LST_DATE), 1, 4) ,
           start_of_year = as.Date(paste0(year, "-01-01")),
           day_of_year = as.numeric(LST_DATE - start_of_year) + 1
      ) |>
    filter(YEAR == year)
  select( day_of_year, T_DAILY_AVG, year) |>
    na.omit()|>
    group_by(day_of_year) |>
    summarize(T_DAILY_AVG = mean(T_DAILY_AVG))
  
    
}



