
#' Estimate the yearly cycle for one weather station
#'
#' Estimate the expected temperature on each day of the year
#'for one USCRN/USRCRN weather station. 
#'
#' @param station name of the station; contains the city name, station number, and station direction
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

create_map<- function(predictions, grid_points){
  num_lat<- length(unique(grid_points$LATITUDE))
  num_lon<- length(unique(grid_points$LONGITUDE))
  
  loc_matrix <- t(matrix(predictions, nrow = num_lat, ncol = num_lon, byrow = TRUE))
  
  image.plot(x = unique(grid_points$LONGITUDE),
             y = unique(grid_points$LATITUDE),
             z = loc_matrix,
             xlab = "Longitude",
             ylab = "Latitude",
             main = "Predicted Average Temperatures")
}