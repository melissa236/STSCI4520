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


interpolate_to_grid<- function(station_data, grid_points, variable){
  y<-as.matrix(station_data[,variable])
  locs<- as.matrix(station_data[,c("LONGITUDE", "LATITUDE")])
  x<- model.matrix(~LONGITUDE + LATITUDE, data = station_data)
  
  model_fit<- fit_model(y, locs, x, covfun_name = "matern_sphere",silent = TRUE)
  
  X_pred<- as.data.frame(cbind(rep(1,times = nrow(grid_points)),
                               grid_points$LONGITUDE, grid_points$LATITUDE))
  colnames(X_pred)<- c("(Intercept)", "LONGITUDE", "LATITUDE")
  predictions<- predictions(model_fit, locs_pred = grid_points,
                            X_pred = X_pred, covfun_name = "matern_sphere")
  return(predictions)
  
}
