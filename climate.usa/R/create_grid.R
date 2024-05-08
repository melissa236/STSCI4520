
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

create_usa_grid<- function(resolution = 1){

  #load map data for contiguous USA and convert to sf
  usa_map<- st_as_sf(map("usa", fill = TRUE, plot = FALSE))
  usa_map <- usa_map[!usa_map$ID %in% c("martha's vineyard", "nantucket island", "manhattan", 
                                            "staten island", "long island", "san juan island", 
                                            "lopez island", "orcas island", "whidbey island"), ]
  usa_map<-as(usa_map, "Spatial")
  
  #generate grid points within bounding box and convert to sf
   bbox<- bbox(usa_map)
   grid<-expand.grid(
     LONGITUDE = seq(from = bbox[1, 1], to = bbox[1, 2], by = resolution),
   LATITUDE = seq(from = bbox[2, 1], to = bbox[2, 2], by = resolution)
   )
 
   # generated grid points outside contiguous us
   
   # inside <- point.in.polygon(grid$LONGITUDE, grid$LATITUDE, 
   #                            usa_map@polygons[[1]]@Polygons[[1]]@coords[,1], 
   #                            usa_map@polygons[[1]]@Polygons[[1]]@coords[,2])
   # grid_points <- grid[inside > 0, ]
   
   return(grid_points)
}







