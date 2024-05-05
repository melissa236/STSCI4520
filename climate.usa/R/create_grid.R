
library(ggplot2)
library(maps)

create_usa_grid<- function(resolution = 1){
  
  #map data for contiguous USA
  usa_map<- map("usa", fill = FALSE, plot = FALSE)
  usa_polygon<- usa_map$range
  #extract longitude and latitude
  x_range<- seq(usa_polygon[1], usa_polygon[2], by = resolution)
  y_range<- seq(usa_polygon[3], usa_polygon[4], by = resolution)
  #generate grid points
  grid_points<- expand.grid(LONGITUDE = x_range, LATITUDE = y_range)
  
  return(grid_points)
}

grid<-create_usa_grid(resolution = 3)


