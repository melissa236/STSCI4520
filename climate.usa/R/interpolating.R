library(dplyr)
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


interpolate_to_grid <- function(grid_points, variable) {

  interpolated_values <- numeric(nrow(grid_points))
  for (i in 1:nrow(grid_points)) {
    point <- grid_points[i, ]
    #calculate distance
    distances <- sqrt((dat$LONGITUDE - point$LONGITUDE)^2 + (dat$LATITUDE - point$LATITUDE)^2)
    #idw formula
    weights <- ifelse(distances > 0, 1 / (distances^2), 0)
    weighted_values <- weights * dat[[variable]]
    interpolated_values[i] <- sum(weighted_values, na.rm = TRUE) / sum(weights, na.rm = TRUE)
  }

  grid_points[[variable]] <- interpolated_values
  return(grid_points)
}

grid_points <- create_usa_grid(resolution = 1)
interpolation<- interpolate_to_grid(grid_points, "T_DAILY_AVG")
ggplot(interpolation, aes(x = LONGITUDE, y = LATITUDE, color = T_DAILY_AVG)) +
  geom_point() +
  scale_color_viridis_c()






