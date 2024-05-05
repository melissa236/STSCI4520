library(dplyr)
library(ggplot2)
library(maps)

create_usa_grid<- function(resolution = 1){

  #map data for contiguous USA
  usa_map<- map("usa", fill = FALSE, plot = TRUE)
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

  return(filtered_grid_points)
}


interpolate_to_grid2<- function(grid_points, variable){
  y<- dat[,"T_DAILY_AVG"]
  locs<-as.matrix(dat[,c("LONGITUDE","LATITUDE")])
  x<-model.matrix(~ > , data = ames_sub)
}

usa_map <- map_data("state")
usa_map <- usa_map[!usa_map$region %in% c("alaska", "hawaii"),]


grid_points <- create_usa_grid(resolution = 1)
interpolation<- interpolate_to_grid(grid_points, "T_DAILY_AVG")

ggplot() +
  geom_polygon(data = usa_map, aes(x = long, y = lat, group = group), fill = "gray", color = "white") +
  geom_point(data = interpolation, aes(x = LONGITUDE, y = LATITUDE, color = T_DAILY_AVG), size = 3) +
  scale_color_viridis_c() +
  labs(title = "Interpolated Temperature Data Over the USA",
       x = "Longitude", y = "Latitude") +
  theme_minimal()







