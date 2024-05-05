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



interpolate_to_grid<- function(grid_points, model_fit){
  
 avg_p<- mean(dat$P_DAILY_CALC, na.rm = TRUE)
 avg_sol<- mean(dat$SOLARAD_DAILY, na.rm = TRUE)
 row1<- c(1, avg_p, avg_sol ) 
 X_pred<- as.data.frame(matrix(rep(row1, times =2500),nrow =2500, byrow =TRUE))
 colnames(X_pred)<- c("(Intercept)", "AVG_P_DAILY_CALC", "AVG_SOLARAD_MIN")
 predictions<- predictions(lms, locs_pred = grid_points,
                           X_pred = X_pred, covfun_name = "matern_sphere")
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







