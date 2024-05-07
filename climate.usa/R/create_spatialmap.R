
library(ggplot2)
library(maps)
library(GpGp)
library(dplyr)


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

grid<-create_usa_grid(resolution = 1)

station_data<- dat |>
  group_by(STATION_NAME)|>
  na.omit()|>
  summarise(LONGITUDE = mean(LONGITUDE),
            LATITUDE = mean(LATITUDE),
            T_DAILY_AVG = mean(T_DAILY_AVG))


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

predictions<-interpolate_to_grid(grid_points = grid,
                                 station_data = station_data, variable = "T_DAILY_AVG")


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
  map("usa", fill = FALSE, plot = TRUE, add = TRUE)
}

create_map(predictions, grid_points = grid)




