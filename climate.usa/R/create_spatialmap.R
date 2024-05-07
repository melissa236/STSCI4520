
library(ggplot2)
library(maps)
library(GpGp)
library(sf)
library(dplyr)


create_usa_grid<- function(resolution = 1){
  
  #map data for contiguous USA
  usa_map<- map("usa", fill = FALSE, plot = FALSE)
  usa_polygon<- usa_map$range
  #extract longitude and latitude
  x_range<- seq(usa_polygon[1], usa_polygon[2], by = resolution)
  y_range<- seq(usa_polygon[3], usa_polygon[4], by = resolution)
  #generate grid points
  grid_points<- expand.grid(LONGITUDE = y_range, LATITUDE = x_range)
  coords<- matrix(c(usa_polygon[1], usa_polygon[3], 
                   usa_polygon[1], usa_polygon[4], 
                  usa_polygon[2], usa_polygon[4], 
                  usa_polygon[2], usa_polygon[3], 
                  usa_polygon[1], usa_polygon[3]),
                  ncol = 2, byrow = TRUE)
  #usa_boundary<- st_polygon(list(coords))
  
  # Convert grid points to a spatial object
  # grid_points_sf <- st_as_sf(all_grid_points, 
  #                            coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  # 
  # # Check which points fall within the boundary of the contiguous USA
  # within_boundary <- st_within(grid_points_sf, usa_boundary)
  # 
  # # Filter out grid points that are within the boundary
  # grid_points<- all_grid_points[within_boundary, ]
  # 
  
  
  
  return(grid_points)
}

grid<-create_usa_grid(resolution = 1)

y<- dat[,"T_DAILY_AVG"]
locs<-as.matrix(dat[,c("LONGITUDE","LATITUDE")])
x<-model.matrix(~ LONGITUDE + LATITUDE, data = dat)



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
usa_map <- map_data("state")
usa_map <- usa_map[!usa_map$region %in% c("alaska", "hawaii"),]


grid_points <- create_usa_grid(resolution = 1)

  

create_map<- function(predictions, grid_points){
  num_lat<- length(unique(grid_points$LATITUDE))
  num_lon<- length(unique(grid_points$LONGITUDE))
  
  loc_matrix <- matrix(predictions, nrow = num_lon, ncol = num_lat, byrow = TRUE)
  
  image.plot(x = unique(grid_points$LATIUDE), 
             y = unique(grid_points$LONGITUDE),
             z = loc_matrix,
             xlab = "Longitude", 
             ylab = "Latitude", 
             main = "Predicted Average Temperatures") 
  map("usa", fill = FALSE, plot = TRUE, add = TRUE)
}

create_map(predictions, grid_points = grid)




