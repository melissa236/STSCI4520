
library(ggplot2)
library(maps)
library(GpGp)
library(sf)


create_usa_grid<- function(resolution = 1){
  
  #map data for contiguous USA
  usa_map<- map("usa", fill = FALSE, plot = FALSE)
  usa_polygon<- usa_map$range
  #extract longitude and latitude
  x_range<- seq(usa_polygon[1], usa_polygon[2], by = resolution)
  y_range<- seq(usa_polygon[3], usa_polygon[4], by = resolution)
  #generate grid points
  grid_points<- expand.grid(LONGITUDE = x_range, LATITUDE = y_range)
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

grid<-create_usa_grid(resolution = 2)

y<- dat[,"T_DAILY_AVG"]
locs<-as.matrix(dat[,c("LONGITUDE","LATITUDE")])
x<-model.matrix(~ LONGITUDE + LATITUDE, data = dat)


lms<- fit_model(y, locs, x, covfun_name = "matern_sphere",silent = TRUE)


interpolate_to_grid<- function(grid_points, model_fit){
  
  X_pred<- as.data.frame(cbind(rep(1,times = nrow(grid_points)),
            grid_points$LONGITUDE, grid_points$LATITUDE))
  colnames(X_pred)<- c("(Intercept)", "LONGITUDE", "LATITUDE")
  predictions<- predictions(model_fit, locs_pred = grid_points,
                            X_pred = X_pred, covfun_name = "matern_sphere")
  return(predictions)
  
}

predictions<-interpolate_to_grid(grid_points = grid, model_fit = lms)
usa_map <- map_data("state")
usa_map <- usa_map[!usa_map$region %in% c("alaska", "hawaii"),]


grid_points <- create_usa_grid(resolution = 1)
interpolation<- interpolate_to_grid(grid_points, "T_DAILY_AVG")


create_map<- function(predictions, grid_points){
  num_lon <- length(unique(grid_points$LONGITUDE))
  num_lat <- length(unique(grid_points$LATITUDE))
  
  unique_lon<- unique(grid_points$LONGITUDE)
  unique_lat<- unique(grid_points$LATITUDE)
  
  loc_matrix <- matrix(predictions, nrow = num_lat, ncol = num_lon, byrow = TRUE)
  
  image.plot(x = unique_lon, 
             y = unique_lat,
             z = loc_matrix,
             xlab = "Longitude", 
             ylab = "Latitude", 
             main = "Predicted Average Temperatures") 
  map("usa", fill = FALSE, plot = TRUE, add = TRUE)
}

create_map(predictions, grid_points = grid)




