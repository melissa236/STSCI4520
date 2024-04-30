library(tidyverse)

# search for all weather datasets
fnames <- list.files( "data/all_files", full.names = TRUE, recursive =TRUE, pattern = "\\.txt$" )
fnames

# read in the first dataset to initialize a data frame
dat <- read.table( fnames[1], header = FALSE)
filename<- basename(fnames[j])
state<- str_extract(filename, "(?<=-)[A-Z]{2}" )
station_name<- str_extract(filename, "(?<=-)[A-Z]{2}(?=_SSE\\.txt$)") #doesn't work
dat$state<- state
dat$station_name<- station_name


# loop over the rest of the files and rbind them
for(j in 2:length(fnames)){
  df<- read.table( fnames[j] , header = FALSE )
  filename<- basename(fnames[j])
  state<- str_extract(filename, "(?<=-)[A-Z]{2}" )
  station_name<- str_extract(filename, "(?<=-)[A-Za-z0-9_]+(?=_SSE\\.txt$)")
  
}
#set column names
colnames(dat)<-c("wbanno","lst_date","crx_vn","longitude","latitude","t_daily_max",
                             "t_daily_min","t_daily_mean","t_daily_avg","p_daily_calc",
                             "solarad_daily","sur_temp_daily_type", "sur_temp_daily_max",
                             "sur_temp_daily_min",  "sur_temp_daily_avg", "rh_daily_max", 
                 "rh_daily_min","rh_daily_avg","soil_moisture_5", "soil_moisture_10", "soil_moisture_20",
                             "soil_moisture_50", "soil_moisture_100", "soil_temp_5",
                             "soil_temp_10", "soil_temp_20", "soil_temp_50", "soil_temp_100","state",
                 "station_name")