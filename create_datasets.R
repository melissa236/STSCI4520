library(tidyverse)

# search for all weather datasets
fnames <- list.files( "data/all_files", full.names = TRUE, recursive =TRUE, pattern = "\\.txt$" )
fnames

# read in the first dataset to initialize a data frame
dat <- read.table( fnames[1], header = FALSE)
filename<- basename(fnames[1])
state<- str_extract(filename, "(?<=-)[A-Z]{2}" )
#filename <- "CRND0103-2013-AL_Gainesville_2_NE.txt"
station_name<- str_extract(filename, "(?<=-[A-Z]{2}_)([A-Za-z0-9_]+)(?=\\.txt)") #doesn't work
station_name <- str_replace_all(station_name, "_", " ")
dat$state<- state
dat$station_name<- station_name


# loop over the rest of the files and rbind them
for (j in 2:length(fnames)) {
  df <- read.table(fnames[j], header = FALSE)
  filename <- basename(fnames[j])
  df$state <- str_extract(filename, "(?<=-)[A-Z]{2}")
  df$station_name <- str_extract(filename, "(?<=-[A-Z]{2}_)([A-Za-z0-9_]+)(?=\\.txt)")
  df$station_name <- str_replace_all(station_name, "_", " ")
  dat <- rbind(dat, df)  
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
colnames(dat) <- toupper(colnames(dat))

#convert date
dat$LST_DATE <- as.Date(as.character(dat$LST_DATE), format = "%Y%m%d")
#Clean data
dat[dat == -9 | dat == -99 | dat == -999 | dat == -9999] <- NA

