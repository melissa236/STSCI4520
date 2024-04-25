# download most recent file from web:
fname <- "CRNH0203-2024-NY_Ithaca_13_E.txt"
url <- paste0("https://www.ncei.noaa.gov/pub/data/uscrn/products/hourly02/2024/",fname)
download.file(url, file.path(data_dir, fname) )

# search for the weather datasets
fnames <- list.files( "../datasets", pattern = "Ithaca" )
fnames

# read in the first dataset to initialize a data frame
dat <- read.table( file.path( data_dir, fnames[1] ), header = FALSE )

# loop over the rest of the files and rbind them
for(j in 2:length(fnames)){
  dat <- rbind(
    dat,
    read.table( file.path( data_dir, fnames[j] ), header = FALSE )
  )}