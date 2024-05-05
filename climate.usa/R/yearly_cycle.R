library(dplyr)

yearly_cycle<- function(station){
  df<- dat |>
    #filter by station name
    filter(STATION_NAME == station)|>
    #extract year from lst_date and calculate day of year 
    mutate(
      year = substr(as.character(LST_DATE), 1, 4) ,
           start_of_year = as.Date(paste0(year, "-01-01")),
           day_of_year = as.numeric(LST_DATE - start_of_year) + 1
      ) |>
  select( day_of_year, T_DAILY_AVG, year) |>
    na.omit()|>
    group_by(day_of_year) |>
    summarize(T_DAILY_AVG = mean(T_DAILY_AVG))
  
    
}

View(yearly_cycle("Asheville 13 S"))

