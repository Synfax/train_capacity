library(devtools)
devtools::install_github("walkerke/mapgl")
library(mapgl)
library(sf)
library(tidyverse)
library(data.table)
library(leaflet)
library(quarto)
library(dtplyr)
library(janitor)
library(arrow)


patronage_data = load_and_clean_patronage_data()

hourly_factors = generate_hourly_information()

service_frequences = generate_service_frequency_information()

generage_station_location_info()

train_stations <- read_sf('shapefiles/ptv/PTV_METRO_TRAIN_STATION.SHP')

load_and_clean_patronage_data <- function() {
  
  patronage_data <- fread('data/Train_Service_Passenger_Counts_Financial_Year_2022-2023.csv') %>%
    as.data.frame()
  
  city_loop_stations = c('Flagstaff', 'Parliament', 'Melbourne Central', 'Flinders Street', 'Southern Cross')
  interchange_stations = c('North Melbourne', 'South Yarra', 'Richmond')
  weekends = c('Saturday', 'Sunday')
  
  patronage_data = patronage_data %>%
    mutate( capacity_factor = Passenger_Departure_Load / 600 ) %>%
    mutate(hour_of_day = sub("\\:.*", "", Arrival_Time_Scheduled)) %>%
    mutate(isCityLoop =  Station_Name %in% city_loop_stations  ) %>%
    mutate(isInterchange = Station_Name %in% interchange_stations,
           isWeekend = Day_of_Week %in% weekends,
           isWeekday = !isWeekend) %>%
    mutate(Direction = case_when( Direction == "U" ~ "Towards Flinders", Direction == "D" ~ "Away from Flinders" ) )
  
  return(patronage_data)
}



## per station data

generate_hourly_information = function() {
  
  hourly_factors <- patronage_data %>%
    group_by(hour_of_day, Station_Name, Direction, Day_Type) %>%
    summarise(capacity_factor = mean(capacity_factor), avg_patronage = mean(Passenger_Departure_Load)) %>%
    as.data.frame()
  
  saveRDS(hourly_factors, 'r_objects/hourly_factors.Rdata')
  
  return(hourly_factors)
  
}
generate_service_frequency_information = function() {
  
  number_of_each_days <- patronage_data %>%
    group_by(Business_Date, Day_Type, Station_Name) %>%
    summarise(n = n()) %>%
    ungroup() %>%
    count(Day_Type, Station_Name)
  
  service_frequencies <- patronage_data %>%
    group_by(hour_of_day, Day_Type, Direction, Station_Name, Line_Name) %>%
    summarise( services_per_hour = n() )  %>%
    left_join(number_of_each_days, by = c('Day_Type', 'Station_Name')) %>%
    rowwise() %>%
    mutate(sph = services_per_hour / n) %>%
    group_by(Station_Name, hour_of_day, Day_Type, Direction) %>%
    summarise(sph = sum(sph)) %>%
    ungroup() %>%
    as.data.frame()
  
  saveRDS(service_frequencies, 'r_objects/service_frequencies.Rdata' )
  
  return(service_frequencies)
  
}


#save lats

generate_station_location_info = function() {
  locations = patronage_data %>%
    group_by(Station_Name) %>%
    summarise(lat = first(Station_Latitude), lng = first(Station_Longitude))
  
  saveRDS(locations, '../r_objects/locations.Rdata')
}


## do line calculations

generate_line_information <- function() {
  
  
  by_line_data = patronage_data %>%
    group_by(Line_Name, isWeekend, Direction) %>% 
    summarise(total_boardings = sum(Passenger_Boardings), total_alightings =sum(Passenger_Alightings)) %>%
    right_join(patronage_data, by = c('Line_Name', 'isWeekend', 'Direction') )
  
  
  by_line_data = by_line_data %>%
    group_by(Line_Name, Station_Name, Direction, isWeekend) %>%
    summarise(pc_boardings = sum(Passenger_Boardings) / first(total_boardings) ,
              pc_alightings = sum(Passenger_Alightings) / first(total_alightings),
              Station_Chainage = first(Station_Chainage),
              median_departing_load = median(Passenger_Departure_Load),
              n = n()) 
  
  saveRDS(by_line_data, '../r_objects/by_line_data.Rdata')
  
  
  
}
