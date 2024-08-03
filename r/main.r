library(devtools)
devtools::install_github("walkerke/mapgl")
library(mapgl)
library(sf)
library(tidyverse)
library(data.table)
library(leaflet)

train_stations <- read_sf('shapefiles/ptv/PTV_METRO_TRAIN_STATION.SHP')


leaflet(train_stations) %>%
  addProviderTiles('CartoDB.Positron') %>%
  addMarkers()

patronage_data <- fread('data/Train_Service_Passenger_Counts_Financial_Year_2022-2023.csv') %>%
  as.data.frame()

city_loop_stations = c('Flagstaff', 'Parliament', 'Melbourne Central', 'Flinders Street', 'Southern Cross')
interchange_stations = c('North Melbourne', 'South Yarra', 'Richmond')

patronage_data = patronage_data %>%
  mutate( capacity_factor = Passenger_Departure_Load / 600 ) %>%
  mutate(hour_of_day = sub("\\:.*", "", Arrival_Time_Scheduled)) %>%
  mutate(isCityLoop =  )

hourly_factors <- patronage_data %>%
  group_by(hour_of_day, Station_Name) %>%
  summarise(capacity_factor = mean(capacity_factor), avg_patronage = mean(Passenger_Departure_Load))

patronage_by_station <- patronage_data %>%
  filter(Mode=='Metro') %>%
  group_by(Station_Name) %>%
  summarise(avg_patronage = mean(Passenger_Departure_Load), lat = first(Station_Latitude), lon = first(Station_Longitude)) %>%
  st_as_sf(., coords = c("lon", "lat"))

pal = colorNumeric('Reds', domain = patronage_by_station$avg_patronage)

leaflet(patronage_by_station) %>%
  addProviderTiles('CartoDB.Positron') %>%
  addCircleMarkers(fillColor = ~pal(avg_patronage), fillOpacity = 1, stroke = NA) %>%
  addLegend(position = 'bottomright', pal = pal, values = patronage_by_station$avg_patronage, title = 'Average number of people on a departing train')

plotStressLevels <- function(station_name) {
  to_plot <- hourly_factors %>% filter(Station_Name == station_name)
  
  plot(to_plot$hour_of_day, to_plot$avg_patronage, type = 'l')
}
