library(devtools)
devtools::install_github("walkerke/mapgl")
library(mapgl)
library(sf)
library(tidyverse)
library(data.table)
library(leaflet)
library(quarto)

train_stations <- read_sf('shapefiles/ptv/PTV_METRO_TRAIN_STATION.SHP')

leaflet(train_stations) %>%
  addProviderTiles('CartoDB.Positron') %>%
  addMarkers()

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


## per station data

hourly_factors <- patronage_data %>%
  group_by(hour_of_day, Station_Name, Direction) %>%
  summarise(capacity_factor = mean(capacity_factor), avg_patronage = mean(Passenger_Departure_Load)) %>%
  as.data.frame()

saveRDS(hourly_factors, '../r_objects/hourly_factors.Rdata')


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






# patronage_by_station <- patronage_data %>%
#   filter(Mode=='Metro') %>%
#   group_by(Station_Name) %>%
#   summarise(avg_patronage = mean(Passenger_Departure_Load),
#             lat = first(Station_Latitude),
#             lon = first(Station_Longitude) ) %>%
#   st_as_sf(., coords = c("lon", "lat"))
# 
# pal = colorNumeric('Reds', domain = patronage_by_station$avg_patronage)

#save lats

locations = patronage_data %>% group_by(Station_Name) %>% summarise(lat = first(Station_Latitude), lng = first(Station_Longitude))

saveRDS(locations, '../r_objects/locations.Rdata')
# leaflet(patronage_by_station) %>%
#   addProviderTiles('CartoDB.Positron') %>%
#   addCircleMarkers(fillColor = ~pal(avg_patronage), fillOpacity = 1, stroke = NA) %>%
#   addLegend(position = 'bottomright', pal = pal, values = patronage_by_station$avg_patronage, title = 'Average number of people on a departing train')
# 
# plotStressLevels <- function(station_name) {
#   to_plot <- hourly_factors %>% filter(Station_Name == station_name)
#   
#   p <- ggplot(to_plot, mapping = aes(x = hour_of_day, y = avg_patronage)) + geom_point()
#   
#   p + facet_grid(rows = vars(dir)) + theme_bw(base_size = 15)
# }

## do line calculations

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

